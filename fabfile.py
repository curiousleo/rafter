from __future__ import with_statement

from os.path import split as split_path
from tempfile import NamedTemporaryFile
from uuid import uuid4

from fabric.api import cd
from fabric.api import env
from fabric.api import parallel
from fabric.api import run
from fabric.api import task
from fabric.context_managers import settings

from awsfabrictasks.decorators import ec2instance
from awsfabrictasks.ec2.api import Ec2InstanceWrapper
from awsfabrictasks.ec2.api import Ec2LaunchInstance
from awsfabrictasks.ec2.api import ec2_rsync_download
from awsfabrictasks.ec2.api import ec2_rsync_upload

@task
def benchmark():
    '''
    Start benchmarks.

    Sweeps over the configuration space, starting and stopping instances as
    appropriate.
    '''
    cluster_sizes = [3, 5, 7, 9, 11, 12, 13, 15, 16, 17, 19, 20]
    protocols = ['majority', 'grid', ('tree', 2), ('tree', 3)]
    failure_modes = ['no_failures',
            ('repeated', 0.8), ('repeated', 0.6), ('repeated', 0.4)]
    followers = []
    for (prev_cluster_size, cluster_size) in \
            zip([0] + cluster_sizes[:-1], cluster_sizes):
        followers += start(cluster_size - prev_cluster_size)
        for protocol in protocols:
            for failure_mode in failure_modes:
                configure(followers, protocol, failure_mode)
                memaslap()
                collect_results()
    stop(followers)

@task
def start(num, config='arch-configured-micro', environment='benchmark'):
    '''
    Start a number of instances and set them up for benchmarking.

    :param num: The number of instances to launch.
    :param config: The configuration to use for the new instances.
    :param environment: The value of the 'Environment' tag for the new instances.
    '''
    launch = lambda name: \
        Ec2LaunchInstance(
            configname=config,
            extra_tags={'Environment': environment, 'Name': name})
    names = ['follower-' + uuid4() for _ in range(int(num))]
    instances = map(launch, names)
    Ec2LaunchInstance.run_many_instances(instances)
    Ec2LaunchInstance.wait_for_running_state_many(instances)
    # dns_names = [instance['public_dns_name'] for instance in instances]
    # return zip(names, dns_names)
    return instances

def configure(followers, protocol, failure_mode):
    current_instance = Ec2InstanceWrapper.get_from_host_string().instance
    run(configure_script(followers, protocol, failure_mode))

@task
@parallel
# @ec2instance(tags={'Environment':'benchmark'})
def deploy(branch='benchmark'):
    '''
    Update an existing deployment Git repository.

    :param branch: The branch to reset to.
    '''
    remote = 'origin'
    with cd(awsfab_settings.RAFTER_DIR):
        run('git fetch {remote}'.format(**locals()))
        run('git reset --hard {remote}/{branch}'.format(**locals()))
        run('rm ebin/*')
        run('make')

@task
@parallel
def stop_cluster():
    '''
    Stop and clean up a cluster.
    '''
    with cd(awsfab_settings.RAFTER_DIR):
        with settings(warn_only=True):
            run('killall beam')
        run('rm -rf data')
        run('mkdir data')

@task
@parallel
def start_followers(leader_name='leader'):
    '''
    Start follower Erlang nodes.

    :param leader_name: The name of the leader node (which is handled by start_leader).
    '''
    instance = Ec2InstanceWrapper.get_from_host_string().instance
    name = instance.tags.get('Name')

    if name != leader_name:
        # instance is a follower
        with cd(awsfab_settings.RAFTER_DIR):
            run('rm -rf data')
            run('mkdir data')
            run('sh bin/start-ec2-node {name}; sleep 1'.format(**locals()))

@task
def start_leader(test, leader_name='leader'):
    '''
    Start leader Erlang node.

    :param test_name: The name of this test run (required).
    :param leader_name: The name of the leader node.
    '''
    current_instance = Ec2InstanceWrapper.get_from_host_string().instance

    if current_instance.tags.get('Name') == leader_name:
        # instance is the leader
        instances = [instancewrapper.instance for instancewrapper
                in env.ec2instances.values()]
        followers = [instance for instance in instances
                if instance.tags.get('Name') != leader_name]

        if len(followers) == 0:
            print 'No followers! Quitting ...'
            return

        # create startup script for leader
        script = leader_script(current_instance, followers)
        script_name = None
        with NamedTemporaryFile() as script_file:
            script_name = split_path(script_file.name)[1]
            script_file.write(script)
            script_file.flush()
            ec2_rsync_upload(script_file.name, awsfab_settings.SCRIPT_DIR)
        with cd(awsfab_settings.RAFTER_DIR):
            run('rm -rf data')
            run('mkdir data')
            run('sh bin/{script_name}'.format(**locals()))
            run('mv /tmp/{{rafter_ttc_log.log,{test}.log}}'.format(**locals()))
        ec2_rsync_download('/tmp/{test}.log'.format(**locals()), '/home/leo/Temp')

def configure_script(followers, protocol, failure_mode):
    follower_names = [follower.tags.get('Name') for follower in followers]
    follower_ips = [follower.ip_address for follower in followers]
    follower_tuples = ['{{{name},\'{name}@{ip}\'}}'.format(**locals())
            for (name, ip) in zip(follower_names, follower_ips)]

    followers = '[{followers}]'.format(followers=','.join(follower_tuples))
    protocol = to_erlang_tuple('{protocol}'.format(**locals()))
    message = 'start_benchmark, {followers}, {protocol}' \
            .format(**locals())
    start_benchmark = 'gen_fsm:send_all_state_event(leader, {{{message}}})' \
            .format(**locals())
    set_failure_mode = failure_mode_code('{failure_mode}'.format(**locals()))

    command = '{start_benchmark},{set_failure_mode}'.format(**locals())
    return 'erl -setcookie rafter_localhost_test -eval "{command}."' \
            .format(**locals())

def leader_script(leader, followers, protocol, failure_mode):
    generator_call = generator_code(protocol)
    failure_command = failure_mode_code(failure_mode)
    follower_names = [follower.tags.get('Name') for follower in followers]
    follower_ips = [follower.ip_address for follower in followers]
    follower_tuples = ['{{{name},\'{name}@{ip}\'}}'.format(**locals())
            for (name, ip) in zip(follower_names, follower_ips)]

    leader_tuple = '{{leader,\'leader@{ip}\'}}' \
            .format(ip=leader.ip_address)
    assign = 'Peers=[{followers},{leader}]' \
            .format(followers=','.join(follower_tuples), leader=leader_tuple)
    create_vstruct = 'Vstruct={generator_call}'.format(**locals())
    set_config = 'rafter:set_config(leader,Vstruct)'

    config = '{assign},{create_vstruct},{set_config}'.format(**locals())
    return '''
cd /root/Code/rafter.git
IP=$(curl --silent http://instance-data/latest/meta-data/public-ipv4)
erl \
-pa deps/*/ebin ebin \
-setcookie rafter_localhost_test \
-name leader@$IP \
-eval "rafter:start_test_node(leader),{config},{failure_command}."
'''.format(**locals())

def generator_code(protocol, peers_var):
    if isinstance(protocol, tuple):
        (protocol, param) = protocol
        return 'rafter_voting_{protocol}:{protocol}({peers_var}, {param})' \
                .format(**locals())
    else:
        return 'rafter_voting_{protocol}:{protocol}({peers_var})' \
                .format(**locals())

def failure_mode_code(failure_mode):
    failures_modes = ['no_failures',
            ('repeated', 0.8), ('repeated', 0.6), ('repeated', 0.4)]
    lambda = 10
    if isinstance(failure_mode, tuple):
        (failure_mode, param) = failure_mode
        if failure_mode == 'repeated':
            message = 'send_start_repeated_failures, {lambda}, {param}' \
                    .format(**locals())
            return 'gen_fsm:send_all_state_event(leader, {{{message}}})' \
                    .format(**locals())
    else:
        if failure_mode == 'no_failures':
            return 'ok'

def to_erlang_tuple(tup):
    return '{tup}'.format(**locals()) \
            .replace("'", '').replace('(', '{').replace(')', '}')

#####################
# Import awsfab tasks
#####################
from awsfabrictasks.ec2.tasks import *
from awsfabrictasks.regions import *
from awsfabrictasks.conf import *
