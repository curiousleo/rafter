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
    leader = Ec2InstanceWrapper.get_by_nametag('leader').instance

    cluster_sizes = [3, 5, 7, 9, 11, 12, 13, 15, 16, 17, 19, 20]
    protocols = ['majority', 'grid', ('tree', 2), ('tree', 3)]
    failure_modes = ['no_failures',
            ('repeated', 0.8), ('repeated', 0.6), ('repeated', 0.4)]

    followers = []
    for (prev_cluster_size, cluster_size) in \
            zip([0] + cluster_sizes[:-1], cluster_sizes):
        followers += start_followers(cluster_size - prev_cluster_size)
        for protocol in protocols:
            for failure_mode in failure_modes:
                configure(followers, protocol, failure_mode)
                memaslap(leader['public_dns_name'])
                collect_results()
    stop(followers)

@task
def start_followers(num, config='arch-configured-micro', environment='benchmark'):
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

    dns_names = [instance['public_dns_name'] for instance in instances]
    execute(deploy, hosts=dns_names)
    execute(start_node, hosts=dns_names)

    return instances

@task
def configure(followers, protocol, failure_mode):
    script = configure_script(followers, protocol, failure_mode)
    run('{script}; sleep 1'.format(**locals()))

@task
def memaslap(leader_address, runtime=60, conf='memaslap.conf'):
    run('memaslap \
            --servers={leader_address}:11211 --binary \
            --stat_freq={runtime}s --time={runtime}s \
            --cfg_cmd={conf}' \
            .format(**locals()))

@task
@parallel
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
def start_node(name):
    '''
    Start follower Erlang nodes.

    :param name: The name of this node.
    '''
    with cd(awsfab_settings.RAFTER_DIR):
        run('rm -rf data')
        run('mkdir data')
        run('sh bin/start-ec2-node {name}; sleep 1'.format(**locals()))
    return Ec2InstanceWrapper.get_from_host_string().instance

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
