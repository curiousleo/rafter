from __future__ import with_statement

from os.path import split as split_path
from tempfile import NamedTemporaryFile
from uuid import uuid4

from fabric.api import cd
from fabric.api import env
from fabric.api import execute
from fabric.api import parallel
from fabric.api import run
from fabric.api import task
from fabric.context_managers import settings

from awsfabrictasks.decorators import ec2instance
from awsfabrictasks.ec2.api import Ec2InstanceWrapper
from awsfabrictasks.ec2.api import Ec2LaunchInstance
from awsfabrictasks.ec2.api import ec2_rsync_download
from awsfabrictasks.ec2.api import wait_for_running_state

@task
def benchmark():
    '''
    Start benchmarks.

    Sweeps over the configuration space, starting and stopping instances as
    appropriate.
    '''
    leader = Ec2InstanceWrapper.get_by_nametag('leader')
    leader.instance.start()
    wait_for_running_state(leader['id'])
    leader = Ec2InstanceWrapper.get_by_nametag('leader')

    cluster_sizes = [3, 5, 7, 9, 11, 12, 13, 15, 16, 17, 19, 20]
    protocols = ['majority', 'grid', ('tree', 2), ('tree', 3)]
    failure_modes = ['no_failures',
            ('repeated', 0.8), ('repeated', 0.6), ('repeated', 0.4)]

    followers = []
    for (prev_cluster_size, cluster_size) in \
            zip([1] + cluster_sizes[:-1], cluster_sizes):
        new_followers_num = cluster_size - prev_cluster_size
        new_followers_names = ['follower{n}'.format(n=n)
                for n in range(len(followers), new_followers_num)]
        new_followers = [Ec2InstanceWrapper.get_by_nametag(name)
                for name in new_followers_names]

        for follower in new_followers: follower.instance.start()
        for follower in new_followers: wait_for_running_state(follower['id'])

        new_followers = [Ec2InstanceWrapper.get_by_nametag(name)
                for name in new_followers_names]
        new_followers_addresses = [follower['public_dns_name']
                for follower in new_followers]

        for follower in new_followers: follower.add_instance_to_env()

        execute(deploy, hosts=new_followers_addresses)
        for (name, address) in zip(new_followers_names, new_followers_addresses):
            execute(start_erlang_node, host=address, name=name)

        followers += new_followers

        for protocol in protocols:
            for failure_mode in failure_modes:
                configure(leader, followers, protocol, failure_mode)
                memaslap(leader['public_dns_name'])
                collect_results(cluster_size, protocol, failure_mode)

    hosts = [node['public_dns_name'] for node in [leader] ++ followers]
    execute(ec2_stop_instance, hosts=hosts)

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
    execute(start_erlang_node, hosts=dns_names)

    return instances

@task
def collect_results(cluster_size, protocol, failure_mode):
    '''
    Copy rafter_ttc_log from leader to local host.

    The parameters are used to construct a file name for the TTC measurements.
    '''
    if isinstance(protocol, tuple):
        (protocol, param) = protocol
        protocol = '{protocol}_{param}'.format(**locals())
    if isinstance(failure_mode, tuple):
        (failure_mode, param) = failure_mode
        failure_mode = '{failure_mode}_{param}'.format(**locals())

    test = '{cluster_size}-{protocol}-{failure_mode}'.format(**locals())
    ec2_rsync_download(
            '/tmp/rafter_ttc_log.log',
            '/home/leo/Temp/{test}'.format(**locals()),
            rsync_args='-avz')

@task
def configure(leader, followers, protocol, failure_mode):
    '''
    Configure the current instance (the leader).

    :param followers: The list of follower instances.
    :param protocol: The structured voting protocol to use.
    :param failure_mode: The failure mode to run the experiment with.
    '''
    command = configure_command(leader, followers, protocol, failure_mode)
    run('{command}; sleep 10'.format(**locals()))

@task
def memaslap(leader_address, runtime=60, conf='memaslap.conf'):
    '''
    Run memaslap on the local host, targeting the leader.

    :param leader_address: The IP address of the leader.
    :param runtime: How many seconds to run the benchmark for.
    :param conf: The configuration file to use with memaslap.
    '''
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
def stop_node():
    '''
    Stop and clean up a node.
    '''
    with cd(awsfab_settings.RAFTER_DIR):
        with settings(warn_only=True):
            run('killall beam')
        run('rm -rf data')
        run('mkdir data')

@task
@parallel
def start_erlang_node(name):
    '''
    Start an Erlang node.

    :param name: The name of this node.
    '''
    with cd(awsfab_settings.RAFTER_DIR):
        run('rm -rf data')
        run('mkdir data')
        run('sh bin/start-ec2-node {name}; sleep 1'.format(**locals()))

def configure_command(leader, followers, protocol, failure_mode):
    follower_names = [follower['tags'].get('Name') for follower in followers]
    follower_ips = [follower.instance.ip_address for follower in followers]
    follower_tuples = ['{{{name},\'{name}@{ip}\'}}'.format(**locals())
            for (name, ip) in zip(follower_names, follower_ips)]

    followers = '[{followers}]'.format(followers=','.join(follower_tuples))
    protocol = to_erlang_tuple('{protocol}'.format(**locals()))

    leader_ip = leader['ip_address']
    set_leader = 'Leader = \'leader@{leader_ip}\''.format(**locals())
    ping = 'pong = net_adm:ping(Leader)'
    connect = '{set_leader},{ping}'.format(**locals())

    message = 'start_benchmark, {followers}, {protocol}' \
            .format(**locals())
    start_benchmark = 'gen_fsm:send_all_state_event({{leader, Leader}}, \
            {{{message}}})'.format(**locals())
    set_failure_mode = failure_mode_code('{failure_mode}'.format(**locals()))

    command = '{connect},{start_benchmark},{set_failure_mode}'.format(**locals())
    return 'erl -setcookie rafter_localhost_test \
                -detached
                -name runner@127.0.0.1 \
                -eval "{command}."' \
            .format(**locals())

def failure_mode_code(failure_mode):
    failures_modes = ['no_failures',
            ('repeated', 0.8), ('repeated', 0.6), ('repeated', 0.4)]
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
