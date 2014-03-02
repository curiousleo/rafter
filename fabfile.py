from __future__ import with_statement

from tempfile import NamedTemporaryFile
from os.path import split as split_path

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
def setup(num, config='arch-configured-micro', environment='benchmark'):
    '''
    Launch a number of instances.

    :param num: The number of instances to launch.
    :param config: The configuration to use for the new instances.
    :param environment: The value of the 'Environment' tag for the new instances.
    '''
    launch = lambda: \
        Ec2LaunchInstance(
            configname=config, extra_tags={'Environment':environment})
    instances = [launch() for _ in range(int(num))]
    Ec2LaunchInstance.run_many_instances(instances)
    Ec2LaunchInstance.wait_for_running_state_many(instances)

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

def leader_script(leader, followers):
    follower_names = [follower.tags.get('Name') for follower in followers]
    follower_ips = [follower.ip_address for follower in followers]
    follower_tuples = ['{{{name},\'{name}@{ip}\'}}'.format(**locals())
            for (name, ip) in zip(follower_names, follower_ips)]

    leader_tuple = '{{leader,\'leader@{ip}\'}}' \
            .format(ip=leader.ip_address)
    assign = 'Peers=[{followers},{leader}]' \
            .format(followers=','.join(follower_tuples), leader=leader_tuple)
    create_vstruct = 'Vstruct=rafter_voting_grid:grid(Peers)'
    set_config = 'rafter:set_config(leader,Vstruct)'

    command = '{assign},{create_vstruct},{set_config}.'.format(**locals())
    script = '''cd /root/Code/rafter.git
IP=$(curl --silent http://instance-data/latest/meta-data/public-ipv4)
erl \
-pa deps/*/ebin ebin \
-setcookie rafter_localhost_test \
-name leader@$IP \
-eval "rafter:start_test_node(leader),{command}" '''.format(**locals())
    return script

#####################
# Import awsfab tasks
#####################
from awsfabrictasks.ec2.tasks import *
from awsfabrictasks.regions import *
from awsfabrictasks.conf import *
