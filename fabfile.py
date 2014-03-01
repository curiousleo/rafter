from __future__ import with_statement

from tempfile import NamedTemporaryFile
from os.path import split as split_path

from fabric.api import cd
from fabric.api import env
from fabric.api import parallel
from fabric.api import run
from fabric.api import task

from awsfabrictasks.decorators import ec2instance
from awsfabrictasks.ec2.api import Ec2InstanceWrapper
from awsfabrictasks.ec2.api import Ec2LaunchInstance
from awsfabrictasks.ec2.api import ec2_rsync_upload

"""
Call as ``awsfab setup:num=X deploy start``. This will

1. *Set up* ``X`` instances
2. *Deploy* Rafter to all of them
3. *Start* a Rafter node on each one
"""

@task
def setup(num):
    launch = lambda: \
        Ec2LaunchInstance(
            configname='arch-configured-micro',
            extra_tags={'Environment':'benchmark'})
    instances = [launch() for _ in range(int(num))]
    Ec2LaunchInstance.run_many_instances(instances)
    Ec2LaunchInstance.wait_for_running_state_many(instances)

@task
@parallel
# @ec2instance(tags={'Environment':'benchmark'})
def deploy(branch='benchmark'):
    remote = 'origin'
    with cd(awsfab_settings.RAFTER_DIR):
        run('git fetch {remote}'.format(**locals()))
        run('git reset --hard {remote}/{branch}'.format(**locals()))
        run('rm -rf data')
        run('rm ebin/*')
        run('make')
        run('mkdir data')

@task
def start_cluster(leader_name='leader'):
    start_followers(leader_name=leader_name)
    start_leader(leader_name=leader_name)

@task
def start_followers(leader_name='leader'):
    instance = Ec2InstanceWrapper.get_from_host_string().instance
    name = instance.tags.get('Name')

    if name != leader_name:
        # instance is a follower
        with cd(awsfab_settings.RAFTER_DIR):
            run('sh bin/start-ec2-node {name}'.format(**locals()))

@task
def start_leader(leader_name='leader'):
    instance = Ec2InstanceWrapper.get_from_host_string().instance

    if instance.tags.get('Name') == leader_name:
        # instance is the leader
        instancewrappers = env.ec2instances.values()
        not_leader = lambda instance: \
                instance.instance.tags.get('Name') != leader_name
        followers = [follower.instance for follower in
                filter(not_leader, instancewrappers)]

        if len(followers) == 0:
            print 'No followers! Quitting ...'
            return

        # create startup script for leader
        script = leader_script(instance, followers)
        script_name = None
        with NamedTemporaryFile() as script_file:
            script_name = split_path(script_file.name)[1]
            script_file.write(script)
            script_file.flush()
            ec2_rsync_upload(script_file.name, awsfab_settings.SCRIPT_DIR)
        with cd(awsfab_settings.RAFTER_DIR):
            run('sh bin/{script_name}'.format(**locals()))

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
-detached \
-pa deps/*/ebin ebin \
-setcookie rafter_localhost_test \
-name leader@$IP \
-eval "rafter:start_test_node(rafter),{command}" '''.format(**locals())
    return script

#####################
# Import awsfab tasks
#####################
from awsfabrictasks.ec2.tasks import *
from awsfabrictasks.regions import *
from awsfabrictasks.conf import *
