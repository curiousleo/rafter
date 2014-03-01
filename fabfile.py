from __future__ import with_statement

from fabric.api import task, local, settings, abort, run, cd, env, parallel
from fabric.contrib.console import confirm

from awsfabrictasks.decorators import ec2instance
from awsfabrictasks.ec2.api import Ec2LaunchInstance, Ec2InstanceWrapper

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
    instance = Ec2InstanceWrapper.get_from_host_string().instance

    if instance.tags.get('Name') == leader_name:
        # instance is the leader
        not_leader = lambda instance: \
                instance.instance.tags.get('Name') != leader_name
        follower_ips = [follower.instance.ip_address
                for follower in filter(not_leader, env.ec2instances.values())]

        if len(follower_ips) == 0:
            print 'No followers! Quitting ...'
            return
        if not all(follower_ips):
            print 'Some followers do not have an IP address! Quitting ...'
            return

        # create startup script for leader
        script = leader_script(follower_ips)

    # with cd(awsfab_settings.RAFTER_DIR):
        # print command
        # run('./bin/start-node rafter')

def leader_script(follower_ips):
    follower_tuples = ['{{follower{n},\'rafter@{ip}\'}}'.format(**locals())
            for (ip, n) in zip(follower_ips, range(len(follower_ips)))]

    leader_tuple = '{{leader,\'rafter@{ip}\'}}'.format(ip=instance.ip_address)
    assign = 'Peers=[{followers},{leader}]'.format(followers=','.join(follower_tuples), leader=leader_tuple)
    create_vstruct = 'Vstruct=rafter_voting_grid:grid(Peers)'
    set_config = 'rafter:set_config(leader,Vstruct)'

    command = '{assign},{create_vstruct},{set_config}.'.format(**locals())
    script = '''cd /root/Code/rafter.git
IP=$(curl --silent http://instance-data/latest/meta-data/public-ipv4)
erl -detached \
-pa deps/*/ebin ebin \
-setcookie rafter_localhost_test \
-name rafter@$IP \
-eval "rafter:start_test_node(rafter),{command}"'''.format(**locals())
    return script

#####################
# Import awsfab tasks
#####################
from awsfabrictasks.ec2.tasks import *
from awsfabrictasks.regions import *
from awsfabrictasks.conf import *
