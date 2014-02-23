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
    print env.ec2instances
    instances = map(
            lambda n:
                Ec2LaunchInstance(
                    configname='arch-configured-micro',
                    extra_tags={'Name':'benchmark{n}'.format(n=n)}),
            range(int(num)))
    Ec2LaunchInstance.run_many_instances(instances)
    Ec2LaunchInstance.wait_for_running_state_many(instances)
    for instance in instances:
        Ec2InstanceWrapper(instance.instance).add_instance_to_env()

@task
def test():
    print env.ec2instances

@task
@parallel
def deploy(branch='benchmark'):
    with cd(awsfab_settings.RAFTER_DIR):
        run('git checkout {branch}'.format(branch=branch))
        run('git pull')
        run('make')
        run('rm -rf data')
        run('mkdir data')

@task
@parallel
def start():
    with cd(awsfab_settings.RAFTER_DIR):
        run('./bin/start-node rafter ok')

@task
def config():
    peers = map(
        lambda v: (v.instance.tags.get('Name'), v.instance.ip_address),
        env.ec2instances.values())
    peers = ','.join(map(
        lambda (n, ip): '{{{n},\'rafter@{ip}\'}}'.format(n=n.lower(), ip=ip),
        peers))
    peers = 'Peers=[{peers}].'.format(peers=peers)
    print peers
    # with cd(awsfab_settings.RAFTER_DIR):
        # run('./bin/start-node rafter ')

#####################
# Import awsfab tasks
#####################
from awsfabrictasks.ec2.tasks import *
from awsfabrictasks.regions import *
from awsfabrictasks.conf import *
