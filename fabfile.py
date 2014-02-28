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
@parallel
def start_rafter():
    with cd(awsfab_settings.RAFTER_DIR):
        run('./bin/start-node rafter ok')

@task
def config():
    instances = env.ec2instances.values()
    ip_addresses = (v.instance.ip_address for v in instances)
    peers = ('{{peer{n},\'rafter@{ip}\'}}'.format(**locals())
            for (ip, n) in zip(ip_addresses, range(len(instances))))
    assignment = 'Peers=[{peers_string}].'.format(peers_string=','.join(peers))
    print assignment
    # with cd(awsfab_settings.RAFTER_DIR):
        # run('./bin/start-node rafter ')

#####################
# Import awsfab tasks
#####################
from awsfabrictasks.ec2.tasks import *
from awsfabrictasks.regions import *
from awsfabrictasks.conf import *