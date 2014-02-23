from __future__ import with_statement

from fabric.api import task, local, settings, abort, run, cd, env, parallel
from fabric.contrib.console import confirm

from awsfabrictasks.decorators import ec2instance

@task
@parallel
def deploy():
    with cd(awsfab_settings.RAFTER_DIR):
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
