import subprocess
from os import environ
env = environ.copy()

env['CLASSPATH'] = "C:\\Users\\bparli\\Documents\\MachineLearning\\Assign2\\ABAGAIL.jar"
env['JYTHON_HOME'] = 'C:\\jython2.7.0'
env['PATH'] = "%s;C:\\jython2.7.0;C:\\jython2.7.0\\bin" % env['PATH']


    
for x in [40, 60, 80, 100, 120, 140]:
    cmd = "jython countones.py %d" % (x)
    print(cmd)
    subprocess.call(cmd, env=env, shell=True)

