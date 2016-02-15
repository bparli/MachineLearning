import subprocess
from os import environ
env = environ.copy() 
env['CLASSPATH'] = "C:\\Users\\bparli\\Documents\\MachineLearning\\Assign2\\ABAGAIL.jar"
env['JYTHON_HOME'] = 'C:\\jython2.7.0'
env['PATH'] = "%s;C:\\jython2.7.0;C:\\jython2.7.0\\bin" % env['PATH']

# generations = 1500

for N in [40, 60, 80, 100, 120, 140]:
    for samples in [40, 60, 80, 100, 120, 140]:
        for tokeep in [0.2, 0.5, 0.7, .9]:
            cmd = "jython knapsack_mimic.py %d %d %d" % (N, samples, int(samples*tokeep))
            print(cmd)
            subprocess.call(cmd, env=env, shell=True)

