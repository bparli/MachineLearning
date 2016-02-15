
import subprocess
from os import environ
env = environ.copy() 
env['CLASSPATH'] = "C:\\Users\\bparli\\Documents\\MachineLearning\\Assign2\\ABAGAIL.jar"
env['JYTHON_HOME'] = 'C:\\jython2.7.0'
env['PATH'] = "%s;C:\\jython2.7.0;C:\\jython2.7.0\\bin" % env['PATH']

# generations = 1500

for N in [20]:
    for samples in [50,100,200]:
        for tokeep in [0.05, 0.1, 0.25, 0.5]:
            cmd = "jython fourpeaks_mimic_tuned.py %d %d %d" % (N, samples, int(samples*tokeep))
            print(cmd)
            subprocess.call(cmd, env=env, shell=True)

