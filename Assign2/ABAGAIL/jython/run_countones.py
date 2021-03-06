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

for N in [40, 60, 80, 100, 120, 140]:
    for samples in [40, 60, 80, 100, 120, 140]:
        for tokeep in [0.2, 0.5, 0.7, .9]:
            cmd = "jython countones_mimic.py %d %d %d" % (N, samples, int(samples*tokeep))
            print(cmd)
            subprocess.call(cmd, env=env, shell=True)

for N in [40, 60, 80, 100, 120, 140]:
    for ga_pop in [0.5, 1, 2, 4]:
        for co_type in [1,2,3]:
            for corate in [0.4,0.6,0.8,1]:
                for mut_rate in [0.01,0.001,0.1]:
                    cmd = "jython countones_ga.py %d %d %d %d %d" % (N, int(ga_pop*N), 
                                                                           int(co_type), 
                                                                           int(ga_pop*N*corate), 
                                                                           int(ga_pop*N*mut_rate))
                    print(cmd)
                    subprocess.call(cmd, env=env, shell=True)