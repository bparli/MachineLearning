
import subprocess
from os import environ
env = environ.copy() 
env['CLASSPATH'] = "C:\\Users\\bparli\\Documents\\MachineLearning\\Assign2\\ABAGAIL.jar"
env['JYTHON_HOME'] = 'C:\\jython2.7.0'
env['PATH'] = "%s;C:\\jython2.7.0;C:\\jython2.7.0\\bin" % env['PATH']

# generations = 1500

for N in [40, 60, 80, 100, 120, 140]:
    for ga_pop in [0.5, 1, 2, 4]:
        for co_type in [1,2,3]:
            for corate in [0.4,0.6,0.8,1]:
                for mut_rate in [0.01,0.001,0.1]:
                    cmd = "jython knapsack_ga.py %d %d %d %d %d" % (N, int(ga_pop*N), 
                                                                           int(co_type), 
                                                                           int(ga_pop*N*corate), 
                                                                           int(ga_pop*N*mut_rate))
                    print(cmd)
                    subprocess.call(cmd, env=env, shell=True)

