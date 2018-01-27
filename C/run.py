import numpy as np
import subprocess
from itertools import product
import os

count=len(os.listdir('data'));
node=10000; # number of nodes
c = np.array([4, 6, 8, 20]); # mean degree 

dt=5000; # save results every dt steps
alph = np.linspace(1.0, 0.0, 101) # rewiring probability
lam = 2.0**(-np.array([2.0, 4.0, 6.0, 8.0, 10.0]))
l_01 = np.arange(.1, .9, 9)
g=2; # number of opinions
gam = np.linspace(0, 1, 11)
max_steps = 10**6
realizations = np.arange(0,1)

# U_naught = np.linspace(0, 1.0, 11)
U_naught = [.5]

U0=1.0/g*np.ones(g,); # list of initial densities
U0[-1]=1-sum(U0[:-1]);
mode = [0, 1]

pars = product(realizations, lam, gam, c, mode, alph, U_naught)
for realization, lamb, gamma, cee, mode, alpha, U0 in pars:

    outfile = 'data/C/mode_' + str(mode) + '_run_' + str(count)
    edge = node*cee/2.0

    cmd='C/bin/DynamicVoter -n {} -m {} -a {} -l {} -g {} -t {} -M {} -T {} -o {} -u {}  '.format(node, edge, alpha, lamb, gamma, dt, mode, max_steps, outfile, U0);
#    for i in xrange(g):
#        cmd=cmd
    print cmd
    subprocess.call(cmd, shell=True);
    count=count+1;

print str(count-1)+' files submitted.';
print 'Mission Complete!';
