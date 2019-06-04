import numpy as np
from scipy.optimize import fsolve
from approximations import *
import pandas as pd
import os

path = "throughput/arch"

if not os.path.isdir(path):
    if not os.path.isdir('throughput'):
        os.mkdir('throughput')
    os.mkdir(path, )

C = [4, 6, 8, 20]
modes = ['random', 'same']

for c in C:
    for mode in modes:
        print('Mean degree: ' + str(c) + '. Mode: ' +  mode)
        df = approx_suite(c, mode)
        df.to_csv(path + '/' + str(c) + '_' + mode + '.csv')
