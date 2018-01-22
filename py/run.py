from avm import avm
import numpy as np
import pandas as pd
from itertools import product

# set params

N_iters = 10
pars = {
	'alpha' : np.linspace(.6, 1, 21),
	'beta'  : 2**(-np.linspace(3, 10, 8)),
	'c'     : np.array([4, 6, 8])
}

data_dir = '../data/py'






if __name__ == '__main__':
	
	pars = pd.DataFrame(list(itertools.product(*pars.values())), columns=pars.keys())
	pars = pars.sort_values(['alpha', 'beta'], axis = 0, ascending = [False, True])
	pars = pars.reset_index(drop=True)
    
	for j in range(N_iters):
	# read data
		data_file = data_dir + '/' + str(j) + '.csv'

		if os.path.isfile(data_file):
			df = pd.read_csv(data_file)
		else:
			df = pd.DataFrame()
            
		if os.path.isfile(vote_file):
			vote_df = pd.read_csv(vote_file)
		else:
			vote_df = pd.DataFrame()

		# execute main loop
		interval = 10

		for i in range(len(pars)):
			new_df, new_vote_df = run_sim(c = pars.c[i], alpha = pars.alpha[i], beta  = pars.beta[i])
			df      = df.append(new_df, ignore_index = True)
			vote_df = vote_df.append(new_vote_df, ignore_index = True)
			if i % interval == 0:
				print 'printing output'
				df.to_csv(data_file, index = False)

# conduct sims

	# loop over params 
	# construct container, determine metrics
	# save container to dir






