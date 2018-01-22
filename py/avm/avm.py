import networkx as nx
import numpy as np
import random 
import pandas as pd

class coev:

	def __init__(self, G):
		'''
		G an undirected networkx Graph object with node attributes
		'''
		
		self.G = G
		self.m = len(G.edges())
		self.n = len(G.nodes())
		self.opinion_space = set(nx.get_node_attributes(G, 'x').values())
		self.tension_dict = {}

		self.update_tension()
		
	def get_graph(self):
		return self.G

	def sample_edge(self):
		'''
		Sample an edge uniformly from G and return it as a pair of node labels.
		Should sample from the tension_dict
		'''

		return random.choice(self.tension_dict.keys()) # 

	def update_tension(self, edge_list = None, remove_list = None):
		'''
		Evaluate tension on edges. 
		'''

		# need to delete the entries corresponding to edges that are no longer active. 
		# need to delete the entries corresponding to edges that are no longer in the graph. 

		if edge_list is None:
			edge_list = self.G.edges_iter()

		edge_list = [order_edge(e) for e in edge_list]
		

		nodes = self.G.nodes(data = True)
		
		def get_tension(e):
			return np.abs(nodes[e[0]][1]['x'] - nodes[e[1]][1]['x']) 

		tension = {e : get_tension(e) for e in edge_list}
		self.tension_dict.update(tension)
		self.tension_dict = {e : t for e, t in self.tension_dict.iteritems() if t > 0} 
		
		if remove_list is not None:
			remove_list = [order_edge(e) for e in remove_list]
			for entry in remove_list:
				# print len(self.tension_dict)
				self.tension_dict.pop(entry, None)
				# print len(self.tension_dict)
		
	def rewire_edge(self, e, verbose = False):
		'''
		e a tuple the edge to rewire
		This is the "rewire-to-random" rule of the Durrett papers
		'''
		end = (np.random.rand() < .5) * 1 # pick node that will be rewired
		i = e[end]		
		
		available_nodes = self.G.nodes()
		ego = nx.ego_graph(self.G, e[end], radius = 1).nodes()
		
		for v in ego:
			available_nodes.remove(v)

		j = np.random.choice(available_nodes)

		self.G.remove_edge(*e)          # delete edge
		self.G.add_edge(i, j)

		self.update_tension(edge_list = [(i,j)], remove_list = [e])

		if(verbose):
			print "Rewired " + str(e) + " to " + str((i,j))
			
	def update_step(self, beta = 0, alpha = 0, weighted = False, verbose = False):
		
		try: 
			if np.random.rand() < beta:
				nodes = self.G.node
				node_labels = nodes.keys()
				i = np.random.choice(node_labels)
				old_opinion = nodes[i]['x']
				new_opinion = np.random.choice(list(self.opinion_space - set([old_opinion])))
				nx.set_node_attributes(self.G, 'x', {i : new_opinion})
				ego = nx.ego_graph(self.G, i, radius = 1).edges()
				self.update_tension(ego)
				# print 'mutating from ' + str(old_opinion) + ' to ' + str(new_opinion)
			else:
				e = self.sample_edge()

				if np.random.rand() < alpha: # rewire
					self.rewire_edge(e, verbose)

				else:  # argue with a node
					i = (np.random.rand() < .5) * 1
					j = 1 - i

					nodes = self.G.nodes(data = True)
					x_i   = nodes[e[i]][1]['x'] 
					x_j   = nodes[e[j]][1]['x']

					new_x_i = x_i + np.sign(x_j - x_i)
					nx.set_node_attributes(self.G, 'x', {e[i] : new_x_i})
					ego = nx.ego_graph(self.G, e[i], radius = 1).edges()
					self.update_tension(ego)
		except IndexError: 
			return None

	def get_v(self):
		return nx.get_node_attributes(self.G, 'x').values()

	def mean_tension(self):
		opinion_dict = nx.get_node_attributes(self.G, 'x')
		opinions = [opinion_dict[v] for v in range(self.n)]
		return np.mean([np.abs(opinions[e[0]] - opinions[e[1]]) for e in self.G.edges()])     


	def variance(self):
		return np.var(self.get_v())

	def mean_v(self):
		return np.mean(self.get_v())

	def percent_tension(self):
		return 1.0 * len(self.tension_dict) / self.m

	
	def dynamics(self, nsteps = None, alpha = 0, beta = 0, weighted = False, sample = False, sample_interval = 100, verbose = False, verbose_interval = 1000, notify_end = True, noisy = False):
		
		if sample:
			tension_list = list()
			variance_list = list()
			mean_list = list()
			mean_tension_list = list()
			t_list = list()
			
		i = 0
		done = False
		while not done:
			i += 1
			self.update_step(alpha = alpha, beta = beta, weighted = True, verbose = False)
			n_tension = len(self.tension_dict)
			
			if nsteps is not None:
				done = i>= nsteps

			if not noisy:
				done = done or n_tension == 0

			if sample and ((i - 1) % sample_interval == 0 or done): 
				tension_list.append(self.percent_tension())
				variance_list.append(self.variance())
				mean_list.append(self.mean_v())
				mean_tension_list.append(self.mean_tension())
				t_list.append(i - 1)
				
			if verbose: 
				if (i - 1) % verbose_interval == 0:
					print i, round(self.mean_tension(), 6)
			
		if sample: 
			d = {'tension'      : np.array(tension_list),
				 'variance'     : np.array(variance_list),
				 'mean_opinion' : np.array(mean_list),
				 'mean_tension' : np.array(mean_tension_list),
				 't'            : np.array(t_list)}

			df = pd.DataFrame(d)
			
		if notify_end:
			print 'Done in ' + str(i) + ' steps.'

		if sample:
			return df


##### 
# HELPERS
##### 

def er_coev(N, c, k, normalize_k = False, rand_opinions = False):
	p = 1.0 * c / N
	G = nx.erdos_renyi_graph(N, p)
	
	if rand_opinions:
		k = 10
		v = np.random.rand(2 * k + 1)
		v = v / v.sum()

		attr = np.random.choice(np.arange(-k, k+1), len(G.nodes()), p = v)
	else:                        
		attr = np.random.choice(np.arange(-k, k+1), len(G.nodes()))

	if normalize_k: 
		attr = 1.0 * attr / k
	
	attr = {i : attr[i] for i in range(len(attr))}
	nx.set_node_attributes(G, 'x', attr)
	return coev(G)

def order_edge(e):
		return tuple(sorted(e))

def run_dynamics(N, c, k, beta, alpha, normalize_k, rand_opinions,  **kwargs):
	coev = er_coev(N, c, k, normalize_k, rand_opinions)
	df = coev.dynamics(alpha = alpha, 
	                   beta = beta, 
					   **kwargs)
	if df is not None:
		param_cols = {'c' : c, 'k' : k, 'N' : N, 'alpha' : alpha}
		for col_name in param_cols:
			df[col_name] = np.repeat(param_cols[col_name], len(df))
	
	return df
