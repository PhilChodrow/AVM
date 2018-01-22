import networkx as nx

def component(N, c, u):
    p = 1.0*c/N
    G = nx.erdos_renyi_graph(N, p)
    nx.set_node_attributes(G, 'x', u)
    return G

def binary_graph(N_1, N_2, c_1, c_2, u_1, u_2):
    G_1 = component(N_1, c_1, u_1)
    G_2 = component(N_2, c_2, u_2)
    
    G = nx.disjoint_union(G_1, G_2)
    return G

def experiment(G, alpha, beta = 0, N_burn = 10000, N_steps = 10000, sample_interval = 100, noisy = True):
    
    print alpha, beta
    pars = {
        'alpha' : alpha,
        'beta'  : beta,
        'verbose' : False,
        'noisy' : noisy,
        'notify_end' : False,
        'sample_interval' : sample_interval
    }
    
    # burn in
    c = coev(G.copy())
    c.dynamics(nsteps = N_burn, **pars)
    
    # sampled run
    d = coev(c.G.copy())
    df, vote_df = d.dynamics(nsteps = N_steps, sample = True,  **pars)
    
    for frame in [df, vote_df]:
        frame['alpha'] = alpha
        frame['beta'] = beta

    return df, vote_df
    