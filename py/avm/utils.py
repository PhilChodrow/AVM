import networkx as nx
from avm import coev

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

def experiment(G, alpha, beta = 0, gamma = 0.5, N_burn = 10000, N_steps = 10000, sample_interval = 100, noisy = True):
    
    pars = {
        'alpha' : alpha,
        'beta'  : beta,
        'gamma' : gamma,
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
    df = d.dynamics(nsteps = N_steps, sample = True, **pars)
    
    df['alpha'] = alpha
    df['beta'] = beta
    df['gamma'] = gamma

    return df

def run_sim(c, N, alpha, beta, gamma,  **d_pars):
    
    g_pars = {
        'N_1' : N/2,
        'N_2' : N/2,
        'c_1' : c,
        'c_2' : c,
        'u_1' : 0,
        'u_2' : 1
    }

    G = binary_graph(**g_pars)
    G = nx.MultiGraph(G)
    
    df = experiment(G, alpha, beta, gamma)
    df['c'] = c
    
    return df

    