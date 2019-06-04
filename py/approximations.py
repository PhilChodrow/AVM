import numpy as np                  # general manipulations
from scipy.optimize import fsolve   # for numerical solving of arches
import pandas as pd

#########################################################################
####################### COMPUTATION OF GRADIENTS ########################
#########################################################################


def coefs(a, u, mode = 'random'):
    '''
    Compute the rate constants for various transitions in terms of the parameters alpha and u. 
    a: alpha, the rewiring rate.
    u: 2-vector, the opinion density vector. 
    mode: char in ['random', 'same'] governing the rewiring mode. 
    '''
    
    if mode not in ['random', 'same']:
        raise ValueError('Please choose mode in [random, same]')
    
    
    if mode == 'random':        
        beta = (1.0 + a*u)/(2.0 - a*(1.0 -u)) 
    else:
        beta = np.repeat((1.0 + a)/2.0, len(u))
    
    if mode == 'random':
        eta = (1.0 - a*(1.0 - u))/(1.0 + a*u)
    else:
        eta = np.repeat((1.0)/(1.0+a), len(u))
    
    if mode == 'random':
        rho = (1.0 - a) / (1.0-a*(1.0-u))
    else:
        rho = np.repeat(1.0 - a, len(u))
    
    if mode == 'random':
        sigma = u*(2.0*(1-a)/(2.0-a))
    else:
        sigma = np.repeat(0, len(u))
    
    return({
        'b' : beta,
        'e' : eta,
        'r' : rho,
        's' : sigma
    })

def EV(a, u, c, x, mode, scale = True):
    '''
    Compute the expected change in the edge census vector x due to voting steps
    a: float, the rewiring rate
    u: 2-vector, the opinion density
    x: a 4-vector of edge densities. 
    '''
    
    # mean number of concordant/discordant edges attached to each node. 
    C = c*(x / np.array([u[0], u[0], u[1], u[1]])) 
    k_0 =     np.array([C[3], C[0]]) # counts of initially concordant edges
    j_0 = 1 + np.array([C[1], C[2]]) # counts of initially discordant edges
    
    # bring coefs into namespace for convenience
    coef_vec   = coefs(a, u, mode)
    b, e, r, s = (coef_vec['b'], coef_vec['e'], coef_vec['r'], coef_vec['s'])
    
    # probability that node V votes
    EV_ = 1- b**k_0
    
    # computation of initial probabilities and expectations
    
    EK = k_0 - b/(1.0 - b)*EV_
    EJ = e*(k_0 - EK) + j_0
    ER = r*(EJ - j_0)
    ES = s*(k_0 - EK - EJ + j_0)
    
    EK_ = k_0/EV_ - b/(1-b)
    EJ_ = e*(k_0 - EK_) + j_0
    
    
    ## impact factors: expected change in the edge vector introduced by voting events of various types
    
    # neighbor votes
    n_00 =     np.array([1+C[2]      ,  -C[0]]    )
    n_11 =     np.array([ -C[3]      , 1+C[1]]    )
    
    # wild votes
    w_00 = 0.5*np.array([1+C[2]-C[0]], 1+C[2]-C[0])
    w_11 = 0.5*np.array([1+C[1]-C[3]], 1+C[1]-C[3])
    
    # backward votes
    s_00 = np.array([-EV_[0]*EJ_[0],  EV_[1]*EK_[1]])
    s_11 = np.array([ EV_[0]*EK_[0], -EV_[1]*EJ_[1]])
    
    # expected total vote count
    n_V = ER + ES + EV_
    
    
    # scale factor
    if scale:
        x_ = (c-1.0)/(2.0*c)
        xx = 4*u[0]*u[1]*x_
        scaling = (xx - (x[1] + x[2]))/xx
    else: 
        scaling = 1
            
    # expected changes in each entry of the edge density vector
    V_00 = 2/n_V*(ER*n_00 + ES*w_00 + s_00*scaling)
    V_11 = 2/n_V*(ER*n_11 + ES*w_11 + s_11*scaling)
    V_01 = -(V_00 + V_11)/2
    V_10 = V_01
    
    V = np.array([V_00, V_01, V_10, V_11])
    
    return(V.mean(axis = 1))

def rewire_term(a, u, mode):
    if mode == 'random':
        return(np.array([u[0], -0.5, -0.5, u[1]]))
    else:
        return(np.array([1, -1, -1, 1]))

# todo: implement mutation

def dx(a, u, c, x, mode, scale = True):
    return a*(rewire_term(a, u, mode)) + (1-a)*(EV(a, u, c, x, mode, scale))


#########################################################################
####################### COMPUTATION OF THE ARCH #########################
#########################################################################

def arch(a, U, c, mode, scale = True, x0 = np.array([0.5, 0])):
    '''
    Compute the arch by setting the increment equal to zero. 
    '''
    
    u = np.array([U, 1 - U])
    
    def h(y):
        '''
        y is a 2-vector, its entries correspond to x_00 and x_01
        '''
        x = np.array([y[0], y[1], y[1], 1 - y[0] - 2*y[1]])
        out = dx(a, u, c, x, mode, scale)
        return(np.array([out[0], out[1]]))
    
    sol = fsolve(h, x0 = x0, full_output=False, epsfcn = 0.0000000001)
    return(sol)
    
def test_valid(x):
    '''
    Check for a numerically valid x: all entries must be between 0 and 1. 
    '''
    X = np.array([x[0], x[1], x[1], 1 - 2*x[1] - x[0]])
    return((X>0).all())

class solver():
    '''
    Mini-class with memory: it uses the previous solution as the initial guess for the next computation. 
    '''
    def __init__(self):
        self.x0 = np.array([0.3, 0.2])
    
    def find_root(self, **kwargs):
        sol = arch(x0 = self.x0, **kwargs)
        if test_valid(sol):
            self.x0 = sol
            return(2.0*sol[1])
        else:
            self.x0 = np.array([0.5, 0])
            return(0)
        
def clean(v):
    i = np.argmax((v - np.roll(v, -1)) < 0)
    v[i:] = np.zeros(len(v[i:]))
    return(v)


def approx(U, c, mode):
    
    A = np.linspace(0, 1, 101)

    s = solver()
    v = [s.find_root(a = alpha, 
                     U = .5, 
                     c = 4, 
                     mode = 'same', 
                     scale = True) for alpha in A]

    v = clean(v)
    
    df = pd.DataFrame(v, columns=['v'])
    df['alpha'] = A
    df['c'] = c
    df['u'] = U
    df['mode'] = mode
    return(df)

def approx_suite(c, mode):
    df = pd.concat([approx(u, 4, 'random') for u in np.linspace(0, 1, 101)])
    return(df)




