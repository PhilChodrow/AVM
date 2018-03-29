library(tidyverse)

# coefficients ------------------------------------------------------------
# test modification.
beta_coef <- function(a, u, mode = 'random'){
    if(mode == 'random'){
        (1.0+a*u)/(2.0-a*(1.0-u))
    }else{
        (1.0+a)/2.0 %>% rep(length(u))
    }
}

eta_coef <- function(a, u, mode = 'random'){
    if(mode == 'random'){
        (1.0-a*(1.0-u))/(1.0+a*u)
    }else{
        (1.0)/(1.0+a) %>% rep(length(u))
    }
}

rho_coef <- function(a, u, mode = 'random'){
    if(mode == 'random'){
        (1.0-a)/(1-a*(1-u))
    }else{
        (1.0-a) %>% rep(length(u))
    }
}

sigma_coef <- function(a, u, mode = 'random'){
    if(mode == 'random'){
        u*(2.0*(1-a)/(2.0-a))
    }else{
        0 %>% rep(length(u))
    }

}

coefs <- function(a, u, mode = 'random'){
    coef_list <- list(beta_coef, eta_coef, rho_coef, sigma_coef) %>% 
        map(~.(a, u, mode))
    names(coef_list) <- c('b', 'e', 'r', 's')
    return(coef_list)
}

# expectation -------------------------------------------------------------

# Expected impact of a voter term 
EV <- function(a, u, c, x, mode){

    k_0 <- c*(1-x)
    j_0 <- 1
    
    w <- ifelse(mode == 'random', c*(1-2*x)-1, c-1)
    
    with(coefs(a, u, mode),{
        # unconditional expectations
        EK  <- k_0 - (b/(1.0-b))*(1.0-b^k_0)
        EJ  <- e*(k_0 - EK) + j_0
        ER  <- r*(EJ - j_0)
        ES  <- s*(k_0 - EK - EJ + j_0)
        
        # conditional expectations, conditioned on K >= 1
        EK_ <- k_0/(1-b^k_0) - b/(1-b)
        EJ_ <- e*(k_0 - EK_) + j_0

        n_V <- ER + ES + (1-b^k_0)

        # print(c(EK, EJ, ER, ES, EK_, EJ_, n_V, w))
        # print(c((ER + ES)*w, (1-b**k_0)*(EJ_ - EK_), n_V))
        1/n_V * ((ER + ES)*w + (1-b**k_0)*(EJ_ - EK_))
    }) 
}

mean_EV <- function(a, u, c, x, mode, k_max = 100){
    k <- 0:k_max
    p <- dpois(k, c)
    (EV(a, u, k, 1, x, mode) * p) %>% sum(na.rm = T)
}

dx <- function(l, u, c, mode, a){

	rewire_term <- ifelse(mode == 'random', -1/2, -1)
	voter_term <- .5*(EV(a, u, c, 0, mode) + EV(a, 1-u, c, 0, mode))
	# print(c(c, rewire_term, voter_term))
	l*c + (1-l)*a*rewire_term + (1-l)*(1-a)*voter_term

}

transition <- function(l, u, c, mode){
    tryCatch({
        uniroot(function(a) dx(l, u, c, mode, a), c(0,1-10^(-5)))$root
    }, error = function(e) NA)
}


linearized_voter <- function(l, u, c, mode){
    
    # values at alpha = 0
    r <- c+1
    q <- (r - sqrt(r^2 - 4*(1-l)^2*(r-1)))/(2*(1-l)*(r-1)) # Allen + Nowak
    x0 <- 1 - (q + (1-q)/2)
    V0 <- -r*(l/(1-l))*(1-2*x0)
    
    # values at phase transition
    a_ <- transition(l, u, c, mode)
    rewire_term <- ifelse(mode == 'random', 1/2, 1)
    # rewire_term <- 1
    x_ <- 0
    V_ <- (-l*c + (1-l)*a_*rewire_term)/((1-l)*(1-a_))
    
    f <- function(x){
        ((x - x0)/(x_ - x0))*V_ + (x/x0)*V0
    }
    return(f)
}

linearized_approx <- function(l, u, c, mode){
    
    # values at $\alpha = 0$
    r <- c+1 # degree correction?
    # x0 <- 0.5*(1 - (r+4*l-((r+l)^2-4*(r-1))^0.5)/(2*(r-1))) # Granovsky + Madras
    
    # Allen + Nowak
    q <- (r - sqrt(r^2 - 4*(1-l)^2*(r-1)))/(2*(1-l)*(r-1))
    x0 <- 1 - (q + (1-q)/2)
    
    V0 <- -r*(l/(1-l))*(1-2*x0)
    
    # values at phase transition
    a_ <- transition(l, u, c, mode) 
    rewire_term <- ifelse(mode == 'random', 1/2, 1)
    x_ <- 0
    V_ <- (-l*c + (1-l)*a_*rewire_term)/((1-l)*(1-a_))
    
    # construct a function of alpha:
    
    f <- function(a){
        # numerator   <- x0*(V_*(1-a)*(1-l) - rewire_term*a*(1-l) + l*c)
        # denominator <- (V_*(1-a)*(1-l) - V0*(1-a)*(1-l)+2*x0*l*c)
        numerator <- x0*((V_*x0)*(1-a)*(1-l) + (x0 - x_)*(rewire_term*(-1+l)*a + c*l))
        denominator <- -V0*(x0 - x_)*(1-a)*(1-l) + x0*(V_*(1-a)*(1-l) + 2*c*(x0 - x_)*l)
        pmax(numerator/denominator, 0)
    }
    return(f)
}

# ------------------------------------------------------------------------------
# BINARY DYNAMICS WITH FULL EDGE TRACKING
# ------------------------------------------------------------------------------

#' P the mutation kernel
#' g the sampling weight

# binary only
mutation_term <- function(c, g, P, u, x){

    # vector of mean degrees 
    U <- rep(u, each = 2)
    C <- c*x/U
    
    # construct transformation matrix
    q <- (g*u)
    q <- q / sum(q)
    
    m_01 <- q[1] * P[1,2]
    m_10 <- q[2] * P[2,1]
    
    M <- matrix(c(-2*m_01,      0, 2*m_10,       0,
                  m_01,  -m_01,  -m_10,    m_10,
                  m_01,  -m_01,  -m_10,    m_10,
                  0, 2*m_01,      0, -2*m_10),
                nrow = 4, 
                ncol = 4, 
                byrow = T)
    
    # compute vector of edge densities
    c(M%*%C)
}

rewire_term <- function(u, mode){
    if(mode == 'random'){
        return(c(u[1], -1/2, -1/2, u[2]))
    }else{
        return(c(1 , -1, -1, 1))
    }

}

# ------------------------------------------------------------------
# VOTER TERM
# ------------------------------------------------------------------

# note: probably easiest to implement a single, vector version and then take an expectation across that. 


#' parameters, local initial conditions, global conditions
EV_multi <- function(a, mode, i, k_0, j_0, u, x, c){
    
    with(coefs(a, u, mode),{

        
        # computation of initial pars
        EK  <- k_0 - (b/(1.0-b))*(1.0-b^k_0)
        EJ  <- e*(k_0 - EK) + j_0
        ER <- r*(EJ - j_0)
        ES <- s*(k_0 - EK - EJ + j_0)
        
        # conditional expectations, conditioned on K >= 1
        EK_ <- k_0/(1-b^k_0) - b/(1-b)
        EJ_ <- e*(k_0 - EK_) + j_0
        
        EV_ <- (1-b^k_0)
        
        # construct term
        n_V <- ER[i+1] + ES[i+1] + EV_[i+1]
        
        # mean-field degrees
        C <- c*(x / rep(u, c(2,2)))
        # C <- c*(x / u)
        
        # check these
        if(i == 0){
            
            n_00 <-  1 + C[3]
            n_11 <- -C[4]
            w_00 <-  (1 + C[3] - C[1])/2
            w_11 <-  (1 + C[2] - C[4])/2
            s_00 <- -EV_[i+1]*EJ_[i+1]
            s_11 <-  EV_[i+1]*EK_[i+1]
            
        }else if(i == 1){
            
            n_00 <- -C[1]
            n_11 <-  1 + C[2]
            w_00 <-  (1 + C[3] - C[1])/2
            w_11 <-  (1 + C[2] - C[4])/2
            s_00 <-  EV_[i+1]*EK_[i+1]
            s_11 <- -EV_[i+1]*EJ_[i+1]
        }
        
        V_00 <-  2/n_V*(ER[i+1]*n_00 + ES[i+1]*w_00 + s_00)
        V_11 <-  2/n_V*(ER[i+1]*n_11 + ES[i+1]*w_11 + s_11)
        V_01 <- -(V_00 + V_11)/2
        V_10 <-  V_01
        
        c(V_00, V_01, V_10, V_11) 
    })
}


# voting only at this stage
EV_m <- function(c, a, l, g, x, u, mode){
    
    C <- c*(c(x[1],x[4]) / u)
    term <- .5 * (EV_multi(a, mode, 1, C[1], 1, u, x, c) + EV_multi(a, mode, 0, C[2], 1, u, x, c))
    return(term)
}






# ------------------------------------------------------------------------------
# ASSEMBLY
# ------------------------------------------------------------------------------

dx_m <- function(c, a, l, g, P, u, x, mode){

    
    mutation <- mutation_term(c, g, P, u, x) 
    rewire <- rewire_term(u, mode)
    vote <- EV_m(c, a, l, g, x, u, mode)
    # print(c(mutation, rewire, vote))
    # print(c(a, mutation[2], rewire[2], vote[2]))
    l*mutation + (1-l)*a*rewire + (1-l)*(1-a)*vote

}
