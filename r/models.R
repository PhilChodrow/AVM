library(tidyverse)

# coefficients ------------------------------------------------------------
# test modification.
beta_coef <- function(a, u, mode = 'random'){
    ifelse(mode == 'random',
           (1.0+a*u)/(2.0-a*(1.0-u)),
           (1.0+a)/2.0)
}

eta_coef <- function(a, u, mode = 'random'){
    ifelse(mode == 'random',
           (1.0-a*(1.0-u))/(1.0+a*u),
           (1.0)/(1.0+a))
}

rho_coef <- function(a, u, mode = 'random'){
    ifelse(mode == 'random',
           (1.0-a)/(1-a*(1-u)),
           (1.0-a))
}

sigma_coef <- function(a, u, mode = 'random'){
    ifelse(mode == 'random',
           u*(2.0*(1-a)/(2.0-a)),
           0)
}

coefs <- function(a, u, mode = 'random'){
    coef_list <- list(beta_coef, eta_coef, rho_coef, sigma_coef) %>% 
        map(~.(a, u, mode))
    names(coef_list) <- c('b', 'e', 'r', 's')
    return(coef_list)
}

# expectation -------------------------------------------------------------

# Expected impact of a voter term 
EV <- function(a, u, k_0, j_0, x, mode){
    w <- ifelse(mode == 'random', k_0*(1-2*x)-1, k_0-1)
    # w <- k_0*(1-2*x)-1
    with(coefs(a, u, mode),{
        # unconditional expectations
        EK  <- k_0 - (b/(1.0-b))*(1.0-b^k_0)
        EJ  <- e*(k_0 - EK) + j_0
        ER  <- r*(EJ - j_0)
        ES  <- s*(k_0 - EK - EJ + j_0)
        
        # conditional expectations, conditioned on K >= 1
        EK_ <- k_0/(1-b^k_0) - b/(1-b)
        EJ_ <- e*(k_0 - EK_) + j_0
        
        ((ER + ES)*w + (1-b**k_0)*(EJ_ - EK_))/(ER + ES + (1-b**k_0))
    }) 
}

mean_EV <- function(a, u, c, x, mode, k_max = 100){
    k <- 0:k_max
    p <- dpois(k, c)
    (EV(a, u, k, 1, x, mode) * p) %>% sum(na.rm = T)
}

transition <- function(l, u, c, mode){
    rewire_term <- ifelse(mode == 'random', 1/2, 1)
    f <- function(a){
        # conjecture that this is the correct way to handle u 
        l*c - (1-l)*a*rewire_term + (1-l)*(1-a)*.5*(EV(a, u, c, 1, 0, mode) + EV(a, 1-u, c, 1, 0, mode))
    }
    tryCatch({
        uniroot(f, c(0,1-10^(-5)))$root
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





