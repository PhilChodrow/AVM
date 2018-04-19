library(tidyverse)
library(assertthat)

coefficients <- function(a, u, c, x = 0, mode = 'random'){
    assert_that(mode %in% c('random', 'same'))
    D <- 0
    if(mode == 'random'){
        b <- (1.0+a*u+(1-a)*D)/(2.0-a*(1.0-u)+(1-a)*D)
        e <- (a*u + (1-a)*(D+1))/(a*u + (1-a)*D + 1)
        # r <- ((1-a)*(D+1))/(a*u + (1-a)*D + 1) # think this is wrong because we are 
                                                 # tracing the causal influence of a single node
        r <- (1.0-a)/(1-a*(1-u))
        s <- u*(2.0*(1-a)/(2.0-a))
    }else{
        b <- (1.0+a)/2.0 %>% rep(length(u))
        e <- (1.0)/(1.0+a) %>% rep(length(u))
        r <- (1.0-a) %>% rep(length(u))
        s <- 0 %>% rep(length(u))
    }
    return(list(b = b, e = e, r = r, s = s))
}

EV <- function(a, u, c, x, mode){
    
    k_0 <- c - c*x   # think this is the correct first-order expression
    j_0 <- 1 + c*x   # think this is the correct first-order expression
    
    # k_0 <- c*(1-2*x)
    # j_0 <- 1
    
    w <- k_0 - j_0
    # w <- c*(1-2*x) - 1
    with(coefficients(a, u, c, x, mode),{
        # coefs(a, u, c, x, mode) %>% attach()
        # unconditional expectations
        EK  <- k_0 - (b/(1.0-b))*(1.0-b^k_0)
        EJ  <- e*(k_0 - EK) + j_0
        ER  <- r*(EJ - j_0)
        ES  <- s*(k_0 - EK - EJ + j_0)
        
        # conditional expectations, conditioned on K >= 1
        EK_ <- k_0/(1-b^k_0) - b/(1-b)
        EJ_ <- e*(k_0 - EK_) + j_0
        
        p_V <- (1-b^k_0)
        n_V <- ER + ES + p_V
        
        x_ <- (c-1)/(2*c)
        
        xx <- 4*u*(1-u)*x_
        
        E <- 1/n_V * ((ER + ES)*w + p_V*(EJ_ - EK_)*((xx - x)/xx))
        
        # w
        
    }) 
}

# --------------------
# --------------------
# --------------------



rewire_term <- function(u, mode){
    if(mode == 'random'){
        return(c(u[1], -1/2, -1/2, u[2]))
    }else if(mode == 'same'){
        return(c(1 , -1, -1, 1))
    }
    
}

# hypothesis: I have the u's backwards somewhere in here. Supported by agreement when u = .5, even when 
# x is asymmetric
EV_m <- function(a, u, c, x, mode, by_type = F){
    
    C   <- c*(x / rep(u, c(2, 2)))
    k_0 <-     c(C[4], C[1])  # issue is here? 
    j_0 <- 1 + c(C[2], C[3])  
    # j_0 <- 1 # old version, toggle off once support issue is fixed
    
    with(coefficients(a, u, c, x, mode),{
        
        # probability that node V votes
        EV_ <- 1-b^k_0
        
        # computation of initial pars
        EK  <- k_0 - b/(1.0-b)*EV_
        EJ  <- e*(k_0 - EK) + j_0
        ER <- r*(EJ - j_0)
        ES <- s*(k_0 - EK - EJ + j_0)
        
        # conditional expectations, conditioned on K >= 1
        EK_ <- k_0/EV_ - b/(1-b)
        EJ_ <- e*(k_0 - EK_) + j_0
        
        # expected total number of votes 
        n_V <- ER + ES + EV_
        
        # impact factors
        n_00 <-    c( 1 +   C[3]        ,   -   C[1]        )
        n_11 <-    c(   -   C[4]        , 1 +   C[2]        )
        w_00 <- .5*c( 1 +   C[3] - C[1] , 1 +   C[3] - C[1] )
        w_11 <- .5*c( 1 +   C[2] - C[4] , 1 +   C[2] - C[4] )
        s_00 <-    c(   - EV_[1]*EJ_[1] ,     EV_[2]*EK_[2] )
        s_11 <-    c(     EV_[1]*EK_[1] ,   - EV_[2]*EJ_[2] )
        
        # print(c(EV_, EK_, EJ_, ER, ES))
        
        # backward vote decay
        x_ <- (c-1)/(2*c) # estimate for top of the arch
        xx <- 4*u[1]*u[2]*x_
        scaling_factor <- ((xx - (x[2] + x[3]) )/ xx)
        # scaling_factor <- 1 # toggle off once support issue is fixed
        
        V_00 <-  2/n_V*(ER*n_00 + ES*w_00 + s_00*scaling_factor)
        V_11 <-  2/n_V*(ER*n_11 + ES*w_11 + s_11*scaling_factor)
        V_01 <- -(V_00 + V_11)/2
        V_10 <-  V_01
        A <- matrix(c(V_00, V_01, V_10, V_11), nrow = 4, byrow = T)
        
        if(by_type){
            return(A)    
        }else{
            return(rowMeans(A))
        }
    })
}


# ------------------------------------------------------------------------------
# ASSEMBLY
# ------------------------------------------------------------------------------

dx_m <- function(a, u, c, x, mode){
    
    rewire <- rewire_term(u, mode)
    vote <- EV_m(a, u, c, x, mode, by_type = F)
    a*rewire + (1-a)*vote
}
