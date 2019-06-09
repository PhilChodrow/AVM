library(tidyverse)
library(assertthat)
library(alabama)


# Compute rate constants associated with different voting events 
coefficients <- function(a, u, c, x = 0, mode = 'random'){
    assert_that(mode %in% c('random', 'same'))
    D <- 0
    if(mode == 'random'){
        b <- (1.0+a*u+(1-a)*D)/(2.0-a*(1.0-u)+(1-a)*D)
        e <- (a*u + (1-a)*(D+1))/(a*u + (1-a)*D + 1)
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

# tech function: normalize to a valid probability distribution
normalize <- function(v) v / sum(v)

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

# compute the expected change in the edge density vector due to voting events. 
EV_m <- function(a, u, c, x, mode, by_type = F, use_mean = T, scale = T){
    

    C   <- c*(x / rep(u, c(2, 2))) # number of neighbors of each kind per node
    k_0 <-     c(C[4], C[1])       # initial discordant edge density 
    j_0 <- 1 + c(C[2], C[3])       # initial concordant edge density
    
    with(coefficients(a, u, c, x, mode),{
        
        # probability that node V votes
        EV_ <- 1-b^k_0
        
        # computation of initial pars
        EK  <- k_0 - b/(1.0-b)*EV_
        EJ  <- e*(k_0 - EK) + j_0
        ER <- r*(EJ - j_0)
        ES <- s*(k_0 - EK - EJ + j_0)
        
        # Expectations conditioned on V voting. 
        EK_ <- k_0/EV_ - b/(1-b)
        EJ_ <- e*(k_0 - EK_) + j_0
        
        # impact factors: expected change in the density vector per voting event of various types
        
        ## Neighbor votes
        n_00 <-    c( 1 +   C[3]        ,   -   C[1]        )
        n_11 <-    c(   -   C[4]        , 1 +   C[2]        )
        
        ## "Wild" votes (unconnected to V)
        w_00 <- .5*c( 1 +   C[3] - C[1] , 1 +   C[3] - C[1] )
        w_11 <- .5*c( 1 +   C[2] - C[4] , 1 +   C[2] - C[4] )
        
        ## Backward votes (V votes)
        s_00 <-    c(   - EV_[1]*EJ_[1] ,     EV_[2]*EK_[2] )
        s_11 <-    c(     EV_[1]*EK_[1] ,   - EV_[2]*EJ_[2] )
        
        # backward vote decay
        if(scale){
            x_ <- (c-1)/(2*c) # estimate for top of the arch
            xx <- 4*u[1]*u[2]*x_
            scaling_factor <- ((xx - (x[2] + x[3]) )/ xx)    
        }
        else{
            scaling_factor <- 1
        }
        
        # expected total number of votes 
        n_V <- ER + ES + EV_
        
        # aggregate impacts on the edge density vector
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

dx_m <- function(a, u, c, x, mode, scale){
    
    rewire <- rewire_term(u, mode)
    vote <- EV_m(a, u, c, x, mode, scale = scale, by_type = F)
    return(a*rewire + (1-a)*vote)
}

sq_norm <- function(x) sum(x*x)

arch <- function(a, u, c, mode = 'random', scale = T, print_pars = F){
    # ask if the initial node state should depend on u....?
    
    if(print_pars){
        print(paste(a, u, c, mode, sep = ' : '))
    }
    
    
    lo  <- c(0, 0, 0, 0)
    hi  <- c(1, 1, 1, 1)
    u <- c(1-u, u)
    
    f <- function(x_0){
        
        h <- function(x){ 
            y <- c(x[1], x[2], x[2], 1 - x[1] - 2*x[2])
            dx_m(a, u, c, y, mode = mode, scale = scale) %>% sq_norm()
        }
        
        hin <- function(x){
            c(x[1], x[2], 1 - x[1], 1 - x[2], 1 - x[1] - 2*x[2])
        }
        
        res <- auglag(par = x_0,
                      fn = h,
                      hin = hin,
                      control.outer = list(trace = F))
        
        return(res)
    }
    
    x_df <- expand.grid(w = seq(0.1, .5, .2),
                        x = seq(0.1, .5, .2)) %>%
        tbl_df() %>%
        mutate(x_0 = map2(w, x, .f = ~c(.x, .y)),
               res = map(x_0, f))
    
    test_df <- x_df %>%
        mutate(x_ = map(res, 'par'),
               val = map_dbl(res, 'value'),
               x_ = map(x_, ~c(.x[1], .x[2], .x[2], 1 - .x[1] - 2*.x[2])))
    return(test_df[test_df$val == min(test_df$val),][1,])
}


# Phase Transition -----------------------------------

transition <- function(u, c, mode = 'random'){
    if(length(u) == 1){
        u <- c(u, 1 - u)
    }
    tryCatch({
        uniroot(function(a) dx_m(a, u, c, c(.5, 0, 0, .5), mode, scale = T)[2], 
                c(0,1-10^(-5)))$root
    }, error = function(e) NA)
}



