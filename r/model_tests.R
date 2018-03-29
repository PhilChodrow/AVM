library(tidyverse)
source('r/models.R')
library(BB)
# --------------------------------------------------------------------------
# SINGLE VARIABLE
# --------------------------------------------------------------------------
sq_norm <- function(x) sum(x*x)

# Visualization of dx
a    <- 0.7
c    <- 4
l    <- 0.000001
u    <- 0.5
x    <- 0
mode <- 'random'

# data_frame(a = (0:100)*0.01) %>% 
# 	mutate(dx = map_dbl(a, ~dx(l, u, c, mode, .x))) %>% 
# 	ggplot() + 
# 	aes(x = a, y = dx) + 
# 	geom_point() + 
# 	scale_x_continuous(breaks = (0:10)*.1,expand = c(0,0)) + 
# 	scale_y_continuous(limits = c(0, NA), expand = c(0,0)) + 
# 	theme_bw() 

arch <- function(a){
	d <- function(a, x){
		l*c*(1-2*x) - (1-l)*a/2 + (1-l)*(1-a)*EV(a, u, c, x, mode) 
	}
	tryCatch({
		res <- spg(par = .1, 
				   fn = function(x) d(a, x) %>% sq_norm())
		# if(res$value < .00001){
			answer <- res$par
			return(answer)
		# }
	},
	error = function(e) NA)
	return(NA)
}

df <- data_frame(a_vals = (seq(0, 100, 1)*.01)) %>% 
	mutate(x = map_dbl(a_vals, arch))

df %>%
	ggplot() + 
	aes(x = a_vals, y = x) + 
	geom_line() + 
	theme_bw() + 
	scale_x_continuous(limits = c(0,1), breaks = (0:10)*.1, expand = c(0,0)) +
	scale_y_continuous(limits = c(0,.5), breaks = (0:10)*.05, expand = c(0,0))

# --------------------------------------------------------------------------
# MULTIVARIABLE
# --------------------------------------------------------------------------

u    <- .5
a    <- 0.7
g    <- c(1-u, u)
P    <- 1 - diag(c(1, 1))
u    <- g
# x    <- c(0.4, .1, .1, 0.4)
x    <- c(0.5, 0, 0, 0.5)
C <- c*x/rep(u, c(2,2))
# quick comparison test

# EV agreement

l <- 10^(-7)
test <- .5*EV_multi(a = a, mode = mode, i = 0, k_0 = C[1], j_0 = 1 , u = u, x = x, c = c) + 
		.5*EV_multi(a = a, mode = mode, i = 1, k_0 = C[4], j_0 = 1 , u = u, x = x, c = c)

test_2 <- EV(a = a, mode = mode, u = u[1], c = c, x = x[2] + x[3])

test_3 <- EV_m(c, a, l, g, x, u, mode)

c(test[2] + test[3], test_2, test_3[2] + test_3[3])


arch <- function(u_1, a, mode = 'random'){
	
		lo  <- c(0, 0, 0, 0)
		hi  <- c(1, 1, 1, 1)
		u <- c(1-u_1, u_1)
		
		f <- function(x_0){
		
			h <- function(x){ # experimental
				y <- c(x[1], x[2], x[2], 1 - x[1] - 2*x[2])
				dx_m(c, a, l, g, P, u, y, mode) %>% sq_norm()
			}
			
			hin <- function(x){
				c(x[1], x[2], 1 - x[1], 1 - x[2], 1 - x[1] - 2*x[2])
			}
			
			res <- auglag(par = x_0, 
						  fn = h,
						  hin = hin)	  
					   
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
		
		return(test_df[test_df$val == min(test_df$val),])
		
}







# --------------------------------------------------------------------------
# Tests for varying u (later)
# --------------------------------------------------------------------------




# do computations
df <- expand.grid(u = seq(0.05, .95, .05),
				  a_hat = c(1, 2 ,3, 4, 5)) %>%
	mutate(alpha = a_hat * .1) %>%
	tbl_df() %>%
	mutate(res = map2(u, alpha, arch, mode = 'same')) %>% 
	unnest(res) %>% 
	mutate(x1 = map_dbl(x_, ~.x[2] + .x[3]))


coeffs <- data_frame(
	a_hat = 1:7,
	a = c(-1.46, -1.49, -1.53, -1.60, -1.71, -1.89, -2.2),
	cee = c(-0.024, -0.05, -0.08, -0.12, -0.19, -0.30, -0.49)) %>%
	mutate(bee = -a)

df %>%
	left_join(coeffs, by = c('a_hat' = 'a_hat')) %>%
	mutate(observed = a*u^2 + bee*u + cee,
		   PA = u*(1-u)*(3 - (alpha/(1-alpha))*(u^2 + (1-u)^2))) %>%
	ggplot() +
	aes(x = u) +
	# geom_line(aes(group = alpha, y = observed)) +
	geom_line(aes(y = observed, group = alpha, color = factor(alpha)), linetype = 'dashed', size = 1) +
	geom_point(aes(y = x1, color = factor(alpha), alpha = -log(val)), size = 1) +
	# geom_line(aes(y = PA), linetype = 'dashed') +
	theme_bw() +
	scale_x_continuous(limits = c(0,1), 
					   breaks = (0:10)*.1) +
	scale_y_continuous(limits = c(-0.001, .35), 
					   breaks = (0:10)*.05) + 
	guides(color = guide_legend(title = expression(alpha))) +
	xlab(expression(italic(u))) + 
	ylab(expression(rho))
	# scale_color_brewer(palette = 'Dark2') 
