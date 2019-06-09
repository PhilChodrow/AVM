library(tidyverse)
source('r/models.R')

if(!dir.exists('throughput')){
    dir.create('throughput')
}

# Construct the base df of parameters on which we will compute. 
arch_df <- expand.grid(
                       u = seq(0.01, .99, .01),
                       alpha = seq(.01, .99, .01),
                       c = c(4, 6, 8, 20),
                       mode = c('random', 'same'),
                       scale = T,
                       stringsAsFactors = F) %>%
    tbl_df() %>% 
    # main computation is here: 
    mutate(opt = pmap(.l = list(alpha, u, c, mode, scale),  
                      .f =  safely(arch),
                      print_pars = T)) 

# Clean the result
arch_df <- arch_df %>%
    mutate(res = map(opt, 'result'),
           valid = map_lgl(res, ~!is.null(.x))) %>% 
    filter(valid) %>% 
    mutate(x_ = map(res, 'x_'),
           x_ = flatten(x_),
           valid = map_lgl(x_, ~!is.null(.x))) %>% 
    filter(valid) %>% 
    mutate(x_ = map(res, 'x_'),
           x_ = map(x_, ~.x[[1]]),
           x_01 = map_dbl(x_, ~.x[2] + .x[3]),
           x_11 = map_dbl(x_, ~.x[4]),
           x_00 = map_dbl(x_, ~.x[1]),
           val = map_dbl(res, 'val')) %>% 
    select(-x_, -opt, -valid, -res)

# write the result, appending to an existing set of results if it exists. 
write.table(arch_df, "throughput/model_arch.csv", sep = ",", 
            col.names = !file.exists("throughput/model_arch.csv"), 
            append = T, 
            row.names = F)

