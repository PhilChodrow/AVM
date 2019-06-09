# This is a utility script designed only to read the output of compute_arch.R into 
# an R session and add a few important columns. 

library(tidyverse)
library(data.table)

read_data <- function(){
    data_dir <- 'data/C/'
    files <- list.files(data_dir, full.names = T) 
    files <- files[grepl('.process', files)]
    
    read_files <- function(mode){
        df <- files[grepl(paste0('mode_', mode), files)] %>% 
            map(fread) %>% 
            bind_rows()
        
        df %>% 
            mutate(mode = ifelse(mode == 1, 'same', 'random'))
    }
    
    df <- c(0,1) %>% 
        map(read_files) %>% 
        bind_rows() %>% 
        tbl_df()
    
    df <- df %>% 
        mutate(c = (N00 + 2*N01 + N11)/ (N0 + N1),
               x = 2*N01 / (N00 + 2*N01 + N11),
               dN = N01 - N01_prev,
               u = N1/(N0 + N1),
               mode = as.character(mode)) %>% 
        filter(c >= 4)  
    df
}

