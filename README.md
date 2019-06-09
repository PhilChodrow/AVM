# AVM

Simulation and approximation code for the paper ["Local Symmetry and Global Structure in Adaptive Voter Models"](https://arxiv.org/abs/1812.05464) by [Phil Chodrow](https://www.philchodrow.com) and [Peter Mucha](http://mucha.web.unc.edu).  

The code for simulating adaptive voter models was lightly modified from that provided by [Feng Bill Shi](http://billshi.net/), available [here](https://github.com/bill10/Evolving-Voter). 

## Overview

### Simulation

The simulation code is implemented in C++. While it is possible to call the simulation directly, a better solution for bulk computation is to interact with the C++ code via the file `C/run.py`, which handles multiple simulations with varying parameters. 

**Requirements**

- Python 2.7+
- Python modules: `numpy`, `subprocess`. 

### Approximations

The novel approximation method described in the paper is implemented in `R`. 
While these functions may be called directly, an easier approach is to program parameter ranges into `compute_arch.R`. 

**Requirements**

- `R` 3.3+
- `R` packages: `tidyverse`, `alabama`

### Visualizations

The file `figs.rmd` is sufficient to produce all figures in the paper, with the exception of Figure 1, once both `C/run.py` and `compute_arch.R` have been run with their provided parameter values. Note that multiple days of computation time may be required to produce the complete set of results and figures from scratch. 

**Requirements**

- `R` 3.3+
- `R` packages: `tidyverse`, `ggjoy`, `data.table`, `RColorBrewer`








