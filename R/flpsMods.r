library(rstan)
library(splines)

options(mc.cores = 8)
rstan_options(auto_write = TRUE)

source('R/classicPS.r')
source('R/flpsRasch.r')
source('R/flps2pl.r')
