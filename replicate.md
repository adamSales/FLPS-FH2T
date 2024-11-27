---
title: "Replication Code"
output: github_document
---



## Step 0: Load packages


```r
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(missForest)
library(forcats)
library(rstan)
library(splines)
library(huxtable)
library(flextable)
library(tidyr)
library(ggplot2)
library(xtable)
library(table1)

sessionInfo()
```

```
## R version 4.2.2 (2022-10-31)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Debian GNU/Linux 11 (bullseye)
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] splines   stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] lme4_1.1-30           Matrix_1.5-1          table1_1.4.3         
##  [4] xtable_1.8-4          tidyr_1.3.0           flextable_0.9.0      
##  [7] huxtable_5.5.2        rstan_2.21.8          ggplot2_3.4.1        
## [10] StanHeaders_2.21.0-7  forcats_0.5.2         missForest_1.5       
## [13] lubridate_1.9.2       purrr_1.0.2           readr_2.1.4          
## [16] dplyr_1.1.3           languageserver_0.3.15 httpgd_1.3.1         
## 
## loaded via a namespace (and not attached):
##  [1] minqa_1.2.4             colorspace_2.0-3        ellipsis_0.3.2         
##  [4] httpcode_0.3.0          bit64_4.0.5             fansi_1.0.5            
##  [7] xml2_1.3.3              codetools_0.2-18        cachem_1.0.6           
## [10] knitr_1.40              itertools_0.1-3         Formula_1.2-4          
## [13] jsonlite_1.8.0          nloptr_2.0.3            broom_1.0.1            
## [16] shiny_1.7.2             compiler_4.2.2          backports_1.4.1        
## [19] assertthat_0.2.1        fastmap_1.1.0           cli_3.6.1              
## [22] later_1.3.0             htmltools_0.5.3         prettyunits_1.1.1      
## [25] tools_4.2.2             gtable_0.3.0            glue_1.6.2             
## [28] doRNG_1.8.6             Rcpp_1.0.10             fontquiver_0.2.1       
## [31] vctrs_0.6.4             crul_1.3                nlme_3.1-160           
## [34] iterators_1.0.14        xfun_0.32               stringr_1.5.0          
## [37] ps_1.7.1                timechange_0.2.0        mime_0.12              
## [40] lifecycle_1.0.3         rngtools_1.5.2          MASS_7.3-58.1          
## [43] scales_1.2.1            vroom_1.6.4             ragg_1.2.5             
## [46] hms_1.1.2               promises_1.2.0.1        parallel_4.2.2         
## [49] inline_0.3.19           fontLiberation_0.1.0    curl_5.0.0             
## [52] memoise_2.0.1           gridExtra_2.3           gdtools_0.3.2          
## [55] loo_2.5.1               stringi_1.7.12          fontBitstreamVera_0.1.1
## [58] highr_0.9               foreach_1.5.2           randomForest_4.7-1.1   
## [61] boot_1.3-28             pkgbuild_1.3.1          zip_2.2.0              
## [64] rlang_1.1.1             pkgconfig_2.0.3         systemfonts_1.0.4      
## [67] matrixStats_0.62.0      evaluate_0.16           lattice_0.20-45        
## [70] bit_4.0.4               processx_3.7.0          tidyselect_1.2.0       
## [73] magrittr_2.0.3          R6_2.5.1                generics_0.1.3         
## [76] DBI_1.1.3               pillar_1.9.0            withr_2.5.1            
## [79] tibble_3.2.1            crayon_1.5.1            gfonts_0.2.0           
## [82] uuid_1.1-0              utf8_1.2.3              tzdb_0.3.0             
## [85] rmarkdown_2.16          officer_0.6.1           grid_4.2.2             
## [88] data.table_1.14.2       callr_3.7.3             digest_0.6.29          
## [91] httpuv_1.6.5            textshaping_0.3.6       openssl_2.0.6          
## [94] RcppParallel_5.1.5      stats4_4.2.2            munsell_0.5.0          
## [97] askpass_1.1
```

## Step 1: Data Munging

```r
source('R/data.r')
source('R/sdatSimp.r')
```

![plot of chunk dataMunge](figure/dataMunge-1.png)


## Step 2: Fit the models

Fitting the models takes a loooonnnng time

This code checks if there's already a fitted model available. If there isn't, it fits the model. To fit the models regardless, change the following to "TRUE":

```r
refit <- FALSE
```

Rasch:

```r
if(refit|!file.exists('fittedModels/flpsRasch1.RData')){
  print("Fitting Rasch")
  source('R/flpsRasch.r')
}
```

2PL:

```r
if(refit|!file.exists('fittedModels/flps2plStan2.RData')){
  print("Fitting 2PL")
  source('R/flps2pl.r')
}
```

GRM:

```r
if(refit|!file.exists('fittedModels/grm2.RData')){
  print("Fitting GRM")
  source('R/grm.r')
}
```


## Step 3: Make all the tables & figures


```r
source('R/tables.r')
```

```
## [1] "fit"
##  [1] "tt"                                               
##  [2] "sdat"                                             
##  [3] ".__global__"                                      
##  [4] ".__C__C++Object"                                  
##  [5] "trtNum"                                           
##  [6] ".__C__Rcpp_stan_fit4model151da8284e6495_rasch1lev"
##  [7] "sdatFake"                                         
##  [8] ".Random.seed"                                     
##  [9] ".requireCachedGenerics"                           
## [10] "nt"                                               
## [11] "flpsRaschCheck0"                                  
## [12] "flpsRasch1"                                       
## [13] "newNum"                                           
## [1] "sdat"                                              
## [2] ".__global__"                                       
## [3] ".__C__C++Object"                                   
## [4] ".__C__Rcpp_stan_fit4model9c1aa1175aa06_stan2plFLPS"
## [5] ".Random.seed"                                      
## [6] ".requireCachedGenerics"                            
## [7] "flps2pl"                                           
## [1] "studDat1"
```
