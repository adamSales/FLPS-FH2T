# Replication code for the applied analysis of
# "Fully Latent Principal Stratification With Measurement Models"
## By Sooyong Lee, Adam C Sales, Hyeon-Ah Kang, Tiffany A. Whittaker


To replicate the applied analysis, first obtain the replication data by following the instructions at https://osf.io/r3nf2/

Place the relevant data files in a folder called "data" 

Create empty folders "fittedModels" "tables" and "plots"

Ensure that the necessary packages have been installed.

Finally, in `R`, run:

```
knitr::knit('replicate.rmd')
```