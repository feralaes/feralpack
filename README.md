# dampack: an R package for decision-analytic modeling
The `dampack` R package implements useful functions to develop and analyze decision-analytic models in R. The current functions compute cost-effectiveness acceptability curves (CEAC) and frontier (CEAF), expected value of perfect information (EVPI), expected value of partial perfect information (EVPPI), sensitivity analysis (SA) using linear regression metamodeling including one- and two-way. 

The package also includes functions to simulate state-transition models and produce expected outcomes of interested.

In addition, this package includes useful functions to obtain parameters of commonly used distributions 

## Installation 
To get the current development version from github:

```R
# install.packages("devtools")
devtools::install_github("feralaes/dampack")
```

## Documentation
Documentation is still under development but the most current description of the function in this package appear in `vignettes`. Specifically, in the vignette `dampack_vignette`, we provide examples on how to use the different functions of the package and in the `Markov_CEA_example` vignette, we provide an example on how to run Markov models for cost-effectiveness analysis (CEA) in R using the functions of the dampack package.

## Example
Below, we provide a brief example on how to plot the cost-effectiveness acceptability curves (CEAC) and frontier (CEAF) of a three-strategy CEA using a probabilistic sensitivity analysis (PSA) dataset.
```R
library(dampack)
# Load PSA dataset
data(psa)
# Name of strategies
strategies <- c("Chemo", "Radio", "Surgery")
# Vector of WTP thresholds
v.wtp <- seq(1000, 150000, by = 10000)
# Matrix of costs
m.c <- psa[, c(2, 4, 6)]
# Matrix of effectiveness
m.e <- psa[, c(3, 5, 7)]
# Compute CEAF
out <- ceaf(v.wtp = v.wtp, strategies = strategies, 
            m.e = m.e , m.c = m.c,
            ceaf.out = TRUE)
# Plot CEAF
out$gg.ceaf
```


