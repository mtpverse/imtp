
<!-- README.md is generated from README.Rmd. Please edit that file -->

# imtp

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

> Non-Parametric Causal Effects Based on Incremental Propensity Score
> Interventions

An implementation of the incremental propensity score intervention
Z-estimator described in [Kennedy
(2019)](https://doi.org/10.1080/01621459.2017.1422737). The UI is
implemented in the same manner as the
[`lmtp`](https://github.com/nt-williams/lmtp) package and provides a
compliment to the main objective of
[`lmtp`](https://github.com/nt-williams/lmtp) for when
treatment/exposure is binary.

## Installation

You can install the development version of `imtp` from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("mtpverse/imtp")
```

## Example

``` r
library(imtp)

n <- 1000
W <- matrix(rnorm(n*3), ncol = 3)
A <- rbinom(n, 1, 1/(1 + exp(-(.2*W[,1] - .1*W[,2] + .4*W[,3]))))
Y <- A + 2*W[,1] + W[,3] + W[,2]^2 + rnorm(n)
R <- rbinom(n, 1, 0.9)
ex <- data.frame(W, A, R, Y = ifelse(R == 1, Y, NA_real_))

imtp_tmle(ex, "A", "Y", paste0("X", 1:3), cens = "R", 
                    folds = 5, delta = 2, outcome_type = "continuous")
#> IPSI Estimator: TMLE
#>          delta: 2
#> 
#> Population intervention estimate
#>       Estimate: 1.5581
#>     Std. error: 0.1054
#>         95% CI: (1.3515, 1.7647)
```

## References

Edward H. Kennedy (2019) Nonparametric Causal Effects Based on
Incremental Propensity Score Interventions, Journal of the American
Statistical Association, 114:526, 645-656, DOI:
10.1080/01621459.2017.1422737

Kwangho Kim and Edward H. Kennedy and Ashley I. Naimi (2019) Incremental
Intervention Effects in Studies with Many Timepoints, Repeated Outcomes,
and Dropout, arXiv: 1907.04004

Iván Díaz, Nicholas Williams, Katherine L. Hoffman & Edward J. Schenck
(2021) Non-parametric causal effects based on longitudinal modified
treatment policies, Journal of the American Statistical Association,
DOI: 10.1080/01621459.2021.1955691
