
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
estimator described in [Kennedy
(2019)](https://doi.org/10.1080/01621459.2017.1422737). The UI is
implemented in the same manner as the
[`lmtp`](https://github.com/nt-williams/lmtp) package and provides a
compliment to the main objective of
[`lmtp`](https://github.com/nt-williams/lmtp) for when
treatment/exposure is binary.

#### Are incremental propensity score interventions MTPs?

Yes! A modified treatment policy is simply an intervention that can be
written as a function of the natural value of exposure. Using this
defintion, an incremenental propensity score intervention may be defined
as a modified treatment policy. See Example 3 in [*Non-parametric causal
effects based on longitudinal modified treatment
policies*](https://doi.org/10.1080/01621459.2021.1955691) for more
details.

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
tmp <- data.frame(W, A, R, Y = ifelse(R == 1, Y, NA_real_))

imtp_tmle(tmp, "A", "Y", paste0("X", 1:3), cens = "R", delta = 2, outcome_type = "continuous")
#> IPSI Estimator: TMLE
#>          delta: 2
#> 
#> Population intervention estimate
#>       Estimate: 1.6243
#>     Std. error: 0.107
#>         95% CI: (1.4145, 1.834)

deltas <- seq(0.1, 2, length.out = 5)
fits <- lapply(deltas, function(d) imtp_tmle(tmp, "A", "Y", paste0("X", 1:3), cens = "R", delta = d, outcome_type = "continuous"))
imtp_simul(fits)
#>      theta mult.conf.low mult.conf.high
#> 1 1.064633     0.9057856       1.223480
#> 2 1.313744     1.1279433       1.499544
#> 3 1.461470     1.2513807       1.671560
#> 4 1.569143     1.3432812       1.795005
#> 5 1.630143     1.3904212       1.869865
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
