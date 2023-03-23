
<!-- README.md is generated from README.Rmd. Please edit that file -->

# signcon

<!-- badges: start -->
<!-- badges: end -->

The goal of signcon is to provide a frequentist statistical framework
to test a nondirectional null hypotheses. Nondirectional hypotheses may
be rejected in cases where individual subjects show reliable effects,
even if these effects have opposite signs and cancel out at the group
level in a standard t-test.

## Installation

You can install the development version of signcon like so:

``` r
remotes::install_github('mufcItay/signcon')
```

## Example

To demonstrate the use of signcon, we use confidence rating data from
an experiment where participants reported the orientation of visual
gratings, and then rated their confidence on a 1-6 scale. We ask whether
confidence levels were similar for the two responses, first using a
standard t-test, and then using non-directional sign-consistency and
classification tests:

``` r
library(signcon)
library(magrittr)

# compare the mean confidence for the two responses, 
# using a standard within-subject t-test:
t_test_result <- visual_metacognition %>%
    dplyr::group_by(Subj_idx,Response) %>%
    dplyr::summarise(Confidence=mean(Confidence), .groups = 'drop_last') %>%
    tidyr::spread(Response, Confidence,sep='.') %>%
    dplyr::mutate(diff=Response.1-Response.0) %>%
    dplyr::pull(diff) %>%
    t.test()

# compare the mean confidence for the two responses, 
# using a non-parametric directional effect test: 
directional_effect_result <- visual_metacognition %>% 
  signcon::test_directional_effect(idv="Subj_idx", 
                                          dv='Confidence', 
                                          iv='Response')
#> [1] "Generating null distribution"
#> ================================================================================

# compare the mean confidence for the two responses, 
# using a non-directional sign-consistency test:
sign_consistency_result <- visual_metacognition %>% 
  signcon::test_sign_consistency(idv="Subj_idx", dv='Confidence', iv='Response')
#> [1] "Generating null distribution"
#> ================================================================================
```

We find that at the group level, the mean difference in confidence
between the two responses is 0.02 on a 1-6 scale, and not significantly
different from zero
(![t(45)=-0.32, p=0.75](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t%2845%29%3D-0.32%2C%20p%3D0.75 "t(45)=-0.32, p=0.75")).
This is also the case when we test for a directional effect with a
non-parameteric test
(![p=0.63](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p%3D0.63 "p=0.63")).
In contrast, sign-consistency equals 0.72, significantly above chance
(![p\<0.001](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p%3C0.001 "p<0.001")),
indicating that individual subjects were consistently more confident in
one response over the other.
