
<!-- README.md is generated from README.Rmd. Please edit that file -->

# weaknull

<!-- badges: start -->
<!-- badges: end -->

The goal of weaknull is to provide a frequentist statistical framework
to test weak, nondirectional null hypotheses. Weak null hypotheses may
be rejected in cases where individual subjects show reliable effects,
even if these effects have opposite signs and cancel out at the group
level in a standard t-test.

## Installation

You can install the development version of weaknull like so:

``` r
remotes::install_github('mufcItay/weaknull')
```

## Example

To demonstrate the use of weaknull, we use confidence rating data from
an experiment where participants reported the orientation of visual
gratings, and then rated their confidence on a 1-6 scale. We ask whether
confidence levels were similar for the two responses, first using a
standard t-test, and then using non-directional sign-consistency and
classification tests:

``` r
library(weaknull)
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.1.3
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.8
#> v tidyr   1.2.0     v stringr 1.4.0
#> v readr   2.1.2     v forcats 0.5.1
#> Warning: package 'ggplot2' was built under R version 4.1.3
#> Warning: package 'tibble' was built under R version 4.1.3
#> Warning: package 'tidyr' was built under R version 4.1.3
#> Warning: package 'readr' was built under R version 4.1.3
#> Warning: package 'purrr' was built under R version 4.1.3
#> Warning: package 'dplyr' was built under R version 4.1.3
#> Warning: package 'stringr' was built under R version 4.1.3
#> Warning: package 'forcats' was built under R version 4.1.3
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

# compare the mean confidence for the two responses, 
# using a standard within-subject t-test:
t_test_result <- visual_metacognition %>%
  group_by(Subj_idx,Response) %>%
  summarise(Confidence=mean(Confidence)) %>%
  spread(Response, Confidence,sep='.') %>%
  mutate(diff=Response.1-Response.0) %>%
  pull(diff) %>%
  t.test()
#> `summarise()` has grouped output by 'Subj_idx'. You can override using the
#> `.groups` argument.

# compare the mean confidence for the two responses, 
# using a non-directional sign-consistency test:
sign_consistency_result <- visual_metacognition %>% 
  weaknull::test_sign_consistency(idv="Subj_idx", dv='Confidence', iv='Response')

# compare the mean confidence for the two responses, 
# using a non-directional classification test 
# (this may take a couple of minutes to run on a standard PC):
classification_result <- visual_metacognition %>% 
  weaknull::test_condition_classification(idv="Subj_idx", 
                                          dv='Confidence', 
                                          iv='Response')
```

We find that at the group level, the mean difference in confidence
between the two responses is 0.02 on a 1-6 scale, and not significantly
different from zero
(![t(45)=-0.32, p=0.75](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t%2845%29%3D-0.32%2C%20p%3D0.75 "t(45)=-0.32, p=0.75")).
In contrast, sign-consistency equals 0.72, significantly above chance
(![p\<0.001](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p%3C0.001 "p<0.001")),
indicating that individual subjects were consistently more confident in
one response over the other. Similarly, a linear classifier predicted
response from confidence with 54% accuracy, significantly above chance
(![p\<0.001](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p%3C0.001 "p<0.001")).
