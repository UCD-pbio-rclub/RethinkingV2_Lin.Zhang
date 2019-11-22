---
title: "Chapter12_HW"
author: "Lin Zhang"
date: "11/15/2019"
output: 
  html_document: 
    keep_md: yes
---

11E3. When count data are zero-inflated, using a model that ignores zero-inflation will tend to induce which kind of inferential error?
zero-inflated Poisson

11E4. Over-dispersion is common in count data. Give an example of a natural process that might produce over-dispersed counts. Can you also give an example of a process that might produce under-dispersed counts?
The main feature of the Poisson model is the assumption that the mean and variance of the count data are equal. However, this equal mean-variance relationship rarely occurs in observational data.In most cases, the observed variance is larger than the assumed variance, which is called overdispersion. 

11M3. Can you modify the derivation of the zero-inflated Poisson distribution (ZIPoisson) from the chapter to construct a zero-inflated binomial distribution?

11H6. The data in data(Fish) are records of visits to a national park. See ?Fish for details. The question of interest is how many fish an average visitor takes per hour, when fishing. The problem is that not everyone tried to fish, so the fish_caught numbers are zero-inflated. As with the monks example in the chapter, there is a process that determines who is fishing (working) and another process that determines fish per hour (manuscripts per day), conditional on fishing (working). We want to model both. Otherwise we’ll end up with an underestimate of rate of fish extraction from the park. You will model these data using zero-inflated Poisson GLMs. Predict fish_caught as a function of any of the other variables you think are relevant. One thing you must do, however, is use a proper Poisson offset/exposure in the Poisson portion of the zero-inflated.

```r
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Loading required package: StanHeaders
```

```
## Loading required package: ggplot2
```

```
## rstan (Version 2.19.2, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## For improved execution time, we recommend calling
## Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
## although this causes Stan to throw an error on a few processors.
```

```
## Loading required package: parallel
```

```
## Loading required package: dagitty
```

```
## rethinking (Version 1.90)
```

```
## 
## Attaching package: 'rethinking'
```

```
## The following object is masked from 'package:stats':
## 
##     rstudent
```

```r
data("Fish")
d <- Fish
```


```r
hist(d$hours)
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
hist(log(d$hours))
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
hist(scale(log(d$hours)))
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
d$H <- scale(log(d$hours))
```


```r
hist(d$fish_caught)
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
hist(d$child)
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
hist(d$persons)
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
dat <- list(FC=d$fish_caught,B=d$livebait, C=d$camper,P=d$persons,CH=d$child,H=d$hours)
```

no prior prediction implemented

```r
m11H6.1 <- ulam(
  alist(
    FC ~ dzipois(p,lambda),
    logit(p) <- ap,
    log(lambda) <- al,
    ap ~ dnorm(-1.5,1),
    al ~ dnorm(1,0.5)
    ),data = dat, chains = 4
)
```

```
## 
## SAMPLING FOR MODEL 'a4e96fd07a31d5982e8117a2849f77e2' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.865 seconds (Warm-up)
## Chain 1:                0.762 seconds (Sampling)
## Chain 1:                1.627 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL 'a4e96fd07a31d5982e8117a2849f77e2' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0.001 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 2: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 2: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 2: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 2: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 2: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 2: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 2: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 2: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 2: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 2: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 2: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 1.084 seconds (Warm-up)
## Chain 2:                0.725 seconds (Sampling)
## Chain 2:                1.809 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL 'a4e96fd07a31d5982e8117a2849f77e2' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 0 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 3: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 3: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 3: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 3: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 3: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 3: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 3: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 3: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 3: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 3: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 3: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 1.03 seconds (Warm-up)
## Chain 3:                0.795 seconds (Sampling)
## Chain 3:                1.825 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL 'a4e96fd07a31d5982e8117a2849f77e2' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 4: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 4: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 4: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 4: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 4: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 4: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 4: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 4: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 4: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 4: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 4: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 1.536 seconds (Warm-up)
## Chain 4:                1.344 seconds (Sampling)
## Chain 4:                2.88 seconds (Total)
## Chain 4:
```

```r
precis(m11H6.1)
```

```
##         mean         sd       5.5%     94.5%    n_eff      Rhat
## ap 0.2432134 0.12738890 0.04171562 0.4500226 1287.521 0.9997624
## al 2.0269760 0.03443616 1.97277759 2.0799813 1413.719 1.0011302
```

```r
plot(precis(m11H6.1))
```

![](Chapter12_HW_files/figure-html/only intercept model-1.png)<!-- -->

```r
inv_logit(0.25)
```

```
## [1] 0.5621765
```

```r
exp(2.03)
```

```
## [1] 7.614086
```

```r
m11H6.2 <- ulam(
  alist(
    FC ~ dzipois(p,lambda),
    logit(p) <- ap+bC*C,
    log(lambda) <- log(H)+al+bP*P+bCH*CH+bB*B,
    ap ~ dnorm(-1.5,1),
    al ~ dnorm(1,0.5),
    bC ~ dnorm(0,1.5),
    bP ~ dnorm(0,1.5),
    bCH ~ dnorm(0,1.5),
    bB ~ dnorm(0,1.5)
    ),data = dat, chains = 4,cores = 4
)
```


```r
precis(m11H6.2)
```

```
##           mean         sd       5.5%      94.5%     n_eff      Rhat
## ap  -1.3496482 0.44185422 -2.0651716 -0.7029074  831.0953 1.0048687
## al  -2.8555284 0.20638249 -3.1981034 -2.5359551  926.2625 1.0004087
## bC   0.1775955 0.50785411 -0.5885400  0.9880253  913.2009 1.0031651
## bP   0.6136935 0.03887272  0.5516955  0.6750770 1292.1205 1.0006929
## bCH  0.5205832 0.08386689  0.3851430  0.6528349 1607.9465 1.0012885
## bB   0.8169419 0.17430194  0.5445020  1.1031295 1332.0412 0.9988483
```

```r
plot(precis(m11H6.2))
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
inv_logit(-1.35+0.16) # probablity of go fishing considering camping or not 
```

```
## [1] 0.2332589
```

```r
#pairs()
# interpret exp() predictor parameters
```

11H1. In 2014, a paper was published that was entitled “Female hurricanes are deadlier than male hurricanes.”185 As the title suggests, the paper claimed that hurricanes with female names have caused greater loss of life, and the explanation given is that people unconsciously rate female hurricanes as less dangerous and so are less likely to evacuate. Statisticians severely criticized the paper after publication. Here, you’ll explore the complete data used in the paper and consider the hypothesis that hurricanes with female names are deadlier. Load the data with:
library(rethinking) data(Hurricanes)
R code 12.38
Acquaint yourself with the columns by inspecting the help ?Hurricanes. In this problem, you’ll focus on predicting deaths using femininity of each hurricane’s name.
Fit and interpret the simplest possible model, a Poisson model of deaths using femininity as a predictor. You can use map or map2stan. Compare the model to an intercept-only Poisson model of deaths. How strong is the association between femininity ofname and deaths? Which storms does the model fit (retrodict) well? Which storms does it fit poorly?


```r
data("Hurricanes")
d <- Hurricanes
```


```r
hist(d$femininity)
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
hist(d$min_pressure)
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
hist(d$deaths)
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

```r
unique(d$category)
```

```
## [1] 3 1 4 2 5
```

```r
hist(d$damage_norm)
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-9-4.png)<!-- -->

```r
dat <- list(D=d$deaths,Fe=d$femininity)
```


```r
m11H1.1 <- ulam(
  alist(
    D ~ dpois(lambda),
    log(lambda) <- al+bFe*Fe,
    al ~ dnorm(1,0.5),
    bFe ~ dnorm(0,1.5)
  ), data = dat, chains = 4,cores = 4
)
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```
## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#tail-ess
```


```r
precis(m11H1.1)
```

```
##           mean          sd       5.5%      94.5%    n_eff     Rhat
## al  2.47209326 0.066242755 2.36658781 2.57821540 365.5240 1.001804
## bFe 0.07712346 0.008316865 0.06368771 0.09058158 370.2845 1.002273
```

```r
plot(precis(m11H1.1))
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
exp(2.47+0.08)
```

```
## [1] 12.8071
```
association between femininity of name and death is very strong

```r
m11H1.2 <- ulam(
  alist(
    D ~ dpois(lambda),
    log(lambda) <- al,
    al ~ dnorm(1,0.5)
  ), data = dat, chains = 4,cores = 4
)
```


```r
precis(m11H1.2)
```

```
##        mean         sd     5.5%    94.5%    n_eff     Rhat
## al 3.023538 0.02319732 2.986093 3.059237 564.9045 1.012304
```

```r
plot(precis(m11H1.2))
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
exp(3.02)
```

```
## [1] 20.49129
```
intercept only model value is even bigger, indicating the strong correlation being suspicious


```r
postcheck(m11H1.1)
```

![](Chapter12_HW_files/figure-html/unnamed-chunk-15-1.png)<!-- -->![](Chapter12_HW_files/figure-html/unnamed-chunk-15-2.png)<!-- -->![](Chapter12_HW_files/figure-html/unnamed-chunk-15-3.png)<!-- -->![](Chapter12_HW_files/figure-html/unnamed-chunk-15-4.png)<!-- -->![](Chapter12_HW_files/figure-html/unnamed-chunk-15-5.png)<!-- -->
fit best storms: 7,8,12,15,30,36,40,49,58,82
fit poorly storms: 92,29,10

11H2. Counts are nearly always over-dispersed relative to Poisson. So fit a gamma-Poisson (aka negative-binomial) model to predict deaths using femininity. Show that the over-dispersed model no longer shows as precise a positive association between femininity and deaths, with an 89% interval that overlaps zero. Can you explain why the association diminished in strength?

```r
dat <- list(D=d$deaths,Fe=d$femininity,fid=d$female)
```


```r
# m11H1.3 <- ulam(
#   alist(
#     D ~ dbetabinom(pbar,theta),
#     logit(pabar) <- al+bFe*Fe,
#     al ~ dnorm(0,1.5),
#     bFe ~ dnorm(0,1.5),
#     theta ~ dexp(1)
#   ), data = dat, chains = 4,cores = 4
# )
```

