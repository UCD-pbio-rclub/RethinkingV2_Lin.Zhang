---
title: "Chapter_13_HW"
output: 
  html_document: 
    keep_md: yes
---

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
## rethinking (Version 1.93)
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

#12E1. Which of the following priors will produce more shrinkage in the estimates? (a) αtank ∼
Normal(0,1); (b) αtank ∼ Normal(0,2).
(a),as this prior is more narrow and produce better prediction about the population mean cross clusters (tanks) data.

#12E2.Make the following model into a multilevel model.
yi ∼ Binomial(1,pi)
logit(pi) = αgroup[i] + βxi
αgroup ∼ Normal(0,10)
β ∼ Normal(0,1)

yi ∼ Binomial(1,pi)
logit(pi) = αgroup[i] + βxi 
agroup ~ Normal(a_bar,sigma)
a_bar ~ Normal(0,1.5)
sigma ~ Exponential (1) #why exponential

#12E3.Make the following model into a multilevel model.
yi ∼ Normal(µi,σ)
µi = αgroup[i] + βxi
αgroup ∼ Normal(0,10)
β ∼ Normal(0,1)
σ ∼ HalfCauchy(0,2)

yi ∼ Normal(µi,σ)
µi = αgroup[i] + βxi
αgroup ∼ Normal(a_bar,sigma)
a_bar ~ Normal(0,1.5)
sigma ~ Exponential
β ∼ Normal(0,1)
σ ∼ HalfCauchy(0,2)

#12M1.Revisit the Reed frog survival data, data(reedfrogs), and add the predation and size
treatment variables to the varying intercepts model. Consider models with either main eﬀect alone,
both main eﬀects, as well as a model including both and their interaction. Instead of focusing on
inferences about these two predictor variables, focus on the inferred variation across tanks. Explain why it changes as it does across models.


```r
data(reedfrogs) 
d <- reedfrogs
str(d)
```

```
## 'data.frame':	48 obs. of  5 variables:
##  $ density : int  10 10 10 10 10 10 10 10 10 10 ...
##  $ pred    : Factor w/ 2 levels "no","pred": 1 1 1 1 1 1 1 1 2 2 ...
##  $ size    : Factor w/ 2 levels "big","small": 1 1 1 1 2 2 2 2 1 1 ...
##  $ surv    : int  9 10 7 10 9 9 10 9 4 9 ...
##  $ propsurv: num  0.9 1 0.7 1 0.9 0.9 1 0.9 0.4 0.9 ...
```


```r
d$tank <- 1:nrow(d)
d$pred_id <- ifelse(d$pred=="no",0L,1L)
d$size_id <- ifelse(d$size=="small",0L,1L)
```

## model with only predation as main effect

```r
dat <- list(S=d$surv,
            P=d$pred_id,
            tank=d$tank,
            N=d$density) 
  
m12M1.1 <- ulam(
  alist(
    S ~ dbinom(N,p),
    logit(p) <- a[tank]+bpred*P,
    a[tank] ~ dnorm(a_bar,sigma),
    bpred ~ dnorm(0,0.3),
    a_bar ~ dnorm(0,1.5),
    sigma ~ dexp(1)
  ),data = dat, chains = 4, log_lik = TRUE,iter = 4000
)
```

```
## 
## SAMPLING FOR MODEL 'bbb7a155ccf3758176e447b0b0dc1eeb' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 1: Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 1: Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 1: Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 1: Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 1: Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 1: Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 1: Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 1: Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 1: Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 1: Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 1: Iteration: 4000 / 4000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.486 seconds (Warm-up)
## Chain 1:                0.385 seconds (Sampling)
## Chain 1:                0.871 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL 'bbb7a155ccf3758176e447b0b0dc1eeb' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 2: Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 2: Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 2: Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 2: Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 2: Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 2: Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 2: Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 2: Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 2: Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 2: Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 2: Iteration: 4000 / 4000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 0.43 seconds (Warm-up)
## Chain 2:                0.388 seconds (Sampling)
## Chain 2:                0.818 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL 'bbb7a155ccf3758176e447b0b0dc1eeb' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 0 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 3: Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 3: Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 3: Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 3: Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 3: Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 3: Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 3: Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 3: Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 3: Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 3: Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 3: Iteration: 4000 / 4000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 0.427 seconds (Warm-up)
## Chain 3:                0.356 seconds (Sampling)
## Chain 3:                0.783 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL 'bbb7a155ccf3758176e447b0b0dc1eeb' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 4: Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 4: Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 4: Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 4: Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 4: Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 4: Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 4: Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 4: Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 4: Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 4: Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 4: Iteration: 4000 / 4000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 0.414 seconds (Warm-up)
## Chain 4:                0.347 seconds (Sampling)
## Chain 4:                0.761 seconds (Total)
## Chain 4:
```

```r
precis(m12M1.1,depth = 2)
```

```
##              mean        sd        5.5%      94.5%    n_eff      Rhat
## a[1]   2.13029563 0.7649875  0.97750920  3.4006019 8674.591 1.0004638
## a[2]   2.82275655 0.9097591  1.50206699  4.3790666 7254.593 1.0005801
## a[3]   1.16401846 0.6475355  0.17966304  2.2261767 8242.790 1.0004737
## a[4]   2.83366464 0.9124472  1.52375920  4.3803111 7368.407 1.0003830
## a[5]   2.15372532 0.7748190  0.99784921  3.4394177 8315.914 1.0000975
## a[6]   2.14634803 0.7829614  0.97804273  3.4741996 8618.785 1.0003637
## a[7]   2.83326086 0.9175528  1.52382586  4.3807085 6926.866 1.0002237
## a[8]   2.14241204 0.7795259  0.96964036  3.4583489 9726.376 0.9998968
## a[9]   0.81016085 0.6512509 -0.21383307  1.8360079 2831.347 1.0010345
## a[10]  2.71617857 0.7506004  1.56922999  3.9629442 6146.259 1.0010750
## a[11]  1.84290897 0.6737748  0.77803511  2.9300984 3875.990 1.0001557
## a[12]  1.48858104 0.6409176  0.48441884  2.5079641 2930.914 1.0006881
## a[13]  1.84449689 0.6631253  0.78828111  2.9205362 4226.171 1.0002059
## a[14]  1.14314670 0.6447523  0.11482566  2.1747399 3140.633 1.0013291
## a[15]  2.69577772 0.7442631  1.54302208  3.9181691 6045.008 1.0011738
## a[16]  2.71296239 0.7448440  1.57005565  3.9492241 6454.740 1.0000863
## a[17]  2.79847840 0.7108124  1.76985262  4.0344478 8479.512 0.9997000
## a[18]  2.37554282 0.6101861  1.47480562  3.4111893 9384.838 0.9998922
## a[19]  2.03152303 0.5666605  1.17713116  2.9679296 8338.817 0.9997756
## a[20]  3.32300675 0.8256588  2.13162065  4.7448795 6853.671 1.0001313
## a[21]  2.37276537 0.6176561  1.45731914  3.4203284 8264.338 0.9995912
## a[22]  2.37040442 0.6287281  1.44661154  3.4177436 9222.809 0.9998070
## a[23]  2.37131569 0.6342359  1.43115789  3.4523952 9715.195 0.9997343
## a[24]  1.75225136 0.5154612  0.97015759  2.5991771 8117.839 1.0005613
## a[25]  0.03214619 0.5296865 -0.83824776  0.8559299 1643.810 1.0019423
## a[26]  1.12207763 0.4853248  0.34087820  1.8965036 1809.199 1.0029104
## a[27] -0.35446775 0.5821483 -1.30209906  0.5336372 1818.861 1.0022697
## a[28]  0.52180014 0.4965328 -0.28623214  1.2904624 1843.850 1.0015459
## a[29]  1.12483937 0.4867180  0.34878546  1.8928708 1958.700 1.0029346
## a[30]  2.29812336 0.5214657  1.47213245  3.1388866 2698.819 1.0010792
## a[31]  0.36904110 0.5056808 -0.44741525  1.1742456 1562.754 1.0024459
## a[32]  0.67619416 0.4943819 -0.12882344  1.4571682 1827.901 1.0023632
## a[33]  3.04358382 0.6767147  2.05620646  4.2102128 7309.364 0.9997031
## a[34]  2.64320829 0.5786181  1.77427405  3.6306850 9119.813 0.9997828
## a[35]  2.64440634 0.5928627  1.74797202  3.6435672 9725.600 1.0001247
## a[36]  2.08541940 0.5053472  1.30550422  2.9198690 8863.982 0.9999477
## a[37]  2.06966602 0.4947976  1.32575051  2.8984989 9495.202 0.9996888
## a[38]  3.55608843 0.8371245  2.35243525  4.9933538 5882.079 1.0004959
## a[39]  2.64423899 0.6027745  1.76721727  3.6558271 7694.620 1.0003220
## a[40]  2.32981180 0.5336018  1.52368448  3.2185571 8795.219 1.0000057
## a[41] -0.71258202 0.5716584 -1.68232938  0.1550593 1645.965 1.0028351
## a[42]  0.41994174 0.4555316 -0.31713714  1.1300662 1501.703 1.0025906
## a[43]  0.53148976 0.4507818 -0.19486726  1.2306864 1421.831 1.0036805
## a[44]  0.64627688 0.4408857 -0.07266245  1.3290828 1461.265 1.0027672
## a[45]  1.51975851 0.4362810  0.82029759  2.2130862 1748.101 1.0025476
## a[46]  0.41637620 0.4560120 -0.32407717  1.1345020 1525.670 1.0043814
## a[47]  2.87177391 0.5316331  2.04064239  3.7285396 3105.600 1.0012561
## a[48]  0.97445773 0.4445576  0.25727370  1.6716210 1723.416 1.0024540
## bpred -0.96000765 0.2912001 -1.41256794 -0.4816442  765.791 1.0057923
## a_bar  1.77360246 0.2436225  1.37793787  2.1604085 1593.282 1.0025815
## sigma  1.19823483 0.2128056  0.88265014  1.5543304 1530.256 1.0028478
```

```r
summary(precis(m12M1.1,depth = 2)$mean)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -0.960   1.048   2.070   1.733   2.644   3.556
```

## model with only size as main effect

```r
dat <- list(S=d$surv,
            Z=d$size_id,
            tank=d$tank,
            N=d$density) 
  
m12M1.2 <- ulam(
  alist(
    S ~ dbinom(N,p),
    logit(p) <- a[tank]+bsize*Z,
    a[tank] ~ dnorm(a_bar,sigma),
    bsize ~ dnorm(0,0.3),
    a_bar ~ dnorm(0,1.5),
    sigma ~ dexp(1)
  ),data = dat, chains = 4, log_lik = TRUE,iter = 2000
)
```

```
## 
## SAMPLING FOR MODEL '645cabea34c2e60fdc6354d6008ce694' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.226 seconds (Warm-up)
## Chain 1:                0.172 seconds (Sampling)
## Chain 1:                0.398 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '645cabea34c2e60fdc6354d6008ce694' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 0.248 seconds (Warm-up)
## Chain 2:                0.173 seconds (Sampling)
## Chain 2:                0.421 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '645cabea34c2e60fdc6354d6008ce694' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 0 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 0.233 seconds (Warm-up)
## Chain 3:                0.174 seconds (Sampling)
## Chain 3:                0.407 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '645cabea34c2e60fdc6354d6008ce694' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 0.247 seconds (Warm-up)
## Chain 4:                0.179 seconds (Sampling)
## Chain 4:                0.426 seconds (Total)
## Chain 4:
```

```r
precis(m12M1.2,depth = 2)
```

```
##               mean        sd         5.5%       94.5%     n_eff      Rhat
## a[1]   2.214605899 0.8892400  0.881540349  3.67596309 4212.7954 0.9997266
## a[2]   3.147615426 1.0840789  1.584488026  5.02133801 4367.5845 0.9997265
## a[3]   1.085006114 0.7121370 -0.021824223  2.25302858 2943.8753 0.9996849
## a[4]   3.131875239 1.1038916  1.514932422  5.02320510 4314.6934 0.9994424
## a[5]   2.149294632 0.8780394  0.895329266  3.68327389 4673.7328 1.0004964
## a[6]   2.153574376 0.8827012  0.842164701  3.66304912 5726.3260 0.9994718
## a[7]   3.058611257 1.0948871  1.493635478  5.00534807 4513.3910 0.9999172
## a[8]   2.144812864 0.8769556  0.848195564  3.65234412 5642.9379 0.9993529
## a[9]  -0.081721781 0.6790803 -1.171147548  1.00230294 2948.1056 0.9994214
## a[10]  2.216231438 0.8974705  0.879233913  3.75181725 4007.8760 0.9993210
## a[11]  1.058192232 0.6976713 -0.029085054  2.20184822 3607.4088 0.9996035
## a[12]  0.670945713 0.6838720 -0.401221818  1.78082667 2937.9589 0.9996551
## a[13]  1.004482603 0.6596957  0.002177578  2.10870214 6317.7323 0.9994410
## a[14]  0.211414449 0.6260426 -0.796704455  1.23770428 6924.1052 0.9993501
## a[15]  2.170447879 0.9032494  0.851686424  3.70138287 5259.1985 0.9992856
## a[16]  2.161874438 0.8802121  0.868237493  3.65288869 5352.1383 0.9997844
## a[17]  2.982607674 0.8012034  1.791155331  4.28589917 3414.2296 0.9995383
## a[18]  2.487599461 0.7042854  1.428440439  3.67119158 3240.6288 0.9997035
## a[19]  2.101483764 0.6420984  1.127892924  3.20372666 3027.8064 0.9994767
## a[20]  3.725406550 1.0318120  2.216562270  5.50655068 3803.5189 0.9994032
## a[21]  2.412164595 0.6703036  1.427705667  3.51648508 5799.7555 0.9994445
## a[22]  2.404132552 0.6655376  1.412461525  3.52847444 4644.1064 0.9995979
## a[23]  2.390936905 0.6504600  1.422564631  3.48513368 6706.0473 0.9996941
## a[24]  1.713432783 0.5325977  0.921749153  2.59598944 6356.3231 0.9994851
## a[25] -0.915920765 0.5092283 -1.733461500 -0.11970562 2038.4423 0.9993846
## a[26]  0.251622886 0.4760520 -0.500176166  1.00871752 1947.0730 1.0006575
## a[27] -1.336043597 0.5661435 -2.275816295 -0.46529656 2619.0805 0.9994099
## a[28] -0.380942379 0.4777292 -1.157882066  0.37068249 1771.3266 0.9996489
## a[29]  0.168456512 0.3854055 -0.447182455  0.77842005 6454.6464 0.9998033
## a[30]  1.451134309 0.4816090  0.724468920  2.23762994 6213.7769 0.9991589
## a[31] -0.629061551 0.4277135 -1.332636883  0.04656938 6614.7029 0.9994734
## a[32] -0.300713164 0.3903987 -0.935296541  0.31608358 6274.1649 0.9993676
## a[33]  3.279327828 0.8028727  2.094223654  4.63073914 3617.3680 0.9993842
## a[34]  2.786685624 0.6727082  1.787430927  3.93081853 3240.4674 0.9995040
## a[35]  2.799294214 0.6726005  1.779946460  3.90262382 2796.9848 0.9992909
## a[36]  2.141501843 0.5660374  1.250739245  3.07982978 2450.8808 0.9993106
## a[37]  2.061918512 0.5230543  1.281843033  2.94481062 6277.3593 0.9995948
## a[38]  3.926659993 1.0235781  2.474892183  5.69042359 4138.2743 0.9996389
## a[39]  2.705399500 0.6249072  1.794640968  3.73653669 5089.5261 0.9997430
## a[40]  2.353936130 0.5638119  1.507575737  3.31590815 5564.9555 0.9998618
## a[41] -1.726719446 0.5399895 -2.610690335 -0.89182438 2304.8245 0.9992163
## a[42] -0.479199957 0.4303138 -1.161038914  0.19941686 1648.7477 0.9995255
## a[43] -0.360942304 0.4224438 -1.036231873  0.30483886 1696.8825 0.9996606
## a[44] -0.249685723 0.4264831 -0.932549511  0.42551817 1667.5631 0.9995423
## a[45]  0.584969837 0.3435758  0.037467337  1.14895506 8027.5304 0.9992759
## a[46] -0.564774194 0.3418014 -1.119580194 -0.03305198 7555.0515 0.9993701
## a[47]  2.069235799 0.5207629  1.294518987  2.94826760 6257.2219 0.9999606
## a[48]  0.003780095 0.3442584 -0.538552427  0.54870327 5615.1096 0.9995097
## bsize -0.093455048 0.2608947 -0.506283929  0.31772968  791.5948 1.0004313
## a_bar  1.388737745 0.2803405  0.960910991  1.84303210 1777.2251 0.9998139
## sigma  1.612883917 0.2179049  1.302674058  1.99127067 2649.8077 0.9993435
```

```r
boxplot(precis(m12M1.1,depth = 2)$mean,precis(m12M1.2,depth = 2)$mean)
```

![](Chapter13_HW_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
more variation with only size as predictor variable


```r
compare(m12M1.1,m12M1.2)
```

```
##             WAIC       SE     dWAIC      dSE    pWAIC    weight
## m12M1.2 200.3781 7.162353 0.0000000       NA 21.05683 0.5071899
## m12M1.1 200.4356 7.445849 0.0575232 1.879531 20.61668 0.4928101
```
mode with only predation as predicator varaible is better at prediction. And it expect less variation in varying intercept. Predation is more important as a predictor variable.

## model with both predation and size as main effect

```r
dat <- list(S=d$surv,
            Z=d$size_id,
            P=d$pred_id,
            tank=d$tank,
            N=d$density) 
  
m12M1.3 <- ulam(
  alist(
    S ~ dbinom(N,p),
    logit(p) <- a[tank]+bsize*Z+bpred*P,
    a[tank] ~ dnorm(a_bar,sigma),
    c(bsize,bpred) ~ dnorm(0,0.3),
    a_bar ~ dnorm(0,1.5),
    sigma ~ dexp(1)
  ),data = dat, chains = 4, log_lik = TRUE,iter = 2000
)
```

```
## 
## SAMPLING FOR MODEL '70827ce4c4aa3a609eb64594e239dc4a' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.389 seconds (Warm-up)
## Chain 1:                0.42 seconds (Sampling)
## Chain 1:                0.809 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '70827ce4c4aa3a609eb64594e239dc4a' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 0.48 seconds (Warm-up)
## Chain 2:                0.29 seconds (Sampling)
## Chain 2:                0.77 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '70827ce4c4aa3a609eb64594e239dc4a' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 0 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 0.309 seconds (Warm-up)
## Chain 3:                0.199 seconds (Sampling)
## Chain 3:                0.508 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '70827ce4c4aa3a609eb64594e239dc4a' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 0.311 seconds (Warm-up)
## Chain 4:                0.379 seconds (Sampling)
## Chain 4:                0.69 seconds (Total)
## Chain 4:
```


```r
precis(m12M1.3,depth = 2)
```

```
##             mean        sd         5.5%      94.5%     n_eff      Rhat
## a[1]   2.2316842 0.7942129  1.010910341  3.5656547 2973.5858 0.9996955
## a[2]   2.9089768 0.8952448  1.586021608  4.4154018 3027.2912 1.0008284
## a[3]   1.3117040 0.6747478  0.254437171  2.4229036 2295.6396 1.0017886
## a[4]   2.9060016 0.8695525  1.617784642  4.3458491 3320.8827 0.9993171
## a[5]   2.1668145 0.7885818  0.963647469  3.4765471 3638.3922 1.0001971
## a[6]   2.1747448 0.8068946  0.963486888  3.5380324 3385.0385 1.0000664
## a[7]   2.8530197 0.9245138  1.485291334  4.4451203 3478.2909 1.0007627
## a[8]   2.1661611 0.7929330  0.963284532  3.4552113 3673.7867 0.9995862
## a[9]   0.9691449 0.6831230 -0.142217901  2.0575180 1245.4486 1.0016274
## a[10]  2.8409574 0.7539613  1.683618661  4.0678320 2304.8641 0.9996973
## a[11]  1.9787151 0.6729385  0.947994380  3.0714746 1627.6880 1.0010761
## a[12]  1.6271603 0.6532618  0.596168337  2.6893316 1468.1009 1.0018805
## a[13]  1.8694567 0.6478869  0.852201172  2.9257486 2024.7965 1.0024260
## a[14]  1.2018858 0.6548627  0.143725402  2.2547225 1635.3805 1.0002101
## a[15]  2.7279740 0.7474383  1.608279712  3.9798279 3016.7516 1.0002888
## a[16]  2.7342953 0.7174256  1.632812598  3.9106720 3696.7170 0.9997406
## a[17]  2.8745037 0.7052400  1.844790070  4.0361655 2819.2824 1.0002931
## a[18]  2.4894906 0.6464306  1.536410395  3.5922493 2664.1770 1.0003011
## a[19]  2.1667881 0.5815343  1.260848603  3.1053171 2737.9278 1.0012604
## a[20]  3.4219644 0.8321939  2.198967212  4.8496132 3013.1277 0.9997133
## a[21]  2.3809488 0.6208529  1.460932654  3.4644194 3793.1452 1.0009679
## a[22]  2.3764798 0.6040114  1.470284432  3.4427090 4317.4760 0.9997047
## a[23]  2.3758386 0.6153586  1.451573202  3.4352708 3332.4146 1.0002817
## a[24]  1.7741441 0.5216983  0.977291563  2.6582157 3824.5580 0.9996521
## a[25]  0.1939329 0.5730050 -0.727461038  1.0956968  919.6229 1.0023308
## a[26]  1.2778258 0.5348602  0.396154363  2.1320243  764.0075 1.0047767
## a[27] -0.1827484 0.6187979 -1.212084674  0.7972570  789.9881 1.0039723
## a[28]  0.6948862 0.5415504 -0.171343377  1.5527793  921.0054 1.0038093
## a[29]  1.1457011 0.4784542  0.384933243  1.9163438 1084.0711 1.0031040
## a[30]  2.3292467 0.5231734  1.513230092  3.1669069 1603.6712 1.0028641
## a[31]  0.4021487 0.5039567 -0.421837826  1.2043958  874.9904 1.0033491
## a[32]  0.7109428 0.4829824 -0.078397666  1.4838775  894.6629 1.0048418
## a[33]  3.1321338 0.6954435  2.090118146  4.3114913 2865.3511 0.9998058
## a[34]  2.7602366 0.6033663  1.844891250  3.7622964 2905.0011 1.0000895
## a[35]  2.7674807 0.6145882  1.838915125  3.8160821 2614.4037 1.0005113
## a[36]  2.1876083 0.5098152  1.383733113  3.0165140 2769.1408 0.9999863
## a[37]  2.0770813 0.4916898  1.323216057  2.9131340 4058.3667 1.0002488
## a[38]  3.5359058 0.8204587  2.378915596  4.9649329 2534.8466 1.0006346
## a[39]  2.6601505 0.5935073  1.770606367  3.6546874 4134.8207 1.0002593
## a[40]  2.3464151 0.5299969  1.547460452  3.2480766 3958.7850 0.9995149
## a[41] -0.5439379 0.6002657 -1.525446536  0.4141236  794.8712 1.0039645
## a[42]  0.5810498 0.5105576 -0.242351223  1.4158266  629.0016 1.0051759
## a[43]  0.6888300 0.5011063 -0.105691662  1.4916280  697.8419 1.0044844
## a[44]  0.8052291 0.5022888  0.007566157  1.6022705  746.2105 1.0041546
## a[45]  1.5482201 0.4393352  0.835497583  2.2461383 1099.2055 1.0032296
## a[46]  0.4444157 0.4479226 -0.282975447  1.1362639  877.6555 1.0035270
## a[47]  2.8835113 0.5101970  2.093122540  3.7286365 1692.8733 1.0008773
## a[48]  0.9944049 0.4280926  0.306171565  1.6737551  963.1755 1.0045152
## bpred -0.9784558 0.2811642 -1.436036583 -0.5306383  461.9644 1.0077995
## bsize -0.1436915 0.2330449 -0.507777956  0.2292189 1142.3681 1.0015716
## a_bar  1.8501315 0.2631604  1.420887846  2.2645141  835.2525 1.0044314
## sigma  1.1705504 0.2071189  0.871755228  1.5194902  691.2054 1.0040397
```

```r
boxplot(precis(m12M1.1,depth = 2)$mean,precis(m12M1.2,depth = 2)$mean,precis(m12M1.3,depth = 2)$mean)
```

![](Chapter13_HW_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
Variation across tanks is similar to model with only predation as predictor

## model with both and interaction 

```r
dat <- list(S=d$surv,
            Z=d$size_id,
            P=d$pred_id,
            tank=d$tank,
            N=d$density) 
  
m12M1.4 <- ulam(
  alist(
    S ~ dbinom(N,p),
    logit(p) <- a[tank]+bsize*Z+bpred*P+bsp*Z*P,
    a[tank] ~ dnorm(a_bar,sigma),
    c(bsize,bpred,bsp) ~ dnorm(0,0.3),
    a_bar ~ dnorm(0,1.5),
    sigma ~ dexp(1)
  ),data = dat, chains = 4, log_lik = TRUE,iter = 2000
)
```

```
## 
## SAMPLING FOR MODEL 'e4db14f8f2c357901ddbde5a72d9247a' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.335 seconds (Warm-up)
## Chain 1:                0.363 seconds (Sampling)
## Chain 1:                0.698 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL 'e4db14f8f2c357901ddbde5a72d9247a' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 0.392 seconds (Warm-up)
## Chain 2:                0.28 seconds (Sampling)
## Chain 2:                0.672 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL 'e4db14f8f2c357901ddbde5a72d9247a' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 0 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 0.343 seconds (Warm-up)
## Chain 3:                0.369 seconds (Sampling)
## Chain 3:                0.712 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL 'e4db14f8f2c357901ddbde5a72d9247a' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 0.362 seconds (Warm-up)
## Chain 4:                0.44 seconds (Sampling)
## Chain 4:                0.802 seconds (Total)
## Chain 4:
```

```r
precis(m12M1.4,depth = 2)
```

```
##              mean        sd        5.5%      94.5%     n_eff      Rhat
## a[1]   2.17606572 0.7368103  1.04523723  3.3729055 3993.1304 0.9992645
## a[2]   2.73012209 0.8128655  1.51145748  4.0765661 4170.5015 1.0007095
## a[3]   1.33240122 0.6459443  0.30770768  2.3744731 2937.4338 0.9998633
## a[4]   2.73394143 0.8199413  1.47709721  4.0998435 2895.8356 0.9996730
## a[5]   2.15461853 0.7221366  1.08246578  3.3606297 4350.8877 1.0006114
## a[6]   2.15943366 0.7137642  1.05304943  3.3183744 4140.9048 1.0012956
## a[7]   2.70476433 0.8032662  1.48612088  4.0530476 3918.2674 1.0011017
## a[8]   2.17011108 0.7027707  1.09688684  3.3367873 4806.9977 1.0005825
## a[9]   1.43733639 0.6648037  0.33521823  2.4549041 1752.7113 1.0008343
## a[10]  3.03400686 0.6856858  1.99140354  4.1662580 2564.7633 1.0004626
## a[11]  2.33136130 0.6346918  1.29959714  3.3401327 1975.2454 1.0011785
## a[12]  2.03623003 0.6431596  1.01711202  3.0415046 1819.4747 1.0004682
## a[13]  1.91280626 0.6121235  0.93415630  2.8946908 2487.3483 1.0007746
## a[14]  1.28562445 0.5987710  0.32724702  2.2468943 2182.1638 1.0010843
## a[15]  2.67995044 0.6855167  1.59211841  3.8049274 3685.1762 0.9999350
## a[16]  2.67863957 0.7064924  1.58997544  3.8413055 3773.9837 0.9996097
## a[17]  2.71861737 0.6449703  1.74274596  3.7814784 3578.2117 0.9998239
## a[18]  2.37881346 0.6090528  1.47141318  3.4115213 3554.9641 0.9993741
## a[19]  2.07235100 0.5740279  1.19541936  3.0182930 3161.5133 0.9997654
## a[20]  3.16796366 0.7685414  2.03130609  4.4579139 3112.5963 1.0011394
## a[21]  2.36061511 0.5878850  1.45579973  3.3719696 3975.4309 1.0000193
## a[22]  2.34093442 0.5631021  1.49673655  3.2963188 4062.0767 1.0001440
## a[23]  2.35646891 0.5848442  1.44173769  3.3319709 4205.8628 0.9992434
## a[24]  1.79209439 0.5020543  1.01693759  2.6399555 5103.9254 1.0000071
## a[25]  0.73515633 0.5692033 -0.22204187  1.6028665 1070.0187 1.0040967
## a[26]  1.75581003 0.5304262  0.87988863  2.5737485 1247.1550 1.0028209
## a[27]  0.36621365 0.6242932 -0.67465891  1.3091069 1123.4820 1.0044627
## a[28]  1.19687707 0.5373407  0.30263287  2.0195679 1134.8826 1.0047934
## a[29]  1.20116781 0.4629584  0.44874547  1.9228204 1541.6548 1.0030219
## a[30]  2.33712746 0.5045298  1.54309683  3.1674012 2618.1147 1.0004129
## a[31]  0.49197956 0.4899766 -0.30222565  1.2699948 1398.0027 1.0027348
## a[32]  0.79704740 0.4703666  0.02231944  1.5356990 1463.9345 1.0030872
## a[33]  2.94924475 0.6363193  2.00735939  4.0215479 3008.2006 1.0010279
## a[34]  2.62117210 0.5692138  1.76949208  3.5543508 3298.0428 0.9999610
## a[35]  2.62361933 0.5946946  1.73786220  3.6246520 3416.7203 1.0001909
## a[36]  2.11879847 0.5113003  1.33363175  2.9591027 3224.2412 0.9993834
## a[37]  2.08488291 0.4626337  1.37800279  2.8597473 4307.1043 0.9997972
## a[38]  3.33261031 0.7099293  2.31505694  4.5473443 2490.6663 1.0011026
## a[39]  2.60368705 0.5696384  1.77274545  3.5629145 4131.5029 0.9998343
## a[40]  2.32423095 0.5102200  1.54648384  3.1774329 3998.3687 1.0007392
## a[41]  0.02611553 0.6134701 -0.99446486  0.9612109 1052.0806 1.0036037
## a[42]  1.10135038 0.5088316  0.26284553  1.8947453 1018.2855 1.0040546
## a[43]  1.20550052 0.5080474  0.37849703  1.9881082 1067.2441 1.0030653
## a[44]  1.31887990 0.4958382  0.49996685  2.0914283 1059.4276 1.0040486
## a[45]  1.58497335 0.4276056  0.89679607  2.2841961 1670.9165 1.0020063
## a[46]  0.52750800 0.4414968 -0.18572963  1.2163164 1444.7848 1.0018208
## a[47]  2.85162526 0.4826889  2.09601610  3.6470788 2693.7857 1.0000004
## a[48]  1.05263153 0.4235638  0.36974786  1.7367076 1527.8980 1.0013342
## bsp   -0.62403287 0.2559928 -1.03663092 -0.2116906 2098.3423 1.0021779
## bpred -0.99774428 0.2664387 -1.41438283 -0.5675715  872.5191 1.0032873
## bsize -0.03249507 0.2330499 -0.40432555  0.3328737 1919.2294 1.0004517
## a_bar  1.93565349 0.2430383  1.53800301  2.3147723  996.1468 1.0050454
## sigma  0.99306143 0.1921388  0.71719412  1.3248155  894.8386 1.0038097
```

```r
boxplot(precis(m12M1.1,depth = 2)$mean,precis(m12M1.2,depth = 2)$mean,precis(m12M1.3,depth = 2)$mean,precis(m12M1.4,depth = 2)$mean)
```

![](Chapter13_HW_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
The variance is even smaller between tanks. So predation is the major predictor variable, with some interaction with size of tadpoles.


#12M2.Compare the models you ft just above, using WAIC. Can you reconcile the diﬀerences in
WAIC with the posterior distributions of the models?

```r
compare(m12M1.1,m12M1.2,m12M1.3,m12M1.4)
```

```
##             WAIC       SE      dWAIC       dSE    pWAIC    weight
## m12M1.3 199.9174 7.300558 0.00000000        NA 20.29676 0.2829680
## m12M1.4 199.9824 7.570203 0.06499611 1.7053130 19.93085 0.2739199
## m12M1.2 200.3781 7.162353 0.46076228 1.8559277 21.05683 0.2247419
## m12M1.1 200.4356 7.445849 0.51828549 0.8027425 20.61668 0.2183701
```
Predation-only model is the best, and very similar to predation,size-both and interaction-included model.


#12H1.In 1980, a typical Bengali woman could have 5 or more children in her lifetime. By the
year 200, a typical Bengali woman had only 2 or 3. You’re going to look at a historical set of data,when contraception was widely available but many families chose not to use it. Tese data reside in data(bangladesh) and come from the 1988 Bangladesh Fertility Survey. Each row is one of 1934 women. Tere are six variables, but you can focus on three of them for this practice problem:
(1) district: ID number of administrative district each woman resided in
(2) use.contraception: An indicator (0/1) of whether the woman was using contraception
(3) urban: An indicator (0/1) of whether the woman lived in a city, as opposed to living in a
rural area
Te frst thing to do is ensure that the cluster variable, district, is a contiguous set of integers. Recall that these values will be index values inside the model. If there are gaps, you’ll have parameters for
which there is no data to inform them. Worse, the model probably won’t run. Look at the unique
values of the district variable:

```r
data("bangladesh")
d <- bangladesh
```


```r
sort(unique(d$district))
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
## [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
## [51] 51 52 53 55 56 57 58 59 60 61
```

```r
d$district_id <- as.integer(as.factor(d$district))
sort(unique(d$district_id))
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
## [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
## [51] 51 52 53 54 55 56 57 58 59 60
```

Now, focus on predicting use.contraception, clustered by district_id. Do not include
urban just yet. Fit both (1) a traditional fxed-eﬀects model that uses dummy variables for district and (2) a multilevel model with varying intercepts for district. Plot the predicted proportions of women in each district using contraception, for both the fxed-eﬀects model and the varying-eﬀects model. That is, make a plot in which district ID is on the horizontal axis and expected proportion using contraception is on the vertical. Make one plot for each model, or layer them on the same plot, as you prefer. How do the models disagree? Can you explain the pattern of disagreement? In particular, can you explain the most extreme cases of disagreement, both why they happen where they do and why the models reach diﬀerent inferences?

predicting use.contraception, clustered by district_id. 
(1)traditional fxed-eﬀects model that uses dummy variables for district

```r
Density <- as.data.frame(table(d$district_id))
use_contrac_aggre <- aggregate(d$use.contraception,list(district=d$district_id),sum)
```


```r
dat <- list(
  C = use_contrac_aggre$x,
  district = c(1:60),
  N = Density$Freq
)

m12H1.1 <- ulam(
  alist(
    C ~ dbinom(N,p),
    logit(p) <- a[district],
    a[district] ~ dnorm(0,1.5)
  ),data = dat,chains = 4,log_lik = TRUE
)
```

```
## 
## SAMPLING FOR MODEL '7da962c8c9deda28e9d40676ec9edae0' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0.001 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
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
## Chain 1:  Elapsed Time: 0.14 seconds (Warm-up)
## Chain 1:                0.105 seconds (Sampling)
## Chain 1:                0.245 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '7da962c8c9deda28e9d40676ec9edae0' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
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
## Chain 2:  Elapsed Time: 0.133 seconds (Warm-up)
## Chain 2:                0.107 seconds (Sampling)
## Chain 2:                0.24 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '7da962c8c9deda28e9d40676ec9edae0' NOW (CHAIN 3).
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
## Chain 3:  Elapsed Time: 0.128 seconds (Warm-up)
## Chain 3:                0.108 seconds (Sampling)
## Chain 3:                0.236 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '7da962c8c9deda28e9d40676ec9edae0' NOW (CHAIN 4).
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
## Chain 4:  Elapsed Time: 0.126 seconds (Warm-up)
## Chain 4:                0.108 seconds (Sampling)
## Chain 4:                0.234 seconds (Total)
## Chain 4:
```
(2) a multilevel model with varying intercepts for district


12M3
12H2
