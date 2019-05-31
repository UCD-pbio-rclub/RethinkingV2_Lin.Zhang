---
title: "Chapter3_HW"
author: "Lin Zhang"
date: "4/19/2019"
output: 
  html_document: 
    keep_md: yes
---


```r
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep (1,1000)
likelihood <- dbinom (6, size = 9, prob=p_grid)
posterior <- likelihood* prior
posterior <- posterior/sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
```

## 3E1

```r
sum(samples <0.2)/length(samples)
```

```
## [1] 5e-04
```
## 3E2


## 3E3

```r
sum (samples > 0.2 & samples <0.8) / length(samples)
```

```
## [1] 0.8878
```
## 3E4

```r
quantile(samples,0.2)
```

```
##       20% 
## 0.5195195
```
## 3E5

```r
quantile(samples,0.2)
```

```
##       20% 
## 0.5195195
```
## 3E6

```r
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Loading required package: ggplot2
```

```
## Loading required package: StanHeaders
```

```
## rstan (Version 2.18.2, GitRev: 2e1f913d3ca3)
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
## rethinking (Version 1.88)
```

```r
HPDI(samples, prob = 0.66)
```

```
##     |0.66     0.66| 
## 0.5205205 0.7847848
```
## 3E7

```r
library(rethinking)
PI(samples, prob = 0.66)
```

```
##       17%       83% 
## 0.5005005 0.7687688
```
## 3M1

```r
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep (1,1000)
likelihood2 <- dbinom (8, size = 15, prob=p_grid)
posterior2 <- likelihood2* prior
posterior2 <- posterior2/sum(posterior2)
```

## 3M2

```r
set.seed(100)
samples2 <- sample(p_grid, prob = posterior2, size = 1e4, replace = TRUE)
library(rethinking)
HPDI(samples, prob = 0.90)
```

```
##      |0.9      0.9| 
## 0.4164164 0.8658659
```
## 3M3

```r
w2 <- rbinom(1e4, size = 15,prob = samples2)
sum(w2==8)/length(w2)
```

```
## [1] 0.1475
```

## 3M4

```r
w3 <- rbinom(1e4, size = 9, prob = samples2)
sum(w3==6)/length(w3)
```

```
## [1] 0.1766
```

