<<<<<<< HEAD
---
title: "02_21_Nightbreak"
author: "Lin_Zhang"
date: "2/21/2020"
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
## rstan (Version 2.19.3, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## For improved execution time, we recommend calling
## Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
## although this causes Stan to throw an error on a few processors.
```

```
## Loading required package: parallel
```

```
## Loading required package: dagitty
```

```
## rethinking (Version 1.95)
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
Q1: a. Load the dataset. Look for column "Score" for the response variable we are interested in. A developmental score of 1 or 2 indicates vegetative growth, while a score of 3, 4, or 5 indicates reproductive growth. Create a "Reproduction" column with values 0 and 1, where 0 indicates vegetative growth and 1 indicates reproductive growth. 

```r
nightbreak <- read.csv(file = "Nightbreak_02_08_20_Rclub.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
nightbreak$Reproduction <- ifelse(nightbreak < 4, 0,1)
```

b. (optional) Take a look at columns "loc1" through "loc5". The levels "A" or "P" indicate which parental allele the RIL has inherited. Can you think of a reason why there are 32 lines in this experiment?
5 loci,each with 2 allele options, 2^5

Q2:
Using the "Reproduction" column you created in Q1a as the response variable, fit a simple model with effects of genotype ("RIL") and treatment ("Treatment") on the rate of transitioning to reproductive growth by 2/8/20. (Things you might want to consider: appropriate likelihood function, intersection term). 

```r
d <- nightbreak
d$Treatment <- as.factor(d$Treatment)
treat_levels <- c(1:3)
d$treat_levels <- treat_levels[d$Treatment]
unique(d$RIL)
```

```
##  [1] "100" "2"   "137" "172" "188" "PI"  "55"  "201" "14"  "58"  "222" "21" 
## [13] "161" "169" "108" "154" "105" "66"  "33"  "112" "53"  "146" "114" "25" 
## [25] "175" "235" "19"  "24"  "Arm" "228" "179" "229"
```

```r
d$RIL[d$RIL=="PI"] <- c("0")
d$RIL[d$RIL=="Arm"] <- c("1")


dat <- list(
  TR = d$treat_levels,
  G = as.numeric(d$RIL),
  R = d$Reproduction
)

m1 <- ulam(
  alist(
    R ~ dbinom(1,p),
    logit(p) <- a[G] + bT*TR,
    a[G] ~ dnorm(0,1.5),
    bT ~ dnorm(0,1.5)
    ), data = dat, chains = 4, log_lik = TRUE
)
```

```
## 
## SAMPLING FOR MODEL '7c6bf41ae292c6489c8d9e36cb7a263a' NOW (CHAIN 1).
## Chain 1: Unrecoverable error evaluating the log probability at the initial value.
## Chain 1: Exception: []: accessing element out of range. index 100 out of range; expecting index to be between 1 and 32; index position = 1a  (in 'model579849927c3_7c6bf41ae292c6489c8d9e36cb7a263a' at line 15)
## 
## Error in sampler$call_sampler(args_list[[i]]) : 
##   Exception: []: accessing element out of range. index 100 out of range; expecting index to be between 1 and 32; index position = 1a  (in 'model579849927c3_7c6bf41ae292c6489c8d9e36cb7a263a' at line 15)
## 
## character(0)
## [1] "error occurred during calling the sampler; sampling not done"
## Stan model '7c6bf41ae292c6489c8d9e36cb7a263a' does not contain samples.
```

```
## Error in validObject(.Object): invalid class "ulam" object: invalid object for slot "coef" in class "ulam": got class "NULL", should be or extend class "numeric"
```

Q3:
Because we are more interested in the effects of individual loci than the performance of specific genotypes, fit a model with additive effects of the five loci and effect of treatment.  

=======
---
title: "02_21_Nightbreak"
author: "Lin_Zhang"
date: "2/21/2020"
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
## rstan (Version 2.19.3, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## For improved execution time, we recommend calling
## Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
## although this causes Stan to throw an error on a few processors.
```

```
## Loading required package: parallel
```

```
## Loading required package: dagitty
```

```
## rethinking (Version 1.95)
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
Q1: a. Load the dataset. Look for column "Score" for the response variable we are interested in. A developmental score of 1 or 2 indicates vegetative growth, while a score of 3, 4, or 5 indicates reproductive growth. Create a "Reproduction" column with values 0 and 1, where 0 indicates vegetative growth and 1 indicates reproductive growth. 

```r
nightbreak <- read.csv(file = "Nightbreak_02_08_20_Rclub.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
nightbreak$Reproduction <- ifelse(nightbreak < 4, 0,1)
```

b. (optional) Take a look at columns "loc1" through "loc5". The levels "A" or "P" indicate which parental allele the RIL has inherited. Can you think of a reason why there are 32 lines in this experiment?
5 loci,each with 2 allele options, 2^5

Q2:
Using the "Reproduction" column you created in Q1a as the response variable, fit a simple model with effects of genotype ("RIL") and treatment ("Treatment") on the rate of transitioning to reproductive growth by 2/8/20. (Things you might want to consider: appropriate likelihood function, intersection term). 

```r
d <- nightbreak
d$Treatment <- as.factor(d$Treatment)
treat_levels <- c(1:3)
d$treat_levels <- treat_levels[d$Treatment]
unique(d$RIL)
```

```
##  [1] "100" "2"   "137" "172" "188" "PI"  "55"  "201" "14"  "58"  "222" "21" 
## [13] "161" "169" "108" "154" "105" "66"  "33"  "112" "53"  "146" "114" "25" 
## [25] "175" "235" "19"  "24"  "Arm" "228" "179" "229"
```

```r
d$RIL[d$RIL=="PI"] <- c("0")
d$RIL[d$RIL=="Arm"] <- c("1")


dat <- list(
  TR = d$treat_levels,
  G = as.numeric(d$RIL),
  R = d$Reproduction
)

m1 <- ulam(
  alist(
    R ~ dbinom(1,p),
    logit(p) <- a[G] + bT*TR,
    a[G] ~ dnorm(0,1.5),
    bT ~ dnorm(0,1.5)
    ), data = dat, chains = 4, log_lik = TRUE
)
```

```
## 
## SAMPLING FOR MODEL '7c6bf41ae292c6489c8d9e36cb7a263a' NOW (CHAIN 1).
## Chain 1: Unrecoverable error evaluating the log probability at the initial value.
## Chain 1: Exception: []: accessing element out of range. index 100 out of range; expecting index to be between 1 and 32; index position = 1a  (in 'model579849927c3_7c6bf41ae292c6489c8d9e36cb7a263a' at line 15)
## 
## Error in sampler$call_sampler(args_list[[i]]) : 
##   Exception: []: accessing element out of range. index 100 out of range; expecting index to be between 1 and 32; index position = 1a  (in 'model579849927c3_7c6bf41ae292c6489c8d9e36cb7a263a' at line 15)
## 
## character(0)
## [1] "error occurred during calling the sampler; sampling not done"
## Stan model '7c6bf41ae292c6489c8d9e36cb7a263a' does not contain samples.
```

```
## Error in validObject(.Object): invalid class "ulam" object: invalid object for slot "coef" in class "ulam": got class "NULL", should be or extend class "numeric"
```

Q3:
Because we are more interested in the effects of individual loci than the performance of specific genotypes, fit a model with additive effects of the five loci and effect of treatment.  

>>>>>>> 1ee3820138d6e136c1a1a0608374144ec3260944
