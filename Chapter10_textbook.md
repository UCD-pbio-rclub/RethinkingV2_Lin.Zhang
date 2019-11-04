---
title: "Chapter10_textbook"
author: "Lin Zhang"
date: "10/24/2019"
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

## Gausian

```r
p <- list()
p$A <- c(0,0,10,0,0)
p$B <- c(0,1,8,1,0)
p$C <- c(0,2,6,2,0)
p$D <- c(1,2,4,2,1)
p$E <- c(2,2,2,2,2)
```


```r
p_norm <- lapply(p,function(q) q/sum(q))
```


```r
(H <- sapply(p_norm,function(q) -sum(ifelse(q==0,0,q*log(q)))))
```

```
##         A         B         C         D         E 
## 0.0000000 0.6390319 0.9502705 1.4708085 1.6094379
```


```r
ways <- c(1,90,1260,37800,113400)
(logwayspp <- log(ways)/10)
```

```
## [1] 0.0000000 0.4499810 0.7138867 1.0540064 1.1638677
```
## Binomial

```r
p <- 0.5
(A <- c( (1-p)^2, p*(1-p),(1-p)*p,p^2))
```

```
## [1] 0.25 0.25 0.25 0.25
```



```r
p <- list()
p[[1]] <- c(1/4,1/4,1/4,1/4)
p[[2]] <- c(2/6,1/6,1/6,2/6)
p[[3]] <- c(1/6,2/6,2/6,1/6)
p[[4]] <- c(1/8,4/8,2/8,1/8)
# compute expected value of each
sapply(p,function(p)sum(p*c(0,1,1,2)))
```

```
## [1] 1 1 1 1
```


```r
sapply(p,function(p)-sum(p*log(p)))
```

```
## [1] 1.386294 1.329661 1.329661 1.213008
```


```r
p <- 0.7
(A <- c( (1-p)^2, p*(1-p),(1-p)*p,p^2))
```

```
## [1] 0.09 0.21 0.21 0.49
```


```r
-sum(A*log(A))
```

```
## [1] 1.221729
```


```r
# ?generates a random distribution with expected value G and then returns its entropy alongwith the distribution
sim.p <- function(G=1.4) { 
  x123 <- runif(3) 
  x4 <- ( (G)*sum(x123)-x123[2]-x123[3] )/(2-G) 
  z <- sum( c(x123,x4) ) 
  p <- c( x123 , x4 )/z 
  list( H=-sum( p*log(p) ) , p=p )
}
```


```r
# simulate to generate random distributions for 1e5 times and plot resulting entropies
H <- replicate(1e5,sim.p(1.4))
dens(as.numeric(H[1,]),adj = 0.1)
```

![](Chapter10_textbook_files/figure-html/simulate random distributions with fixed expected value-1.png)<!-- -->


```r
entropies <- as.numeric(H[1,])
distributions <- H[2,]
```


```r
max(entropies)
```

```
## [1] 1.221728
```

```r
distributions[which.max(entropies)]
```

```
## [[1]]
## [1] 0.09007077 0.21009730 0.20976117 0.49007077
```

