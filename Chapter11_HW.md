---
title: "Chapter11_HW"
author: "Lin Zhang"
date: "11/2/2019"
output: 
  html_document: 
    keep_md: yes
---



# 10E1. If an event has probability 0.35, what are the log-odds of this event?

```r
log(0.35/(1-0.35))
```

```
## [1] -0.6190392
```

```r
logit(.35)
```

```
## [1] -0.6190392
```
# 10E2. If an event has log-odds 3.2, what is the probability of this event?

```r
inv_logit(3.2)
```

```
## [1] 0.9608343
```

```r
exp(3.2)/(1+exp(3.2))
```

```
## [1] 0.9608343
```

#10E3. Suppose that a coefficient in a logistic regression has value 1.7. What does this imply about
the proportional change in odds of the outcome?

```r
#???
inv_logit(1.7)
```

```
## [1] 0.8455347
```

#10M1. As explained in the chapter, binomial data can be organized in aggregated and disaggregated
forms, without any impact on inference. But the likelihood of the data does change when the data are
converted between the two formats. Can you explain why?


# Week6_Problem_1 NWO fund

```r
data("NWOGrants")
d <- NWOGrants
str(d)
```

```
## 'data.frame':	18 obs. of  4 variables:
##  $ discipline  : Factor w/ 9 levels "Chemical sciences",..: 1 1 6 6 7 7 3 3 9 9 ...
##  $ gender      : Factor w/ 2 levels "f","m": 2 1 2 1 2 1 2 1 2 1 ...
##  $ applications: int  83 39 135 39 67 9 230 166 189 62 ...
##  $ awards      : int  22 10 26 9 18 2 33 32 30 13 ...
```


```r
#build model for total effect of gender on grant awards
d$gid <- ifelse(d$gender=="m",1,2)
m1.1 <- quap(
  alist(
    awards ~ dbinom(applications,p),
    logit(p) <- a[gid],
    a[gid] ~ dnorm(0,1.5)
    ),data = d
  )
precis(m1.1,depth = 2)
```

```
##           mean         sd      5.5%     94.5%
## a[1] -1.531418 0.06462435 -1.634700 -1.428136
## a[2] -1.737428 0.08121367 -1.867223 -1.607633
```


```r
#compute contrast on the logit scale relative and outcome scale absolute
post <- extract.samples(m1.1)
diff_a <- post$a[,1]-post$a[,2]
diff_p <- inv_logit(post$a[,1])-inv_logit(post$a[,2])
precis(list(diff_a=diff_a,diff_p=diff_p))
```

```
##              mean         sd        5.5%     94.5%
## diff_a 0.20571688 0.10379438 0.040961210 0.3690745
## diff_p 0.02800787 0.01401572 0.005634545 0.0499346
##                                                                                       histogram
## diff_a                         <U+2581><U+2581><U+2582><U+2587><U+2587><U+2583><U+2581><U+2581>
## diff_p <U+2581><U+2581><U+2581><U+2582><U+2585><U+2587><U+2587><U+2583><U+2581><U+2581><U+2581>
```

```r
# in general, across discipline, male is 0.03 higher in probablity space to get awarded
```


```r
library(dagitty)
dag1 <- dagitty("dag{
                G->D
                D->A
                G->A
                }"
)
coordinates(dag1) <- list(x=c(G=0,D=1,A=2),y=c(G=0,A=0,D=1))
drawdag(dag1)
```

![](Chapter11_HW_files/figure-html/DAG-1.png)<!-- -->


```r
#build model for total effect of gender on grant awards
d$dis_id <- rep(1:9,each=2)
m1.2 <- quap(
  alist(
    awards ~ dbinom(applications,p),
    logit(p) <- a[gid]+delta[dis_id],
    a[gid] ~ dnorm(0,1.5),
    delta[dis_id] ~ dnorm(0,1.5)
  ),data = d
)
precis(m1.2,depth = 2)
```

```
##                mean        sd       5.5%      94.5%
## a[1]     -1.1589445 0.4565096 -1.8885350 -0.4293539
## a[2]     -1.2966118 0.4597878 -2.0314415 -0.5617821
## delta[1]  0.1648129 0.4908966 -0.6197346  0.9493604
## delta[2] -0.1872926 0.4856750 -0.9634950  0.5889098
## delta[3]  0.1410153 0.5114906 -0.6764454  0.9584760
## delta[4] -0.4092662 0.4709092 -1.1618701  0.3433376
## delta[5] -0.3797510 0.4792298 -1.1456528  0.3861507
## delta[6] -0.4452979 0.4895073 -1.2276251  0.3370293
## delta[7] -0.1743717 0.4742527 -0.9323191  0.5835757
## delta[8] -0.6358742 0.4640405 -1.3775005  0.1057522
## delta[9] -0.5145260 0.4687246 -1.2636383  0.2345864
```


```r
post <- extract.samples(m1.2)
diff_a <- post$a[,1]-post$a[,2]
diff_p <- inv_logit(post$a[,1])-inv_logit(post$a[,2])
precis(list(diff_a=diff_a,diff_p=diff_p))
```

```
##              mean        sd         5.5%      94.5%
## diff_a 0.13720938 0.1075952 -0.033233058 0.30837612
## diff_p 0.02373207 0.0197317 -0.005961658 0.05652221
##                                                                               histogram
## diff_a <U+2581><U+2581><U+2581><U+2582><U+2585><U+2587><U+2585><U+2581><U+2581><U+2581>
## diff_p         <U+2581><U+2581><U+2582><U+2587><U+2587><U+2583><U+2581><U+2581><U+2581>
```

```r
# condition on  discipline, male is 0.02 higher in probablity space to get awarded
```

# Consider an unobserved confound

```r
library(dagitty)
dag1 <- dagitty("dag{
                G->D
                D->A
                G->A
                U->D
                U->A
                }"
)
coordinates(dag1) <- list(x=c(G=0,D=0,A=1,U=1),y=c(G=0,A=0,D=1,U=1))
drawdag(dag1)
```

![](Chapter11_HW_files/figure-html/DAG2-1.png)<!-- -->


```r
# conditional on discipline causes gender and unobserved confound(such as career stages) to be correlated, the collider is open
# it does not provide an un-confounded estimate of the direct path from gender to an award, as the backdoor is open through the unobserved confound
# simulate?
# but for a regression conditioning on both gender adn discipline to suggest zero influence??
```

