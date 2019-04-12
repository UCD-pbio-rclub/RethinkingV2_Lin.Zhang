2E1
---

1.  Pr(rain|Monday) \#\# 2E2
2.  Pr(Monday|rain): The probability that it is Monday, given that it is
    raining \#\# 2E3
3.  2M1 grid approximate posterior distribution for globe tossing model
    -------------------------------------------------------------------

<!-- -->

    p_grid <- seq(from=0,to=1, length.out = 20)
    # define prior
    prior <- rep(1,20)
    # compute likelihood at each value in grid
    likelihood <- dbinom(3,size = 3,prob = p_grid)
    # compute product of likelihood and prior
    unstd.posterior <- likelihood*prior
    # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior/sum(unstd.posterior)
    plot(p_grid,posterior,type = "b", xlab = "probability of water", ylab = "posterior probability")
    mtext("20 points")

![](Stat_rethinking2_chapter2_LinZhang_files/figure-markdown_strict/2M1(1)W,W,W-1.png)

    p_grid <- seq(from=0,to=1, length.out = 20)
    # define prior
    prior <- rep(1,20)
    # compute likelihood at each value in grid
    likelihood <- dbinom(3,size = 4,prob = p_grid)
    # compute product of likelihood and prior
    unstd.posterior <- likelihood*prior
    # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior/sum(unstd.posterior)
    plot(p_grid,posterior,type = "b", xlab = "probability of water", ylab = "posterior probability")
    mtext("20 points")

![](Stat_rethinking2_chapter2_LinZhang_files/figure-markdown_strict/2M1(2)%20W,W,W,L-1.png)

    p_grid <- seq(from=0,to=1, length.out = 20)
    # define prior
    prior <- rep(1,20)
    # compute likelihood at each value in grid
    likelihood <- dbinom(5,size = 7,prob = p_grid)
    # compute product of likelihood and prior
    unstd.posterior <- likelihood*prior
    # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior/sum(unstd.posterior)
    plot(p_grid,posterior,type = "b", xlab = "probability of water", ylab = "posterior probability")
    mtext("20 points")

![](Stat_rethinking2_chapter2_LinZhang_files/figure-markdown_strict/2M1(3)%20L,W,W,L,W,W,W-1.png)

\#\#2M2

    p_grid <- seq(from=0,to=1, length.out = 20)
    # define prior
    prior <- ifelse(p_grid<0.5, 0, 1)
    # compute likelihood at each value in grid
    likelihood <- dbinom(3,size = 3,prob = p_grid)
    # compute product of likelihood and prior
    unstd.posterior <- likelihood*prior
    # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior/sum(unstd.posterior)
    plot(p_grid,posterior,type = "b", xlab = "probability of water", ylab = "posterior probability")
    mtext("20 points")

![](Stat_rethinking2_chapter2_LinZhang_files/figure-markdown_strict/2M2(1)W,W,W-1.png)

    p_grid <- seq(from=0,to=1, length.out = 20)
    # define prior
    prior <- ifelse(p_grid<0.5, 0, 1)
    # compute likelihood at each value in grid
    likelihood <- dbinom(3,size = 4,prob = p_grid)
    # compute product of likelihood and prior
    unstd.posterior <- likelihood*prior
    # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior/sum(unstd.posterior)
    plot(p_grid,posterior,type = "b", xlab = "probability of water", ylab = "posterior probability")
    mtext("20 points")

![](Stat_rethinking2_chapter2_LinZhang_files/figure-markdown_strict/2M2(2)W,W,W,L-1.png)

    p_grid <- seq(from=0,to=1, length.out = 20)
    # define prior
    prior <- ifelse(p_grid<0.5, 0, 1)
    # compute likelihood at each value in grid
    likelihood <- dbinom(5,size = 7,prob = p_grid)
    # compute product of likelihood and prior
    unstd.posterior <- likelihood*prior
    # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior/sum(unstd.posterior)
    plot(p_grid,posterior,type = "b", xlab = "probability of water", ylab = "posterior probability")
    mtext("20 points")

![](Stat_rethinking2_chapter2_LinZhang_files/figure-markdown_strict/2M2(3)L,W,W,L,W,W,W-1.png)
\#2M3

    p_earth=0.5
    p_mars=0.5
    p_land_earth=0.3
    p_land_mars=1
    p_land=p_land_earth*p_earth+p_land_mars*p_mars
    p_earth_land=(p_land_earth*p_earth)/p_land
    p_earth_land

    ## [1] 0.2307692

    #p_grid <- seq(from=0,to=1,length.out = 20)
    # define prior
    #prior <- rep(0.5,20)
    # compute likelihood at each value in grid
    #likelihood <- dbinom(23,size = 100, prob = p_grid)

\#2M4

    p_card_one=1/3
    p_card_two=1/3
    p_card_three=1/3
    p_b_cardOne=1
    p_b_cardTwo=1/2
    p_cardOne_b=p_b_cardOne/(p_b_cardOne+p_b_cardTwo)
    print(p_cardOne_b)

    ## [1] 0.6666667

\#2M5

    p_card_one=1/4
    p_card_two=1/4
    p_card_three=1/4
    p_card_four=1/4
    p_b_cardOne=1
    p_b_cardTwo=1/2
    p_b_cardThree=0
    p_b_cardFour=1
    p_b_b=(p_b_cardOne+p_b_cardFour)/(p_b_cardOne+p_b_cardTwo+p_b_cardFour)
    print(p_b_b)

    ## [1] 0.8
