# Assignment_Chapter_07
XIAOYAN YUE  
06/04/2016  
###7E1. For each of the causal relationships below, name a hypothetical third variable that would lead to an interaction effect.

####(1) Bread dough rises because of yeast.
time, temperature

####(2) Education leads to higher income.
family background,country

####(3) Gasoline makes a car go.
physical situation of the car

###7E2. Which of the following explanations invokes an interaction?
(2)and (3)

###7E3. For each of the explanations in 7E2, write a linear model that expresses the stated relationship.
(1)caramelized ~ Normal(mu,sigma)

mu = a + b_h*heat + b_w*water+ b_hw*heat*water

(2)car_faster ~ Normal(mu,sigma)

mu = a + b_c*cylinder + b_f*fuel_injector + b_cf*cylinder*fuel_injector

(3)politicol_beliefs ~ Normal(mu,sigma)

mu = a + b_p*parent_beliefs + b_f*friends_beliefs + b_pf*parent_beliefs*frends_beliefs

(4)intelligent_animal ~ Normal(mu,sigma)

mu = a + b_s*social+b_a*appendages + b_sa*social*appendages

###7M1
Under hot temperature, at any water and light conditionsno tupips could grow blooms. 

###7M2
temperature <- c(0,1)

bloom_size ~ water*shade*temperature

###7M3

```r
##invent a set of data? not just creat a model?
```

###7H1

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
## rstan (Version 2.9.0-3, packaged: 2016-02-11 15:54:41 UTC, GitRev: 05c3d0058b6a)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## rstan_options(auto_write = TRUE)
## options(mc.cores = parallel::detectCores())
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.58)
```

```r
data(tulips)
d <- tulips

##make centered verison of shade and water
d$shade.c <- d$shade - mean(d$shade)
d$water.c <- d$water - mean(d$water)

#set the dummy variable for the data
d$bed_a <- ifelse(d$bed=="a",1,0)
d$bed_b <- ifelse(d$bed=="b",1,0)
d$bed_c <- ifelse(d$bed=="c",1,0)

##creat the model
m.7H1 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- a + bW*water.c + bS*shade.c + bWS*water.c*shade.c + bA*bed_a + bB*bed_b + bC*bed_c, 
    a ~ dnorm( 130 , 100 ) ,
    bW ~ dnorm( 0 , 100 ) ,
    bS ~ dnorm( 0 , 100 ) ,
    bWS ~ dnorm( 0 , 100 ) ,
    bA ~ dnorm( 0, 1 ),
    bB ~ dnorm( 0, 1 ),
    bC ~ dnorm( 0, 1),#set the prior for the added parameter
    sigma ~ dunif( 0 , 100 )
  ) ,
  data=d ,
  start=list(a=mean(d$blooms),bW=0,bS=0,bWS=0,bA=0,bB=0,bC=0,sigma=sd(d$blooms)) )
precis(m.7H1)
```

```
##         Mean StdDev   5.5%  94.5%
## a     129.00   8.68 115.12 142.88
## bW     74.94  10.60  58.01  91.88
## bS    -41.13  10.59 -58.06 -24.20
## bWS   -51.96  12.94 -72.64 -31.28
## bA     -0.14   1.00  -1.74   1.46
## bB      0.06   1.00  -1.54   1.66
## bC      0.08   1.00  -1.52   1.68
## sigma  45.20   6.16  35.35  55.04
```

####7H2

```r
#fit the model omited bed
m.7H2 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- a + bW*water.c + bS*shade.c + bWS*water.c*shade.c ,
    a ~ dnorm( 130 , 100 ) ,
    bW ~ dnorm( 0 , 100 ) ,
    bS ~ dnorm( 0 , 100 ) ,
    bWS ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 100 )
  ) ,
  data=d ,
  start=list(a=mean(d$blooms),bW=0,bS=0,bWS=0,sigma=sd(d$blooms)) )

compare(m.7H1,m.7H2)#compare these two models using WAIC
```

```
##        WAIC pWAIC dWAIC weight    SE  dSE
## m.7H1 296.3   6.8   0.0   0.58 10.37   NA
## m.7H2 297.0   7.0   0.7   0.42 10.80 0.55
```
It seems the bed variable did not effect the outcomes in this study.


```r
#what means reconcile the WAIC results?
```

####7H3

```r
library(rethinking)
data(rugged)
d <- rugged

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )
# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

#(a) using map to fit just the interaction model without country Seychelles

# extract country Seychelles
dd1 <- dd[dd$country!="Seychelles",]
#fit the model
m.7H3_1 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bAR*rugged*cont_africa + bA*cont_africa,
    a ~ dnorm( 8 , 100 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=dd1 )

#using the same model fit to the full data
m.7H3_2 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bAR*rugged*cont_africa + bA*cont_africa,
    a ~ dnorm( 8 , 100 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=dd )
#compare the inference of two models
precis(m.7H3_1)
```

```
##        Mean StdDev  5.5% 94.5%
## a      9.19   0.14  8.97  9.40
## bA    -1.79   0.22 -2.14 -1.43
## bR    -0.19   0.08 -0.31 -0.07
## bAR    0.25   0.14  0.04  0.47
## sigma  0.93   0.05  0.85  1.01
```

```r
precis(m.7H3_2)
```

```
##        Mean StdDev  5.5% 94.5%
## a      9.18   0.14  8.97  9.40
## bA    -1.85   0.22 -2.20 -1.50
## bR    -0.18   0.08 -0.31 -0.06
## bAR    0.35   0.13  0.14  0.55
## sigma  0.93   0.05  0.85  1.01
```

```r
#it seems the only difference is the interaction effect is smaller in the model without country Seychelles
```

(b)plot the posterior predictions for above two models

```r
#set the value of rugged
rugged.seq <- seq(from=-1,to=8,by=0.25)
# compute mu over samples for data without country Seychelles and the full data
```
TO BE DONE SOON


