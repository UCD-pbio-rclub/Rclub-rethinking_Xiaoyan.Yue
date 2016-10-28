# Assignment_Chapter_12
XIAOYAN YUE  
2016年10月27日  

##12E1 
As smaller sample sizes, shrink more. Hence, (a)will create more shrinkage.

##12E2
yi ~ Binomial(1,pi)
logit(pi) = a_group[i]+bxi
a_group ~ Normal(a,sigma) #creat varing intercepts prior
b ~ Normal(0,1)
a ~ Normal(0,1)#prior for average group
sigma ~ HalfCauchy(0,1)#prior for standard deviation of groups

The prior for the a_group intercept is now a function of two parameters,a and sigma. The Gaussian distribution with mean 'a' and standard deviation 'sigma' is the prior for each tank's intercept. Hence, there are two levels in the model,each resembling a simpler model.

##12M1
###Revisit the Reed frog survival data, data(reedfrogs), and add the predation and size treatment variables to the varying intercepts model. Consider models with either main effect alone, both main effects, as well as a model including both and their interaction. Instead of focusing on inferences about these two predictor variables, focus on the inferred variation across tanks. Explain why it changes as it does across models.

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
## rstan (Version 2.10.1, packaged: 2016-06-24 13:22:16 UTC, GitRev: 85f7a56811da)
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
## rethinking (Version 1.59)
```

```r
data(reedfrogs)
d <- reedfrogs
head(d)
```

```
##   density pred  size surv propsurv
## 1      10   no   big    9      0.9
## 2      10   no   big   10      1.0
## 3      10   no   big    7      0.7
## 4      10   no   big   10      1.0
## 5      10   no small    9      0.9
## 6      10   no small    9      0.9
```

```r
colnames(d)
```

```
## [1] "density"  "pred"     "size"     "surv"     "propsurv"
```

```r
d
```

```
##    density pred  size surv  propsurv
## 1       10   no   big    9 0.9000000
## 2       10   no   big   10 1.0000000
## 3       10   no   big    7 0.7000000
## 4       10   no   big   10 1.0000000
## 5       10   no small    9 0.9000000
## 6       10   no small    9 0.9000000
## 7       10   no small   10 1.0000000
## 8       10   no small    9 0.9000000
## 9       10 pred   big    4 0.4000000
## 10      10 pred   big    9 0.9000000
## 11      10 pred   big    7 0.7000000
## 12      10 pred   big    6 0.6000000
## 13      10 pred small    7 0.7000000
## 14      10 pred small    5 0.5000000
## 15      10 pred small    9 0.9000000
## 16      10 pred small    9 0.9000000
## 17      25   no   big   24 0.9600000
## 18      25   no   big   23 0.9200000
## 19      25   no   big   22 0.8800000
## 20      25   no   big   25 1.0000000
## 21      25   no small   23 0.9200000
## 22      25   no small   23 0.9200000
## 23      25   no small   23 0.9200000
## 24      25   no small   21 0.8400000
## 25      25 pred   big    6 0.2400000
## 26      25 pred   big   13 0.5200000
## 27      25 pred   big    4 0.1600000
## 28      25 pred   big    9 0.3600000
## 29      25 pred small   13 0.5200000
## 30      25 pred small   20 0.8000000
## 31      25 pred small    8 0.3200000
## 32      25 pred small   10 0.4000000
## 33      35   no   big   34 0.9714286
## 34      35   no   big   33 0.9428571
## 35      35   no   big   33 0.9428571
## 36      35   no   big   31 0.8857143
## 37      35   no small   31 0.8857143
## 38      35   no small   35 1.0000000
## 39      35   no small   33 0.9428571
## 40      35   no small   32 0.9142857
## 41      35 pred   big    4 0.1142857
## 42      35 pred   big   12 0.3428571
## 43      35 pred   big   13 0.3714286
## 44      35 pred   big   14 0.4000000
## 45      35 pred small   22 0.6285714
## 46      35 pred small   12 0.3428571
## 47      35 pred small   31 0.8857143
## 48      35 pred small   17 0.4857143
```

```r
# make the tank cluster variable
d$tank <- 1:nrow(d)

#make dummy variables for predation and size
d$pred2 <- ifelse(d$pred=="no",0,1)
d$size2 <- ifelse(d$size=="small",0,1)

#fit the model with either main effect alone
##pred
m12m.1.pred <- map2stan(
 alist(
  surv ~ dbinom( density , p ) ,
  logit(p) <- a_tank[tank] + bp*pred2,
  a_tank[tank] ~ dnorm( a , sigma ) ,
  bp ~ dnorm(0,2),
  a ~ dnorm(0,5) ,
  sigma ~ dcauchy(0,1)
 ), data=d , iter=4000 , chains=4 )
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used
```

```
## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 1, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 1, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 1, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 1, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 1, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 1, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 1, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 1, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 1, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 1, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 1, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 2.87 seconds (Warm-up)
##                3.106 seconds (Sampling)
##                5.976 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 2, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 2, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 2, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 2, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 2, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 2, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 2, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 2, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 2, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 2, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 2, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 2.742 seconds (Warm-up)
##                3.103 seconds (Sampling)
##                5.845 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 3).
## 
## Chain 3, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 3, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 3, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 3, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 3, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 3, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 3, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 3, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 3, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 3, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 3, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 3, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 2.733 seconds (Warm-up)
##                2.997 seconds (Sampling)
##                5.73 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 4).
## 
## Chain 4, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 4, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 4, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 4, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 4, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 4, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 4, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 4, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 4, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 4, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 4, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 4, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 2.77 seconds (Warm-up)
##                2.747 seconds (Sampling)
##                5.517 seconds (Total)
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used

## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 0 seconds (Warm-up)
##                0.001 seconds (Sampling)
##                0.001 seconds (Total)
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## [ 800 / 8000 ]
[ 1600 / 8000 ]
[ 2400 / 8000 ]
[ 3200 / 8000 ]
[ 4000 / 8000 ]
[ 4800 / 8000 ]
[ 5600 / 8000 ]
[ 6400 / 8000 ]
[ 7200 / 8000 ]
[ 8000 / 8000 ]
```

```
## Aggregated binomial counts detected. Splitting to 0/1 outcome for WAIC calculation.
```

```r
precis(m12m.1.pred,depth=2)
```

```
##             Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a_tank[1]   2.57   0.69       1.44       3.63  8000    1
## a_tank[2]   3.08   0.78       1.88       4.35  8000    1
## a_tank[3]   1.73   0.63       0.76       2.76  8000    1
## a_tank[4]   3.10   0.76       1.85       4.28  8000    1
## a_tank[5]   2.58   0.72       1.46       3.74  8000    1
## a_tank[6]   2.58   0.71       1.42       3.67  8000    1
## a_tank[7]   3.10   0.77       1.87       4.31  8000    1
## a_tank[8]   2.58   0.71       1.42       3.66  8000    1
## a_tank[9]   2.37   0.58       1.46       3.30  2881    1
## a_tank[10]  3.77   0.65       2.67       4.73  2838    1
## a_tank[11]  3.18   0.60       2.25       4.14  2835    1
## a_tank[12]  2.92   0.59       2.02       3.89  2963    1
## a_tank[13]  3.18   0.60       2.27       4.14  3208    1
## a_tank[14]  2.65   0.59       1.76       3.64  3129    1
## a_tank[15]  3.77   0.66       2.70       4.80  2650    1
## a_tank[16]  3.77   0.66       2.70       4.75  2794    1
## a_tank[17]  2.98   0.63       2.02       4.00  8000    1
## a_tank[18]  2.63   0.59       1.74       3.61  8000    1
## a_tank[19]  2.32   0.54       1.48       3.16  8000    1
## a_tank[20]  3.41   0.72       2.29       4.54  4511    1
## a_tank[21]  2.62   0.58       1.70       3.49  8000    1
## a_tank[22]  2.62   0.59       1.67       3.54  8000    1
## a_tank[23]  2.62   0.57       1.68       3.50  8000    1
## a_tank[24]  2.06   0.51       1.22       2.85  8000    1
## a_tank[25]  1.72   0.49       0.92       2.48  2339    1
## a_tank[26]  2.69   0.47       1.98       3.45  2037    1
## a_tank[27]  1.39   0.51       0.58       2.20  2419    1
## a_tank[28]  2.15   0.47       1.42       2.93  2180    1
## a_tank[29]  2.70   0.48       1.88       3.39  1985    1
## a_tank[30]  3.71   0.51       2.88       4.49  2077    1
## a_tank[31]  2.02   0.48       1.28       2.81  2103    1
## a_tank[32]  2.30   0.46       1.55       3.03  2061    1
## a_tank[33]  3.15   0.62       2.18       4.11  8000    1
## a_tank[34]  2.83   0.56       1.89       3.67  8000    1
## a_tank[35]  2.82   0.56       1.90       3.67  8000    1
## a_tank[36]  2.31   0.49       1.54       3.09  8000    1
## a_tank[37]  2.30   0.48       1.50       3.05  8000    1
## a_tank[38]  3.54   0.68       2.45       4.56  8000    1
## a_tank[39]  2.83   0.57       1.91       3.69  8000    1
## a_tank[40]  2.55   0.51       1.74       3.33  8000    1
## a_tank[41]  1.06   0.50       0.30       1.89  2401    1
## a_tank[42]  2.07   0.45       1.32       2.74  1594    1
## a_tank[43]  2.17   0.45       1.44       2.87  1816    1
## a_tank[44]  2.28   0.43       1.59       2.95  1781    1
## a_tank[45]  3.09   0.44       2.41       3.80  1786    1
## a_tank[46]  2.06   0.44       1.36       2.76  1735    1
## a_tank[47]  4.21   0.52       3.35       4.99  2060    1
## a_tank[48]  2.58   0.43       1.91       3.26  1745    1
## bp         -2.62   0.31      -3.12      -2.12   967    1
## a           2.68   0.24       2.28       3.06  1128    1
## sigma       0.85   0.15       0.62       1.07  2480    1
```

```r
##size
m12m.1.size <- map2stan(
 alist(
  surv ~ dbinom( density , p ) ,
  logit(p) <- a_tank[tank] + bs*size2,
  a_tank[tank] ~ dnorm( a , sigma ) ,
  bs ~ dnorm(0,2),
  a ~ dnorm(0,5) ,
  sigma ~ dcauchy(0,1)), 
 data=d , iter=4000 , chains=4 )
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used

## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 1, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 1, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 1, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 1, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 1, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 1, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 1, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 1, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 1, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 1, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 1, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 3.546 seconds (Warm-up)
##                3.1 seconds (Sampling)
##                6.646 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 2, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 2, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 2, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 2, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 2, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 2, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 2, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 2, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 2, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 2, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 2, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 3.255 seconds (Warm-up)
##                3.343 seconds (Sampling)
##                6.598 seconds (Total)
## 
## [1] "The following numerical problems occured the indicated number of times after warmup on chain 2"
##                                                                                 count
## Exception thrown at line 20: normal_log: Scale parameter is 0, but must be > 0!     1
## [1] "When a numerical problem occurs, the Metropolis proposal gets rejected."
## [1] "However, by design Metropolis proposals sometimes get rejected  even when there are no numerical problems."
## [1] "Thus, if the number in the 'count' column is small,  do not ask about this message on stan-users."
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 3).
## 
## Chain 3, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 3, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 3, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 3, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 3, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 3, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 3, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 3, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 3, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 3, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 3, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 3, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 3.692 seconds (Warm-up)
##                3.316 seconds (Sampling)
##                7.008 seconds (Total)
## 
## [1] "The following numerical problems occured the indicated number of times after warmup on chain 3"
##                                                                                 count
## Exception thrown at line 20: normal_log: Scale parameter is 0, but must be > 0!     1
## [1] "When a numerical problem occurs, the Metropolis proposal gets rejected."
## [1] "However, by design Metropolis proposals sometimes get rejected  even when there are no numerical problems."
## [1] "Thus, if the number in the 'count' column is small,  do not ask about this message on stan-users."
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 4).
## 
## Chain 4, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 4, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 4, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 4, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 4, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 4, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 4, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 4, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 4, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 4, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 4, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 4, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 3.381 seconds (Warm-up)
##                3.673 seconds (Sampling)
##                7.054 seconds (Total)
## 
## [1] "The following numerical problems occured the indicated number of times after warmup on chain 4"
##                                                                                 count
## Exception thrown at line 20: normal_log: Scale parameter is 0, but must be > 0!     1
## [1] "When a numerical problem occurs, the Metropolis proposal gets rejected."
## [1] "However, by design Metropolis proposals sometimes get rejected  even when there are no numerical problems."
## [1] "Thus, if the number in the 'count' column is small,  do not ask about this message on stan-users."
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used

## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 0 seconds (Warm-up)
##                0 seconds (Sampling)
##                0 seconds (Total)
```

```
## Computing WAIC
## Constructing posterior predictions
```

```
## [ 800 / 8000 ]
[ 1600 / 8000 ]
[ 2400 / 8000 ]
[ 3200 / 8000 ]
[ 4000 / 8000 ]
[ 4800 / 8000 ]
[ 5600 / 8000 ]
[ 6400 / 8000 ]
[ 7200 / 8000 ]
[ 8000 / 8000 ]
```

```
## Aggregated binomial counts detected. Splitting to 0/1 outcome for WAIC calculation.
```

```r
precis(m12m.1.size,depth=2)
```

```
##             Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a_tank[1]   2.54   0.98       0.96       4.08  8000 1.00
## a_tank[2]   3.47   1.19       1.59       5.26  8000 1.00
## a_tank[3]   1.42   0.84       0.10       2.76  2031 1.00
## a_tank[4]   3.45   1.18       1.61       5.28  8000 1.00
## a_tank[5]   2.22   0.89       0.73       3.53  8000 1.00
## a_tank[6]   2.23   0.88       0.74       3.50  8000 1.00
## a_tank[7]   3.23   1.17       1.41       5.01  8000 1.00
## a_tank[8]   2.22   0.90       0.77       3.60  8000 1.00
## a_tank[9]   0.23   0.76      -1.04       1.40  1668 1.00
## a_tank[10]  2.54   1.00       0.98       4.13  8000 1.00
## a_tank[11]  1.41   0.82       0.10       2.69  1876 1.00
## a_tank[12]  0.99   0.79      -0.17       2.35  1723 1.00
## a_tank[13]  1.04   0.68      -0.04       2.09  8000 1.00
## a_tank[14]  0.23   0.61      -0.77       1.20  8000 1.00
## a_tank[15]  2.23   0.88       0.85       3.64  8000 1.00
## a_tank[16]  2.23   0.89       0.79       3.59  8000 1.00
## a_tank[17]  3.32   0.91       1.85       4.73  2129 1.00
## a_tank[18]  2.81   0.81       1.53       4.10  1808 1.00
## a_tank[19]  2.44   0.74       1.28       3.62  1429 1.00
## a_tank[20]  4.09   1.11       2.38       5.85  8000 1.00
## a_tank[21]  2.45   0.68       1.30       3.42  8000 1.00
## a_tank[22]  2.45   0.67       1.39       3.47  8000 1.00
## a_tank[23]  2.44   0.66       1.39       3.46  8000 1.00
## a_tank[24]  1.73   0.53       0.87       2.55  8000 1.00
## a_tank[25] -0.57   0.66      -1.60       0.48  1172 1.01
## a_tank[26]  0.59   0.64      -0.41       1.63  1147 1.01
## a_tank[27] -1.01   0.70      -2.17       0.06  1321 1.01
## a_tank[28] -0.04   0.64      -1.00       1.04  1116 1.01
## a_tank[29]  0.18   0.40      -0.46       0.79  8000 1.00
## a_tank[30]  1.47   0.50       0.64       2.23  8000 1.00
## a_tank[31] -0.62   0.42      -1.28       0.06  8000 1.00
## a_tank[32] -0.29   0.39      -0.92       0.33  8000 1.00
## a_tank[33]  3.61   0.88       2.26       5.03  2074 1.00
## a_tank[34]  3.14   0.80       1.88       4.38  1853 1.00
## a_tank[35]  3.13   0.79       1.89       4.39  1787 1.00
## a_tank[36]  2.49   0.70       1.40       3.63  1375 1.00
## a_tank[37]  2.09   0.52       1.25       2.88  8000 1.00
## a_tank[38]  4.02   1.04       2.36       5.54  8000 1.00
## a_tank[39]  2.75   0.66       1.72       3.77  8000 1.00
## a_tank[40]  2.39   0.58       1.44       3.26  8000 1.00
## a_tank[41] -1.39   0.68      -2.47      -0.32  1189 1.00
## a_tank[42] -0.15   0.60      -1.12       0.81   985 1.01
## a_tank[43] -0.02   0.60      -0.95       0.95   941 1.01
## a_tank[44]  0.10   0.60      -0.87       1.05   967 1.01
## a_tank[45]  0.60   0.35       0.02       1.13  8000 1.00
## a_tank[46] -0.56   0.35      -1.11       0.00  8000 1.00
## a_tank[47]  2.09   0.52       1.30       2.91  8000 1.00
## a_tank[48]  0.02   0.33      -0.54       0.52  8000 1.00
## bs         -0.44   0.51      -1.27       0.35   722 1.01
## a           1.61   0.36       1.04       2.20  1096 1.01
## sigma       1.64   0.23       1.28       1.98  4286 1.00
```

```r
##fit models with both main effects, including pred and size
m12m.1.pred.size <- map2stan(
 alist(
  surv ~ dbinom( density , p ) ,
  logit(p) <- a_tank[tank] + bp*pred2 + bs*size2 ,
  a_tank[tank] ~ dnorm( a , sigma ) ,
  bp ~ dnorm(0,2),
  bs ~ dnorm(0,2),
  a ~ dnorm(0,5) ,
  sigma ~ dcauchy(0,1)), 
 data=d , iter=4000 , chains=4 )
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used

## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 1, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 1, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 1, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 1, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 1, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 1, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 1, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 1, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 1, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 1, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 1, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 3.635 seconds (Warm-up)
##                3.353 seconds (Sampling)
##                6.988 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 2, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 2, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 2, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 2, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 2, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 2, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 2, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 2, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 2, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 2, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 2, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 3.546 seconds (Warm-up)
##                3.648 seconds (Sampling)
##                7.194 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 3).
## 
## Chain 3, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 3, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 3, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 3, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 3, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 3, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 3, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 3, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 3, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 3, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 3, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 3, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 3.278 seconds (Warm-up)
##                3.398 seconds (Sampling)
##                6.676 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 4).
## 
## Chain 4, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 4, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 4, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 4, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 4, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 4, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 4, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 4, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 4, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 4, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 4, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 4, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 3.167 seconds (Warm-up)
##                4.053 seconds (Sampling)
##                7.22 seconds (Total)
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used

## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 0 seconds (Warm-up)
##                0 seconds (Sampling)
##                0 seconds (Total)
```

```
## Computing WAIC
## Constructing posterior predictions
```

```
## [ 800 / 8000 ]
[ 1600 / 8000 ]
[ 2400 / 8000 ]
[ 3200 / 8000 ]
[ 4000 / 8000 ]
[ 4800 / 8000 ]
[ 5600 / 8000 ]
[ 6400 / 8000 ]
[ 7200 / 8000 ]
[ 8000 / 8000 ]
```

```
## Aggregated binomial counts detected. Splitting to 0/1 outcome for WAIC calculation.
```

```r
precis(m12m.1.pred.size,depth=2)
```

```
##             Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a_tank[1]   2.90   0.69       1.82       4.01  8000    1
## a_tank[2]   3.33   0.77       2.12       4.56  8000    1
## a_tank[3]   2.14   0.64       1.10       3.13  8000    1
## a_tank[4]   3.34   0.76       2.12       4.50  8000    1
## a_tank[5]   2.73   0.72       1.55       3.84  8000    1
## a_tank[6]   2.75   0.70       1.68       3.87  8000    1
## a_tank[7]   3.22   0.75       2.00       4.37  8000    1
## a_tank[8]   2.74   0.70       1.62       3.84  8000    1
## a_tank[9]   2.78   0.62       1.79       3.78  1572    1
## a_tank[10]  4.07   0.68       3.01       5.14  1726    1
## a_tank[11]  3.53   0.63       2.54       4.54  1638    1
## a_tank[12]  3.28   0.62       2.32       4.27  1635    1
## a_tank[13]  3.24   0.58       2.25       4.10  8000    1
## a_tank[14]  2.73   0.56       1.85       3.64  8000    1
## a_tank[15]  3.82   0.64       2.78       4.80  8000    1
## a_tank[16]  3.83   0.64       2.81       4.80  8000    1
## a_tank[17]  3.31   0.64       2.25       4.30  8000    1
## a_tank[18]  2.99   0.60       2.02       3.91  8000    1
## a_tank[19]  2.71   0.57       1.80       3.61  8000    1
## a_tank[20]  3.67   0.71       2.52       4.72  8000    1
## a_tank[21]  2.75   0.58       1.81       3.64  8000    1
## a_tank[22]  2.74   0.58       1.85       3.67  8000    1
## a_tank[23]  2.74   0.59       1.80       3.62  8000    1
## a_tank[24]  2.17   0.51       1.33       2.97  8000    1
## a_tank[25]  2.17   0.56       1.26       3.06  1241    1
## a_tank[26]  3.12   0.54       2.23       3.94  1169    1
## a_tank[27]  1.84   0.58       0.92       2.75  1352    1
## a_tank[28]  2.60   0.55       1.74       3.48  1150    1
## a_tank[29]  2.74   0.46       1.99       3.47  1879    1
## a_tank[30]  3.72   0.49       2.96       4.51  1885    1
## a_tank[31]  2.08   0.47       1.34       2.85  1892    1
## a_tank[32]  2.35   0.46       1.62       3.08  1922    1
## a_tank[33]  3.48   0.63       2.48       4.46  8000    1
## a_tank[34]  3.18   0.58       2.28       4.12  8000    1
## a_tank[35]  3.19   0.58       2.27       4.09  8000    1
## a_tank[36]  2.72   0.53       1.86       3.55  8000    1
## a_tank[37]  2.40   0.49       1.59       3.14  8000    1
## a_tank[38]  3.61   0.67       2.55       4.67  8000    1
## a_tank[39]  2.92   0.56       2.01       3.80  8000    1
## a_tank[40]  2.65   0.52       1.80       3.46  8000    1
## a_tank[41]  1.53   0.57       0.61       2.44  1265    1
## a_tank[42]  2.52   0.52       1.71       3.38  1081    1
## a_tank[43]  2.63   0.53       1.79       3.48  1033    1
## a_tank[44]  2.73   0.52       1.93       3.59  1008    1
## a_tank[45]  3.11   0.43       2.44       3.81  1672    1
## a_tank[46]  2.12   0.43       1.44       2.82  1795    1
## a_tank[47]  4.21   0.50       3.36       4.97  1919    1
## a_tank[48]  2.62   0.42       1.95       3.29  1614    1
## bp         -2.61   0.31      -3.10      -2.12   956    1
## bs         -0.49   0.30      -0.97      -0.02  1459    1
## a           2.91   0.29       2.44       3.38   772    1
## sigma       0.80   0.15       0.58       1.03  2795    1
```

```r
##fit model including both and their interaction
m12m.1.pred.size.interaction <- map2stan(
 alist(
  surv ~ dbinom( density , p ) ,
  logit(p) <- a_tank[tank] + bp*pred2 + bs*size2 +bps*pred2*size2,
  a_tank[tank] ~ dnorm( a , sigma ) ,
  bp ~ dnorm(0,2),
  bs ~ dnorm(0,2),
  bps ~ dnorm(0,1),
  a ~ dnorm(0,5) ,
  sigma ~ dcauchy(0,1)), 
 data=d , iter=4000 , chains=4 )
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used

## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 1, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 1, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 1, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 1, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 1, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 1, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 1, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 1, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 1, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 1, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 1, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 4.266 seconds (Warm-up)
##                4.466 seconds (Sampling)
##                8.732 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 2, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 2, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 2, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 2, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 2, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 2, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 2, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 2, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 2, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 2, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 2, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 4.353 seconds (Warm-up)
##                4.694 seconds (Sampling)
##                9.047 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 3).
## 
## Chain 3, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 3, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 3, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 3, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 3, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 3, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 3, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 3, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 3, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 3, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 3, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 3, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 4.351 seconds (Warm-up)
##                4.214 seconds (Sampling)
##                8.565 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 4).
## 
## Chain 4, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 4, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 4, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 4, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 4, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 4, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 4, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 4, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 4, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 4, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 4, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 4, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 4.379 seconds (Warm-up)
##                5.9 seconds (Sampling)
##                10.279 seconds (Total)
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used

## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## 
## SAMPLING FOR MODEL 'surv ~ dbinom(density, p)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 0 seconds (Warm-up)
##                0 seconds (Sampling)
##                0 seconds (Total)
```

```
## Computing WAIC
## Constructing posterior predictions
```

```
## [ 800 / 8000 ]
[ 1600 / 8000 ]
[ 2400 / 8000 ]
[ 3200 / 8000 ]
[ 4000 / 8000 ]
[ 4800 / 8000 ]
[ 5600 / 8000 ]
[ 6400 / 8000 ]
[ 7200 / 8000 ]
[ 8000 / 8000 ]
```

```
## Aggregated binomial counts detected. Splitting to 0/1 outcome for WAIC calculation.
```

```r
precis(m12m.1.pred.size.interaction,depth=2)
```

```
##             Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a_tank[1]   2.58   0.72       1.47       3.76  2017    1
## a_tank[2]   3.01   0.76       1.80       4.21  1763    1
## a_tank[3]   1.85   0.68       0.77       2.94  2473    1
## a_tank[4]   3.01   0.75       1.84       4.22  2003    1
## a_tank[5]   2.58   0.67       1.51       3.60  3643    1
## a_tank[6]   2.57   0.68       1.46       3.63  3639    1
## a_tank[7]   3.01   0.72       1.84       4.09  2433    1
## a_tank[8]   2.56   0.66       1.57       3.68  3614    1
## a_tank[9]   2.64   0.61       1.65       3.57  1715    1
## a_tank[10]  3.87   0.66       2.84       4.91  1410    1
## a_tank[11]  3.37   0.63       2.34       4.31  1471    1
## a_tank[12]  3.13   0.61       2.13       4.05  1587    1
## a_tank[13]  2.91   0.63       1.91       3.90  1575    1
## a_tank[14]  2.41   0.59       1.42       3.32  1750    1
## a_tank[15]  3.44   0.67       2.36       4.48  1552    1
## a_tank[16]  3.44   0.66       2.43       4.51  1669    1
## a_tank[17]  2.93   0.67       1.83       3.97  1965    1
## a_tank[18]  2.62   0.64       1.60       3.62  1827    1
## a_tank[19]  2.35   0.62       1.35       3.34  1823    1
## a_tank[20]  3.29   0.73       2.11       4.39  1834    1
## a_tank[21]  2.63   0.56       1.73       3.51  3987    1
## a_tank[22]  2.63   0.57       1.72       3.53  4051    1
## a_tank[23]  2.62   0.57       1.74       3.52  4112    1
## a_tank[24]  2.09   0.51       1.27       2.88  5273    1
## a_tank[25]  2.09   0.54       1.24       2.93  1435    1
## a_tank[26]  3.03   0.52       2.23       3.87  1172    1
## a_tank[27]  1.78   0.56       0.91       2.69  1512    1
## a_tank[28]  2.51   0.52       1.64       3.31  1293    1
## a_tank[29]  2.38   0.50       1.59       3.18  1212    1
## a_tank[30]  3.34   0.54       2.44       4.15  1187    1
## a_tank[31]  1.75   0.50       0.94       2.54  1254    1
## a_tank[32]  2.00   0.51       1.16       2.79  1284    1
## a_tank[33]  3.09   0.65       2.11       4.19  1780    1
## a_tank[34]  2.79   0.62       1.81       3.77  1748    1
## a_tank[35]  2.81   0.62       1.84       3.80  1773    1
## a_tank[36]  2.32   0.58       1.40       3.24  1755    1
## a_tank[37]  2.32   0.48       1.52       3.04  4848    1
## a_tank[38]  3.42   0.65       2.41       4.46  2861    1
## a_tank[39]  2.80   0.54       1.95       3.66  4115    1
## a_tank[40]  2.55   0.50       1.76       3.34  4646    1
## a_tank[41]  1.48   0.54       0.66       2.39  1448    1
## a_tank[42]  2.44   0.50       1.66       3.24  1211    1
## a_tank[43]  2.55   0.50       1.72       3.32  1166    1
## a_tank[44]  2.65   0.50       1.88       3.47  1129    1
## a_tank[45]  2.73   0.48       1.99       3.52  1067    1
## a_tank[46]  1.76   0.48       0.99       2.50  1136    1
## a_tank[47]  3.81   0.54       2.97       4.69  1190    1
## a_tank[48]  2.25   0.48       1.50       3.03  1098    1
## bp         -2.21   0.38      -2.84      -1.61   739    1
## bs          0.01   0.42      -0.64       0.69   976    1
## bps        -0.85   0.51      -1.69      -0.09  1221    1
## a           2.67   0.32       2.19       3.20   637    1
## sigma       0.75   0.15       0.52       0.99  1615    1
```

##12M2
### Compare the models you fit just above, using WAIC. Can you reconcile the differences in WAIC with the posterior distributions of the models?

```r
#compare the models
compare(m12m.1.pred,m12m.1.size,m12m.1.pred.size,m12m.1.pred.size.interaction)
```

```
##                                WAIC pWAIC dWAIC weight    SE  dSE
## m12m.1.pred.size             1001.1  28.7   0.0   0.40 38.14   NA
## m12m.1.pred                  1001.5  29.8   0.4   0.33 38.10 2.01
## m12m.1.pred.size.interaction 1001.9  28.7   0.8   0.27 38.20 2.04
## m12m.1.size                  1010.6  38.4   9.4   0.00 38.37 7.06
```


