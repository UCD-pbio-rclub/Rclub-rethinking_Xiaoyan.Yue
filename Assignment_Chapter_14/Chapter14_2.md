# Chapter_14_2
Xiaoyan Yue  
3/1/2017  



##14E2
Ti ~ Poisson(mu)  
log(mu)= a + b*logP  
logP ~ Normal (V_p,sigma_p)  
V_p ~ Normal(0.5,1)  
a ~ Normal(0,10)  
b ~ Normal(0,1)  
sigma_p ~ Cauchy(0,1)
  
##14M1 Using the mathematical form of the imputation model in the chapter, expalin what is being assumed about how the missing values were generated.  
####It assumed that the missing value is generated completely at random.

  
##14M2
###load the data

```r
data(milk)
```

###Repeat the model in Chapter 6

```r
#Remove NA 
d1 <- milk[ complete.cases(milk) , ]
d1$neocortex <- d1$neocortex.perc / 100
dim(d1)
```

```
## [1] 17  9
```

```r
a.start <- mean(d1$kcal.per.g)
sigma.start <- log(sd(d1$kcal.per.g))
#Fit different model
m6.11 <- map(
  alist(
    kcal.per.g ~ dnorm( a , exp(log.sigma) )
  ) ,
  data=d1 , start=list(a=a.start,log.sigma=sigma.start) )
m6.12 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bn*neocortex
  ) ,
  data=d1 , start=list(a=a.start,bn=0,log.sigma=sigma.start) )
m6.13 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bm*log(mass)
  ) ,
  data=d1 , start=list(a=a.start,bm=0,log.sigma=sigma.start) )
m6.14 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bn*neocortex + bm*log(mass)
  ) ,
  data=d1 , start=list(a=a.start,bn=0,bm=0,log.sigma=sigma.start) )

#Compare the models
(milk.models <- compare( m6.11 , m6.12 , m6.13 , m6.14 ))
```

```
##        WAIC pWAIC dWAIC weight   SE  dSE
## m6.14 -14.8   4.9   0.0   0.92 7.61   NA
## m6.13  -8.1   3.0   6.8   0.03 5.74 5.36
## m6.11  -7.9   2.0   7.0   0.03 4.68 7.41
## m6.12  -6.7   2.7   8.2   0.02 4.19 7.61
```
  
###Imputing neocortex

```r
#Load the data
d2 <- milk
d2$neocortex.prop <- d2$neocortex.perc / 100
d2$logmass <- log(d2$mass)
summary(d2)
```

```
##               clade                 species     kcal.per.g    
##  Ape             :9   A palliata        : 1   Min.   :0.4600  
##  New World Monkey:9   Alouatta seniculus: 1   1st Qu.:0.4900  
##  Old World Monkey:6   Callimico goeldii : 1   Median :0.6000  
##  Strepsirrhine   :5   Callithrix jacchus: 1   Mean   :0.6417  
##                       Cebuella pygmaea  : 1   3rd Qu.:0.7300  
##                       Cebus apella      : 1   Max.   :0.9700  
##                       (Other)           :23                   
##     perc.fat      perc.protein    perc.lactose        mass      
##  Min.   : 3.93   Min.   : 7.37   Min.   :27.09   Min.   : 0.12  
##  1st Qu.:21.22   1st Qu.:13.00   1st Qu.:37.80   1st Qu.: 1.62  
##  Median :36.84   Median :15.80   Median :48.64   Median : 3.47  
##  Mean   :33.99   Mean   :16.40   Mean   :49.61   Mean   :14.73  
##  3rd Qu.:46.08   3rd Qu.:20.77   3rd Qu.:60.12   3rd Qu.:10.72  
##  Max.   :55.51   Max.   :25.30   Max.   :71.91   Max.   :97.72  
##                                                                 
##  neocortex.perc  neocortex.prop      logmass       
##  Min.   :55.16   Min.   :0.5516   Min.   :-2.1203  
##  1st Qu.:64.54   1st Qu.:0.6454   1st Qu.: 0.4824  
##  Median :68.85   Median :0.6885   Median : 1.2442  
##  Mean   :67.58   Mean   :0.6758   Mean   : 1.4418  
##  3rd Qu.:71.26   3rd Qu.:0.7126   3rd Qu.: 2.3721  
##  Max.   :76.30   Max.   :0.7630   Max.   : 4.5821  
##  NA's   :12      NA's   :12
```

```r
#Create the data list
data_list <- list(
  kcal = d2$kcal.per.g,
  neocortex = d2$neocortex.prop,
  logmass = d2$logmass )

#Fit different models
m14M2.1 <- map2stan(
  alist(
    kcal ~ dnorm( mu , sigma ),
    mu <- a,
    a ~ dnorm(0,100),
    sigma ~ dcauchy(0,1)
  ) ,
  data=data_list , iter = 1e4, chains = 2 )
```

```
## In file included from file2ebc76fc0b2e.cpp:8:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/src/stan/model/model_header.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/mat.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core.hpp:12:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core/gevv_vvv_vari.hpp:5:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core/var.hpp:7:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/math/tools/config.hpp:13:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/config.hpp:39:
## /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/config/compiler/clang.hpp:196:11: warning: 'BOOST_NO_CXX11_RVALUE_REFERENCES' macro redefined [-Wmacro-redefined]
## #  define BOOST_NO_CXX11_RVALUE_REFERENCES
##           ^
## <command line>:6:9: note: previous definition is here
## #define BOOST_NO_CXX11_RVALUE_REFERENCES 1
##         ^
## 1 warning generated.
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 1, Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Chain 1, Iteration: 2000 / 10000 [ 20%]  (Warmup)
## Chain 1, Iteration: 3000 / 10000 [ 30%]  (Warmup)
## Chain 1, Iteration: 4000 / 10000 [ 40%]  (Warmup)
## Chain 1, Iteration: 5000 / 10000 [ 50%]  (Warmup)
## Chain 1, Iteration: 5001 / 10000 [ 50%]  (Sampling)
## Chain 1, Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Chain 1, Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Chain 1, Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Chain 1, Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Chain 1, Iteration: 10000 / 10000 [100%]  (Sampling)
##  Elapsed Time: 0.075472 seconds (Warm-up)
##                0.061358 seconds (Sampling)
##                0.13683 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 2, Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Chain 2, Iteration: 2000 / 10000 [ 20%]  (Warmup)
## Chain 2, Iteration: 3000 / 10000 [ 30%]  (Warmup)
## Chain 2, Iteration: 4000 / 10000 [ 40%]  (Warmup)
## Chain 2, Iteration: 5000 / 10000 [ 50%]  (Warmup)
## Chain 2, Iteration: 5001 / 10000 [ 50%]  (Sampling)
## Chain 2, Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Chain 2, Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Chain 2, Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Chain 2, Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Chain 2, Iteration: 10000 / 10000 [100%]  (Sampling)
##  Elapsed Time: 0.073269 seconds (Warm-up)
##                0.062373 seconds (Sampling)
##                0.135642 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 5e-06 seconds (Warm-up)
##                2.5e-05 seconds (Sampling)
##                3e-05 seconds (Total)
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```r
m14M2.2 <- map2stan(
  alist(
    kcal ~ dnorm( mu , sigma ),
    neocortex ~ dnorm(nu,sigma_N),
    mu <- a + bN*neocortex,
    a ~ dnorm(0,100),
    bN ~ dnorm(0,10),
    nu ~ dnorm(0.5,1),
    sigma_N ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1)
  ) ,
  data=data_list , iter = 1e4, chains = 2 )
```

```
## Imputing 12 missing values (NA) in variable 'neocortex'.
```

```
## In file included from file2ebc7b8f6991.cpp:8:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/src/stan/model/model_header.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/mat.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core.hpp:12:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core/gevv_vvv_vari.hpp:5:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core/var.hpp:7:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/math/tools/config.hpp:13:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/config.hpp:39:
## /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/config/compiler/clang.hpp:196:11: warning: 'BOOST_NO_CXX11_RVALUE_REFERENCES' macro redefined [-Wmacro-redefined]
## #  define BOOST_NO_CXX11_RVALUE_REFERENCES
##           ^
## <command line>:6:9: note: previous definition is here
## #define BOOST_NO_CXX11_RVALUE_REFERENCES 1
##         ^
## 1 warning generated.
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 1, Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Chain 1, Iteration: 2000 / 10000 [ 20%]  (Warmup)
## Chain 1, Iteration: 3000 / 10000 [ 30%]  (Warmup)
## Chain 1, Iteration: 4000 / 10000 [ 40%]  (Warmup)
## Chain 1, Iteration: 5000 / 10000 [ 50%]  (Warmup)
## Chain 1, Iteration: 5001 / 10000 [ 50%]  (Sampling)
## Chain 1, Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Chain 1, Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Chain 1, Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Chain 1, Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Chain 1, Iteration: 10000 / 10000 [100%]  (Sampling)
##  Elapsed Time: 1.82526 seconds (Warm-up)
##                1.84605 seconds (Sampling)
##                3.67131 seconds (Total)
```

```
## The following numerical problems occured the indicated number of times on chain 1
```

```
##                                                                                 count
## Exception thrown at line 31: normal_log: Scale parameter is 0, but must be > 0!     2
```

```
## When a numerical problem occurs, the Hamiltonian proposal gets rejected.
```

```
## See http://mc-stan.org/misc/warnings.html#exception-hamiltonian-proposal-rejected
```

```
## If the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 2, Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Chain 2, Iteration: 2000 / 10000 [ 20%]  (Warmup)
## Chain 2, Iteration: 3000 / 10000 [ 30%]  (Warmup)
## Chain 2, Iteration: 4000 / 10000 [ 40%]  (Warmup)
## Chain 2, Iteration: 5000 / 10000 [ 50%]  (Warmup)
## Chain 2, Iteration: 5001 / 10000 [ 50%]  (Sampling)
## Chain 2, Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Chain 2, Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Chain 2, Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Chain 2, Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Chain 2, Iteration: 10000 / 10000 [100%]  (Sampling)
##  Elapsed Time: 1.77317 seconds (Warm-up)
##                1.49917 seconds (Sampling)
##                3.27234 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 4e-06 seconds (Warm-up)
##                3.2e-05 seconds (Sampling)
##                3.6e-05 seconds (Total)
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```r
m14M2.3 <- map2stan(
  alist(
    kcal ~ dnorm(mu,sigma),
    mu <- a + bM*logmass,
    a ~ dnorm(0,100),
    bM ~ dnorm(0,10),
    sigma ~ dcauchy(0,1)
  ) ,
  data=data_list , iter=1e4 , chains=2 )
```

```
## In file included from file2ebc1863f04a.cpp:8:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/src/stan/model/model_header.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/mat.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core.hpp:12:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core/gevv_vvv_vari.hpp:5:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core/var.hpp:7:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/math/tools/config.hpp:13:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/config.hpp:39:
## /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/config/compiler/clang.hpp:196:11: warning: 'BOOST_NO_CXX11_RVALUE_REFERENCES' macro redefined [-Wmacro-redefined]
## #  define BOOST_NO_CXX11_RVALUE_REFERENCES
##           ^
## <command line>:6:9: note: previous definition is here
## #define BOOST_NO_CXX11_RVALUE_REFERENCES 1
##         ^
## 1 warning generated.
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 1, Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Chain 1, Iteration: 2000 / 10000 [ 20%]  (Warmup)
## Chain 1, Iteration: 3000 / 10000 [ 30%]  (Warmup)
## Chain 1, Iteration: 4000 / 10000 [ 40%]  (Warmup)
## Chain 1, Iteration: 5000 / 10000 [ 50%]  (Warmup)
## Chain 1, Iteration: 5001 / 10000 [ 50%]  (Sampling)
## Chain 1, Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Chain 1, Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Chain 1, Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Chain 1, Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Chain 1, Iteration: 10000 / 10000 [100%]  (Sampling)
##  Elapsed Time: 0.129158 seconds (Warm-up)
##                0.121278 seconds (Sampling)
##                0.250436 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 2, Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Chain 2, Iteration: 2000 / 10000 [ 20%]  (Warmup)
## Chain 2, Iteration: 3000 / 10000 [ 30%]  (Warmup)
## Chain 2, Iteration: 4000 / 10000 [ 40%]  (Warmup)
## Chain 2, Iteration: 5000 / 10000 [ 50%]  (Warmup)
## Chain 2, Iteration: 5001 / 10000 [ 50%]  (Sampling)
## Chain 2, Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Chain 2, Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Chain 2, Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Chain 2, Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Chain 2, Iteration: 10000 / 10000 [100%]  (Sampling)
##  Elapsed Time: 0.124745 seconds (Warm-up)
##                0.118591 seconds (Sampling)
##                0.243336 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 3e-06 seconds (Warm-up)
##                2.1e-05 seconds (Sampling)
##                2.4e-05 seconds (Total)
```

```
## Computing WAIC
## Constructing posterior predictions
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```r
m14M2.4 <- map2stan(
  alist(
    kcal ~ dnorm(mu,sigma),
    mu <- a + bN*neocortex + bM*logmass,
    neocortex ~ dnorm(nu,sigma_N),
    a ~ dnorm(0,100),
    c(bN,bM) ~ dnorm(0,10),
    nu ~ dnorm(0.5,1),
    sigma_N ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1)
  ) ,
  data=data_list , iter=1e4 , chains=2 )
```

```
## Imputing 12 missing values (NA) in variable 'neocortex'.
```

```
## In file included from file2ebc67c5e8bf.cpp:8:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/src/stan/model/model_header.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/mat.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core.hpp:12:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core/gevv_vvv_vari.hpp:5:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core/var.hpp:7:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/math/tools/config.hpp:13:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/config.hpp:39:
## /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/config/compiler/clang.hpp:196:11: warning: 'BOOST_NO_CXX11_RVALUE_REFERENCES' macro redefined [-Wmacro-redefined]
## #  define BOOST_NO_CXX11_RVALUE_REFERENCES
##           ^
## <command line>:6:9: note: previous definition is here
## #define BOOST_NO_CXX11_RVALUE_REFERENCES 1
##         ^
## 1 warning generated.
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 1, Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Chain 1, Iteration: 2000 / 10000 [ 20%]  (Warmup)
## Chain 1, Iteration: 3000 / 10000 [ 30%]  (Warmup)
## Chain 1, Iteration: 4000 / 10000 [ 40%]  (Warmup)
## Chain 1, Iteration: 5000 / 10000 [ 50%]  (Warmup)
## Chain 1, Iteration: 5001 / 10000 [ 50%]  (Sampling)
## Chain 1, Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Chain 1, Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Chain 1, Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Chain 1, Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Chain 1, Iteration: 10000 / 10000 [100%]  (Sampling)
##  Elapsed Time: 2.682 seconds (Warm-up)
##                2.96946 seconds (Sampling)
##                5.65147 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 2, Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Chain 2, Iteration: 2000 / 10000 [ 20%]  (Warmup)
## Chain 2, Iteration: 3000 / 10000 [ 30%]  (Warmup)
## Chain 2, Iteration: 4000 / 10000 [ 40%]  (Warmup)
## Chain 2, Iteration: 5000 / 10000 [ 50%]  (Warmup)
## Chain 2, Iteration: 5001 / 10000 [ 50%]  (Sampling)
## Chain 2, Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Chain 2, Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Chain 2, Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Chain 2, Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Chain 2, Iteration: 10000 / 10000 [100%]  (Sampling)
##  Elapsed Time: 2.23008 seconds (Warm-up)
##                2.79701 seconds (Sampling)
##                5.02709 seconds (Total)
```

```
## The following numerical problems occured the indicated number of times on chain 2
```

```
##                                                                                 count
## Exception thrown at line 31: normal_log: Scale parameter is 0, but must be > 0!     2
```

```
## When a numerical problem occurs, the Hamiltonian proposal gets rejected.
```

```
## See http://mc-stan.org/misc/warnings.html#exception-hamiltonian-proposal-rejected
```

```
## If the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## 
## SAMPLING FOR MODEL 'kcal ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 3e-06 seconds (Warm-up)
##                3.6e-05 seconds (Sampling)
##                3.9e-05 seconds (Total)
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```r
#Compare the models
(milk.models1 <- compare( m14M2.1 , m14M2.2 , m14M2.3 , m14M2.4 ))
```

```
##          WAIC pWAIC dWAIC weight   SE  dSE
## m14M2.4 -29.6   6.1   0.0   0.96 5.48   NA
## m14M2.3 -22.4   2.1   7.2   0.03 5.68 3.03
## m14M2.1 -20.9   1.4   8.7   0.01 5.31 4.59
## m14M2.2 -19.5   2.6  10.1   0.01 4.99 4.76
```
  
####It seems the weight of the model with imputation increased to 0.95 from 0.91 in the model excluded the missing data, and the standared error also decreased 
  
##14H3
###create the data

```r
set.seed(100)
x <- c(rnorm(10), NA)
y <- c(rnorm(10,x),100)
d <- list(x=x,y=y)
```

###Fit the model by imputing the one missing value for x

```r
m14H3.2 <- map2stan(
  alist(
    y ~ dnorm(mu,sigma),
    mu <- a + bX*x,
    x ~ dnorm(0,1),
    a ~ dnorm(0,100),
    bX ~ dnorm(0,100),
    sigma ~ dcauchy(0,1)
  ) ,
  data=d , iter=1e4 , chains=2 )
```

```
## Imputing 1 missing values (NA) in variable 'x'.
```

```
## In file included from file2d7a3c652412.cpp:8:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/src/stan/model/model_header.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/mat.hpp:4:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core.hpp:12:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core/gevv_vvv_vari.hpp:5:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/StanHeaders/include/stan/math/rev/core/var.hpp:7:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/math/tools/config.hpp:13:
## In file included from /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/config.hpp:39:
## /Library/Frameworks/R.framework/Versions/3.3/Resources/library/BH/include/boost/config/compiler/clang.hpp:196:11: warning: 'BOOST_NO_CXX11_RVALUE_REFERENCES' macro redefined [-Wmacro-redefined]
## #  define BOOST_NO_CXX11_RVALUE_REFERENCES
##           ^
## <command line>:6:9: note: previous definition is here
## #define BOOST_NO_CXX11_RVALUE_REFERENCES 1
##         ^
## 1 warning generated.
## 
## SAMPLING FOR MODEL 'y ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 1, Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Chain 1, Iteration: 2000 / 10000 [ 20%]  (Warmup)
## Chain 1, Iteration: 3000 / 10000 [ 30%]  (Warmup)
## Chain 1, Iteration: 4000 / 10000 [ 40%]  (Warmup)
## Chain 1, Iteration: 5000 / 10000 [ 50%]  (Warmup)
## Chain 1, Iteration: 5001 / 10000 [ 50%]  (Sampling)
## Chain 1, Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Chain 1, Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Chain 1, Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Chain 1, Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Chain 1, Iteration: 10000 / 10000 [100%]  (Sampling)
##  Elapsed Time: 0.278956 seconds (Warm-up)
##                0.281002 seconds (Sampling)
##                0.559958 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'y ~ dnorm(mu, sigma)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 2, Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Chain 2, Iteration: 2000 / 10000 [ 20%]  (Warmup)
## Chain 2, Iteration: 3000 / 10000 [ 30%]  (Warmup)
## Chain 2, Iteration: 4000 / 10000 [ 40%]  (Warmup)
## Chain 2, Iteration: 5000 / 10000 [ 50%]  (Warmup)
## Chain 2, Iteration: 5001 / 10000 [ 50%]  (Sampling)
## Chain 2, Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Chain 2, Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Chain 2, Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Chain 2, Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Chain 2, Iteration: 10000 / 10000 [100%]  (Sampling)
##  Elapsed Time: 0.22962 seconds (Warm-up)
##                0.177936 seconds (Sampling)
##                0.407556 seconds (Total)
```

```
## Warning: There were 305 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## 
## SAMPLING FOR MODEL 'y ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 3e-06 seconds (Warm-up)
##                2.4e-05 seconds (Sampling)
##                2.7e-05 seconds (Total)
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```
## Warning in map2stan(alist(y ~ dnorm(mu, sigma), mu <- a + bX * x, x ~ dnorm(0, : There were 305 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

```r
precis(m14H3.2)
```

```
## Warning in precis(m14H3.2): There were 305 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

```
##           Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## x_impute  0.75   2.46      -3.10       3.79   204 1.01
## a         3.80   6.70      -6.02      14.89  3097 1.00
## bX        7.08  25.74     -33.20      35.91   222 1.01
## sigma    20.02   7.38       8.74      29.68  1230 1.00
```
####I think now it is not a strong positive relationship between two variables.The mean posterior of bX is 7.08, and the standared error is 25.74, almost half of the values are lower than 0. Because we defined a certain normal distribution for the missing value in x, it makes sampling bias. I think may be we need to specify the distribution of the missing value xi based on the relationships between x and y. 

