# Chapter_14
Xiaoyan Yue  
2/22/2017  



##14E1  
Ti ~ Poisson(mu)  
log(mu)= a + b*log(P_est)  
log(P_obs) ~ Normal (log(P_est),P_SD)  
a ~ Normal(0,10)  
b ~ Normal(0,1)


##14M3
###First, repeat model in the chapter

```r
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

dlist <- list(
  div_obs=d$Divorce,
  div_sd=d$Divorce.SE,
  R=d$Marriage,
  A=d$MedianAgeMarriage
)

m14.1 <- map2stan(
  alist(
    div_est ~ dnorm(mu,sigma),
    mu <- a + bA*A + bR*R,
    div_obs ~ dnorm(div_est,div_sd),
    a ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2.5)
  ) ,
  data=dlist ,
  start=list(div_est=dlist$div_obs) ,
  WAIC=FALSE , iter=5000 , warmup=1000 , chains=2 , cores=2 ,
  control=list(adapt_delta=0.95) )
```

```
## In file included from file1be54052fcac.cpp:8:
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
## SAMPLING FOR MODEL 'div_est ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 2e-06 seconds (Warm-up)
##                4.4e-05 seconds (Sampling)
##                4.6e-05 seconds (Total)
```

```r
precis(m14.1,depth = 2)
```

```
##              Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## div_est[1]  11.78   0.68      10.69      12.85  8000    1
## div_est[2]  11.20   1.04       9.56      12.86  8000    1
## div_est[3]  10.47   0.61       9.46      11.41  8000    1
## div_est[4]  12.32   0.88      10.86      13.70  8000    1
## div_est[5]   8.05   0.23       7.68       8.42  8000    1
## div_est[6]  11.02   0.74       9.87      12.22  8000    1
## div_est[7]   7.24   0.64       6.28       8.34  8000    1
## div_est[8]   9.36   0.91       7.95      10.86  8000    1
## div_est[9]   7.02   1.11       5.21       8.76  8000    1
## div_est[10]  8.54   0.31       8.04       9.04  8000    1
## div_est[11] 11.15   0.52      10.32      11.97  8000    1
## div_est[12]  9.09   0.89       7.68      10.51  8000    1
## div_est[13]  9.68   0.90       8.28      11.13  8000    1
## div_est[14]  8.10   0.42       7.45       8.79  8000    1
## div_est[15] 10.68   0.55       9.77      11.55  8000    1
## div_est[16] 10.17   0.71       9.05      11.33  8000    1
## div_est[17] 10.50   0.80       9.25      11.81  8000    1
## div_est[18] 11.95   0.65      10.91      12.96  8000    1
## div_est[19] 10.48   0.71       9.31      11.58  8000    1
## div_est[20] 10.18   1.02       8.59      11.84  8000    1
## div_est[21]  8.76   0.59       7.83       9.70  8000    1
## div_est[22]  7.77   0.48       7.00       8.55  8000    1
## div_est[23]  9.14   0.47       8.40       9.93  8000    1
## div_est[24]  7.74   0.53       6.94       8.64  8000    1
## div_est[25] 10.44   0.78       9.16      11.68  8000    1
## div_est[26]  9.54   0.59       8.60      10.47  8000    1
## div_est[27]  9.43   0.97       7.93      11.02  8000    1
## div_est[28]  9.25   0.72       8.14      10.38  8000    1
## div_est[29]  9.18   0.94       7.71      10.69  8000    1
## div_est[30]  6.38   0.43       5.70       7.08  8000    1
## div_est[31]  9.97   0.79       8.76      11.25  8000    1
## div_est[32]  6.69   0.31       6.21       7.19  8000    1
## div_est[33]  9.89   0.44       9.20      10.57  8000    1
## div_est[34]  9.76   0.96       8.21      11.29  8000    1
## div_est[35]  9.44   0.42       8.77      10.10  8000    1
## div_est[36] 11.97   0.76      10.82      13.23  8000    1
## div_est[37] 10.07   0.67       8.96      11.10  8000    1
## div_est[38]  7.79   0.40       7.17       8.43  8000    1
## div_est[39]  8.21   1.00       6.62       9.79  8000    1
## div_est[40]  8.40   0.60       7.48       9.37  8000    1
## div_est[41] 10.01   1.06       8.38      11.73  8000    1
## div_est[42] 10.93   0.63       9.94      11.97  8000    1
## div_est[43] 10.02   0.33       9.50      10.54  8000    1
## div_est[44] 11.07   0.80       9.74      12.30  8000    1
## div_est[45]  8.87   0.98       7.32      10.40  8000    1
## div_est[46]  8.99   0.48       8.23       9.74  8000    1
## div_est[47]  9.94   0.56       9.05      10.86  8000    1
## div_est[48] 10.62   0.88       9.22      12.00  8000    1
## div_est[49]  8.46   0.51       7.67       9.29  8000    1
## div_est[50] 11.51   1.10       9.81      13.30  8000    1
## a           21.25   6.70      10.64      32.02  2839    1
## bA          -0.55   0.22      -0.91      -0.22  2959    1
## bR           0.13   0.08       0.01       0.25  3326    1
## sigma        1.13   0.20       0.82       1.46  2822    1
```

###Second, double the standard error

```r
data(WaffleDivorce)
d <- WaffleDivorce

d$Divorce.SE_double <- 2*d$Divorce.SE
head(d)
```

```
##     Location Loc Population MedianAgeMarriage Marriage Marriage.SE Divorce
## 1    Alabama  AL       4.78              25.3     20.2        1.27    12.7
## 2     Alaska  AK       0.71              25.2     26.0        2.93    12.5
## 3    Arizona  AZ       6.33              25.8     20.3        0.98    10.8
## 4   Arkansas  AR       2.92              24.3     26.4        1.70    13.5
## 5 California  CA      37.25              26.8     19.1        0.39     8.0
## 6   Colorado  CO       5.03              25.7     23.5        1.24    11.6
##   Divorce.SE WaffleHouses South Slaves1860 Population1860 PropSlaves1860
## 1       0.79          128     1     435080         964201           0.45
## 2       2.05            0     0          0              0           0.00
## 3       0.74           18     0          0              0           0.00
## 4       1.22           41     1     111115         435450           0.26
## 5       0.24            0     0          0         379994           0.00
## 6       0.94           11     0          0          34277           0.00
##   Divorce.SE_double
## 1              1.58
## 2              4.10
## 3              1.48
## 4              2.44
## 5              0.48
## 6              1.88
```

```r
dlist <- list(
  div_obs=d$Divorce,
  div_sd=d$Divorce.SE_double,
  R=d$Marriage,
  A=d$MedianAgeMarriage
)

m14.M3 <- map2stan(
  alist(
    div_est ~ dnorm(mu,sigma),
    mu <- a + bA*A + bR*R,
    div_obs ~ dnorm(div_est,div_sd),
    a ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2.5)
  ) ,
  data=dlist ,
  start=list(div_est=dlist$div_obs) ,
  WAIC=FALSE , iter=5000 , warmup=1000 , chains=2 , cores=2,
  control=list(adapt_delta=0.95))
```

```
## In file included from file28f231c5b63.cpp:8:
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
```

```
## Warning: There were 306 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
## http://mc-stan.org/misc/warnings.html#bfmi-low
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## 
## SAMPLING FOR MODEL 'div_est ~ dnorm(mu, sigma)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 3e-06 seconds (Warm-up)
##                5e-05 seconds (Sampling)
##                5.3e-05 seconds (Total)
```

```
## Warning in map2stan(alist(div_est ~ dnorm(mu, sigma), mu <- a + bA * A + : There were 306 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

```r
precis(m14.M3,depth = 2)
```

```
## Warning in precis(m14.M3, depth = 2): There were 306 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

```
##              Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## div_est[1]  10.07   0.50       9.27      10.81   869 1.00
## div_est[2]  11.21   0.70      10.14      12.34   768 1.00
## div_est[3]   9.70   0.46       9.01      10.44  1228 1.00
## div_est[4]  11.84   0.70      10.73      12.94   770 1.00
## div_est[5]   8.50   0.35       7.97       9.08  1130 1.01
## div_est[6]  10.43   0.57       9.57      11.36  1021 1.00
## div_est[7]   7.83   0.48       7.04       8.53  2455 1.00
## div_est[8]   9.76   0.63       8.78      10.76  1087 1.00
## div_est[9]   6.86   0.78       5.63       8.08  1016 1.00
## div_est[10]  8.54   0.38       7.91       9.12  1875 1.00
## div_est[11] 10.12   0.51       9.35      10.92   966 1.00
## div_est[12]  9.96   0.76       8.77      11.20   856 1.00
## div_est[13] 12.10   0.75      10.93      13.28   618 1.00
## div_est[14]  8.35   0.41       7.67       8.96  2891 1.00
## div_est[15]  9.71   0.47       8.98      10.44  1041 1.00
## div_est[16] 10.11   0.50       9.32      10.86  1464 1.00
## div_est[17] 10.46   0.52       9.68      11.26  1106 1.00
## div_est[18] 10.74   0.53       9.93      11.57   763 1.00
## div_est[19]  9.71   0.48       9.00      10.47  1336 1.00
## div_est[20]  7.91   0.68       6.82       8.95   911 1.00
## div_est[21]  8.38   0.47       7.66       9.14  1890 1.00
## div_est[22]  7.20   0.53       6.35       8.00   963 1.00
## div_est[23]  8.55   0.47       7.83       9.31  1270 1.00
## div_est[24]  8.16   0.53       7.34       8.98  1320 1.00
## div_est[25]  9.47   0.48       8.77      10.22  1444 1.00
## div_est[26]  9.37   0.47       8.67      10.11  1791 1.00
## div_est[27]  9.28   0.50       8.54      10.09  1956 1.00
## div_est[28]  9.65   0.50       8.87      10.40  1800 1.00
## div_est[29]  8.31   0.51       7.54       9.08  1812 1.00
## div_est[30]  7.15   0.49       6.40       7.92  1347 1.00
## div_est[31]  9.67   0.48       8.90      10.37  1733 1.00
## div_est[32]  7.16   0.43       6.49       7.86  1440 1.00
## div_est[33]  9.73   0.42       9.08      10.38  1475 1.00
## div_est[34] 11.22   0.75      10.07      12.49   749 1.00
## div_est[35]  8.74   0.46       8.01       9.45  1085 1.00
## div_est[36] 11.24   0.59      10.28      12.13   900 1.00
## div_est[37]  9.27   0.47       8.48       9.92  1587 1.00
## div_est[38]  7.81   0.45       7.11       8.51  1742 1.00
## div_est[39]  7.15   0.58       6.21       7.98  1177 1.00
## div_est[40]  8.74   0.45       8.05       9.42  2775 1.00
## div_est[41]  9.69   0.49       8.96      10.45  1652 1.00
## div_est[42]  9.88   0.50       9.05      10.60  1225 1.00
## div_est[43] 10.17   0.40       9.54      10.81  1212 1.00
## div_est[44] 12.91   0.87      11.60      14.42   570 1.00
## div_est[45]  8.16   0.52       7.38       8.95  1941 1.00
## div_est[46]  9.27   0.43       8.62       9.96  1774 1.00
## div_est[47]  9.82   0.46       9.10      10.53  1411 1.00
## div_est[48] 10.49   0.53       9.68      11.31  1164 1.00
## div_est[49]  8.62   0.45       7.96       9.34  2023 1.00
## div_est[50] 12.75   0.98      11.03      14.24   653 1.00
## a           19.62   6.67       9.17      30.38   723 1.00
## bA          -0.56   0.21      -0.88      -0.22   735 1.00
## bR           0.22   0.09       0.07       0.36   629 1.00
## sigma        0.39   0.21       0.11       0.66   223 1.01
```
  
###There is no many differences in the posterior of bA and bR between two models, but increasing the standard error of the outcome do decrease the posterior of the sigma in the model.


##14H1  
###Load the data

```r
data("elephants")
d1 <- elephants
#look at the data
head(d1)
```

```
##   AGE MATINGS
## 1  27       0
## 2  28       1
## 3  28       1
## 4  28       1
## 5  28       3
## 6  29       0
```

```r
#log the AGE
d1$log_age <- log(d1$AGE)
str(d1)
```

```
## 'data.frame':	41 obs. of  3 variables:
##  $ AGE    : int  27 28 28 28 28 29 29 29 29 29 ...
##  $ MATINGS: int  0 1 1 1 3 0 0 0 2 2 ...
##  $ log_age: num  3.3 3.33 3.33 3.33 3.33 ...
```

```r
summary(d1)
```

```
##       AGE           MATINGS         log_age     
##  Min.   :27.00   Min.   :0.000   Min.   :3.296  
##  1st Qu.:29.00   1st Qu.:1.000   1st Qu.:3.367  
##  Median :34.00   Median :2.000   Median :3.526  
##  Mean   :35.85   Mean   :2.683   Mean   :3.564  
##  3rd Qu.:42.00   3rd Qu.:3.000   3rd Qu.:3.738  
##  Max.   :52.00   Max.   :9.000   Max.   :3.951
```
  
###Fit a poisson model

```r
m14.H1 <- map2stan(
  alist(
    MATINGS ~ dpois( lambda ) ,
    log(lambda) <- a + bA*log_age,
    a ~ dnorm(0,10),
    bA ~ dnorm(0,1)),
  data=d1 , iter=3000 , warmup=1000 , chains=4 )
```

```
## In file included from file28f13fbc8ad.cpp:8:
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
## SAMPLING FOR MODEL 'MATINGS ~ dpois(lambda)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 3000 [  0%]  (Warmup)
## Chain 1, Iteration:  300 / 3000 [ 10%]  (Warmup)
## Chain 1, Iteration:  600 / 3000 [ 20%]  (Warmup)
## Chain 1, Iteration:  900 / 3000 [ 30%]  (Warmup)
## Chain 1, Iteration: 1001 / 3000 [ 33%]  (Sampling)
## Chain 1, Iteration: 1300 / 3000 [ 43%]  (Sampling)
## Chain 1, Iteration: 1600 / 3000 [ 53%]  (Sampling)
## Chain 1, Iteration: 1900 / 3000 [ 63%]  (Sampling)
## Chain 1, Iteration: 2200 / 3000 [ 73%]  (Sampling)
## Chain 1, Iteration: 2500 / 3000 [ 83%]  (Sampling)
## Chain 1, Iteration: 2800 / 3000 [ 93%]  (Sampling)
## Chain 1, Iteration: 3000 / 3000 [100%]  (Sampling)
##  Elapsed Time: 0.120979 seconds (Warm-up)
##                0.270044 seconds (Sampling)
##                0.391023 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'MATINGS ~ dpois(lambda)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 3000 [  0%]  (Warmup)
## Chain 2, Iteration:  300 / 3000 [ 10%]  (Warmup)
## Chain 2, Iteration:  600 / 3000 [ 20%]  (Warmup)
## Chain 2, Iteration:  900 / 3000 [ 30%]  (Warmup)
## Chain 2, Iteration: 1001 / 3000 [ 33%]  (Sampling)
## Chain 2, Iteration: 1300 / 3000 [ 43%]  (Sampling)
## Chain 2, Iteration: 1600 / 3000 [ 53%]  (Sampling)
## Chain 2, Iteration: 1900 / 3000 [ 63%]  (Sampling)
## Chain 2, Iteration: 2200 / 3000 [ 73%]  (Sampling)
## Chain 2, Iteration: 2500 / 3000 [ 83%]  (Sampling)
## Chain 2, Iteration: 2800 / 3000 [ 93%]  (Sampling)
## Chain 2, Iteration: 3000 / 3000 [100%]  (Sampling)
##  Elapsed Time: 0.13097 seconds (Warm-up)
##                0.249156 seconds (Sampling)
##                0.380126 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'MATINGS ~ dpois(lambda)' NOW (CHAIN 3).
## 
## Chain 3, Iteration:    1 / 3000 [  0%]  (Warmup)
## Chain 3, Iteration:  300 / 3000 [ 10%]  (Warmup)
## Chain 3, Iteration:  600 / 3000 [ 20%]  (Warmup)
## Chain 3, Iteration:  900 / 3000 [ 30%]  (Warmup)
## Chain 3, Iteration: 1001 / 3000 [ 33%]  (Sampling)
## Chain 3, Iteration: 1300 / 3000 [ 43%]  (Sampling)
## Chain 3, Iteration: 1600 / 3000 [ 53%]  (Sampling)
## Chain 3, Iteration: 1900 / 3000 [ 63%]  (Sampling)
## Chain 3, Iteration: 2200 / 3000 [ 73%]  (Sampling)
## Chain 3, Iteration: 2500 / 3000 [ 83%]  (Sampling)
## Chain 3, Iteration: 2800 / 3000 [ 93%]  (Sampling)
## Chain 3, Iteration: 3000 / 3000 [100%]  (Sampling)
##  Elapsed Time: 0.124967 seconds (Warm-up)
##                0.255481 seconds (Sampling)
##                0.380448 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'MATINGS ~ dpois(lambda)' NOW (CHAIN 4).
## 
## Chain 4, Iteration:    1 / 3000 [  0%]  (Warmup)
## Chain 4, Iteration:  300 / 3000 [ 10%]  (Warmup)
## Chain 4, Iteration:  600 / 3000 [ 20%]  (Warmup)
## Chain 4, Iteration:  900 / 3000 [ 30%]  (Warmup)
## Chain 4, Iteration: 1001 / 3000 [ 33%]  (Sampling)
## Chain 4, Iteration: 1300 / 3000 [ 43%]  (Sampling)
## Chain 4, Iteration: 1600 / 3000 [ 53%]  (Sampling)
## Chain 4, Iteration: 1900 / 3000 [ 63%]  (Sampling)
## Chain 4, Iteration: 2200 / 3000 [ 73%]  (Sampling)
## Chain 4, Iteration: 2500 / 3000 [ 83%]  (Sampling)
## Chain 4, Iteration: 2800 / 3000 [ 93%]  (Sampling)
## Chain 4, Iteration: 3000 / 3000 [100%]  (Sampling)
##  Elapsed Time: 0.125776 seconds (Warm-up)
##                0.26254 seconds (Sampling)
##                0.388316 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'MATINGS ~ dpois(lambda)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 3e-06 seconds (Warm-up)
##                2.6e-05 seconds (Sampling)
##                2.9e-05 seconds (Total)
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

```r
precis(m14.H1,depth=2)
```

```
##     Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a  -6.25   1.71      -8.97      -3.55  1239    1
## bA  2.01   0.47       1.25       2.74  1239    1
```
###Fit a model with measurement errors with a standard error of +/- 5 years

```r
dlist <- list(
  MATINGS = d1$MATINGS,
  AGE_obs=d1$log_age,
  AGE_sd=rep(log(5),length(d1$log_age))
)

m14.H1.sd <- map2stan(
  alist(
    MATINGS ~ dpois( lambda ) ,
    log(lambda) <- a + bA*AGE_est[i],
    AGE_obs ~ dnorm(AGE_est,AGE_sd),
    a ~ dnorm(0,10),
    bA ~ dnorm(0,1)),
  data=dlist ,
  start=list(AGE_est=dlist$AGE_obs) ,
  WAIC=FALSE , iter=5000 , warmup=1000 , chains=2 , cores=2,
  control=list(adapt_delta=0.95))
```

```
## In file included from file28f1c876a7b.cpp:8:
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
## SAMPLING FOR MODEL 'MATINGS ~ dpois(lambda)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 3e-06 seconds (Warm-up)
##                5.5e-05 seconds (Sampling)
##                5.8e-05 seconds (Total)
```

```r
precis(m14.H1.sd,depth = 2)
```

```
##              Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## AGE_est[1]   2.06   1.39      -0.06       4.32  8000 1.00
## AGE_est[2]   2.65   1.33       0.45       4.69  8000 1.00
## AGE_est[3]   2.66   1.32       0.64       4.83  8000 1.00
## AGE_est[4]   2.65   1.32       0.60       4.79  8000 1.00
## AGE_est[5]   3.67   1.21       1.79       5.66  8000 1.00
## AGE_est[6]   2.12   1.40      -0.11       4.29  8000 1.00
## AGE_est[7]   2.09   1.40      -0.11       4.26  8000 1.00
## AGE_est[8]   2.12   1.40      -0.17       4.25  8000 1.00
## AGE_est[9]   3.20   1.24       1.14       5.11  8000 1.00
## AGE_est[10]  3.20   1.24       1.28       5.20  8000 1.00
## AGE_est[11]  3.21   1.25       1.28       5.26  8000 1.00
## AGE_est[12]  2.73   1.32       0.57       4.77  8000 1.00
## AGE_est[13]  3.25   1.24       1.26       5.21  8000 1.00
## AGE_est[14]  4.20   1.21       2.33       6.15  8000 1.00
## AGE_est[15]  3.77   1.21       1.88       5.71  8000 1.00
## AGE_est[16]  3.77   1.21       1.91       5.71  8000 1.00
## AGE_est[17]  3.76   1.21       1.79       5.66  8000 1.00
## AGE_est[18]  3.28   1.28       1.22       5.25  8000 1.00
## AGE_est[19]  2.77   1.31       0.55       4.71  8000 1.00
## AGE_est[20]  2.78   1.32       0.66       4.83  8000 1.00
## AGE_est[21]  3.28   1.28       1.26       5.28  8000 1.00
## AGE_est[22]  3.76   1.25       1.73       5.70  8000 1.00
## AGE_est[23]  4.67   1.19       2.76       6.59  8000 1.00
## AGE_est[24]  5.06   1.18       3.25       6.92  8000 1.00
## AGE_est[25]  2.84   1.29       0.85       5.01  8000 1.00
## AGE_est[26]  2.84   1.33       0.81       5.04  8000 1.00
## AGE_est[27]  5.05   1.19       3.29       7.00  8000 1.00
## AGE_est[28]  3.38   1.26       1.39       5.39  8000 1.00
## AGE_est[29]  2.88   1.30       0.76       4.88  8000 1.00
## AGE_est[30]  3.90   1.23       2.08       6.00  8000 1.00
## AGE_est[31]  4.36   1.19       2.54       6.26  8000 1.00
## AGE_est[32]  2.38   1.37       0.13       4.43  8000 1.00
## AGE_est[33]  3.43   1.25       1.51       5.49  8000 1.00
## AGE_est[34]  3.92   1.23       2.03       5.94  8000 1.00
## AGE_est[35]  4.33   1.18       2.40       6.14  8000 1.00
## AGE_est[36]  6.11   1.26       4.58       8.08   261 1.01
## AGE_est[37]  3.93   1.22       1.88       5.76  8000 1.00
## AGE_est[38]  4.77   1.18       2.94       6.61  8000 1.00
## AGE_est[39]  5.50   1.19       3.85       7.41  8000 1.00
## AGE_est[40]  3.50   1.26       1.50       5.49  8000 1.00
## AGE_est[41]  6.18   1.23       4.61       8.00   252 1.01
## a           -0.38   0.53      -1.08       0.27   200 1.01
## bA           0.34   0.13       0.20       0.50   158 1.01
```
  
###After including the standard errors, the positive relationship between age and matting are going to disappear. The posterior mean of b changed from 2.02 to 0.32.  

  
##14H2
###Increase the sd value to 100, make the posterior mean to reach zero 

```r
dlist <- list(
  MATINGS = d1$MATINGS,
  AGE_obs=d1$log_age,
  AGE_sd=rep(log(100),length(d1$log_age))
)

m14.H1.sd.increase <- map2stan(
  alist(
    MATINGS ~ dpois( lambda ) ,
    log(lambda) <- a + bA*AGE_est[i],
    AGE_obs ~ dnorm(AGE_est,AGE_sd),
    a ~ dnorm(0,10),
    bA ~ dnorm(0,1)),
  data=dlist ,
  start=list(AGE_est=dlist$AGE_obs) ,
  WAIC=FALSE , iter=5000 , warmup=1000 , chains=2 , cores=2,
  control=list(adapt_delta=0.95))
```

```
## In file included from file916135077ed.cpp:8:
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
## SAMPLING FOR MODEL 'MATINGS ~ dpois(lambda)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 3e-06 seconds (Warm-up)
##                6e-05 seconds (Sampling)
##                6.3e-05 seconds (Total)
```

```r
precis(m14.H1.sd.increase,depth = 2)
```

```
##             Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## AGE_est[1]  2.00   5.34      -6.48      10.54    22 1.05
## AGE_est[2]  2.62   4.23      -3.97       9.53  8000 1.02
## AGE_est[3]  2.62   4.25      -4.12       9.50  8000 1.02
## AGE_est[4]  2.66   4.31      -4.01       9.69    41 1.02
## AGE_est[5]  3.66   3.48      -1.89       9.12  8000 1.00
## AGE_est[6]  2.08   5.27      -6.44      10.29    22 1.04
## AGE_est[7]  2.05   5.27      -6.39      10.43    21 1.05
## AGE_est[8]  2.02   5.39      -6.71      10.50    22 1.04
## AGE_est[9]  3.25   3.59      -2.26       9.01  8000 1.00
## AGE_est[10] 3.21   3.66      -2.76       8.89  8000 1.00
## AGE_est[11] 3.18   3.65      -2.42       9.13  8000 1.00
## AGE_est[12] 2.69   4.28      -4.12       9.45    43 1.02
## AGE_est[13] 3.28   3.65      -2.75       8.91  8000 1.00
## AGE_est[14] 4.24   3.86      -1.94      10.28    47 1.02
## AGE_est[15] 3.78   3.63      -2.20       9.41  8000 1.00
## AGE_est[16] 3.77   3.47      -1.66       9.30  8000 1.00
## AGE_est[17] 3.77   3.54      -1.78       9.48  8000 1.00
## AGE_est[18] 3.34   3.64      -2.72       8.68  8000 1.00
## AGE_est[19] 2.81   4.28      -4.47       9.23    40 1.02
## AGE_est[20] 2.76   4.32      -3.91       9.82    40 1.02
## AGE_est[21] 3.24   3.64      -2.55       9.02  8000 1.00
## AGE_est[22] 3.78   3.48      -1.86       9.22  8000 1.00
## AGE_est[23] 4.69   4.40      -2.71      11.08    23 1.04
## AGE_est[24] 5.07   5.16      -3.37      12.36    17 1.06
## AGE_est[25] 2.84   4.27      -4.45       9.26    42 1.02
## AGE_est[26] 2.85   4.26      -3.92       9.69    45 1.02
## AGE_est[27] 5.07   5.12      -3.33      12.53    17 1.05
## AGE_est[28] 3.36   3.65      -2.34       9.32  8000 1.00
## AGE_est[29] 2.84   4.32      -3.71      10.05  8000 1.02
## AGE_est[30] 3.90   3.53      -1.78       9.40  8000 1.00
## AGE_est[31] 4.33   3.84      -1.98      10.25    42 1.02
## AGE_est[32] 2.37   5.24      -5.95      10.81    21 1.04
## AGE_est[33] 3.34   3.66      -2.50       9.09  8000 1.01
## AGE_est[34] 3.91   3.48      -1.48       9.43  8000 1.00
## AGE_est[35] 4.37   3.90      -2.26      10.19    48 1.02
## AGE_est[36] 6.12   7.38      -6.17      14.99    13 1.08
## AGE_est[37] 3.95   3.52      -2.02       9.21  8000 1.00
## AGE_est[38] 4.83   4.38      -2.50      11.23    23 1.04
## AGE_est[39] 5.54   5.90      -4.20      13.45    15 1.07
## AGE_est[40] 3.55   3.65      -2.00       9.60  8000 1.00
## AGE_est[41] 6.22   7.42      -5.79      15.42    13 1.08
## a           0.68   0.46       0.08       1.43    13 1.07
## bA          0.04   0.12      -0.15       0.17    12 1.09
```
  
###It is going to reache zero, when the standard error gets to 100.
