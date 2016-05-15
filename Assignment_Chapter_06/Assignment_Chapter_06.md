# Assignment _Chapter_06
XIAOYAN YUE  
##6E1 State the three motivating criteria that define information entropy. Try to express each in your own words.
(1)The variation of information entropy are continuous.

(2)The information entropy vary with the number of possible events.The more number of possible events, the larger of the information entropy should be.

(3)The information entropy of different possible events could be additive.

##6E2 Suppose a coin is weighted such that, when it is tossed and lands on a table, it comes up heads 70% of the time. What is the entropy of this coin?
calculate formula:

H(p) = - (0.3*log0.3)+(0.7*log0.7))

R calculation:

```r
p <- c( 0.3 , 0.7 )
-sum( p*log(p) )
```

```
## [1] 0.6108643
```

##6E3 Suppose a four-sided die is loaded such that, when tossed onto a table, it shows “1” 20%, “2” 25%, ”3” 25%, and ”4” 30% of the time. What is the entropy of this die?

```r
p <- c(0.20,0.25,0.25,0.30 )
-sum( p*log(p) )
```

```
## [1] 1.376227
```
##6E4 Suppose another four-sided die is loaded such that it never shows “4”. The other three sides show equally often. What is the entropy of this die?

```r
p <- c(1/3,1/3,1/3)
-sum( p*log(p) )
```

```
## [1] 1.098612
```
##6M1 Write down and compare the definitions of AIC, DIC, and WAIC. Which of these criteria is most general? Which assumptions are required to transform a more general criterion into a less general one?

They are three kinds of information criterions which provide estimates of the average out-of-sample deviance.

Amongst, **AIC** is the oldest and _**most restrictive**_, which is an approximation that is reliable based on three restrictive conditions: the priors are flat or overwhelmed by the likelihood, the posterior distribution is approximately multivariate Gaussian, and the sample size is much larger than the number of parameters. Comparatively, **DIC** is a more _**common**_ criterion which accommondates informative priors but still assumes that the posterior is multivariate Gaussian and the sample size is much larger than the number of parameters. While, **WAIC** is the _**most general**_ criterion, which do not make any assumption about the shape of the posterior.


##6M3 When comparing models with an information criterion, why must all models be fit to exactly the same observations? What would happen to the information criterion values, if the models were fit to different numbers of observations? Perform some experiments, if you are not sure.

##6M4 What happens to the effective number of parameters, as measured by DIC or WAIC, as a prior becomes more concentrated? Why? Perform some experiments, if you are not sure.

##6M5 Provide an informal explanation of why informative priors reduce overfitting.

##6M6 Provide an information explanation of why overly informative priors result in underfitting.
