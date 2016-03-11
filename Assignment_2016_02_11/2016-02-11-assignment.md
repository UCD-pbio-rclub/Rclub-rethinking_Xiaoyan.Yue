# Statistical Rethinking Chapter 2, sections 2.1 - 2.3

Name: Xiaoyan Yue

## 2E1
(2) and (4)  **Pr(rain|Monday)and Pr(rain,Monday)/Pr(Monday)**

## 2E2
(3) **The probability that it is Monday, given that it is raining** 

## 2E3
(1) and (4) **Pr(Monday|rain) and Pr(rain|Monday)Pr(Monday)/Pr(rain)**

## 2E4
It means that the world is probably consist of 70% water and 30% land, but this can not be sure to be the reality.

## 2M3
Pr(Land)=(0.3+1)/(1+1)=0.65
**Pr(Earth|land)=Pr(Land|earth)x Pr(Earth)/Pr(Land)=0.3x0.5/0.65=0.23**

## 2M4

Pr(black,black)=Pr(black|black)*Pr(black)(the probability of two sides are black at the same time is equal to the probability that the other side is also black, when the one side is black, times the probability of a black side)

Pr(black|black)=Pr(black,black)/Pr(black)
Pr(black,black)=1/3（the probability of two sides are black at the same time）
Pr(black)=1/2(the probability of black sides)
So,Pr(black|black)=(1/3)/(1/2)=2/3

##2M1
>###(1)WWW
```
p_grid <- seq( from=0, to=1, length.out = 20)
prior<-rep(1,3)
likelihood <- dbinom(3, size=9, prob = p_grid)
unstd.posterior <- likelihood*prior
posterior<- unstd.posterior / sum(unstd.posterior)
plot(p_grid,posterior,type="b",
     xlab="probability of water", ylab = "posterior probability")
mtext("3 points")
```

![Plot result](https://drive.google.com/a/ucdavis.edu/file/d/0B0jaB6qWXt99QWhZd290TEpsUVU/view)

>(2)
```
p_grid <- seq( from=0, to=1, length.out = 4)
prior<-c(1,1,1,0)
likelihood <- dbinom(4, size=9, prob = p_grid)
unstd.posterior <- likelihood*prior
posterior<- unstd.posterior / sum(unstd.posterior)
plot(p_grid,posterior,type="b",
     xlab="probability of water", ylab = "posterior probability",
     ylim = c(0,1))
mtext("4 points")
```

![Plot result](https://drive.google.com/a/ucdavis.edu/file/d/0B0jaB6qWXt99UXdfUk1ncVlfQnM/view)

>###(3)LWWLWWW
```
p_grid <- seq( from=0, to=1, length.out = 7)
prior<-c(0,1,1,0,1,1,1)
likelihood <- dbinom(7, size=9, prob = p_grid)
unstd.posterior <- likelihood*prior
posterior<- unstd.posterior / sum(unstd.posterior)
plot(p_grid,posterior,type="b",
     xlab="probability of water", ylab = "posterior probability",
     ylim = c(0,1))
mtext("7 points")
```

![Plot result](https://drive.google.com/a/ucdavis.edu/file/d/0B0jaB6qWXt99S3lEekFScm1sS3M/view)
##2M2
>(1)
```
p_grid <- seq( from=0, to=1, length.out = 3)
prior<-ifelse(p_grid<0.5, 0, 1)
likelihood <- dbinom(3, size=9, prob = p_grid)
unstd.posterior <- likelihood*prior
posterior<- unstd.posterior / sum(unstd.posterior)
png("Homework_2M2_1.png")
plot(p_grid,posterior,type="b",
     xlab="probability of water", ylab = "posterior probability",
     ylim = c(0,1))
mtext("3 points")
dev.off()
```

>(2)
```
p_grid <- seq( from=0, to=1, length.out = 4)
prior<-ifelse(p_grid<0.5, 0, 1)
likelihood <- dbinom(4, size=9, prob = p_grid)
unstd.posterior <- likelihood*prior
posterior<- unstd.posterior / sum(unstd.posterior)
png("Homework_2M2_2.png")
plot(p_grid,posterior,type="b",
     xlab="probability of water", ylab = "posterior probability",
     ylim = c(0,1))
mtext("4 points")
dev.off()
```

>(3)
```
p_grid <- seq( from=0, to=1, length.out = 7)
prior<-ifelse(p_grid<0.5, 0, 1)
likelihood <- dbinom(7, size=9, prob = p_grid)
unstd.posterior <- likelihood*prior
posterior<- unstd.posterior / sum(unstd.posterior)
png("Homework_2M2_3.png")
plot(p_grid,posterior,type="b",
     xlab="probability of water", ylab = "posterior probability",
     ylim = c(0,1))
mtext("7 points")
dev.off()
```

##2M5
Pr(B|B)=Pr(B,B)/Pr(B)

Pr(B,B)=2/4=1/2

Pr(B)=5/8

Pr(B|B)=4/5
##2M6

##2M7




