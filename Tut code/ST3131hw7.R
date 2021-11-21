####################
## ST3131 Tutorial 7
####################
 
####################################################################################
### Question 1, SENIC data

senic.dat = read.table("D://Rsession/SENIC.txt",header =TRUE)

y = senic.dat$y; z1=senic.dat$x2; z2=senic.dat$x8; z3=senic.dat$x10; z4=senic.dat$x3;  R=factor(senic.dat$x7)

q3.fit=lm(y~z1+z2+z3+z4+z1:z2+z1:z3+z1:z4+z2:z3+z2:z4+z3:z4);summary(q3.fit)


b=q3.fit$coef; b.i =b[6:11]
V=vcov(q3.fit); V.i=V[6:11,6:11] 
F=t(b.i)%*%solve(V.i)%*%b.i/6;
p.value = pf(F,6,102,lower.tail=FALSE)

c(F, p.value)

# Sheffe's and Bonferroni criterion:
  sqrt(6 * qf(0.95,6,102))
  qt(0.05/2/6,102,lower.tail=FALSE)

############################################################################################
### Question 3, Insurance example
insur.dat=read.table("D://Rsession/insurance.txt",header=TRUE)
full.fit = lm(log.Y.~X1+X2+X3+X4+X5+X6+X7+X8,data=insur.dat)
summary(full.fit)

##(i)
pi= 4*(4*atan(1/5) - atan(1/239))
AIC=54 *log (0.2093^2*45/54) + 2*10 + 54 *(log(2*pi) + 1) 
BIC=54 *log (0.2093^2*45/54) + log(54)*10 + 54 *(log(2*pi) + 1) 


sigma=summary(full.fit)$sigma
sigma2= 45/54*sigma^2

AIC2=54 *log (sigma2) + 2*10 + 54 *(log(2*pi) + 1) 
BIC2=54 *log (sigma2) + log(54)*10 + 54 *(log(2*pi) + 1) 


AIC(full.fit)
BIC(full.fit)

cbind(c(AIC, AIC2, AIC(full.fit)), c(BIC, BIC2, BIC(full.fit))) 

## (ii) 
library(boot)


glm.fit = glm(log.Y.~X1+X2+X3+X4+X5+X6+X7+X8,data=insur.dat)

# set.seed(12345)
 r=1
 CV=NULL
 while (r<=4) {
    CV.1.f = cv.glm(insur.dat,glm.fit)$delta[2]
    CV.k.f = cv.glm(insur.dat,glm.fit,K=11)$delta[2]
    CV=rbind(CV, c(CV.1.f, CV.k.f) )
    r=r+1
}

CV

