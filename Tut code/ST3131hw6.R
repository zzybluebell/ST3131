####################
## ST3131 Tutorial 6
####################


##########################################################################################################
### Question 1: Clinical trial

## Create data frame
 y1=c(31.02, 24.03, 28.37, 26.57, 25.83, 25.87, 26.08, 24.89, 28.32, 29.98, 20.41, 20.31, 23.45)
 y2=c(35.71, 34.83, 35.13, 38.30, 33.12, 42.07, 38.69, 43.16, 33.11, 42.47, 40.45, 31.95, 38.69, 42.26)
 y3=c(25.85, 39.77, 32.88, 46.29, 33.07, 29.23, 31.39, 28.28, 22.43, 38.42)
 y4=c(49.47, 42.29, 48.60, 47.10, 33.63, 32.00, 49.31, 50.81)
 y = c(y1,y2,y3,y4)

 A=factor(rep(c("N","Y","N","Y"),c(13,14,10,8)))
 B=factor(rep(c("N","Y"),c(27,18)))
 AB=factor(rep(c(1,2,3,4), c(13,14,10,8)))

 q1.dat =data.frame(y,A,B,AB)

## Fitting of interaction model with order A, B
 AB.fit = lm(y~A*B,data=q1.dat); anova(AB.fit)

## Fitting of interaction model with order B, A
 BA.fit = lm(y~B*A,data=q1.dat); anova(BA.fit)

## Critical value controlling overall type I error rate at level 0.05
 qf(0.05/3,1,41,lower.tail=FALSE)

## p values of the tests
 F=c(0.044,54.797,16.75)
 3*pf(F,1,41,lower.tail=FALSE)

## Model treating combined levels as those of a single factor
 SF.fit=lm(y~AB,data=q1.dat); summary(SF.fit)

## Identified constrasts and their t-statistics
 C=matrix(c(1,0,0,
            0,1,0,
            0,0,1,
            1,-1,-1,
            -1,1,-1), ncol=3,byrow=T)
 b=SF.fit$coef[-1];V=vcov(SF.fit)[-1,-1]
 L = C%*%b
 sd.L = sqrt(diag(C%*%V%*%t(C))) 
 T=L/sd.L 
            
## Sheffe criterion
  sqrt(3*qf(0.95,3,41))

## Sheffe p values
   data.frame(t.value = T, p.value = 1-pf(T^2/3,3,41))
  
#########################################################################################
### Question 2, SMSA data revisited 

 SMSA.dat = read.table("D://Rsession/SMSA.txt",header=TRUE)
 attach(SMSA.dat)
 y=crimes; x1=T.p/Land; x2=income; x3=h.sch
 z=factor(region)

# (i) Fitting of main effect model 
  add.fit = lm(y~x1+x2+x3+z)
  summary(add.fit)

# (ii) Testing differences between regions 2 and 4, 1 and 4, 1 and 4.
  b = add.fit$coef[5:7]
  V = vcov(add.fit)[5:7,5:7]
  C = matrix(c(1,0,-1,
              0,1,0,
              0,0,1),ncol=3,byrow =T)
  L=C%*%b
  sd.L = sqrt(diag(C%*%V%*%t(C)))
  T=L/sd.L
  p=2*(1-pt(abs(T),134))
  data.frame(t.value = T, p.value = p)


# (iii) Interaction model
  int.fit = lm(y~x1+x2+x3*z)
  summary(int.fit)

# (iv) Testing for interaction
  b.i=int.fit$coef[8:10]
  V.i = vcov(int.fit)[8:10,8:10]

  F = t(b.i)%*%solve(V.i)%*%b.i/3 
  pf(F,3,131,lower.tail=FALSE)

 
####################################################################################
### Question 3, SENIC data

senic.dat = read.table("D://Rsession/SENIC.txt",header =TRUE)

y = senic.dat$y; z1=senic.dat$x2; z2=senic.dat$x8; z3=senic.dat$x10; R=factor(senic.dat$x7)

q3.fit=lm(y~z1+z2+z3+R+z1:R+z2:R+z3:R);summary(q3.fit)
b=q3.fit$coef; b1=b[8:10]; b2=b[11:13]; b3=b[14:16]
V=vcov(q3.fit); V1=V[8:10,8:10]; V2=V[11:13,11:13]; V3=V[14:16,14:16]
F1=t(b1)%*%solve(V1)%*%b1/3; F2=t(b2)%*%solve(V2)%*%b2/3; F3=t(b3)%*%solve(V3)%*%b3/3;
F=c(F1,F2,F3)

data.frame(Vriable=c("z1","z2","z3"), F.value = F, p.value = pf(F,3,97,lower.tail=FALSE))


# Sheffe's criterion:
  S_{0.05} = sqrt(3 * qf(0.95,3,97))


############################################################################################

