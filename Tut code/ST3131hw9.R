###########################
## ST3131 Tutorial 9
###########################

#######################################################
## Question 1: SMSA data: number of physicians as response

  SMSA.dat = read.table("C://Rsession/SMSA.txt",header=TRUE)
  attach(SMSA.dat)

  y=doctor; x1=Land; x2=T.p; x3=P.city; x4=p.65; x5=beds; x6=h.sch; 
  x7=labor; x8=income; x9=crimes; x10=factor(region);
  p.dat=data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)


# (i) selecting models
  library(MASS)
  null1 = lm(y~1, data=p.dat); null2 = lm(y~x10, data=p.dat)

  f1=stepAIC(null1,scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10,lower=~1),direction="forward")
  summary(f1)
 
  f2=stepAIC(null2,scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10,
                lower=~x10),direction="forward")
  summary(f2)

  AIC(f1); AIC(f2)
## f1, which has the smaller AIC, is the selected model. The select model: y.p~x1 + x2 + x5 + x8 + x9 + x10.


# (ii) Diagnostics

################## Round 1 
# compute raw materials
  fit1=glm(y~x1 + x2 + x5 + x8 + x9 + x10,data=p.dat)
  yhat = fit1$fitted.values
  r =	residuals(fit1,type="pearson")
  h =	hatvalues(fit1,type="diagonal")
	infl = influence(fit1, do.coef = FALSE)
  rsta = rstandard(fit1, infl, type = "pearson")
  rstu = rstudent(fit1, infl, type = "pearson")
  cook =cooks.distance(fit1, infl,res = infl$pear.res, 
	        dispersion = summary(fit1)$dispersion,hat = infl$hat)
# Check for linearity
  par(mfrow=c(2,3))
  plot(x1, r,xlab="Residual vs. x1")
  plot(x2, r,xlab="Residual vs. x2")
  plot(x5, r,xlab="Residual vs. x5")
  plot(x8, r,xlab="Residual vs. x8")
  plot(x9, r,xlab="Residual vs. x9")
  plot(yhat,r,xlab="Residual vs. fitted value")


# Check for homogeneity and normality
  par(mfrow=c(1,2))
  plot(yhat, r,xlab="Residual vs. fitted values")
  qqnorm(rsta,xlab="Q-Q plot of standardized residuals")

# Check for outliers

  par(mfrow=c(2,2))
  qqnorm(h,xlab="",ylab="",main="Q-Q  plot of the hat values") # checking leverage
  qqnorm(rstu,xlab="",ylab="",main="Q-Q  plot of the studentized deletion residuals") # checking consistency
  qqnorm(cook,xlab="",ylab="",main="Q-Q  plot of the Cook's distances") # checking influence

  n = length(cook)
  cbind( order(cook), cook[order(cook)], order(rstu), rstu[order(rstu)],order(h),h[order(h)] )[(n-5):n,]

 # test the significance of observation 1 
  u=rep(0,n); u[1]=1; 
  f.outlier = lm(y~x1 + x2 + x5 + x8 + x9 + x10+u,data=p.dat)
  summary(f.outlier)

#################### Round 2

## Refit by removing observation 1:
   p.dat2=p.dat[-1, ]
   fit2=glm(y~x1 + x2 + x5 + x8 + x9 + x10,data=p.dat2)

   h = hatvalues(fit2,type="diagonal")
	infl = influence(fit2, do.coef = FALSE)
  rsta = rstandard(fit2, infl, type = "pearson")
  rstu = rstudent(fit2, infl, type = "pearson")
  cook2 =cooks.distance(fit2, infl,res = infl$pear.res, 
	        dispersion = summary(fit2)$dispersion, hat = infl$hat)

  par(mfrow=c(2,2))
  qqnorm(h,xlab="",ylab="",main="Q-Q  plot of the hat values") # checking leverage
  qqnorm(rstu,xlab="",ylab="",main="Q-Q  plot of the studentized deletion residuals") # checking consistency
  qqnorm(cook2,xlab="",ylab="",main="Q-Q  plot of the Cook's distances") # checking influence

  n = length(cook2)
  cbind(cook2,h)
  cbind( order(cook2), cook2[order(cook2)], order(h), h[order(h)])[(n-2):n,]

# test significance of obserbation 4 (with new index 3 in p.dat2 )
  u=rep(0,n); u[3]=1; 
  f.outlier = lm(y~x1 + x2 + x5 + x8 + x9 + x10+u,data=p.dat2)
  summary(f.outlier)

#################### Round 3

# Re-fit removing obs 1 and 4
   p.dat3=p.dat[-c(1,4),]
   fit3=glm(y~x1 + x2 + x5 + x8 + x9 + x10,data=p.dat3)

   h = hatvalues(fit3,type="diagonal")
	infl = influence(fit3, do.coef = FALSE)
  rsta = rstandard(fit3, infl, type = "pearson")
  rstu = rstudent(fit3, infl, type = "pearson")
  cook3 =cooks.distance(fit3, infl,res = infl$pear.res, 
	        dispersion = summary(fit3)$dispersion, hat = infl$hat)

  par(mfrow=c(2,2))
  qqnorm(h,xlab="",ylab="",main="Q-Q  plot of the leverage values")
  qqnorm(rstu,xlab="",ylab="",main="Q-Q  plot of the studentized deletion residuals")
  qqnorm(cook3,xlab="",ylab="",main="Q-Q  plot of the Cook's distances")

  n = length(cook3)
  cbind( order(cook3), cook3[order(cook3)], order(h), h[order(h)])[(n-2):n,]

# test significance of obserbation 5 (with new index 3 in p.dat3 )
  u=rep(0,n); u[3]=1; 
  f.outlier = lm(y~x1 + x2 + x5 + x8 + x9 + x10+u,data=p.dat3)
  summary(f.outlier)

#################### Round 4

# Re-fit removing obs 1, 4, 5.
   p.dat4=p.dat[-c(1,4,5),]
   fit4=glm(y~x1 + x2 + x5 + x8 + x9 + x10,data=p.dat4)

    h = hatvalues(fit4,type="diagonal")
	infl = influence(fit4, do.coef = FALSE)
  rsta = rstandard(fit4, infl, type = "pearson")
  rstu = rstudent(fit4, infl, type = "pearson")
  cook4 =cooks.distance(fit4, infl,res = infl$pear.res, 
	        dispersion = summary(fit4)$dispersion, hat = infl$hat)

  par(mfrow=c(2,2))
  qqnorm(h,xlab="",ylab="",main="Q-Q  plot of the leverage values")
  qqnorm(rstu,xlab="",ylab="",main="Q-Q  plot of the studentized deletion residuals")
  qqnorm(cook4,xlab="",ylab="",main="Q-Q  plot of the Cook's distances")

 n = length(cook4)
  cbind( order(cook4), cook4[order(cook4)], order(h), h[order(h)])[(n-2):n,]

# test significance of obserbation 3 (with new index 2 in p.dat4 )
  u=rep(0,n); u[2]=1; 
  f.outlier = lm(y~x1 + x2 + x5 + x8 + x9 + x10+u,data=p.dat4)
  summary(f.outlier)

#################### Round 5
  
# Re-fit removing obs 1,3,4,5
  p.dat5=p.dat[-c(1,3,4,5),]
  fit5=glm(y~x1 + x2 + x5 + x8 + x9 + x10,data=p.dat5)

     h = hatvalues(fit5,type="diagonal")
	infl = influence(fit5, do.coef = FALSE)
  rsta = rstandard(fit5, infl, type = "pearson")
  rstu = rstudent(fit5, infl, type = "pearson")
  cook5 =cooks.distance(fit5, infl,res = infl$pear.res, 
	        dispersion = summary(fit5)$dispersion, hat = infl$hat)

  par(mfrow=c(2,2))
  qqnorm(h,xlab="",ylab="",main="Q-Q  plot of the leverage values")
  qqnorm(rstu,xlab="",ylab="",main="Q-Q  plot of the studentized deletion residuals")
  qqnorm(cook5,xlab="",ylab="",main="Q-Q  plot of the Cook's distances")

 n = length(cook5)
  cbind( order(cook5), cook5[order(cook5)], order(h), h[order(h)])[(n-2):n,]

# test significance of obserbation 9 (with new index 5 in p.dat5 )
  u=rep(0,n); u[5]=1; 
  f.outlier = lm(y~x1 + x2 + x5 + x8 + x9 + x10+u,data=p.dat5)
  summary(f.outlier)


#################### Round 6
  
# Re-fit removing obs 1,3,4,5,9
  p.dat6=p.dat[-c(1,3,4,5,9),]
  fit6=glm(y~x1 + x2 + x5 + x8 + x9 + x10,data=p.dat6)

     h = hatvalues(fit6,type="diagonal")
	infl = influence(fit6, do.coef = FALSE)
  rsta = rstandard(fit6, infl, type = "pearson")
  rstu = rstudent(fit6, infl, type = "pearson")
  cook6 =cooks.distance(fit6, infl,res = infl$pear.res, 
	        dispersion = summary(fit6)$dispersion, hat = infl$hat)

  par(mfrow=c(2,2))
  qqnorm(h,xlab="",ylab="",main="Q-Q  plot of the leverage values")
  qqnorm(rstu,xlab="",ylab="",main="Q-Q  plot of the studentized deletion residuals")
  qqnorm(cook6,xlab="",ylab="",main="Q-Q  plot of the Cook's distances")

  n = length(cook6)
  cbind( order(cook6), cook6[order(cook6)], order(h), h[order(h)])[(n-2):n,]

# test significance of obserbation 7 (with new index 3 in p.dat6 )
  u=rep(0,n); u[3]=1; 
  f.outlier = lm(y~x1 + x2 + x5 + x8 + x9 + x10+u,data=p.dat6)
  summary(f.outlier)

###################################################################################
 
## Checking significance of outliers simultaneously
  fit1=glm(y.p~x1 + x2 + x5 + x8 + x9 + x10,data=p.dat)
 
  h =	hatvalues(fit1,type="diagonal")
	infl = influence(fit1, do.coef = FALSE)
  rsta = rstandard(fit1, infl, type = "pearson")
  rstu = rstudent(fit1, infl, type = "pearson")
  cook =cooks.distance(fit1, infl,res = infl$pear.res, 
	        dispersion = summary(fit1)$dispersion,hat = infl$hat)

  par(mfrow=c(2,2))
  qqnorm(h,xlab="",ylab="",main="Q-Q  plot of the hat values") # checking leverage
  qqnorm(rstu,xlab="",ylab="",main="Q-Q  plot of the studentized deletion residuals") # checking consistency
  qqnorm(cook,xlab="",ylab="",main="Q-Q  plot of the Cook's distances") # checking influence

  n = length(cook)
  u1=rep(0,n); u2=u1; u3=u1; u4=u1; u5=u1; u6=u1
  u1[1]=1; u2[3]=1;u3[4]=1; u4[5]=1;u5[7]=1;u6[9]=1
  outlier =  lm(y~x1 + x2 + x5 + x8 + x9 + x10+u1+u2+u3+u4+u5+u6,data=p.dat)
  summary(outlier)

 m1.dat=p.dat[-c(1,3,4,5,9),]
 m2.dat=p.dat[-c(1,3,4,5,7),]
 m3.dat=p.dat[-c(1,3,4,5),]
 m1=lm(y.p~x1 + x2 + x5 + x8 + x9 + x10,data=m1.dat); summary(m1)
 m2=lm(y.p~x1 + x2 + x5 + x8 + x9 + x10,data=m2.dat); summary(m2)
 m3=lm(y.p~x1 + x2 + x5 + x8 + x9 + x10,data=m3.dat); summary(m3)

 
#############################################################################
# consider the data in R: mtcars with variables 
# mpg,cyl +disp + hp +drat +   wt + qsec+ vs +am +gear+ carb
# Regress  mpg on the ohters
# read.table("D://Rsession/mtcars.txt",header=TRUE)

vifChen=function(object) {
 ## Input:
  # object -- a fitted lm object.
 ## Output:
  # vif -- variance inflation factors

 X = object$x[,-1]
 V = vcov(object)[-1,-1]
 n = dim(X)[1] 
 sigma=summary(object)$sigma

 v = diag(V)
 S = diag(var(X))*(n-1)
 vif = v*S/sigma^2
 vif
}

#(i)
# compute correlation matrix:
  round(cor(mtcars),2)

# Fitting different models:
 full = lm(mpg~cyl +disp + hp +drat +   wt + qsec+ vs +am +gear+ carb, x=TRUE, data=mtcars)
 rm.wt = lm(mpg~cyl +disp + hp +drat + qsec+ vs +am +gear+ carb, x=TRUE, data=mtcars)
 rm.hp = lm(mpg~cyl +disp + drat +   wt + qsec+ vs +am +gear+ carb, x=TRUE, data=mtcars)
 round(full$coef,3); round(rm.wt$coef,3); round(rm.hp$coef,3)

# calculate VIF
round(vifChen(full),3)

#(ii)
rm.disp = lm(mpg~cyl + hp +drat +   wt + qsec+ vs +am +gear+ carb, x=TRUE, data=mtcars)
round(vifChen(rm.disp),3)

rm.cy1 = lm(mpg~ hp +drat +   wt + qsec+ vs +am +gear+ carb, x=TRUE, data=mtcars)
round(vifChen(rm.cy1),3)

rm.wt = lm(mpg~ hp +drat +   qsec+ vs +am +gear+ carb, x=TRUE, data=mtcars)
round(vifChen(rm.wt),3)


rm.hp = lm(mpg~ drat +   qsec+ vs +am +gear+ carb, x=TRUE, data=mtcars)
round(vifChen(rm.hp),3)

null=lm(mpg~1,data=mtcars)
f.fit=stepAIC(null,scope=list(upper=~drat + qsec+ vs +am +gear+ carb,lower=~1),direction="forward")
summary(f.fit)


library(MASS)
regRidge = lm.ridge(mpg~cyl +disp + hp +drat +   wt + qsec+ vs +am +gear+ carb, data=mtcars, lambda = seq(1, 50, 0.01))
par(mfrow=c(1,1))
plot(regRidge$lambda, regRidge$GCV,type="l")

# the minimum CV is achieved at 

lambda_best = regRidge$lambda[which(regRidge$GCV==min(regRidge$GCV))]
regRidge_best = lm.ridge(mpg~cyl +disp + hp +drat +   wt + qsec+ vs +am +gear+ carb, data=mtcars, lambda = lambda_best)
round(regRidge_best$coef,3)

#########################################################
## Question 3

# (i)
q3.dat = read.table("C://Rsession/kidney.txt",header=TRUE)
pairs(q3.dat)
round(cor(q3.dat),3)

#(ii)
q3.fit=lm(C.clear~serum+Age.y+weight,x=TRUE,data=q3.dat)
vifChen(q3.fit)

#(iii)
 fit1=glm(C.clear~serum+Age.y+weight,data=q3.dat)

  yhat = fit1$fitted.values
  r =	residuals(fit1,type="pearson")
  h =	hatvalues(fit1,type="diagonal")
	infl = influence(fit1, do.coef = FALSE)
  rsta = rstandard(fit1, infl, type = "pearson")
  rstu = rstudent(fit1, infl, type = "pearson")
  cook =cooks.distance(fit1, infl,res = infl$pear.res, 
	        dispersion = summary(fit1)$dispersion,hat = infl$hat)

# Check for homogeneity and normality
  par(mfrow=c(1,2))
  plot(yhat, r,xlab="Residual vs. fitted values")
  qqnorm(rsta,xlab="Q-Q plot of standardized residuals")

# Check for outliers

  par(mfrow=c(2,2))
  qqnorm(h,xlab="",ylab="",main="Q-Q  plot of the hat values") # checking leverage
  qqnorm(rstu,xlab="",ylab="",main="Q-Q  plot of the studentized deletion residuals") # checking consistency
  qqnorm(cook,xlab="",ylab="",main="Q-Q  plot of the Cook's distances") # checking influence

  n = length(cook)
  cbind( order(cook), cook[order(cook)], order(rstu), rstu[order(rstu)],order(h),h[order(h)] )[(n-5):n,]

  u1=rep(0,n);u2=u1;u3=u1;u4=u1;
  u1[26]=1;u2[16]=1;u3[21]=1;u4[20]=1
  outlier=glm(C.clear~serum+Age.y+weight+u1+u2+u3+u4,data=q3.dat)
  summary(outlier)

###########################################################################
### Question 4: Trial of children with neurological problems
y = c(1.38,1.26,1.51,1.46,1.61,1.59, 1.36,1.28,1.41,1.39,1.51,1.44)
n = c(41,41,33,45,18,23,40,38,35,46,20,23)
s = c(0.22,0.25,0.31,0.28,0.34,0.46,0.28,0.19,0.27,0.33,0.41,0.30)

S=factor(rep(c(1,1,2,2,3,3),2)) #social class
G=factor(rep(c(1,2),6)) # gender
T=factor( c(rep(1,6), rep(2,6))) #treatment

fit0=lm(y~S+G+T,weight=n); r0=fit0$resid; y0=fit0$fitted
r0.w=r0*n^(1/2); y0.w=y0*n^(1/2)
#(i) 
fit.a=lm(log(s)~log(y))
summary(fit.a)

y.t=y^(-3/2)
fit=lm(y.t~S+G+T,weight=n); r=fit$resid; y.h=fit$fitted
r.w=r*n^(1/2); y.w=y.h*n^(1/2)

par(mfrow=c(2,1))
plot(y0.w,r0.w); plot(y.w,r.w);


  
#############################################################################
