##############################
### ST3131  Tutorial 8
##############################

library(boot)


############################################################################
## Question 1
pull.dat=read.csv(file="C://Rsession/pullstrength.csv",header=TRUE)
M1 = lm(y~x1, data=pull.dat)
M2 = lm(y~x1+x2, data=pull.dat)
M3 = lm(y~x1+x2+x3, data=pull.dat)
M4 = lm(y~x1+x2+x3+x4, data=pull.dat)
M5 = lm(y~x1+x2+x3+x4+x5, data=pull.dat)
M6 = lm(y~x1+x2+x3+x4+x5+x6, data=pull.dat)

round(diag(vcov(M1)),4)
round(diag(vcov(M2)),4)
round(diag(vcov(M3)),4)
round(diag(vcov(M4)),4)
round(diag(vcov(M5)),4)
round(diag(vcov(M6)),4)

s1=summary(M1)$sigma; s2=summary(M2)$sigma; s3=summary(M3)$sigma;
s4 =summary(M4)$sigma; s5=summary(M5)$sigma;s6=summary(M6)$sigma;


round(diag(vcov(M1))/s1^2,4)
round(diag(vcov(M2))/s2^2,4)
round(diag(vcov(M3))/s3^2,4)
round(diag(vcov(M4))/s4^2,4)
round(diag(vcov(M5))/s5^2,4)
round(diag(vcov(M6))/s6^2,4)

Model=c("M1","M2","M3","M4","M5","M6") 
R.squared =c( summary(M1)$r.squared,summary(M2)$r.squared,
              summary(M3)$r.squared,summary(M4)$r.squared,
              summary(M5)$r.squared,summary(M6)$r.squared )
R.squared.adj =c( summary(M1)$adj.r.squared,summary(M2)$adj.r.squared,
                  summary(M3)$adj.r.squared,summary(M4)$adj.r.squared,
                  summary(M5)$adj.r.squared,summary(M6)$adj.r.squared) 
data.frame(Model, R.squared, R.squared.adj) 

M1 = glm(y~x1, data=pull.dat)
M2 = glm(y~x1+x2, data=pull.dat)
M3 = glm(y~x1+x2+x3, data=pull.dat)
M4 = glm(y~x1+x2+x3+x4, data=pull.dat)
M5 = glm(y~x1+x2+x3+x4+x5, data=pull.dat)
M6 = glm(y~x1+x2+x3+x4+x5+x6, data=pull.dat)

AIC=c(AIC(M1), AIC(M2),AIC(M3),AIC(M4),AIC(M5),AIC(M6))
BIC=c(BIC(M1), BIC(M2),BIC(M3),BIC(M4),BIC(M5),BIC(M6))
CV=c( cv.glm(pull.dat,M1)$delta[2], cv.glm(pull.dat,M2)$delta[2],
      cv.glm(pull.dat,M3)$delta[2], cv.glm(pull.dat,M4)$delta[2],
      cv.glm(pull.dat,M5)$delta[2],cv.glm(pull.dat,M6)$delta[2])

data.frame(Model, AIC,BIC,CV)

#################################################################################
## Question 2 


  SMSA.dat = read.table("C://Rsession/SMSA.txt",header=TRUE)
  attach(SMSA.dat)

  y.p=doctor; x1=Land; x2=T.p; x3=P.city; x4=p.65; x5=beds; x6=h.sch; 
  x7=labor; x8=income; x9=crimes; x10=factor(region);
  p.dat=data.frame(y.p,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

## 1(i) Selection procedures
  library(MASS)
  lower.m = lm(y.p~1, data=p.dat)
  upper.m = lm(y.p~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data=p.dat)

# Forward selection
 forward=stepAIC(lower.m,scope=list(lower=~1,upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10),direction="forward")
 summary(forward)
  
# Backword selction
  backward=stepAIC(upper.m,scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10,
                lower=~1),direction="backward")
  summary(backward)
  
# Stepwise upwards
   stepUp=stepAIC(lower.m,scope=list(lower=~1, upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10),direction="both")
   summary(stepUp)
  
# Stepwise downwards
   stepDown=stepAIC(upper.m,scope=list(lower=~1, upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10),direction="both")
   summary(stepDown)
  
fselect=glm(y.p~x1+x2+x5+x8+x9+x10,data=p.dat); summary(fselect)

#(iii) 

# Penalized likelihood
  library(iterators)
  library(foreach)
  library(Matrix)
  library(shape)
  library(glmnet)

# (iii a)
  r=lm(y.p~x10,data=p.dat)$resid

  X1 = as.matrix(p.dat[,2:10])
  penal = cv.glmnet(X1, r)
  coef(penal, s = penal$lambda.min)
## Selected model: x1+x3+x4+x5+x6+x8+x10 
  
# (iii b)
  z=p.dat$x10; y =p.dat$y.p
  u2 = rep(0,length(z)); u3=u2; u4=u2
  u2[z==2] = 1; u3[z==3]=1; u4[z==4]=1

  X2=cbind(X1,u2,u3,u4)
  penal.2 = cv.glmnet(X2, y)
  coef(penal.2, s = penal.2$lambda.min)
## Selected model: x1+x3+x5+x6+x8+x9+x10 


# comparison

 library(boot)
 m1 = glm(y.p~x1+x3+x4+x5+x6+x8+x10,data=p.dat)
 m2 = glm(y.p~x1+x3+x5+x6+x8+x9+x10,data=p.dat)

 AIC(m1); AIC(m2);
 BIC(m1); BIC(m2)

 cv.1=cv.glm(data=p.dat, m1)$delta[2]; cv.2=cv.glm(data=p.dat, m2)$delta[2]

data.frame(model=c("M1", "M2"),AIC=c( AIC(m1),AIC(m2)),BIC=c(BIC(m1),BIC(m2)),CV=c(cv.1,cv.2))



