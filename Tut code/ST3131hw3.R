#######################
## ST3131 Tutorial 3 ##
#######################

####################################################################
## Question 1

  q1.dat = read.table("D://Rsession/brand.txt",header=TRUE)
  y = q1.dat$degree; x1=q1.dat$moisture; x2=q1.dat$sweetness

# (a)
  one = rep(1,length(y))
  cbind(one,x1,x2)

#(b)
  q1.fit = lm(y~x1+x2)
  summary(q1.fit)

# (c) R^2 can be read from summary
  
# (d) 
  yhat = q1.fit$fitted
  summary(lm(y~yhat))
  cor(y,yhat)^2


############################################################################
## Question 2 
  SMSA.dat = read.table("D://Rsession/SMSA.txt",header=TRUE)
  attach(SMSA.dat)
  y1=doctor; x11=T.p; x12=Land; x13=income; x21=x11/x12; x22=P.city; x23=x13

# (a) Scatter plot matrix
  X1=cbind(y1,x11,x12,x13)
  pairs(X1)
  round(cor(X1),4)


  X2=cbind(y1,x21,x22,x23)
  pairs(X2)
  round(cor(X2),4)

# (b) Model fit
  model1.fit = lm(y1~x11+x12+x13)
  summary(model1.fit)

  model2.fit = lm(y1~x21+x22+x23)
  summary(model2.fit)


# (d) Regional fit
  SMSA1.dat = SMSA.dat[region==1,]; SMSA2.dat = SMSA.dat[region==2,]
  SMSA3.dat = SMSA.dat[region==3,]; SMSA4.dat = SMSA.dat[region==4,]

  R1.fit = lm(crimes~I(T.p/Land)+income+h.sch,data = SMSA1.dat)
  R2.fit = lm(crimes~I(T.p/Land)+income+h.sch,data = SMSA2.dat)
  R3.fit = lm(crimes~I(T.p/Land)+income+h.sch,data = SMSA3.dat)
  R4.fit = lm(crimes~I(T.p/Land)+income+h.sch,data = SMSA4.dat)

  summary(R1.fit); summary(R2.fit)
  summary(R3.fit); summary(R4.fit)


##########################################################################
## Question 3

# (a)
q3.dat = read.table("D://Rsession/ChemShip.txt",header=TRUE)
y=q3.dat$time; x1=q3.dat$number; x2=q3.dat$weight; Z=cbind(x1,x2)
q3.fit=lm(y~x1+x2)
summary(q3.fit)

# (c)
yhat=q3.fit$fitted
r = q3.fit$resid
b = q3.fit$coef
V = vcov(q3.fit)

#  (d)
cor(y,yhat)^2

# (e)
 Z=cbind(x1,x2); v.z=var(Z)
 v.zy=cov(Z,y);  Z=cbind(x1,x2)
 v.y=var(y)
 mcor2=t(v.zy)%*%solve(v.z)%*%v.zy/v.y