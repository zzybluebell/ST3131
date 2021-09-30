### ST3131 Tutorial 1

#############################################################################
##Question 1 

  q1.dat=read.table("Datasets/AdverImpact.txt",header=TRUE) # (ia)

  y=q1.dat$impre; x=q1.dat$adver                                # (ib)

  plot(x, y, xlab='Expenditure', ylab='Impression')             # (ic)

  q1.fit = lm(y~x); summary(q1.fit)                             # (id)

  r = q1.fit$resid
  fitted = q1.fit$fitted
  plot(fitted, r, xlab='Fitted values', ylab='Residuals')       # (ie)



#############################################################################
## Question 2
q2.dat = read.table("Datasets/GPA.txt",header=TRUE)
attach(q2.dat)

q2.fit = lm(GPA~ACT,data=q2.dat)
summary(q2.fit)

plot(ACT,GPA)
abline(q2.fit,col="blue")

predict(q2.fit, list(ACT=30))


#############################################################################
## Question 3

q3.dat = read.table("D://Rsession/robbery.txt")

Y = q3.dat$V1; X = q3.dat$V2

q3.fit = lm(Y~X); summary(q3.fit)

plot(X,Y,xlab="Population density",ylab="Robbery rate")
abline(q3.fit,col="blue")


r = q3.fit$resid
r[10]

