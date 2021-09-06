########################
## ST3131 Tutorial 2  ##
########################


####################################################################
## Question 1 
q1.dat = read.table("D://Rsession/calculator.txt",header=TRUE)
y = q1.dat$service.time; x=q1.dat$machine.num

# (a), (b)
q1.fit = lm(y~x)
summary(q1.fit)
qt(0.95,16)

# (c) 
 1-pt(1.421754,16)

(14.7383 - 14)/0.5193

# (d)
#  using function predict
   predict(calc.fit, list(x=6), interval="confidence",level=0.9)

# compute directly
  y.new = b0+b1*6 
  d=c(1,6)
  v.y = t(d)%*%v%*%d
  s.y = sqrt(v.y)
  q = qt(0.95,16)
  c.i = c(y.new - s.y*q, y.new+s.y*q)

# (e) 
# using function predict
  predict(calc.fit, list(x=6), interval="prediction",level=0.9)

# compute directly
   sigma2 = 4.482^2
   s.pred = sqrt(v.y +sigma2)
   p.i = c(y.new - s.pred*q, y.new+s.pred*q)

# (f) 
  c.i.pm = c.i/6

######################################################################
## Question 2
q2.dat = read.table("D://Rsession/airfreight.txt",header=TRUE)
y = q2.dat$break.num; x=q2.dat$tran.times

q2.fit=lm(y~x); summary(q2.fit)

# (a)
plot(x,y); abline(q2.fit)

# (b)  using function confint or compute directly 

 confint(q2.fit,level=0.95)
 b1 = q2.fit$coef[2];  v = vcov(q2.fit);  s.b1 = sqrt(v[2,2])
 q = qt(0.975,8);  c.b1 =c(b1-s.b1*q, b1+s.b1*q)

# (c) It is equivalent to test H0: beta0 < 9. 
  b0 = air.fit$coef[1]
  s.b0 = sqrt(v[1,1])
  t.b0 = (b0-9)/s.b0
  p.value = 1-pt(t.b0,8)


########################################################################
## Question 3


# (b)
lh.f =function(beta, x, y){ 
      x=c(7,12,4,14,25,30)
      y=c(128,213,75,250,446,540)
      - sum((y-beta*x)^2)/12
}

c(lh.f(17), lh.f(18), lh.f(19)) 
# (c)
 x=c(7,12,4,14,25,30)
 y=c(128,213,75,250,446,540)
 q3.fit = lm(y~-1+x); summary(q3.fit)

# (d) 
l=NULL
b=seq(17,19,by=0.01)
n=length(b)
for (i in 1:n) {
   l[i]=lh.f(b[i])
}

plot(b,l,type="l")


