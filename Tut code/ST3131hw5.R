### ST3131 Tutorial 5 ###
#########################
rm(list = ls())

#################################################################
## Question 1  Cash Offer

## Create data
 offer=c(21,23,19,22,22,23,21,22,20,21,   25,
         30,29,26,28,27,27,26,29,27,28,27,29,
         25,22,23,   22,21,23,19,20,21,20,20)
 age=factor(rep(c("Young","Middle","Elderly"),c(11,12,11)))
 gender=factor(c(rep(c("M","F"),c(6,5)),rep(c("M","F"),c(6,6)),
                 rep(c("M","F"),c(5,6))) )

 q1.dat=data.frame(offer,age,gender)


## (ii) 
## Fit interaction model 
 int = lm(offer~gender*age,data=q1.dat)
 summ.i=summary(int);sigma.i = summ.i$sigma; summ.i
 anova(int)
### p-value is  0.1842; which not significant

## (iii) 
## Fit main effect model with order: age, gender
 main.ag = lm(offer~age+gender,data=q1.dat)
 summary(main.ag)
 anova(main.ag)

## Fit main effect model with order: gender, age
 main.ga = lm(offer~gender+age,data=q1.dat)
 summ.m=summary(main.ga); sigma.m=summ.m$sigma; summ.m
 anova(main.ga)

# (iv)
## Compute F-statistics by using Wald statistics

   b=main.ga$coef
   V=vcov(main.ga)

   b1=b[2]
   V1=V[2,2]
   F1 = b1^2/V1

   b2=b[3:4]
   V2 =V[3:4,3:4]
   F2=t(b2)%*%solve(V2)%*%b2/2

   F=c(F1, F2)
## (v)
## adjusting F
   F.a = F*sigma.m^2/sigma.i^2
   p =c(1-pf(F.a[1],1,28), 1-pf(F.a[2],2,28))
   data.frame(Factor=c("gender","age"),Orig.F = F, Adj.F=F.a, p.value=p)

## Question 3 ##
   summ.m
   V= vcov(main.ga)
## (i) 
   V.a = V*sigma.i^2/sigma.m^2

   s.b=sqrt(diag(V.a))
   t.b = b/s.b
   p =1- pt(abs(t.b),28)

  data.frame(Estiamte = b, Std.Error = s.b, t.value = t.b, p.value = p)

   
   

###############################################################
## Question 2 DVD sales

q2.dat=read.table("D://Rsession/dvd.txt")

# dim(q2.dat); levels(q2.dat$Genre); levels(q2.dat$Rating)

y=log(q2.dat$DVD); G=q2.dat$Genre; R=q2.dat$Rating

# Interaction model

int=lm(y~R*G)
summary(int)
anova(int)

# main effect model 

main=lm(y~G+R)
summary(main)
anova(main)

b.g=main$coef[2:3]; V.g=vcov(main)[2:3,2:3]
W=t(b.g)%*%solve(V.g)%*%b.g; F=W/2

## adjustment 
 sigma.i=summary(int)$sigma; sigma.m=summary(main)$sigma
 b=main$coef; V=vcov(main); V.a = V*sigma.i^2/sigma.m^2
 sd.b= sqrt(diag(V.a)); t=b/sd.b; p = 1-pt(abs(t),109)
 round(V.a, 4)
 round(data.frame(Estimate=b, Std.Error = sd.b, t.value = t, p.value=p),5)

-0.31876-(-0.71296)
0.0497+0.052 - 2*0.0281
 1-pt(0.3942/0.0455^(0.5), 109)


## Create data
 offer=c(21,23,19,22,22,23,21,22,20,21,   25,
         30,29,26,28,27,27,26,29,27,28,27,29,
         25,22,23,   22,21,23,19,20,21,20,20)
 age=factor(rep(c("Young","Middle","Elderly"),c(11,12,11)))
 gender=factor(c(rep(c("M","F"),c(6,5)),rep(c("M","F"),c(6,6)),
                 rep(c("M","F"),c(5,6))) )


####################################################################
## Question 3, Q1 revisited;
# Create data
 offer=c(21,23,19,22,22,23,21,22,20,21,   25,
         30,29,26,28,27,27,26,29,27,28,27,29,
         25,22,23,   22,21,23,19,20,21,20,20)
 age=factor(rep(c("Young","Middle","Elderly"),c(11,12,11)))
 gender=factor(c(rep(c("M","F"),c(6,5)),rep(c("M","F"),c(6,6)),
                 rep(c("M","F"),c(5,6))) )


 q3.dat=data.frame(offer,age,gender)


## (ii) 
## Fit interaction model 
 int = lm(offer~gender*age,data=q3.dat)
 summ.i=summary(int); sigma.i = summ.i$sigma; summ.i
 

(6.2639 - 0.2097)/\sqrt{0.3926+0.4113-2*0.2056} 

## (iii) 
## Fit main effect model with order: age, gender
 main = lm(offer~gender+age,data=q3.dat)
 summ.m=summary(main); summ.m; sigma.m=summ.m$sigma
 
b=main$coef; V=vcov(main); 

# Adjusted variance matrix and summary table
  b=main$coef; V=vcov(main); 
  V.a = V*sigma.i^2/sigma.m^2;  round(V.a,4)
  sd.b = sqrt(diag(V.a)); t = b/sd.b;  p = 1-pt(abs(t),28)

 
 data.frame(Estimate=b, Std.Error=sd.b,t.value = t, p.value=p)
 round(V.a,4)

sqrt(2*qf(0.95,2,28))

T=c(9.9971,9.6611) 

1-pf(T^2/2,2,28)

q.t = 3.499/sqrt(2)

d=c(b[3:4] ,b[3]-b[4])
sd = c(sqrt(diag(V.a)[3:4]), sqrt(0.3926+0.4113-2*0.2056))

cbind(d-sd*q.t, d+sd*q.t)





