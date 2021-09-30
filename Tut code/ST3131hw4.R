######################
## ST3131 Tutorial 4 #
######################


#################################################################
## Question 1

# (b) p value for the one-sided test
1-pt(6.489,13) 

# (c) conficence interval for beta_1
 c(4.425 - 0.3011 *qt(0.975,13),  4.425 + 0.3011 *qt(0.975,13))

# (d) conficence bound for beta_2
  4.375-0.6733*qt(0.95,13)

##################################################################
## Question 2

q3.dat = read.table("Datasets/ChemShip.txt",header=TRUE)
y=q3.dat$time; x1=q3.dat$number; x2=q3.dat$weight
q3.fit=lm(y~x1+x2)
summary(q3.fit)

# (a) p-values of the tests
1-pt(6.135,17)

1-pt(7.632,17)

t=(3.7681 - 5)/0.6142
pt(t,17)

##################################################################
## Question 3  Cash Offer

## (a)
## Create data
 offer=c(21,23,19,22,22,23,21,22,20,21,19,25,
         30,29,26,28,27,27,26,29,27,28,27,29,
         25,22,23,21,22,21,23,19,20,21,20,20)
 ageGroup=factor(rep(c("Young","Middle","Elderly"),c(12,12,12)))
 gender=factor(rep(rep(c("M","F"),c(6,6)),3))
 offer.dat=data.frame(offer,ageGroup,gender)

##Create vectors yij for age group i and gender j:
  y11=offer.dat[ageGroup=="Young"&gender=="M",1]
  y12=offer.dat[ageGroup=="Young"&gender=="F",1]
  y21=offer.dat[ageGroup=="Middle"&gender=="M",1]
  y22=offer.dat[ageGroup=="Middle"&gender=="F",1]
  y31=offer.dat[ageGroup=="Elderly"&gender=="M",1]
  y32=offer.dat[ageGroup=="Elderly"&gender=="F",1]

##Create response vector y and compute total mean
  y=c(y11,y21,y31,y12,y22,y32)
  y..=mean(y)

##Compute level mean for age group
  y1.=mean(c(y11,y12))
  y2.=mean(c(y21,y22))
  y3.=mean(c(y31,y32))
  yA=c(y1.,y2.,y3.)

##Compute level mean for gender
  y.1= mean(c(y11,y21,y31))
  y.2= mean(c(y12,y22,y32))
  yG=c(y.1,y.2)

##Compute cell mean
  yAG=c(mean(y11),mean(y12),mean(y21),mean(y22),mean(y31),mean(y32))

##Compute sum of squares and mean sum of squares
  SSA = sum((yA-y..)^2)*12;    MSA=SSA/2
  SSG = sum((yG-y..)^2)*18;    MSG=SSG
  SSAG = sum((yAG - rep(yA,c(2,2,2)) - rep(yG,3)+y..)^2)*6; MSAG = SSAG/2
  SST = sum((y-y..)^2)
  SSE = SST-SSA-SSG-SSAG;    MSE=SSE/30

##Generate ANOVA table
ANOVA.table=matrix(c(1,2,2,30,
                     SSA,SSG,SSAG,SSE,
                     MSA,MSG,MSAG,MSE,
                     MSA/MSE,MSG/MSE,MSAG/MSE,0), ncol=4)

##Compute ANOVA table by fitting a linear regression model
 lm.fit=lm(offer~ageGroup*gender,data=offer.dat) 
 anova(lm.fit)

## (b) p value of F for interation effect
  1-pf(1.058,2,30)

## (c) p values for the main effect
1-pf(66.291,2,30)
1-pf(2.279,1,30)
