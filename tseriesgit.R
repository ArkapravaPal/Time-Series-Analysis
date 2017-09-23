#Time series

l<-read.csv("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/7. Time Series Analysis/Data/case1.csv")
View(l)
plot(l,type="b")
ts<-ts(l,start=1947,end=2000)
class(ts)
plot.ts(ts,type="b")


library(devtools)
install_github("abhishekumrawal/trendseason")
library(trendseason)
ro.test(ts)

t1<-time(ts)
lse<-lm(ts~t1)

abline(a=lse$coefficients[1],b=lse$coefficients[2])
lse_trend<-lse$fitted.values
plot(lse_trend)
detrended=as.vector(ts)-lse$fitted.values
plot(detrended)
library(forecast)
require(forecast)
forecast(lse$fitted.values,h=5)



#case2
l1<-read.csv("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/7. Time Series Analysis/Data/case2.csv")
View(l1)
s<-l1[,c(1,2)]
c<-ts(s,start=2005,end=2013)
plot.ts(c,type="b")
ro.test(c)
par(mfrow=c(3,2))
t<-time(c)
ls1<-lm(c~t)
summary(ls1)
plot(ls1$fitted.values,type="b",pch=20)
#-------------------------------------------------------------------------------
s1<-l1[,c(1,3)]
c1<-ts(s1,start=2005,end=2013)
plot.ts(c1,type="b")
ro.test(c1)

t1<-time(c1)
ls2<-lm(c1~t1)
summary(ls2)
plot(ls2$fitted.values,type="b",pch=20)
#----------------------------------------------------------------------------------
s2<-l1[,c(1,4)]
c2<-ts(s2,start=2005,end=2013)
plot.ts(c2,type="b")
ro.test(c2)

t2<-time(c2)
ls3<-lm(c2~t2)
summary(ls3)
plot(ls3$fitted.values,type="b",pch=20)
#----------------------------------------------------------------------------------
s3<-l1[,c(1,5)]
c3<-ts(s3,start=2005,end=2013)
plot.ts(c3,type="b")
ro.test(c3)

t3<-time(c3)
ls4<-lm(c3~t3)
summary(ls4)
plot(ls4$fitted.values,type="b",pch=20)
#-----------------------------------------------------------------------------------
s4<-l1[,c(1,6)]
c4<-ts(s4,start=2005,end=2013)
plot.ts(c4,type="b")
ro.test(c4)

t4<-time(c4)
ls5<-lm(c4~t4)
summary(ls5)
plot(ls5$fitted.values,type="b",pch=20)

#case3

o<-read.csv("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/7. Time Series Analysis/Data/case3.csv")
View(o1)
o1<-ts(o,start=1994,end=2013)
plot.ts(o1,type="b",pch=20)
ro.test(o1)

library(TTR)
MAtrend<-SMA(o1[,2],n=5)
plot((o1),type="l")
plot(MAtrend,type="l")
forecast(MAtrend,h=3)

#case4
k<-read.csv("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/7. Time Series Analysis/Data/case4,5,7.csv")
View(k)
l<-ts(k1,start=1949,end=1960,frequency=12)
plot.ts(l,type="b",pch=20)
ro.test(l)

ti<-time(l)
s<-lm(l~ti)
summary(s)

plot(s$fitted.values)
detren<-as.vector(l)-s$fitted.values
plot(detren)

library(trendseason)
friedman.test(detren,12)

require(forecast)
month<-seasonaldummy(l)
m<-lm(l~ti+month)

#case5
k1<-k[,1:3]
l<-ts(k1,start=1949,end=1960,frequency=12)
plot.ts(l,type="b",pch=20)
ro.test(l)

ti<-time(l)
s<-lm(l~ti)
summary(s)

plot(s$fitted.values)
detren<-as.vector(l)-s$fitted.values
plot(detren)

library(trendseason)
friedman.test(detren,12)


decomp<-stl(l,t.window =12,s.window = 12 )
forecast(decomp)

#case7


data.dec<-decompose(l,type = "additive")
plot(data.dec)

#case8
c<-read.csv("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/7. Time Series Analysis/Data/case8.csv")
View(c)

tm<-ts(c,start=2001,end=2005,frequency=4)
plot.ts(tm)
ro.test(tm)

library(trendseason)
friedman.test(tm,4)

dec<-decompose(tm,type="additive")
plot(dec)

#case6
k<-read.csv("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/7. Time Series Analysis/Data/case6.csv")
View(k)
tk<-ts(k,start=2005,end=2014,frequency=12)
plot.ts(tk)
ro.test(tk)

library(trendseason)
friedman.test(tk,4)

deco<-decompose(tk,type = "additive")
plot(deco)
