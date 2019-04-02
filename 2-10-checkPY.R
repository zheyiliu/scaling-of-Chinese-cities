x = 1:10000
curve(a$Intercept[1]*(x^a$Beta[2]))

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

dfi = get('GDP')
aaa = dfi[which(dfi$year==1988 & grepl(rangeStat,dfi$index) & dfi$value/10000 > 11.49),]
bbb = dfi[which(dfi$year==2017 & grepl(rangeStat,dfi$index) & dfi$value/10000 > 511.13),]

cc = (aaa$value - 11.49)/10000
cc = (bbb$value - 511.13)/10000
h = hist(cc,freq=F,breaks=40,border='white',col='grey86',cex.lab=1.5,cex.main=1.5)
lwd = 2
yhist = h$density
cx = seq(min(cc),max(cc),length.out=length(h$breaks)-1)
fpp = nls(yhist~a*cx^(-b),start=list(a=-1,b=0.4))
fp = summary(fpp)
scalefit = fp$parameters[1]
shapefit = (-1)*fp$parameters[2]
xfit = seq(min(cc),max(cc),length.out=64)
ypowl = scalefit*xfit^shapefit
curve(scalefit*x^shapefit,col='turquoise3',lwd=lwd,add=T)
pp = scalefit*cx^shapefit
ptest = ks.test(yhist, pp)


x = 0:4
y = log10(24.7) + (x*-1.63)
plot(x, y, type='l',col=2,  ann=F,lwd=3, ylim=)
curve(log10(8.51) + (x*-1.94), add=T, col=3,lwd=3)
abline(h=0)

x = 0:60
y = 24.7 * (x^-1.63)
plot(x, y, type='l',col=2,  ann=F,lwd=2)
curve(8.51 * (x^-1.94), add=T, col=3,lwd=2)
abline(h=0)
