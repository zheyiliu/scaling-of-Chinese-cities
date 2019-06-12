library(ggplot2)
modelname='OLS'
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
setwd(paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname))
load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))

gdpdf = sumlmHorizontal[which(sumlmHorizontal$yIndex=='GDP'),]
gdpdf = na.omit(gdpdf)
gdpdf = gdpdf[,c('Beta', 'Intercept', 'year')]
gdpdf$year = gdpdf$year - 1

gdpall = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/GDPall.csv', header=T)
urbanization = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/urbanization.csv', header=T)
rate = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/DepositRate.csv', header=T)

upyear = 1984:2016
upyear0 = c(gdpdf$year[gdpdf$year %in% upyear],1992)


dIntercept = vector()
dBeta = vector()
dyear = vector()
for (i in 1:length(upyear0)){
  gdp1a = gdpdf[gdpdf$year==upyear0[i],]$Intercept
  gdp2a = gdpdf[gdpdf$year==(upyear0[i]+1),]$Intercept
  if (length(gdp2a)!=0){
    da = gdp2a - gdp1a
    gdp1b = gdpdf[gdpdf$year==upyear0[i],]$Beta
    gdp2b = gdpdf[gdpdf$year==upyear0[i]+1,]$Beta
    db = gdp2b - gdp1b
    dIntercept[i] = da
    dBeta[i] = db
    dyear[i] = upyear0[i] + 1
  }
}

dd = data.frame(year=dyear, dBeta=dBeta, dIntercept=dIntercept)

mydf = merge(gdpdf, gdpall, by='year')
mydf = merge(mydf, urbanization, by='year')
mydf = merge(mydf, rate, by='year')
mydf = merge(mydf, dd, by='year')

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
for (n in 2:ncol(mydf)){mydf[,n] = range01(mydf[,n])}

plot(mydf$year, mydf$dBeta, col=1)
lines(mydf$year, mydf$dBeta, col=1)

points(mydf$year, mydf$d.deposit.rate, col=3)
lines(mydf$year, mydf$d.deposit.rate, col=3)

plot(mydf$year, mydf$gdp.growth.rate,col=4)
lines(mydf$year, mydf$gdp.growth.rate,col=4)

dgdp = as.numeric(mydf$d.gdp.rate > 0) + 1

plot(dBeta, dIntercept, type='n',xlim=c(-0.1,0.1), col=dgdp)
#text(dBeta, dIntercept, labels=dyear, col=dgdp,cex=((range01(mydf$d.gdp.rate)+0.5)*0.8))
text(dBeta, dIntercept, labels=dyear, cex=0.8, col=dgdp)
#plot(dBeta, dIntercept,xlim=c(-0.1,0.1), pch=19, col=dgdp,cex=((range01(mydf$d.gdp.rate)+0.5)*2))
abline(v=0)
abline(h=0)

drate = as.numeric(mydf$d.deposit.rate > 0) + 1

#plot(dBeta, dIntercept, type='n',xlim=c(-0.1,0.1), col=drate)
#text(dBeta, dIntercept, labels=dyear, cex=0.8, col=drate)
plot(dBeta, dIntercept,xlim=c(-0.1,0.1), col=drate,cex=(range01(mydf$d.deposit.rate)*2+0.5))
abline(v=0)
abline(h=0)
