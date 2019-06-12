# mydf$financial = rgb(0,0,0,150,maxColorValue=255)
# mydf$financial[which(mydf$d.deposit.rate > 0)] = rgb(225,0,0,150,maxColorValue=255)
# mydf$financial[which(mydf$d.deposit.rate < 0)] = rgb(0,0,255,150,maxColorValue=255)
# 
# plot(mydf$dBeta, mydf$dIntercept, type='n',xlim=c(-0.1,0.1), col=mydf$financial)
# text(mydf$dBeta, mydf$dIntercept, labels=mydf$year, col=mydf$financial,cex=(range01(mydf1$d.deposit.rate)+0.6))
# #plot(dBeta, dIntercept,xlim=c(-0.1,0.1), col=drate,cex=(range01(mydf$d.deposit.rate)*2+0.5))
# abline(v=0)
# abline(h=0)

library(ggplot2)
modelname='OLS1_DJS'
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
upyear0 = c(gdpdf$year[gdpdf$year %in% upyear])


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
mydf1 = mydf
for (n in 2:ncol(mydf1)){mydf1[,n] = range01(mydf[,n])}

plot(mydf1$year, mydf1$Beta, col=1)
lines(mydf1$year, mydf1$Beta, col=1)

points(mydf1$year, mydf1$deposit.rate, col=3)
lines(mydf1$year, mydf1$deposit.rate, col=3)

plot(mydf1$year, mydf1$gdp.growth.rate,col=4)
lines(mydf1$year, mydf1$gdp.growth.rate,col=4)

points(mydf1$year, mydf1$d.gdp.rate, col=2)
lines(mydf1$year, mydf1$d.gdp.rate, col=2)

plot(mydf$year,mydf$d.gdp.rate)
lines(mydf$year,mydf$d.gdp.rate)
abline(h=0)

####################################################################
mydf$up = 0
mydf$up[which(mydf$year %in% c(1998:2004))] = 1
mydf$up = mydf$up + 1

plot(mydf$dBeta, mydf$dIntercept, type='n',xlim=c(-0.1,0.1), col=mydf$up)
text(mydf$dBeta, mydf$dIntercept, labels=mydf$year, col=mydf$up)
# #plot(dBeta, dIntercept,xlim=c(-0.1,0.1), col=drate,cex=(range01(mydf$d.deposit.rate)*2+0.5))
abline(v=0)
abline(h=0)

####################################################################
mydf$speed = 'a' #GDP增速没有变化
mydf$speed[which(mydf$d.gdp.rate > 0)] ='b'  #GDP增速加速
mydf$speed[which(mydf$d.gdp.rate < 0)] ='c'  #GDP增速减缓

png(filename=paste0(home,'/Results/dBetadIntercept/GDPgrowth.png'),
    width=17,height=15, units='cm',res=180)
p = ggplot(data=mydf, aes(x=dBeta, y=dIntercept, color=speed)) + 
  geom_text(#vjust = 0, nudge_y = 0.01, 
    aes(label=year, color=speed,size=(range01(mydf1$d.gdp.rate)*1.5+0.5))) +
  scale_color_manual(values=alpha(c('red','blue'), 0.6)) +
  xlim(-0.1,0.1) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x ='Beta', y='Intercept') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)
dev.off()

#####################################################################
mydf$financial = 'a' #没有宏观操作
mydf$financial[which(mydf$d.deposit.rate > 0)] ='b'  #积极货币政策
mydf$financial[which(mydf$d.deposit.rate < 0)] ='c'  #紧缩性货币政策

png(filename=paste0(home,'/Results/dBetadIntercept/MonetaryPolicy.png'),
    width=17,height=15, units='cm',res=180)
p = ggplot(data=mydf, aes(x=dBeta, y=dIntercept, color=financial)) + 
  geom_text(#vjust = 0, nudge_y = 0.01, 
            aes(label=year, color=financial,size=(range01(mydf1$d.deposit.rate)*1.5+0.5))) +
  scale_color_manual(values=alpha(c('black','red','blue'), 0.6)) +
  xlim(-0.1,0.1) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x ='Beta', y='Intercept') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)
dev.off()

