library(ggplot2)
#library(MASS)
#library(actuar)
#library(fitdistrplus)
library(poweRlaw)

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

Alpha = function(dfname, rangeStat, yeari, scale, denfun){
  dfi = get(dfname)
  dat = dfi[which(dfi$year == yeari & grepl(rangeStat,dfi$index)),]
  
  if (!scale){cs = dat$value
  } else {cs = dat$value/10000
  }
  
  if(length(cs)<50){
    all = data.frame(yIndex=paste0(dfname,rangeStat), Alpha=NA, KS_Pvalue=NA, Observation=length(cs), year=yeari)
  } else {
    
    m_m=eval(parse(text = paste0(denfun, '$new(cs)')))
    m_m$getXmin()#返回预设的Xmin,其值为1
    
    #Xmin与alpha参数的调节方法
    #m_m$setXmin(5)
    #m_m$setPars(2)
    
    #通过实际分布函数与理论分布之间的距离最小化，求出Xmin
    est = estimate_xmin(m_m)
    m_m$setXmin(est)
    est = estimate_pars(m_m)
    
    #parallel::detectCores()#查看有几个线程
    #bs = bootstrap(m_m, no_of_sims=100, threads=4)
    #plot(jitter(bs$bootstraps[,2], factor=1.2), bs$bootstraps[,3])
    bs_p = bootstrap_p(m_m, no_of_sims=100, threads=4)
    
    
    png(file=paste0('C:/Sync/CoolGirl/Fhe/Results/tau/',dfname,yeari,denfun,'.png'),
        width = 480, height = 480, units = "px")
    plot(m_m, ann=F)
    lines(m_m,col=2,lw=3,lty=3)
    abline(v=m_m$xmin, col=rgb(0, 0, 255, 60, maxColorValue=255), lw=3)
    l1 = paste0(dfname,' (',yeari,')')
    l2 = paste0('τ = ',round(m_m$pars,3), ' (Pks=', round(bs_p$p,3), ')')
    l3 = paste0("Ymin = ", round(m_m$xmin,2))
    legend ('bottomleft', c(l1,l2,l3), 
            col=c(1,2,rgb(0, 0, 255, 60, maxColorValue=255)), cex=1.5, pch=c(1,NA,NA), lty=c(0,3,1), lwd=3)
    title(xlab= 'y', ylab = 'P(Y≥y)')
    dev.off()
    
    all = data.frame(yIndex=paste0(dfname,rangeStat), Alpha=m_m$pars, KS_Pvalue=bs_p$p, Observation=length(cs), year=yeari)
  }
}

all0 = data.frame()
for (yearii in 1986:2017){
  ee = Alpha(dfname = 'POP', rangeStat='市辖区', yeari = yearii, scale=F, denfun='conpl') #conlnorm
  all0 = rbind(all0, ee)
}

all1 = data.frame()
for (yearii in 1986:2017){
  ee = Alpha(dfname = 'GDP', rangeStat='市辖区', yeari = yearii, scale=T, denfun='conpl')
  all1 = rbind(all1, ee)
}



dfname = 'GDP'
rangeStat='市辖区'
yeari = 1986
scale=T
denfun='conpl'



fl = fitdistr(cs, 'log-normal')
ltest = ks.test(cs, 'plnorm', meanlog = fl$estimate[1], sdlog = fl$estimate[2])
fg = fitdistr(cs, 'gamma')
gtest = ks.test(cs, 'pgamma', fg$estimate[1], fg$estimate[2])
fw = fitdistr(cs, 'weibull')
wtest = ks.test(cs,"pweibull", scale=fw$estimate[2], shape=fw$estimate[1])

s = 500
xfit = seq(min(cs),max(cs),length.out=s)
himids = hist(cs, xfit, plot=F)$mids
hidensity = hist(cs, xfit, plot=F)$density
pyc = rev(cumsum(rev(hidensity)))

#par(mfrow=c(3,1))
hist(cs,breaks=s,freq=F)
#lines(himids, hidensity,col='red')
#plot(log(himids), log(hidensity))
#plot(log(himids), log(pyc))

fit1 = lm(log(pyc)~log(himids))
sum1 = summary(fit1)
a=sum1$coefficients[1,1]
b=sum1$coefficients[2,1]
#abline(a, b, col='red')
alpha = a + 1

fpp = nls(hidensity~a*himids^(-b),start=list(a=0.4,b=1))
fp = summary(fpp)
scalefit = fp$parameters[1]
shapefit = (-1)*fp$parameters[2]
ypowl = scalefit*xfit^shapefit
curve(scalefit*x^shapefit,col='turquoise3',lwd=2,add=T)
pp = scalefit*himids^shapefit
ptest = ks.test(hidensity, pp)

ywei = dweibull(xfit,scale=fw$estimate[2], shape=fw$estimate[1])
ylnorm = dlnorm(xfit, meanlog=fl$estimate[1], sdlog=fl$estimate[2])

lines(xfit, ylnorm, col=4,lwd=2,add=T)
lines(xfit, ywei, col=2,lwd=2,add=T)
