library(ggplot2)
#library(MASS)
#library(actuar)
#library(fitdistrplus)
library(poweRlaw)

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

Alpha = function(dfname, rangeStat, yeari, denfun){
  #dfname='POP'
  #yeari=1994
  #rangeStat='市辖区'
  #denfun=c('conpl','conlnorm')
  dfi = get(dfname)
  delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市')
  dfi = dfi[which(!(dfi$city %in% delcity)),]
  dfi = na.omit(dfi)
  dat = dfi[which(dfi$year == yeari & grepl(rangeStat,dfi$index)),]
  
  if (max(dat$value)<50000){cs = dat$value
  } else {cs = dat$value/10000
  }
  
  if(length(cs)<50){
    alla = data.frame(yIndex=paste0(dfname,rangeStat), Alpha=NA, KS_Pvalue=NA, Observation=length(cs), year=yeari)
  } else {
    
    #通过实际分布函数与理论分布之间的距离最小化，求出Xmin
    m1=eval(parse(text = paste0(denfun[1], '$new(cs)')))
    m1$setXmin(estimate_xmin(m1))
    m1$setPars(estimate_pars(m1))
    
    m2 =eval(parse(text = paste0(denfun[2], '$new(cs)')))
    m2$setXmin(estimate_xmin(m2))
    m2$setPars(estimate_pars(m2))
    
    #Xmin与alpha参数的调节方法
    #m1$setXmin(5)
    #m1$setPars(2)
    
    #parallel::detectCores()#查看有几个线程
    #bs = bootstrap(m1, no_of_sims=100, threads=4)
    #plot(jitter(bs$bootstraps[,2], factor=1.2), bs$bootstraps[,3])
    bs_p1 = bootstrap_p(m1, no_of_sims=100, threads=4)
    bs_p2 = bootstrap_p(m2, no_of_sims=100, threads=4)
    
    
    if (dfname=='POP'){
      png(file=paste0('C:/Sync/CoolGirl/Fhe/Results/3powerlaw/alpha/',dfname,yeari,'.png'),
          width = 480, height = 480, units = "px")
      plot(m1, ann=F)
      lines(m1,col=2,lw=3,lty=3)
      lines(m2, col=4, lw=3, lty=3)
      abline(v=m1$xmin, col=rgb(255, 0, 0, 60, maxColorValue=255), lw=3)
      abline(v=m2$xmin, col=rgb(0, 0, 255, 60, maxColorValue=255), lw=3)
      l0 = paste0(dfname,' (',yeari-1,')')
      l1 = paste0('power-law:  α  = ',round(m1$pars,2), ' (Pks=', round(bs_p1$p,3), ')')
      l2 = paste0("power-law:  Nmin = ", round(m1$xmin,2))
      l3 = paste0('log-normal: pars = ',round(m2$pars[1],2),', ',round(m2$pars[2],2), ' (Pks=', round(bs_p2$p,3), ')')
      l4 = paste0("log-normal: Nmin = ", round(m2$xmin,2))
      legend ('bottomleft', c(l0,l1,l2,l3,l4),
              col=c(1,2,rgb(255, 0, 0, 60, maxColorValue=255), 4, col=rgb(0, 0, 255, 60, maxColorValue=255)), 
              cex=1, pch=c(1,NA,NA,NA,NA), lty=c(0,3,1,3,1), lwd=3)
      title(xlab= 'n', ylab = 'P (N ≥ n)')
      dev.off()
    } else {
      png(file=paste0('C:/Sync/CoolGirl/Fhe/Results/3powerlaw/tau/',dfname,yeari,'.png'),
          width = 480, height = 480, units = "px")
      plot(m1, ann=F)
      lines(m1,col=2,lw=3,lty=3)
      lines(m2, col=4, lw=3, lty=3)
      abline(v=m1$xmin, col=rgb(255, 0, 0, 60, maxColorValue=255), lw=3)
      abline(v=m2$xmin, col=rgb(0, 0, 255, 60, maxColorValue=255), lw=3)
      l0 = paste0(dfname,' (',yeari-1,')')
      l1 = paste0('power-law:  τ  = ',round(m1$pars,2), ' (Pks=', round(bs_p1$p,3), ')')
      l2 = paste0("power-law:  Ymin = ", round(m1$xmin,2))
      l3 = paste0('log-normal: pars = ',round(m2$pars[1],2),', ',round(m2$pars[2],2), ' (Pks=', round(bs_p2$p,3), ')')
      l4 = paste0("log-normal: Ymin = ", round(m2$xmin,2))
      legend ('bottomleft', c(l0,l1,l2,l3,l4),
              col=c(1,2,rgb(255, 0, 0, 60, maxColorValue=255), 4, col=rgb(0, 0, 255, 60, maxColorValue=255)), 
              cex=1, pch=c(1,NA,NA,NA,NA), lty=c(0,3,1,3,1), lwd=3)
      title(xlab= 'y', ylab = 'P (Y ≥ y)')
      dev.off()
    }
    
    alla = data.frame(yIndex=paste0(dfname,rangeStat), Alpha=m1$pars,xmin_PL=m1$xmin, xmin_LN=m2$xmin, Lnorm1=m2$pars[1], Lnorm2=m2$pars[2], Pks_PL=bs_p1$p, Pks_LN=bs_p2$p, Observation=length(cs), year=yeari)
  }
  alla
}

AlphaAll = function(dfname0, rangeStat0, yeari0, denfun0){
  all0 = data.frame()
  for (yearii in yeari0){
    ee = Alpha(dfname0, rangeStat0, yearii, denfun0)
    all0 = rbind(all0, ee)
  }
  Rdataname = paste0(dfname0,'conpl')
  assign(Rdataname, all0)
  eval(parse(text=paste0('save(', Rdataname, ",file='C:/Sync/CoolGirl/Fhe/Results/3powerlaw/",Rdataname,".Rdata')")))
  all0
}

POPdf = AlphaAll(dfname0='POP', rangeStat0='市辖区', yeari0=1985:2017, denfun0=c('conpl','conlnorm'))
GDPdf = AlphaAll(dfname0='GDP', rangeStat0='市辖区', yeari0=1985:2017, denfun0=c('conpl','conlnorm'))

dftau0 = data.frame()
dflist = gsub('.Rdata', '', dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL'))
Ydflist = dflist[!grepl('POP', dflist)]
for (i in length(Ydflist)){
  dfname = Ydflist[i]
  dftau = AlphaAll(dfname0=dfname, rangeStat0='市辖区', yeari0=1985:2017, denfun0=c('conpl','conlnorm'))
  dftau0 = rbind(dftau0, dftau)
}
dftau0 = rbind(GDPdf, dftau0)
save(dftau0, file='C:/Sync/CoolGirl/Fhe/Results/3powerlaw/dftau0.Rdata')



# dfname = 'GDP'
# rangeStat='市辖区'
# yeari = 1986
# 
# 
# 
# fl = fitdistr(cs, 'log-normal')
# ltest = ks.test(cs, 'plnorm', meanlog = fl$estimate[1], sdlog = fl$estimate[2])
# fg = fitdistr(cs, 'gamma')
# gtest = ks.test(cs, 'pgamma', fg$estimate[1], fg$estimate[2])
# fw = fitdistr(cs, 'weibull')
# wtest = ks.test(cs,"pweibull", scale=fw$estimate[2], shape=fw$estimate[1])
# 
# s = 500
# xfit = seq(min(cs),max(cs),length.out=s)
# himids = hist(cs, xfit, plot=F)$mids
# hidensity = hist(cs, xfit, plot=F)$density
# pyc = rev(cumsum(rev(hidensity)))
# 
# #par(mfrow=c(3,1))
# hist(cs,breaks=s,freq=F)
# #lines(himids, hidensity,col='red')
# #plot(log(himids), log(hidensity))
# #plot(log(himids), log(pyc))
# 
# fit1 = lm(log(pyc)~log(himids))
# sum1 = summary(fit1)
# a=sum1$coefficients[1,1]
# b=sum1$coefficients[2,1]
# #abline(a, b, col='red')
# alpha = a + 1
# 
# fpp = nls(hidensity~a*himids^(-b),start=list(a=0.4,b=1))
# fp = summary(fpp)
# scalefit = fp$parameters[1]
# shapefit = (-1)*fp$parameters[2]
# ypowl = scalefit*xfit^shapefit
# curve(scalefit*x^shapefit,col='turquoise3',lwd=2,add=T)
# pp = scalefit*himids^shapefit
# ptest = ks.test(hidensity, pp)
# 
# ywei = dweibull(xfit,scale=fw$estimate[2], shape=fw$estimate[1])
# ylnorm = dlnorm(xfit, meanlog=fl$estimate[1], sdlog=fl$estimate[2])
# 
# lines(xfit, ylnorm, col=4,lwd=2,add=T)
# lines(xfit, ywei, col=2,lwd=2,add=T)
