setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}
dflist = gsub('.Rdata', '', dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL'))

rangeStat = '市辖区'
dfname = 'Green'
dfi = get(dfname)
dfi = dfi[which(grepl('市',dfi$city)),]
citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)
citypre = subset(citylist, citylist$Administrative_level != 'county')$City_ch
n = 15
par(mfrow=c(4,5))
for (cityi in citypre[(n*20-19):(n*20)]){
  dat = dfi[which(dfi$city == cityi & grepl(rangeStat,dfi$index)),]
  dat = na.omit(dat)
  if(dim(dat)[1]>0){
    plot(as.numeric(dat$year)-1, dat$value, main=cityi,xlab='year',ylab=dfname,pch=1, xlim=c(1985,2017))
  } else {
    plot(1:5,1:5,main=cityi,xlab='year',ylab=dfname,cex=0)
  }
}

Outliers = function(cityi0, yeari0, valuei0){
  dat0 = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
  dat0[order(dat0$year),2:5]
  dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index) & dfi$year %in% yeari0),]$value = valuei0
  dfi
}

dfi = Outliers('玉溪市', 2017, NA)
dfi = Outliers('齐齐哈尔市', c(2008:2009), NA)
dfi = Outliers('珠海市', c(1996:2000), NA)
dfi = Outliers('汕尾市', 1999, NA)
dfi = Outliers('韶关市', 2009, NA)
dfi = Outliers('云浮市', 2015, NA)
dfi = Outliers('湛江市', c(2006,2010), NA)
dfi = Outliers('河源市', 2010, NA)
dfi = Outliers('重庆市', c(2009:2010), NA)
dfi = Outliers('日照市', 2014, NA)
dfi = Outliers('咸阳市', 2012, 2169)
dfi = Outliers('长春市', 1985:1987, NA)
dfi = Outliers('白山市', 2007, 530)
dfi = Outliers('白城市', 2006, 1000)
dfi = Outliers('娄底市', 2006, 1600)
dfi = Outliers('白山市', 2007, 530)
dfi = Outliers('呼和浩特市', 2010:2012, 6000)
dfi = Outliers('秦皇岛市', 2015, 5700)
dfi = Outliers('北海市', 2015, 2454)
dfi = Outliers('河池市', 2004, 453)
dfi = Outliers('梧州市', 2002, 1838)

cityi0 = '梧州市'
dat0 = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
dat0[order(dat0$year),2:5]


assign(dfname, dfi)
eval(parse(text=paste0('save(',dfname,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/",dfname,".Rdata')")))


################################################
################################################

###批量画图

rangeStat = '市辖区'
dfname = 'Area'
dfi = get(dfname)
citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)
citypre = subset(citylist, citylist$Administrative_level != 'county')$City_ch
for (n in 1:15){
  png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/Ytemporal/',dfname,'/', dfname, rangeStat,n,'.png'),width=1600,height=1080, units='px',res=200)
  par(mfrow=c(4,5), mar=rep(2,4))
  for (cityi in citypre[(n*20-19):(n*20)]){
    dat = dfi[which(dfi$city == cityi & grepl(rangeStat,dfi$index)),]
    dat = na.omit(dat)
    if(dim(dat)[1]>0){
      plot(as.numeric(dat$year)-1, dat$value, main=cityi,xlab='year',ylab=dfname,cex=0.8,pch=19, xlim=c(1985,2017))
    } else {
      plot(1:5,1:5,main=cityi,xlab='year',ylab=dfname,cex=0)
    }
  }
  dev.off()
}
dev.off()