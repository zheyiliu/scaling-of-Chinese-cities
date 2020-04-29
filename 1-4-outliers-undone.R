library(ggplot2)
#library(mice)
library(longitudinalData)

#home = '/home/zheyi'
home = 'C:/Sync/CoolGirl/Fhe'
setwd(paste0(home,'/ecosocialData/indexSQL'))
for (rdat in dir()){load(rdat)}

citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)
citypre = subset(citylist, citylist$Administrative_level != 'county')$City_ch
citycoun = subset(citylist, citylist$Administrative_level == 'county')$City_ch
cityqu = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/ToDistrict.csv',stringsAsFactors=F)[,1]
mass = c('内蒙市', '胡南省')


dflist = gsub('.Rdata', '', dir(paste0(home,'/ecosocialData/indexSQL')))
for (yi in 1:length(dflist)){
  dfname = dflist[yi]
  df = get(dfname)
  df$year = as.integer(df$year)
  df$value = as.numeric(df$value)
  df$value[which(df$value==0)] = NA
  
  citydf = unique(df$city)
  citydel = citydf[!citydf %in% citypre & citydf %in% c(citycoun,cityqu,mass)]
  df = subset(df, !df$city %in% citydel)
  
  assign(dfname, df)
  eval(parse(text = paste0("save(",dfname, ",file='",dfname,".Rdata')")))
  print(yi)
}


  
rangeStat = '市辖区'
dfname = 'Area'
for (yi in 1:length(dflist)){
  dfname = dflist[yi]
  dfi = get(dfname)
  dfi = dfi[which(grepl('市',dfi$city)),]
  
  iddelf = vector()
  f= dfi[duplicated(dfi[,c(-1,-5)]),]
  for (i in 1:dim(f)[1]){
    ff = subset(dfi,dfi$city==f$city[i] & dfi$year==f$year[i] & dfi$index==f$index[i])
    if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
    }else{iddelf = c(iddelf, ff[order(ff$value),]$id[2])}
  }
  dfi = dfi[which(!dfi$id %in% iddelf),]
  
  # citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)
  # citypre = subset(citylist, citylist$Administrative_level != 'county')$City_ch
  # n = 1
  # par(mfrow=c(4,5))
  # for (cityi in citypre[(n*20-19):(n*20)]){
  #   dat = dfi[which(dfi$city == cityi & grepl(rangeStat,dfi$index)),]
  #   dat = na.omit(dat)
  #   if(dim(dat)[1]>0){
  #     plot(as.numeric(dat$year)-1, dat$value/10000, main=cityi,xlab='year',ylab=dfname,pch=1, xlim=c(1985,2017))
  #   } else {
  #     plot(1:5,1:5,main=cityi,xlab='year',ylab=dfname,cex=0)
  #   }
  # }
  
  #################################################
  # Outliers = function(cityi0, yeari0, valuei0){
  #   dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index) & dfi$year %in% yeari0),]$value = valuei0
  #   dfi
  # }
  
  #dfi = Outliers('宁波市', 2003, NA)
  ##################################################
  #t = 3.8
  t = 1.8
  outs = data.frame()
  imps = data.frame()
  rarecity = vector()
  xxx = c(0)
  for (iii in 2:8) {
    for (cityi0 in citypre){
      #cityi0='安康市'
      dati = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
      dat0 = na.omit(dati)
      if (dim(dat0)[1]<=6){
        rarecity = c(rarecity, cityi0)
        next
      }
      for (ni in 1:4){
        #m = mean(head(dat0)$value[-ni])
        #s = sd(head(dat0)$value[-ni])
        m = mean(head(dat0)$value)
        s = sd(head(dat0)$value)
        if (dat0$value[ni] > m + t*s | dat0$value[ni] < m - t*s){
          outs = rbind(outs, dat0[ni,])
          dfi[which(dfi$id == dat0[ni,]$id),]$value = NA
        }
      }
      for (nj in 5:(dim(dat0)[1]-3)){
        #m = mean(dat0[c((nj-3),(nj-2),(nj-1),(nj+1),(nj+2)),]$value)
        #s = sd(dat0[c((nj-3),(nj-2),(nj-1),(nj+1),(nj+2)),]$value)
        m = mean(dat0[c((nj-3),(nj-2),(nj-1),nj,(nj+1),(nj+2)),]$value)
        s = sd(dat0[c((nj-3),(nj-2),(nj-1),nj,(nj+1),(nj+2)),]$value)
        if (dat0$value[nj] > m + t*s | dat0$value[nj] < m - t*s){
          outs = rbind(outs, dat0[nj,])
          dfi[which(dfi$id == dat0[nj,]$id),]$value = NA
        }
      }
      for (nk in (dim(dat0)[1]-2):(dim(dat0)[1])){
        nki = nk - dim(dat0)[1] + 6
        #m = mean(tail(dat0)$value[-nki])
        #s = sd(tail(dat0)$value[-nki])
        m = mean(tail(dat0)$value)
        s = sd(tail(dat0)$value)
        if (dat0$value[nk] > m + t*s | dat0$value[nk] < m - t*s){
          outs = rbind(outs, dat0[nk,])
          dfi[which(dfi$id == dat0[nk,]$id),]$value = NA
        }
      }
    }
    xxx[iii] = dim(outs)[1]
    if (xxx[iii]==xxx[iii-1]) {
      break
    }
  }
  
  ###### imputation #####    
  for (cityi0 in citypre){
    #cityi0='安康市'
    dati = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
    dat0 = na.omit(dati)
    
    forclus = data.frame(year=1985:2016)
    x0 = merge(forclus, dat0, by='year', all=T)[,c('year','value')]

    # #methodimp = 'norm'
    # methodimp = 'pmm'
    
    # set.seed(111)
    # imp <- mice(x0, print=F, method=methodimp)
    # 
    # x3 = complete(imp,action=2)
    # #stripplot(imp, value~.imp, pch=20, cex=2)
    # plot(x0$year, x0$value,cex=2)
    # points(x3$year,x3$value,col=2,cex=1)
    # points(x3[1,],x3[2,],col=2,cex=1)
    
    if (nrow(dat0) < 20){
      next
    }
    
    x1 = t(as.matrix(x0))
    x3 = imputation(x1, method='linearInterpol.bisector', lowerBound = 0)
    x4 = as.data.frame(t(x3))
    x5 = merge(x4, dat0, by='year', all=T)
    dat1 = subset(x5, is.na(x5$city))
    
    city = rep(cityi0,(dim(dat1)[1]))
    index = rep(dat0$index[1],(dim(dat1)[1]))
    value = dat1$value.x
    year = dat1$year
    let = letters[sample(26,length(city),replace=T)]
    id = paste(city, year, index, let, sep='-')
    datinew = data.frame(id=id, city=city, year=year, index=index, value=value,stringsAsFactors = FALSE)
    imps = rbind(imps, datinew)
    dfi = rbind(dfi, datinew)
  }
  
  
  
  # cityi0 = '呼伦贝尔市'
  # dat0 = na.omit(dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),])
  # dat0[order(dat0$year),2:5]
  # outs[which(outs$city == cityi0),]
  #dat0 = na.omit(dfi[which(grepl(rangeStat,dfi$index)),])
  
  assign(dfname, dfi)
  dfnameout = paste0('OUTS',dfname)
  dfnameimp = paste0('IMPS',dfname)
  assign(dfnameout, outs)
  assign(dfnameimp, imps)
  eval(parse(text=paste0('save(',dfname,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/",dfname,".Rdata')")))
  eval(parse(text=paste0('save(',dfnameout,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/outliers/",dfnameout,".Rdata')")))
  eval(parse(text=paste0('save(',dfnameimp,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/imputations/",dfnameimp,".Rdata')")))
  write.csv(rarecity, file=paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/outliers/rarecity',dfname,'.csv'))
}


################################################
################################################

###批量画图

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}
dflist = gsub('.Rdata', '', dir('C:/Sync/CoolGirl/Fhe/Results/Ytemporal'))

for (yi in 1:length(dflist)){
  dfname = dflist[yi]
  dir.create(dfname)
  #rangeStat = '市辖区'
  #dfname = 'Area'
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
        plot(as.numeric(dat$year)-1, dat$value/100, main=cityi,xlab='year',ylab=dfname,cex=0.8,pch=19, xlim=c(1985,2017))
      } else {
        plot(1:5,1:5,main=cityi,xlab='year',ylab=dfname,cex=0)
      }
    }
    dev.off()
  }
  #dev.off()
}

################################################################
################################################################

###查看异常年份

dflist = gsub('.Rdata', '', dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL'))
rangeStat = '市辖区'
#rangeStat = '建成区'
dfname = dflist[24]

dfi = get(dfname)
dfi = dfi[which(grepl('市',dfi$city)),]
delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市','哈密市','吐鲁番市')
dfi = dfi[which(!(dfi$city %in% delcity)),]

dfyear0 = na.omit(dfi[which(grepl(rangeStat,dfi$index)),])
dfyear1 = split(dfyear0, dfyear0$year)
dfyear = split(dfyear0$value, dfyear0$year)
a = vapply(dfyear, mean, numeric(1))
dfa = data.frame(Wmean=a, year=as.numeric(names(a)))
plot(dfa$year,dfa$Wmean)
vapply(dfyear,length,numeric(1))

#dfi[which(grepl(rangeStat,dfi$index) & dfi$year %in% 1995),]$value = dfi[which(grepl(rangeStat,dfi$index) & dfi$year %in% 1992),]$value*10000
