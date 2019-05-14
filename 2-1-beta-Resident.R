library(ggplot2)

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

# horizontal 是同一年份cross-city的 scaling，要 for (yi in year){} 【优先】
# longtitudinal 是同一个城市 不同年份的 scaling，要 for (ci in city){}


### 每个指标每年一个beta
IDDELF = function(ddat){ #手动去掉一个年份多个数据的情况
  iddelf = vector()
  f= ddat[duplicated(ddat[,c(-1,-5)]),]
  for (i in 1:dim(f)[1]){
    ff = subset(ddat,ddat$city==f$city[i] & ddat$year==f$year[i] & ddat$index==f$index[i])
    if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
    }else{iddelf = c(iddelf, ff$id[1])}
  }
  ddat = ddat[which(!ddat$id %in% iddelf),]
  return(ddat)
}

SearchCorValue = function(ORI, COR){ #找到某指标对应的另一指标的值
  ORI = IDDELF(ORI)
  COR = IDDELF(COR)
  corValue = vector()
  for (i100i in 1:dim(ORI)[1]){
    cityL = COR$city==ORI$city[i100i]
    yearL = COR$year==ORI$year[i100i]
    corrr = COR[which(cityL & yearL),]
    corV = corrr$value
    if (length(corV)==0){
      corV = NA}
    if (length(corV)>1){
      print(corrr)
      corV = corV[1]}
    corValue[i100i] = corV
  }
  if (length(ORI$value)==0){
    corValueDF = data.frame(NA)
  }else{
    corValueDF = data.frame(xindex = ORI$value, yindex = corValue, city=ORI$city, year=ORI$year)
  }
  return(corValueDF)
}


dflist = gsub('.Rdata', '', dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQLforCal'))
#dflist = gsub('.Rdata', '', dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL'))

############## OLS
sumlmHorizontal = data.frame()
modelname = 'OLS_R'
#rangeStatList = c('全市', 'Total','BetaT/')
for (yeari in 1985:2017){
  Beta = vector()
  Intercept = vector()
  Pvalue = vector()
  BetaLower = vector()
  BetaUpper = vector()
  Rsquare = vector()
  Observation = vector()
  InterceptLower = vector()
  InterceptUpper = vector()
  for (yi in 1:length(dflist)){
    xdfname = 'Resident'
    ydfname = dflist[yi]
    if (!grepl('Built', ydfname)){
      rangeStatList = c('市辖区', 'Districts', 'BetaD/')
    }else{
      rangeStatList = c('建成区', 'Districts', 'BetaD/')
    }
    rangeStat = rangeStatList[1] #按照指标对应的区域更改
    year = yeari
    xdf = get(xdfname)
    delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市','哈密市','吐鲁番市')
    xdf = xdf[which(!(xdf$city %in% delcity)),]
    ydf = get(ydfname)
    if (rangeStat=='建成区'){
      ORII = xdf[grepl('市辖区', xdf$index) & xdf$year==year,]
    }else{
      ORII = xdf[grepl(rangeStat, xdf$index) & xdf$year==year,]
    }
    CORR = ydf[grepl(rangeStat, ydf$index) & ydf$year==year,]
    cordf = SearchCorValue(ORII, CORR)
    if (sum(is.na(cordf))>=dim(cordf)[1] | dim(na.omit(cordf))[1]<130){
      Beta[yi] = NA
      Intercept[yi] = NA
      Pvalue[yi]=NA
      BetaLower[yi]=NA
      BetaUpper[yi]=NA
      Rsquare[yi]=NA
      Observation[yi]=NA
      InterceptLower[yi]=NA
      InterceptUpper[yi]=NA
    }else{
      data = data.frame(Y = log(cordf$yindex), X = log(cordf$xindex))
      flm = lm(Y~X, data=data) #1.22 ##LM比GLM的AIC都小
      #fsma = sma(Y~X, data=data, robust=T) #1.44,1.48
      #fdeming = deming(Y~X,data=data,cv=T) #1.23
      
      Beta[yi] = summary(flm)$coefficients[2,1]
      Pvalue[yi] = summary(flm)$coefficients[2,4]
      Rsquare[yi] = summary(flm)$r.squared
      Intercept[yi] = summary(flm)$coefficients[1,1]
      Observation[yi] = summary(flm)$df[2] + 2
      confident = confint(flm, level=0.95)
      if (dim(confident)[1]!=2){
        BetaLower[yi]=NA
        BetaUpper[yi]=NA
        InterceptLower[yi]=NA
        InterceptUpper[yi]=NA
      }else{
        BetaLower[yi]=confident[2,1]
        BetaUpper[yi]=confident[2,2]
        InterceptLower[yi]=confident[1,1]
        InterceptUpper[yi]=confident[1,2]
      }
      
      xlab = paste0(unique(ORII$index),' (log)')
      ylab = paste0(unique(CORR$index),' (log)')
      png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/',rangeStatList[2],'/',year,ydfname,rangeStat,'.png'),width=15,height=15, units='cm',res=150)
      g = ggplot(data=cordf, aes(x=log(xindex), y=log(yindex))) + 
        geom_point(size = 2.2, colour='#336699', alpha=0.4) +
        geom_smooth(method = 'lm') + 
        geom_abline(intercept=Intercept[yi], slope=1, colour= '#666666', alpha=0.4, size=1) +
        geom_abline(intercept=Intercept[yi], slope=7/6, colour= '#FF6666', alpha=0.4, size=1) +
        geom_abline(intercept=Intercept[yi], slope=5/6, colour= '#99CC66', alpha=0.4, size=1) +
        labs(x=xlab, y=ylab, title=as.character(year))+
        theme(text = element_text(size=18))
      print(g)
      dev.off()}
  }
  sumlm = data.frame(yIndex=dflist, Beta=Beta, Intercept=Intercept, Pvalue=Pvalue, BetaLower=BetaLower, BetaUpper=BetaUpper, InterceptLower=InterceptLower, InterceptUpper=InterceptUpper, Rsquare=Rsquare, Observation=Observation, year=yeari)
  sumlmHorizontal = na.omit(rbind(sumlmHorizontal, sumlm))
}
save(sumlmHorizontal, file=paste('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/sumlmHorizontal_',rangeStatList[2],'.Rdata',sep=''))
write.csv(sumlmHorizontal, file=paste('C:/Sync/CoolGirl/Fhe/Results/', modelname,'/sumlmHorizontal_',rangeStatList[2],'.csv',sep=''))

