library(ggplot2)

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}
dflist = gsub('.Rdata', '', dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQLforCal'))

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
			#print(corrr)
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

Threshold = function(Method='POP',rangeStatList=c('市辖区','Districts','BetaD/'),rangeyear=1985:2017,rangey=dflist,reverse=F,pix=100){
  sumlmHorizontal = data.frame()
  for (yeari in rangeyear){
    rangeStat = rangeStatList[1]
    POPdfocal = subset(POPdensity, grepl(rangeStat,POPdensity$index) & POPdensity$year==yeari)
    POPfocal = subset(POP, grepl(rangeStat, POP$index) & POP$year==yeari)
    if (Method=='POP'){
      tsh = POPfocal
    } else if (Method=='PD'){
      tsh = POPdfocal
    } else {print('POP or PD')}
    a = na.omit(tsh$value)
    if (length(a) != 0){
      if (reverse){
        piseq = seq(quantile(a, probs=0.9), quantile(a, probs=0.1), length.out = pix)
        fn = paste0(Method, 'threshold', 'reverse')
      }else{
        piseq = seq(quantile(a, probs=0.1), quantile(a, probs=0.9), length.out = pix)
        fn = paste0(Method, 'threshold')
      }
      for (p in piseq){
        if (reverse){
          cityset = subset(tsh, tsh$value <= p)$city
        }else{
          cityset = subset(tsh, tsh$value >= p)$city
        }
        POPset = subset(POPfocal, POPfocal$city %in% cityset)
        Beta = vector()
        Intercept = vector()
        Pvalue = vector()
        BetaLower = vector()
        BetaUpper = vector()
        Rsquare = vector()
        Observation = vector()
        for (yi in 1:length(rangey)){
          xdfname = 'POPset'
          ydfname = rangey[yi]
          year = yeari
          xdf = get(xdfname)
          ydf = get(ydfname)
          if (rangeStat=='建成区'){
            ORII = xdf[grepl('市辖区', xdf$index) & xdf$year==year,]
          }else{
            ORII = xdf[grepl(rangeStat, xdf$index) & xdf$year==year,]
          }
          CORR = ydf[grepl(rangeStat, ydf$index) & ydf$year==year,]
          cordf = SearchCorValue(ORII, CORR)
          if (sum(is.na(cordf))>=dim(cordf)[1] | dim(na.omit(cordf))[1]<20){
            Beta[yi] = NA
            Intercept[yi] = NA
            Pvalue[yi]=NA
            BetaLower[yi]=NA
            BetaUpper[yi]=NA
            Rsquare[yi]=NA
            Observation[yi]=NA
          }else{
            data = data.frame(Y = log(cordf$yindex), X = log(cordf$xindex))
            flm = lm(Y~X, data=data) #1.22
            #fsma = sma(Y~X, data=data, robust=T) #1.44,1.48
            #fdeming = deming(Y~X,data=data,cv=T) #1.23
            
            Beta[yi] = summary(flm)$coefficients[2,1]
            Pvalue[yi] = summary(flm)$coefficients[2,4]
            Rsquare[yi] = summary(flm)$r.squared
            Observation[yi] = summary(flm)$df[2] + 2
            Intercept[yi] = summary(flm)$coefficients[1,1]
            confident = confint(flm, level=0.95)
            if (dim(confident)[1]!=2){
              BetaLower[yi]=NA
              BetaUpper[yi]=NA
            }else{
              BetaLower[yi]=confident[2,1]
              BetaUpper[yi]=confident[2,2]
            }
          }
        }
        sumlm = data.frame(yIndex=paste0(dflist,rangeStat), Beta=Beta, Intercept=Intercept, Pvalue=Pvalue, BetaLower=BetaLower, BetaUpper=BetaUpper, Rsquare=Rsquare, Observation=Observation, year=yeari, threshold=p)
        sumlmHorizontal = na.omit(rbind(sumlmHorizontal, sumlm))
        print(p)
      }
    }
    print(yeari)
  }
  save(sumlmHorizontal, file=paste('C:/Sync/CoolGirl/Fhe/Results/',fn,'/sumlmThreshold_',rangeStatList[2],'.Rdata',sep=''))
  write.csv(sumlmHorizontal, file=paste('C:/Sync/CoolGirl/Fhe/Results/',fn,'/sumlmThreshold_',rangeStatList[2],'.csv',sep=''))
  for (yname in unique(sumlmHorizontal$yIndex)){
    for (yeari in unique(sumlmHorizontal$year)){
      dfBeta = sumlmHorizontal[sumlmHorizontal$yIndex==yname & sumlmHorizontal$year==yeari,]
      dat = dfBeta
      png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',fn,'/',rangeStatList[3],yname, yeari,'.png'),width=15,height=15, units='cm',res=150)
      pic = ggplot(data=dat, aes(x=threshold, y=Beta)) + 
        geom_point(size = 2.2, colour='#FF6600') +
        geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.1, colour='#FF6600') +
        geom_hline(yintercept = c(7/6,1,5/6),alpha=0.4) +
        labs(x = 'Population Threshold (p < pi)', y='β', title=paste0(gsub(rangeStatList[1], '', yname),'.',rangeStatList[2])) +
        theme(text = element_text(size=18))
      print(pic)
      dev.off()
    }
  }
  return(sumlmHorizontal)
}




############## 人口，排除大城市

sumPOP = Threshold()
sumPOPr = Threshold(reverse=T)
sumPOPd = Threshold(Method='PD')
sumPOPdr = Threshold(Method='PD',reverse=T)



### 开始呈现出这些beta
economoicdf = c('DepositHousehold市辖区','GDP市辖区','Loan市辖区','Salary市辖区')
infrasdf = c('CityRoadArea市辖区','Hospital市辖区','School市辖区')
infrigiddf = c('Bus市辖区','HospitalBerth市辖区')
energydf = c('Electricity市辖区','Water市辖区')
othersdf = c('Book市辖区','Green市辖区')
interactdf = 'BusPassenger市辖区'
pollutiondf = 'WasteWater全市'


setwd(paste0('C:/Sync/CoolGirl/Fhe/Results/POPthresholdReverse'))
load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))

for (yname in unique(sumlmHorizontal$yIndex)){
  for (yeari in unique(sumlmHorizontal$year)){
    dfBeta = sumlmHorizontal[sumlmHorizontal$yIndex==yname & sumlmHorizontal$year==yeari,]
    dat = dfBeta
    png(filename=paste0(rangeStatList[3],yname, yeari,'.png'),width=15,height=15, units='cm',res=150)
    pic = ggplot(data=dat, aes(x=threshold, y=Beta)) + 
      geom_point(size = 2.2, colour='#FF6600') +
      geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.1, colour='#FF6600') +
      geom_hline(yintercept = c(7/6,1,5/6),alpha=0.4) +
      labs(x = 'Population Threshold (p < pi)', y='β', title=paste0(gsub(rangeStatList[1], '', yname),'.',rangeStatList[2])) +
      theme(text = element_text(size=18))
    print(pic)
    dev.off()
  }
}


setwd(paste0('C:/Sync/CoolGirl/Fhe/Results/PDthresholdReverse'))
load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))

for (yname in unique(sumlmHorizontal$yIndex)){
	for (yeari in unique(sumlmHorizontal$year)){
		dfBeta = sumlmHorizontal[sumlmHorizontal$yIndex==yname & sumlmHorizontal$year==yeari,]
		dat = dfBeta
		png(filename=paste0(rangeStatList[3],yname, yeari,'.png'),width=15,height=15, units='cm',res=150)
		pic = ggplot(data=dat, aes(x=threshold, y=Beta)) + 
		geom_point(size = 2.2, colour='#FF6600') +
		geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.1, colour='#FF6600') +
		geom_hline(yintercept = c(7/6,1,5/6),alpha=0.4) +
		labs(x = 'Population Density Threshold (d < di)', y='β', title=paste0(gsub(rangeStatList[1], '', yname),'.',rangeStatList[2])) +
		theme(text = element_text(size=18))
	print(pic)
	dev.off()
	}
}
