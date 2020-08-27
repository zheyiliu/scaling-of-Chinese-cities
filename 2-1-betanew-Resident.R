library(ggplot2)
#library(smatr)
#library(tls)
#library(deming)

#home = '/home/zheyi'
home = 'C:/Sync/CoolGirl/Fhe'
setwd(paste0(home,'/ecosocialData/SuperIndex'))
for (rdat in dir()){load(rdat)}

dflist0 = gsub('.Rdata', '', dir(paste0(home,'/ecosocialData/SuperIndex'))) #全部都需要预处理
#dflist = grep('POP|\\d', dflist0, invert=T, value=T) #用来算OLS的
dflist = dflist0[!dflist0 %in% c('POPResident', 'POPResidentOrigin')]


for (yi in 1:length(dflist0)){
	dfname = dflist0[yi]
	df = get(dfname)
	df$year = as.integer(df$year)
	df$value = as.numeric(df$value)
	df$value[which(df$value==0)] = NA
	assign(dfname, df)
	#eval(parse(text = paste0('return(',dfname, ')')))
}

yearrange = 1985:2018
adminchange = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminchange.csv',header=T,stringsAsFactors=F)



s=Sys.time()

IDDELF = function(ddat){
  #ddat = GDP
  ddat = na.omit(ddat)
  ddat = ddat[!duplicated(ddat),]
  iddelf = vector()
  wield = paste0(ddat[,2], ddat[,3], ddat[,4])
  f= ddat[duplicated(wield),]
  if (nrow(f)>0){
	for (i in 1:dim(f)[1]){
		ff = subset(ddat,ddat$city==f$city[i] & ddat$year==f$year[i] & ddat$index==f$index[i])
	  iddelf = c(iddelf, ff$id[-1])
	}
  ddat = ddat[which(!ddat$id %in% iddelf),]
  }
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
      corV = corV[!is.na(corV)][1]}
    corValue[i100i] = corV
  }
  if (length(ORI$value)==0){
    corValueDF = data.frame(NA)
  }else{
    corValueDF = data.frame(xindex = ORI$value, yindex = corValue, city=ORI$city, year=ORI$year)
  }
  return(corValueDF)
}


############## OLS
#modelname = 'OLS_R'
#rangeStatList = c('全市', 'Total','BetaT/')
modelname = 'OLS_DJS_sxq_R'
rangeStatList = c('市辖区', 'Districts', 'BetaD/', 'FigD/')
WithoutXJS = TRUE
sumlmHorizontal = data.frame()


dir.create(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/"),showWarnings = F)
setwd(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/"))
file.remove(dir())
  
dir.create(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/",rangeStatList[2]),showWarnings = F)
setwd(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/",rangeStatList[2]))
file.remove(dir())
  
dir.create(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/",rangeStatList[3]),showWarnings = F)
setwd(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/",rangeStatList[3]))
file.remove(dir())

dir.create(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/",rangeStatList[4]),showWarnings = F)
setwd(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/",rangeStatList[4]))
file.remove(dir())
  
  

for (yeari in yearrange){
    Beta = vector()
    Intercept = vector()
    Pvalue = vector()
    BetaLower = vector()
    BetaUpper = vector()
    Rsquare = vector()
    Observation = vector()
	ObservationAll = vector()
    InterceptLower = vector()
    InterceptUpper = vector()
    for (yi in 1:length(dflist)){
      print(c(yeari, yi))
      xdfname = 'POPResident'
      ydfname = dflist[yi]
      rangeStat = rangeStatList[1] #按照指标对应的区域更改
      year = yeari
      xdf = get(xdfname)
	  delcity = '三沙市'
	  #delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市','哈密市','吐鲁番市')
	  xdf = xdf[which(!(xdf$city %in% delcity)),]

	  
	  ### 操作有没有县级市
	  if (WithoutXJS){
		xdf = xdf[!grepl('县级',xdf$admin),]
		adminy = yeari
		adminyi = subset(adminchange, adminchange$yeari==adminy & !grepl('县级',adminchange$admin))
	  } else {
		adminy = yeari
		adminyi = subset(adminchange, adminchange$yeari==adminy)
	  }
      ydf = get(ydfname)
	  
	  ### 操作有没有市辖区，并找到对应POP年份的Y指标数值
      ORII = xdf[grepl(rangeStat, xdf$index) & xdf$year==year,]
      CORR = ydf[grepl(rangeStat, ydf$index) & ydf$year==year,]
      cordf = SearchCorValue(ORII, CORR)
  
	  ### 
      if (sum(is.na(cordf))>=dim(cordf)[1] | nrow(na.omit(cordf)) < (1/3 * nrow(ORII))){
        Beta[yi] = NA
        Intercept[yi] = NA
        Pvalue[yi]=NA
        BetaLower[yi]=NA
        BetaUpper[yi]=NA
        Rsquare[yi]=NA
        Observation[yi]=nrow(na.omit(cordf))
		ObservationAll[yi]=nrow(adminyi)
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
        Observation[yi] = nrow(na.omit(cordf))
		ObservationAll[yi] = nrow(adminyi)
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
	}
  }

  sumlm = data.frame(yIndex=dflist, Beta=Beta, Intercept=Intercept, Pvalue=Pvalue, 
				   BetaLower=BetaLower, BetaUpper=BetaUpper, 
				   InterceptLower=InterceptLower, InterceptUpper=InterceptUpper, 
				   Rsquare=Rsquare,
				   Observation=Observation,
				   ObservationAll=ObservationAll,
				   year=yeari)
  sumlmHorizontal = rbind(sumlmHorizontal, sumlm)
}
sumlmHorizontal = sumlmHorizontal[sumlmHorizontal$Observation!=0,]
save(sumlmHorizontal, file=paste(home,'/Results/',modelname,'/sumlmHorizontal_',rangeStatList[2],'.Rdata',sep=''))
write.csv(sumlmHorizontal, file=paste(home,'/Results/', modelname,'/sumlmHorizontal_',rangeStatList[2],'.csv',sep=''))

#load(file=paste(home,'/Results/',modelname,'/sumlmHorizontal_',rangeStatList[2],'.Rdata',sep=''))
#sumlmHorizontal[which(sumlmHorizontal$Observation<=(2/3*sumlmHorizontal$ObservationAll)),c('Beta','Intercept','Rsquare')] = NA
#save(sumlmHorizontal, file=paste(home,'/Results/',modelname,'/2-3_sumlmHorizontal_',rangeStatList[2],'.Rdata',sep=''))
#write.csv(sumlmHorizontal, file=paste(home,'/Results/', modelname,'/2-3_sumlmHorizontal_',rangeStatList[2],'.csv',sep=''))
