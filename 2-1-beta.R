library(ggplot2)
#library(smatr)
#library(tls)
#library(deming)

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

POP = POP[which(POP$year!=1995),]
# datname = ls()[!grepl('rdat',ls())]
# for (i in 1:length(datname)){
	# a = get(datname[i])
	# a$value[which(a$value==0)] = NA
	# a = a[with(a, order(index,city,year)),]
	# assign(datname[i], a)
	# eval(parse(text = paste0("save(",datname[i], ",file='",datname[i],".Rdata')")))
# }


# horizontal 是同一年份cross-city的 scaling，要 for (yi in year){} 【优先】
# longtitudinal 是同一个城市 不同年份的 scaling，要 for (ci in city){}

IndexYearPics = function(dfname, rangeStat, cityRequest, ylabtext, titletext){
	dfi = get(dfname)
	dat = dfi[which(dfi$city %in% cityRequest & grepl(rangeStat,dfi$index)),]
	p = ggplot(data=dat, aes(x=year, y=value, colour=city)) + 
		geom_point(size = 2.2) +
		geom_line(size = 1) +
		labs(x = 'Year(1985-2017)', y=ylabtext, title=titletext)+
		theme(axis.title = element_text(size=18))
	return(p)
}

# 一张PPT展示中国城市化程度，第二、三产业GDP占比； 市辖区面积/建成区面积； 城市人口占比
# a = c('上海市', '深圳市', '杭州市','太原市', '汕头市', '河源市', '长治市')

# dfname='POP'
# rangeStat='市辖区'
# ylab='Population / 10000 person'
# title = 'Population of Distrits Under City'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()

# dfname='POP'
# rangeStat='全市'
# ylab='Population / 10000 person'
# title = 'Population of Cities'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()

# dfname='GDP2nd'
# rangeStat='市辖区'
# ylab='Secondary Industry as Percentage to GRP / %'
# title = 'Secondary Industry as Percentage to GRP of Districts Under City'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()

# dfname='GDP2nd'
# rangeStat='全市'
# ylab='Secondary Industry as Percentage to GRP / %'
# title = 'Secondary Industry as Percentage to GRP of Cities'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()

# dfname='GDP3rd'
# rangeStat='市辖区'
# ylab='Tertiary Industry as Percentage to GRP / %'
# title = 'Tertiary Industry as Percentage to GRP of Districts Under City'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()

# dfname='GDP3rd'
# rangeStat='全市'
# ylab='Tertiary Industry as Percentage to GRP / %'
# title = 'Tertiary Industry as Percentage to GRP of Cities'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()

# dfname='GDP23'
# rangeStat='市辖区'
# ylab='Percentage to GRP / %'
# title = 'Secondary & Tertiary Industry as Percentage to GRP of Districts Under City'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()

# dfname='GDP23'
# rangeStat='全市'
# ylab='Percentage to GRP / %'
# title = 'Secondary & Tertiary Industry as Percentage to GRP of Cities'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()


# dfname='AreaBuilt'
# rangeStat='建成区'
# ylab='Built-up Area / sq.km'
# title = 'Built-up Area of Administrative region'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()

# dfname='Area'
# rangeStat='市辖区'
# ylab='Area / sq.km'
# title = 'Built-up Area of Districts Under City'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()

# dfname='Area'
# rangeStat='全市'
# ylab='Area / sq.km'
# title = 'Built-up Area of Cities'
# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',dfname,rangeStat,'.png'),width=18,height=15, units='cm',res=150)
# p1 = IndexYearPics(dfname, rangeStat, a, ylab, title)
# p1
# dev.off()


# scaling; 城市面积和户籍人口从1985-2017变化都不大，longitude先放着
# horizontal scaling

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
modelname = 'OLS'
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
		xdfname = 'POP'
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



# ############## Deming
# sumlmHorizontal = data.frame()
# modelname = 'Deming'
# rangeStatList = c('市辖区', 'Districts')
# for (yeari in 1985:2017){
	# Beta = vector()
	# BetaLower = vector()
	# BetaUpper = vector()
	# Intercept = vector()
	# Observation = vector()
	# for (yi in 1:length(dflist)){
		# xdfname = 'POP'
		# ydfname = dflist[yi]
		# rangeStat = rangeStatList[1]
		# year = yeari
		# xdf = get(xdfname)
		# ydf = get(ydfname)
		# if (rangeStat=='建成区'){
			# ORII = xdf[grepl('市辖区', xdf$index) & xdf$year==year,]
		# }else{
			# ORII = xdf[grepl(rangeStat, xdf$index) & xdf$year==year,]
		# }
		# CORR = ydf[grepl(rangeStat, ydf$index) & ydf$year==year,]
		# cordf = SearchCorValue(ORII, CORR)
		# if (sum(is.na(cordf))>=dim(cordf)[1]){
			# Beta[yi] = NA
			# Intercept[yi] = NA
			# BetaLower[yi]=NA
			# BetaUpper[yi]=NA
			# Observation[yi]=NA
		# }else{
			# #data = data.frame(Y = log(cordf$yindex+1), X = log(cordf$xindex+1))
			# #fdeming = deming(Y~X,data=data,cv=T) #1.23
			# data = data.frame(Y = log(cordf$yindex), X = log(cordf$xindex))
			# fdeming = deming(Y~X,data=data) #1.23
		
			# Beta[yi] = fdeming$coefficients[2]
			# Intercept[yi] = fdeming$coefficients[1]
			# Observation[yi] = fdeming$n
			# confident = fdeming$ci
			# if (dim(confident)[1]!=2){
				# BetaLower[yi]=NA
				# BetaUpper[yi]=NA
			# }else{
				# BetaLower[yi]=confident[2,1]
				# BetaUpper[yi]=confident[2,2]
			# }

			# xlab = paste0(unique(ORII$index),' (log)')
			# ylab = paste0(unique(CORR$index),' (log)')
			# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/',rangeStatList[2],'/',year,ydfname,rangeStat,'.png'),width=15,height=15, units='cm',res=150)
			# g = ggplot(data=cordf, aes(x=log(xindex), y=log(yindex))) + 
				# geom_point(size = 2.2, colour='#336699', alpha=0.4) +
				# geom_abline(intercept=Intercept[yi], slope=Beta[yi], color='blue',size=1) + 
				# geom_abline(intercept=Intercept[yi], slope=1, colour= '#666666', alpha=0.4, size=1) +
				# geom_abline(intercept=Intercept[yi], slope=7/6, colour= '#FF6666', alpha=0.4, size=1) +
				# geom_abline(intercept=Intercept[yi], slope=5/6, colour= '#99CC66', alpha=0.4, size=1) +
				# labs(x=xlab, y=ylab, title=as.character(year))+
				# theme(text = element_text(size=18))
			# print(g)
			# dev.off()}
	# }
	# sumlm = data.frame(yIndex=paste0(dflist,rangeStat), Beta=Beta, Intercept=Intercept, BetaLower=BetaLower, BetaUpper=BetaUpper, Observation=Observation, year=yeari)
	# sumlmHorizontal = na.omit(rbind(sumlmHorizontal, sumlm))
# }
# save(sumlmHorizontal, file=paste('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/sumlmHorizontal_',rangeStatList[2],'.Rdata',sep=''))
# write.csv(sumlmHorizontal, file=paste('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/sumlmHorizontal_',rangeStatList[2],'.csv',sep=''))



# ############ SMA, standardised major axis regression
# sumlmHorizontal = data.frame()
# modelname = 'SMA'
# rangeStatList = c('市辖区', 'Districts')
# for (yeari in 1985:2017){
	# Beta = vector()
	# Intercept = vector()
	# Pvalue = vector()
	# BetaLower = vector()
	# BetaUpper = vector()
	# Rsquare = vector()
	# Observation = vector()
	# for (yi in 1:length(dflist)){
		# xdfname = 'POP'
		# ydfname = dflist[yi]
		# rangeStat = rangeStatList[1] #按照指标对应的区域更改
		# year = yeari
		# xdf = get(xdfname)
		# ydf = get(ydfname)
		# if (rangeStat=='建成区'){
			# ORII = xdf[grepl('市辖区', xdf$index) & xdf$year==year,]
		# }else{
			# ORII = xdf[grepl(rangeStat, xdf$index) & xdf$year==year,]
		# }
		# CORR = ydf[grepl(rangeStat, ydf$index) & ydf$year==year,]
		# cordf = SearchCorValue(ORII, CORR)
		# if (sum(is.na(cordf))>=dim(cordf)[1]){
			# Beta[yi] = NA
			# Intercept[yi] = NA
			# Pvalue[yi]=NA
			# BetaLower[yi]=NA
			# BetaUpper[yi]=NA
			# Rsquare[yi]=NA
			# Observation[yi]=NA
		# }else{
			# data = data.frame(Y = log(cordf$yindex), X = log(cordf$xindex))
			# fsma = sma(Y~X, data=data) #1.44,1.48
		
			# Beta[yi] = fsma$coef[[1]][2,1]
			# Intercept[yi] = fsma$coef[[1]][1,1]
			# Pvalue[yi] = fsma$pval[[1]]
			# Rsquare[yi] = fsma$r2[[1]]
			# Observation[yi] = fsma$n[[1]]
			# BetaLower[yi]=fsma$coef[[1]][2,2]
			# BetaUpper[yi]=fsma$coef[[1]][2,3]

			# xlab = paste0(unique(ORII$index),' (log)')
			# ylab = paste0(unique(CORR$index),' (log)')
			# png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/',rangeStatList[2],'/',year,ydfname,rangeStat,'.png'),width=15,height=15, units='cm',res=150)
			# g = ggplot(data=cordf, aes(x=log(xindex), y=log(yindex))) + 
				# geom_point(size = 2.2, colour='#336699', alpha=0.4) +
				# geom_abline(intercept=Intercept[yi], slope=Beta[yi], color='blue',size=1) + 
				# geom_abline(intercept=Intercept[yi], slope=1, colour= '#666666', alpha=0.4, size=1) +
				# geom_abline(intercept=Intercept[yi], slope=7/6, colour= '#FF6666', alpha=0.4, size=1) +
				# geom_abline(intercept=Intercept[yi], slope=5/6, colour= '#99CC66', alpha=0.4, size=1) +
				# labs(x=xlab, y=ylab, title=as.character(year))+
				# theme(text = element_text(size=18))
			# print(g)
			# dev.off()}
	# }
	# sumlm = data.frame(yIndex=paste0(dflist,rangeStat), Beta=Beta, Intercept=Intercept, Pvalue=Pvalue, BetaLower=BetaLower, BetaUpper=BetaUpper, Rsquare=Rsquare, Observation=Observation, year=yeari)
	# sumlmHorizontal = na.omit(rbind(sumlmHorizontal, sumlm))
# }
# save(sumlmHorizontal, file=paste('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/sumlmHorizontal_',rangeStatList[2],'.Rdata',sep=''))
# write.csv(sumlmHorizontal, file=paste('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/sumlmHorizontal_',rangeStatList[2],'.csv',sep=''))



### 开始呈现出这些beta

modelname='OLS'
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
###rangeStatList = c('建成区', 'Built', 'BetaB/')
setwd(paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname))
load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))

### temporal dynamics of each Y
#sumlmHorizontal = sumlmHorizontal[which(sumlmHorizontal$Rsquare>=0.9),]
for (yname in unique(sumlmHorizontal$yIndex)){
  dfBeta = sumlmHorizontal[sumlmHorizontal$yIndex==yname,]
  dat = dfBeta
  png(filename=paste0(rangeStatList[3],yname,'.png'),width=15,height=15, units='cm',res=150)
  p = ggplot(data=dat, aes(x=year-1, y=Beta)) + 
    geom_point(size = 2.2, colour='#FF6600') +
    geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.1, colour='#FF6600') +
    geom_hline(yintercept = c(7/6,1,5/6),alpha=0.4) +
    labs(x = 'Year(1984-2016)', y='β', title=paste0(gsub(rangeStat, '', yname),'.',rangeStatList[2])) +
    theme(text = element_text(size=18))
  print(p)
  dev.off()
}

### 4 classifications
col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
ylists = c('GDP', 'Salary', 'DepositHousehold') #'Book', 'PostTele'
ylisti = c('CityRoadArea','Hospital')  #'HospitalBerth'
yliste = c('Electricity', 'Water')
ylista = c('Area', 'AreaBuilt')
for (ylist in list(ylists, ylisti, yliste, ylista)){
  ###ylist = ylista
  ###dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist & !sumlmHorizontal$year %in% c(1989, 1995,1985)),]
  dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist),]
  dfBeta$yIndex = as.character(dfBeta$yIndex)
  png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/',ylist[1],'BetaTemporal.png'),
      width=32,height=13, units='cm',res=180)
  p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, color=yIndex,shape=yIndex)) + 
    geom_line(size=1) + geom_point(size=2) +
    geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
    geom_hline(yintercept = c(2/3,1,5/6,7/6),alpha=0.4, color=col, lwd=1.5) +
    #geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
    labs(x = 'Year(1984-2016)', y='β') +
    theme(
      text = element_text(size=18),
      panel.background = element_rect(fill = "transparent",colour = 'black'), 
      panel.grid.minor = element_line(color='azure3'), 
      panel.grid.major = element_line(color='azure3'),
      legend.key = element_rect(fill = "transparent", color = "transparent"), 
      #plot.background = element_rect(fill = "transparent",colour = NA),
      legend.title=element_blank()
    )
  print(p)
  dev.off()
}



### comparing betas of different years

#经济 #基建 #个人需要
col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','BusPassenger','DepositHousehold','GDP','Loan','Salary','PostTele','WasteWater','Bus')
infrasdf = c('CityRoadArea','Hospital','School','Cinema','Green','HospitalBerth','GreenBuilt')
needdf = c('Electricity','Water')
areadf = c('Area', 'AreaBuilt')

sumlmHorizontal$type=NA
sumlmHorizontal[sumlmHorizontal$yIndex %in% economoicdf,]$type = 'socio-economic'
sumlmHorizontal[sumlmHorizontal$yIndex %in% infrasdf,]$type = 'infrastructure'
sumlmHorizontal[sumlmHorizontal$yIndex %in% needdf,]$type = 'individual need'
sumlmHorizontal[sumlmHorizontal$yIndex %in% areadf,]$type = 'Area'

yearlist = c(1988, 1998, 2008, 2017)
dfBeta = sumlmHorizontal[sumlmHorizontal$year %in% yearlist & !is.na(sumlmHorizontal$type),]
ymina = min(dfBeta$BetaLower)
ymaxa = max(dfBeta$BetaUpper)

for (yearii in yearlist){
  dfBeta1 = dfBeta[dfBeta$year == yearii,]
  dfBeta2 = dfBeta1[order(dfBeta1$year, dfBeta1$Beta),]
  
  png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/',yearii-1,'AllTemporal.png'),
      width=17,height=15, units='cm',res=180)
  p = ggplot(data=dfBeta2, aes(x=reorder(yIndex,Beta), y=Beta, color=type)) + 
    ylim(ymina, ymaxa) +
    scale_colour_manual(values = col) +
    geom_point(size = 2.2) +
    geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.1,alpha=0.8) +
    geom_hline(yintercept = c(2/3,1,5/6,7/6),alpha=0.4, color=col, lwd=1.5) +
    labs(x = paste0('Urban attributes (', yearii-1, ')'), y='β') +
    theme(text = element_text(size=18),
          legend.title=element_blank(),
          panel.grid =element_blank(),                                             #默认主题
          panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
          legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
          #axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  print(p)
  dev.off()
}


############################### temporal dynamics of intercept
rangeStatList = c('市辖区', 'Districts', 'InterceptD/')

for (yname in unique(sumlmHorizontal$yIndex)){
  dfBeta = sumlmHorizontal[sumlmHorizontal$yIndex==yname,]
  dat = dfBeta
  png(filename=paste0(rangeStatList[3],yname,'Intercept.png'),width=15,height=15, units='cm',res=150)
  p = ggplot(data=dat, aes(x=year-1, y=Intercept)) + 
    geom_point(size = 2.2, colour='#FF6600') +
    geom_errorbar(aes(ymin=InterceptLower, ymax=InterceptUpper), width=.1, colour='#FF6600') +
    #geom_hline(yintercept = c(7/6,1,5/6),alpha=0.4) +
    labs(x = 'Year(1984-2016)', y='Intercept', title=paste0(gsub(rangeStat, '', yname),'.',rangeStatList[2])) +
    theme(text = element_text(size=18))
  print(p)
  dev.off()
}


ylists = c('GDP', 'Salary', 'DepositHousehold') #'Book', 'PostTele'
ylisti = c('CityRoadArea','Hospital')  #'HospitalBerth'
yliste = c('Electricity', 'Water')
ylista = c('Area', 'AreaBuilt')
for (ylist in list(ylists, ylisti, yliste, ylista)){
  ###ylist = ylista
  ###dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist & !sumlmHorizontal$year %in% c(1989, 1995,1985)),]
  dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist),]
  dfBeta$yIndex = as.character(dfBeta$yIndex)
  png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/',ylist[1],'InterceptTemporal.png'),
      width=32,height=13, units='cm',res=180)
  p = ggplot(data=dfBeta, aes(x=year-1, y=Intercept, color=yIndex,shape=yIndex)) + 
    geom_line(size=1) + geom_point(size=2) +
    geom_errorbar(aes(ymin=InterceptLower, ymax=InterceptUpper), width=.2, alpha=0.4) +
    #geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
    labs(x = 'Year(1984-2016)', y='Intercept') +
    theme(
      text = element_text(size=18),
      panel.background = element_rect(fill = "transparent",colour = 'black'), 
      panel.grid.minor = element_line(color='azure3'), 
      panel.grid.major = element_line(color='azure3'),
      #plot.background = element_rect(fill = "transparent",colour = NA),
      legend.title=element_blank()
    )
  print(p)
  dev.off()
}