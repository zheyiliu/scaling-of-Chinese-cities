##################################
### 开始呈现出这些beta
library(ggplot2)

s=Sys.time()

#for (modelname in c('OLS_DJS_sxq', 'OLS_DJS_qs', 'OLS_XJS_sxq', 'OLS_XJS_qs')){
for (modelname in c('OLS_DJS_sxq')){
  if (modelname == 'OLS_DJS_sxq'){
    modelname='OLS_DJS_sxq'
    rangeStatList = c('市辖区', 'Districts', 'BetaD/', 'FigD/')
    WithoutXJS = TRUE
	
  } else if(modelname == 'OLS_XJS_sxq'){
    rangeStatList = c('市辖区', 'Districts', 'BetaD/', 'FigD/')
    WithoutXJS = FALSE
	
  } else if(modelname == 'OLS_DJS_qs'){
    rangeStatList = c('全市', 'Total', 'BetaT/', 'FigT/')
    WithoutXJS = TRUE
	
  } else if(modelname == 'OLS_XJS_qs'){
    rangeStatList = c('全市', 'Total', 'BetaT/', 'FigT/')
    WithoutXJS = FALSE
  }
  
	#modelname='OLS_DJS_sxq'
	#rangeStatList = c('市辖区', 'Districts', 'BetaD/', 'FigD/')
	home = 'C:/Sync/CoolGirl/Fhe'
	setwd(paste0(home,'/Results/',modelname))
	load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))
	#sumlmHorizontal[sumlmHorizontal$yIndex=='GDP' & sumlmHorizontal$year < 1991,c('Beta','Intercept','Rsquare')] = NA
	#sumlmHorizontal[sumlmHorizontal$yIndex=='Doctor' & sumlmHorizontal$year == 1999,c('Beta','Intercept','Rsquare')] = NA
	#sumlmHorizontal[sumlmHorizontal$yIndex=='Green' & sumlmHorizontal$year == 1992,c('Beta','Intercept','Rsquare')] = NA


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
		labs(x = 'Year(1984-2017)', y='β', title=paste0(gsub(rangeStatList[1], '', yname),'.',rangeStatList[2])) +
		theme(text = element_text(size=18))
	  print(p)
	  dev.off()
	}



	### comparing betas of different years
	
	
	# #经济 #基建 #个人需要
	# col = c("#619CFF", "#00BA38", "grey52", "#F8766D")
	# economoicdf = c('DepositHousehold','Electricity','FixedAssets','GDP','Loan','Retail','Salary','WasteWater','Water') 
	# #最先扔掉的'Passenger', 'BusPassenger', 'Cinema', 'PostTele', 'Crash','Fire','Deposit'
	# infrasdf = c('UrbanRoadArea','Sewage.Length','Green')
	# #'Gas.Length','PavedRoad.Length','WaterSupply.Length','Bus'
	# needdf = c('LivingSpace','Doctor','Hospital','HospitalBerth','PrimarySchool','PrimaryTeacher')
	# #'ElectricityResident','WaterResident','School'
	# areadf = c('Area', 'AreaBuilt')
	
	

	# #经济 #基建 #个人需要
	# col = c("#619CFF", "#00BA38", "grey52", "#F8766D")
	# economoicdf = c('DepositHousehold','FixedAssets','GDP','Retail','Salary') 
	# # 'Electricity','Water','ForeignCapital','Loan'
	# infrasdf = c('UrbanRoadArea','Green')
	# #'ElectricityResident','WaterResident','Sewage.Length','WaterSupply.Length','Bus'
	# needdf = c('Doctor','Hospital','HospitalBeds','PrimarySchool','PrimaryTeacher')
	# #'School','PrimaryStudent','Employee','LivingSpace','ExpendEdu','ExpendScience'
	# areadf = c('Area', 'AreaBuilt')
	
	
	#经济 #基建 #个人需要
	col = c("#F8766D", "grey52", "#619CFF", "#00BA38") #红，灰，蓝，绿
	mark = c(7/6, 1, 5/6, 2/3)
	economoicdf = c('GDP','DepositHousehold','Salary','Retail')
	#'Loan'数据少不要了 #,'Water'R2奇怪 #,'FixedAssets','Electricity'形状一般，不要也罢 #'ForeignCapital'R2低
	needdf = c('Hospital','HospitalBeds','PrimarySchool','PrimaryTeacher')
	#,'Employee','ExpendEdu','ExpendScience','PrimaryStudent') #Doctor波动大, 'LivingSpace'数据少直接不要了算啦，聚类省去插补步骤 ,'School'
	infrasdf = c('UrbanRoadArea','Green')
	#,'Bus','WaterSupply.Length') #'Sewage.Length'数据少不要了？#,'WaterResident'R2奇怪 #,'ElectricityResident'有点牵强
	areadf = c('Area', 'AreaBuilt')
	

	sumlmHorizontal$type=NA
	sumlmHorizontal[sumlmHorizontal$yIndex %in% needdf,]$type = 'Basic Services'
	sumlmHorizontal[sumlmHorizontal$yIndex %in% infrasdf,]$type = 'Infrastructure'
	sumlmHorizontal[sumlmHorizontal$yIndex %in% areadf,]$type = 'Land Use'
	sumlmHorizontal[sumlmHorizontal$yIndex %in% economoicdf,]$type = 'Socio-economic'
	
	
	sumlmHorizontal <- within(sumlmHorizontal, yIndex <- factor(yIndex, levels = c(economoicdf, needdf, infrasdf, areadf)))
	with(sumlmHorizontal, levels(yIndex))
	
	sumlmHorizontal <- within(sumlmHorizontal, type <- factor(type, levels = c('Socio-economic', 'Basic Services', 'Infrastructure', 'Land Use')))
	with(sumlmHorizontal, levels(type))
	
	save(sumlmHorizontal, file=paste(home,'/Results/',modelname,'/sumlmHorizontal_type_',rangeStatList[2],'.Rdata',sep=''))
	write.csv(sumlmHorizontal, file=paste(home,'/Results/', modelname,'/sumlmHorizontal_type_',rangeStatList[2],'.csv',sep=''))

	#yearlist = c(1985,1993,2002,2010,2018)
	yearlist = c(1985, 1996, 2007, 2018)
	dfBeta = sumlmHorizontal[sumlmHorizontal$year %in% yearlist & !is.na(sumlmHorizontal$type),]
	dfBeta = na.omit(dfBeta)
	ymina = min(dfBeta$BetaLower)
	ymaxa = max(dfBeta$BetaUpper)

	for (yearii in yearlist){
	  dfBeta1 = dfBeta[dfBeta$year == yearii,]
	  dfBeta2 = dfBeta1[order(dfBeta1$year, dfBeta1$Beta),]
	  
	  png(filename=paste0(home,'/Results/',modelname,'/',rangeStatList[4],yearii-1,'AllTemporal1.png'),
		  width=10,height=13, units='cm',res=180)
	  p = ggplot(data=dfBeta2, aes(x=reorder(yIndex,Beta), y=Beta, color=type)) + 
		scale_y_continuous(limits=c(ymina, ymaxa), breaks=round(seq(ymina,ymaxa,by=0.2),1)) +
		scale_colour_manual(values = col) +
		geom_point(size = 2.2) +
		geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.1,alpha=0.8) +
		geom_hline(yintercept = mark,alpha=0.4, color=col, lwd=1.5) +
		labs(x = NULL, y='β') +
		theme(text = element_text(size=18),
			  legend.position = c(0.68,0.2),
			  legend.title=element_blank(),
			  panel.grid =element_blank(),                                             #默认主题
			  panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
			  legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
			  #axis.line = element_line(colour = "black"),
			  axis.text.x = element_text(angle = 45, vjust=1.05, hjust=1))
	  print(p)
	  dev.off()
	}
	
	
	for (yearii in yearlist){
	  dfBeta1 = dfBeta[dfBeta$year == yearii,]
	  dfBeta2 = dfBeta1[order(dfBeta1$year, dfBeta1$Beta),]
	  
	  png(filename=paste0(home,'/Results/',modelname,'/',rangeStatList[4],yearii-1,'AllTemporal2.png'),
		  width=12,height=13, units='cm',res=180)
	  p = ggplot(data=dfBeta2, aes(x=reorder(yIndex,Beta), y=Beta, color=type)) + 
		scale_y_continuous(limits=c(ymina, ymaxa), breaks=round(seq(ymina,ymaxa,by=0.2),1)) +
		scale_colour_manual(values = col) +
		geom_point(size = 2.2) +
		geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.1,alpha=0.8) +
		geom_hline(yintercept = mark,alpha=0.4, color=col, lwd=1.5) +
		labs(x = NULL, y='β') +
		theme(text = element_text(size=18),
			  legend.position = c(0.7,0.2),
			  legend.title=element_blank(),
			  panel.grid =element_blank(),                                             #默认主题
			  panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
			  legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
			  #axis.line = element_line(colour = "black"),
			  axis.text.x = element_text(angle = 45, vjust=1.05, hjust=1))
	  print(p)
	  dev.off()
	}

	### 4 classifications
	for (ylist in list(economoicdf, infrasdf, needdf, areadf)){
	  dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist),]
	  dfBeta$yIndex = as.character(dfBeta$yIndex)
	  png(filename=paste0(home, '/Results/',modelname,'/',rangeStatList[4],ylist[1],'BetaTemporal.png'),
		  width=32,height=13, units='cm',res=180)
	  p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, color=yIndex)) + 
		geom_line(size=1,alpha=0.4, na.rm=T) + 
		geom_point(size=2, na.rm=T) +
		geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
		geom_hline(yintercept = mark,alpha=0.4, color=col, lwd=1.5) +
		#geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
		labs(x = 'Year(1984-2017)', y='β') +
		#scale_y_continuous(limits=c(ymina, ymaxa), breaks=round(seq(ymina,ymaxa,by=0.2),1)) +
		scale_x_continuous(breaks=seq(1985,2018, by=5)) +
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

	for (ylist in list(economoicdf, infrasdf, needdf, areadf)){
	  dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist),]
	  dfBeta$yIndex = as.character(dfBeta$yIndex)
	  png(filename=paste0(home, '/Results/',modelname,'/',rangeStatList[4],ylist[1],'InterceptTemporal.png'),
		  width=32,height=13, units='cm',res=180)
	  p = ggplot(data=dfBeta, aes(x=year-1, y=Intercept, color=yIndex)) + 
		geom_line(size=1,alpha=0.4, na.rm=T) + 
		geom_point(size=2, na.rm=T) +
		geom_errorbar(aes(ymin=InterceptLower, ymax=InterceptUpper), width=.2, alpha=0.4) +
		#geom_hline(yintercept = mark,alpha=0.4, color=col, lwd=1.5) +
		#geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
		labs(x = 'Year(1984-2017)', y='α') +
		#scale_y_continuous(limits=c(ymina, ymaxa), breaks=round(seq(ymina,ymaxa,by=0.2),1)) +
		scale_x_continuous(breaks=seq(1985,2018, by=5)) +
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

	dfBeta = sumlmHorizontal[!is.na(sumlmHorizontal$type),]
	  png(filename=paste0(home, '/Results/',modelname,'/',rangeStatList[4],'BetaTemporalFacet.png'),
		  width=30,height=18, units='cm',res=180)
	  p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, color=yIndex)) + 
		facet_wrap(.~type,nrow=2) +
		scale_y_continuous(limits=c(ymina, ymaxa), breaks=round(seq(ymina,ymaxa,by=0.2),1)) +
		scale_x_continuous(breaks=seq(1985,2018, by=5)) +
		geom_line(size=1,alpha=0.4, na.rm=T) + 
		geom_point(size=2, na.rm=T) +
		geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
		geom_hline(yintercept=mark,alpha=0.4, lwd=1.5,
				   col = c( c(col[1],NA,NA,NA),
							c(NA,col[2],NA,NA),
							c(NA,NA,col[3],NA),
							c(NA,NA,NA,col[4]))) +
		geom_hline(yintercept = c(1,1,1,1),alpha=0.2,  lwd=1.5) +
		#geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
		labs(x = 'Year(1984-2017)', y='β') +
		theme(
		  text = element_text(size=23),
		  panel.background = element_rect(fill = "transparent",colour = 'black'), 
		  #panel.grid.minor = element_line(color='azure3'),
		  panel.grid.minor = element_blank(),		  
		  panel.grid.major = element_line(color='azure3'),
		  legend.key = element_rect(fill = "transparent", color = "transparent"), 
		  #plot.background = element_rect(fill = "transparent",colour = NA),
		  axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1),
		  legend.title=element_blank()
		)
	  print(p)
	  dev.off()

	### selected index painted in one fig #####
	###########################################

	economoicdf = c('GDP','Retail')
	needdf = c('Hospital','PrimarySchool')
	infrasdf = c('UrbanRoadArea','Green')
	areadf = c('Area', 'AreaBuilt')
	dfBeta = subset(sumlmHorizontal, sumlmHorizontal$yIndex %in% c(economoicdf, needdf, infrasdf, areadf))
	
	col1 = rep(col[1],length(economoicdf))
	names(col1) = economoicdf
	col2 = rep(col[2],length(needdf))
	names(col2) = needdf
	col3 = rep(col[3],length(infrasdf))
	names(col3) = infrasdf
	col4 = rep(col[4],length(areadf))
	names(col4) = areadf
	collist = c(col1,col2,col3,col4)

	shp1 = c(0,15)
	names(shp1) = economoicdf
	shp2 = c(2,17)
	names(shp2) = needdf
	shp3 = c(1,16)
	names(shp3) = infrasdf
	shp4 = c(23,18)
	names(shp4) = areadf
	shplist = c(shp1,shp2,shp3,shp4)

	dfBeta$yIndex = as.character(dfBeta$yIndex)
	png(filename=paste0(home, '/Results/',modelname,'/',rangeStatList[4],'Select2.png'),
		width=32,height=13, units='cm',res=180)
	p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, color=yIndex,shape=yIndex)) +
	  scale_colour_manual(values = collist, limits=c(economoicdf, needdf, infrasdf, areadf)) +
	  scale_shape_manual(values = shplist, limits=c(economoicdf, needdf, infrasdf, areadf)) +

	  #geom_line(size=1, alpha=0.8, na.rm=T) +
	  geom_point(size=2, na.rm=T, alpha=0.6) +
	  geom_smooth(se=F,span=0.7,lwd=1) +
	  #geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
	  geom_hline(yintercept = mark,alpha=0.4, color=col, lwd=1.5) +
	  #xlim(1984, 2019) +
	  #geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
	  labs(x = 'Year(1984-2017)', y='β') +
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

	png(filename=paste0(home, '/Results/',modelname,'/',rangeStatList[4],'Select1.png'),
		width=32,height=13, units='cm',res=180)
	p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, color=yIndex,shape=yIndex)) +
	  scale_colour_manual(values = collist) +
	  scale_shape_manual(values = shplist) +
	  scale_fill_discrete(limits=c(economoicdf, needdf, infrasdf, areadf)) +
	  geom_line(size=1, alpha=0.8, na.rm=T) +
	  geom_point(size=2.5, na.rm=T) +
	  #geom_smooth(se=F,span=0.7,lwd=1) +
	  #geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
	  geom_hline(yintercept = mark,alpha=0.4, color=col, lwd=1.5) +
	  #xlim(1984, 2019) +
	  #geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
	  labs(x = 'Year(1984-2017)', y='β') +
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


### intercept和beta是R2=0.98的线性关系
############################################################
# output = data.frame()
# sumlmHorizontal = na.omit(sumlmHorizontal)
# dat = split(sumlmHorizontal, sumlmHorizontal$yIndex)
# for (i in 1:length(dat)){
  # dfname = names(dat)[i]
  # dfi = dat[[i]]
  # if(dim(dfi)[1]<10){
    # next
  # }
  # fai = summary(lm(dfi$Intercept~dfi$Beta))
  # a = fai$coefficients[1,1]
  # b = fai$coefficients[2,1]
  # outi = cbind(fai$coefficients, fai$r.squared, dfname, mean(dfi$Rsquare),dim(dfi)[1])
  # output = rbind(output, outi)
  # png(file=paste0('C:/Sync/CoolGirl/Fhe/Results/whylinear/',dfname,'.png'),
      # width = 480, height = 480, units = "px")
  # plot(dfi$Beta, dfi$Intercept, ann=F)
  # abline(a=a, b=b,col=2,lty=3,lw=3)
  # l2 = paste0('α  = ',round(a,2), ' + (', round(b,2), ') * β')
  # l3 = paste0('R-square = ',round(fai$r.squared,2))
  # legend ('bottomleft', c(dfname,l2,l3),
          # col=c(1,1,1), cex=1, pch=c(1,NA,NA))
  # title(xlab= 'β', ylab = 'α')
  # dev.off()
# }
# colnames(output)[c(5,7,8)] = c('linear-R-square','origin-R-square','sample-size')
# write.csv(output, file='C:/Sync/CoolGirl/Fhe/Results/whylinear/statresult.csv')

e=Sys.time()
e-s