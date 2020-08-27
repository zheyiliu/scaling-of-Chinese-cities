######################################################

#home = '/home/zheyi'
home = 'C:/Sync/CoolGirl/Fhe'
setwd(paste0(home,'/ecosocialData/SuperIndex'))
for (rdat in dir()){load(rdat)}

dflist0 = gsub('.Rdata', '', dir(paste0(home,'/ecosocialData/SuperIndex'))) #全部都需要预处理
#dflist = grep('POP|\\d', dflist0, invert=T, value=T) #用来算OLS的
dflist = dflist0[!dflist0 %in% c('POP')]

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

### 每个指标每年一个beta
### 去除同年同城市同指标的不同数值的情况
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

########################################################################################
########################################################################################
########################################################################################

library(ggplot2)
library(rethinking)
library(RColorBrewer)
#library(smatr)
#library(tls)
#library(deming)


############## 需要设置 ##############################


modelname = 'OLS_DJS_sxq'
rangeStatList = c('市辖区', 'Districts', 'BetaD/', 'FigD/')
WithoutXJS = TRUE


dir.create(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/capita"),showWarnings = F)
setwd(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/capita"))
file.remove(dir())

col = c("#F8766D", "grey52", "#619CFF", "#00BA38") #红，灰，蓝，绿
mark = c(7/6, 1, 5/6, 2/3)
economoicdf = c('DepositHousehold','GDP','Retail','Salary')
#'Loan'数据少不要了 #,'Water'R2奇怪 #,'FixedAssets','Electricity'形状一般，不要也罢 #'ForeignCapital'R2低
needdf = c('Hospital','HospitalBeds','PrimarySchool','PrimaryTeacher','School')
#,'Employee','ExpendEdu','ExpendScience','PrimaryStudent') #Doctor波动大, 'LivingSpace'数据少直接不要了算啦，聚类省去插补步骤
infrasdf = c('UrbanRoadArea','Green')
#,'Bus','WaterSupply.Length') #'Sewage.Length'数据少不要了？#,'WaterResident'R2奇怪 #,'ElectricityResident'有点牵强
areadf = c('Area', 'AreaBuilt')

dflist = c(economoicdf, needdf, infrasdf, areadf)
chooseyear = c(1985,1993,2002,2010,2018)
#chooseyear = seq(1985,2018, by=5)

#####################################################

fordens = vector("list", 5)
names(fordens) = chooseyear


cordfally = data.frame()
for (yi in 1:length(dflist)){
    
	xdfname = 'POP'
    ydfname = dflist[yi]
    rangeStat = rangeStatList[1] #按照指标对应的区域更改
    xdf = get(xdfname)
	ydf = get(ydfname)
	delcity = '三沙市'
	#delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市','哈密市','吐鲁番市')
	xdf = xdf[which(!(xdf$city %in% delcity)),]
	
	
	cordfall = data.frame()
	for (yeari in 1:length(chooseyear)){
	  
      print(c(chooseyear[yeari], dflist[yi]))
	  year = chooseyear[yeari]
	  
	  ### 操作有没有县级市
	  if (WithoutXJS){
		xdf = xdf[!grepl('县级',xdf$admin),]
		adminy = chooseyear[yeari]
		adminyi = subset(adminchange, adminchange$yeari==adminy & !grepl('县级',adminchange$admin))
	  } else {
		adminy = chooseyear[yeari]
		adminyi = subset(adminchange, adminchange$yeari==adminy)
	  }
      
	  ### 操作有没有市辖区，并找到对应POP年份的Y指标数值
      ORII = xdf[grepl(rangeStat, xdf$index) & xdf$year==year,]
      CORR = ydf[grepl(rangeStat, ydf$index) & ydf$year==year,]
      cordf = SearchCorValue(ORII, CORR)
	  
	  cordf$capita = log((cordf$yindex / cordf$xindex)*10000)
	  cordfall = rbind(cordfall, cordf)
	  
	  fordens[[yeari]] = cordf$capita
	}
	
	cordfall$yIndex = ydfname
	save(cordfall, file=paste(home,'/Results/',modelname,'/capita/cordfall_',ydfname,'.Rdata',sep=''))
    write.csv(cordfall, file=paste(home,'/Results/', modelname,'/capita/cordfall_',ydfname,'.csv',sep=''))
    
	cordfally = rbind(cordfally, cordfall)
	
	fordens = lapply(fordens, na.omit)
	thed = lapply(fordens, density, adj=1)
	drange = unlist(lapply(thed, function(x){range(x$y)}))
	
	dflist = c(economoicdf, needdf, infrasdf, areadf)
	if(ydfname %in% economoicdf){
		cols <- brewer.pal(8,"Reds")[3:8]
	}else if(ydfname %in% needdf){
		cols <- brewer.pal(8,"Greys")[3:8]
	}else if(ydfname %in% infrasdf){
		cols <- brewer.pal(8,"PuBu")[3:8]
	}else if(ydfname %in% areadf){
		cols <- brewer.pal(8,"BuGn")[3:8]
	}
	
	
	png(filename=paste0(home,'/Results/',modelname,'/capita/Fig_',ydfname,'_capita.png'),
		  width=14,height=14, units='cm',res=180)
	plot(x=c(),y=c(),xlim=range(unlist(fordens)), ylim=range(drange),type="n",
		ylab="Density",xlab=paste0(ydfname,' per capita (log)'),main=NULL,cex.lab=1.25,cex.axis=1.25)
	lapply(1:length(thed),function(x) lines(thed[[x]],col=cols[x],lwd=3))
	legend("topright", legend=chooseyear, col=cols[2:(2+length(thed))],lty=1,lwd=2)             
	dev.off()
}
  
save(cordfally, file=paste(home,'/Results/',modelname,'/capita/cordfally.Rdata',sep=''))
write.csv(cordfally, file=paste(home,'/Results/', modelname,'/capita/cordfally.csv',sep=''))
  
 