
######################################################################
##### 接下来是单年份多维度
##### 展示单年份单城市的城市功能维度图
# GDP
# CityRoadArea
# School
# AreaBuilt

##### 0. 指标选择，数据准备
##### 1. 计算多维度残差，扔进同一个df
##### 2. 可视化：雷达图
##### 3. 各个功能维度的residual之间有什么相互作用
##### 4. 受什么宏观属性影响？
##### 5. 建立城市功能可持续性/平衡性的指标；
#####    参数的使用取决于它的相对数值或不同参数之间的可比关系，而不是它的绝对量
##### 6. 整个城市系统可持续性/平衡性的分布情况；四分法
##### 7. 聚类


######################################################################
######################################################################

library(ggplot2)

##### 0. 指标选择，数据准备
#####    按R2大小排序

home = 'C:/Sync/CoolGirl/Fhe'
modelname='OLS_DJS_sxq'
setwd(paste0(home, '/Results/',modelname))
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
#sumlmHorizontal = read.csv(file=paste0('sumlmHorizontal_type_',rangeStatList[2],'.csv'),stringsAsFactors=F)
load(file=paste0('sumlmHorizontal_type_',rangeStatList[2],'.Rdata'))
load(file=paste0(home,'/Results/', modelname,'/capitaTradition/popycy.Rdata'))
col = c("#F8766D", "grey52", "#619CFF", "#00BA38") #红，灰，蓝，绿
mark = c(7/6, 1, 5/6, 2/3)

sumlmHorizontal = na.omit(sumlmHorizontal)
sumlmHorizontal = sumlmHorizontal[!is.na(sumlmHorizontal$type),]

#sumlmHorizontal = merge(sumlmHorizontal, popycy, by = c('year','yIndex'))
#sumlmHorizontal$YN.log = log(sumlmHorizontal$Y)/log(sumlmHorizontal$N)
#sumlmHorizontal$YN.log.s = (sumlmHorizontal$YN.log-min(sumlmHorizontal$YN.log))/(max(sumlmHorizontal$YN.log)-min(sumlmHorizontal$YN.log))

a = sumlmHorizontal$Intercept
sumlmHorizontal$Intercept.s = (a - min(a))/(max(a)-min(a))

balof = '/BalanceAlpha/'

dir.create(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname, balof),showWarnings = F)
setwd(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname, balof))
file.remove(dir())

#经济 #基建 #个人需要
col = c("#F8766D", "grey52", "#619CFF", "#00BA38") #红，灰，蓝，绿
mark = c(7/6, 1, 5/6, 2/3)
economoicdf = c('GDP','DepositHousehold','Salary','Retail')
#'Loan'数据少不要了 #,'Water'R2奇怪 #,'FixedAssets','Electricity'形状一般，不要也罢 #'ForeignCapital'R2低
needdf = c('Hospital','HospitalBeds','PrimarySchool','PrimaryTeacher')
#,'Employee','ExpendEdu','ExpendScience','PrimaryStudent') #Doctor波动大, 'LivingSpace'数据少直接不要了算啦，聚类省去插补步骤 ,'School'
infrasdf = c('UrbanRoadArea','Green')
#,'Bus','WaterSupply.Length') #'Sewage.Length'数据少不要了？#,'WaterResident'R2奇怪 #,'ElectricityResident'有点牵强
areadf = c('Area','AreaBuilt')
#areadf = c('AreaBuilt')
	

dfmat = data.frame(
	yname = c(economoicdf,needdf,infrasdf,areadf),
	type = c(rep('1economy', length(economoicdf)), rep('2survice', length(needdf)), 
			 rep('3infras', length(infrasdf)), rep('4landuse', length(areadf))),
	weight = c(rep(1/length(economoicdf),length(economoicdf)), rep(1/length(needdf),length(needdf)),
			   rep(1/length(infrasdf),length(infrasdf)), rep(1/length(areadf),length(areadf))),
	theory = c(rep(7/6, length(economoicdf)), rep(1, length(needdf)), 
			 rep(5/6, length(infrasdf)), rep(2/3, length(areadf))),
	year = 'ANY',
	stringsAsFactors = FALSE
)

suml = sumlmHorizontal[order(sumlmHorizontal$year),]
suml = sumlmHorizontal[sumlmHorizontal$yIndex %in% dfmat$yname,]
radardat = data.frame(year = 1985:2018)
for (yi in 1:length(dfmat$yname)){
	radardat[dfmat$yname[yi]] = suml[suml$yIndex==dfmat$yname[yi],'Intercept.s']
}
rownames(radardat) = radardat$year - 1
radardat = radardat[,-1]

#################################################################################
### 2. 可视化：雷达图。选择一种模型(lm/bm)画图展示一个/几个城市单年份多维度功能图
library(fmsb)

up = round(max(radardat), 1) + 0.1
down = round(min(radardat), 1) - 0.1
dat = rbind(rep(up, ncol(radardat)), 
            rep(down, ncol(radardat)), 
#            rep(0, ncol(radardat)),
            radardat[c('1984','2000','2017'),])

### radar chart
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.2,0.5,0.2) , rgb(0.7,0.5,0.1,0.2) )

png(filename=paste0(home, '/Results/',modelname,balof,'balanceIntercept.png'),
		  width=15,height=15, units='cm',res=180)
radarchart( dat  , axistype=1 , pty=32,
   pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1, seg=6,
   cglcol="grey", cglty=1, axislabcol="darkgrey",  cglwd=0.8,
   vlcex=1, centerzero=T, caxislabels=seq(down, up, length.out=7),
   )
legend(x=0.9, y=1.3, legend = rownames(dat[-c(1,2),]), 
  bty = "n", pch=20 , col=colors_border , text.col = "black", 
  cex=1.2, pt.cex=3)
dev.off()


### radar chart, type
a = aggregate(sumlmHorizontal$Intercept.s, by=list(sumlmHorizontal$type,sumlmHorizontal$year),mean)
a$Group.1 = as.character(a$Group.1)
f0 = data.frame(year=1984:2017)
s = unique(a$Group.1)
for (i in 1:4){
	f0[s[i]] = a[a$Group.1==s[i],'x']
}

radardatt = f0[,-1]
rownames(radardatt) = f0[,1]
up = round(max(radardatt), 1)
down = round(min(radardatt), 1) - 0.2
dat = rbind(rep(up, ncol(radardatt)), 
            rep(down, ncol(radardatt)), 
#            rep(0, ncol(radardatt)),
            radardatt[c('1984','2000','2017'),])

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.2,0.5,0.2) , rgb(0.7,0.5,0.1,0.2) )

png(filename=paste0(home, '/Results/',modelname,balof,'balanceIntercept1.png'),
		  width=15,height=15, units='cm',res=180)
radarchart( dat  , axistype=1 , pty=32, vlabels=c("Socio-economic", "Basic\nServices", 
   "Infrastructure", "Land\nUse"), 
   pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1, seg=4,
   cglcol="darkgrey", cglty=1, axislabcol="darkgrey",  cglwd=1,
   vlcex=1.2, centerzero=T, caxislabels=seq(down, up, length.out=5),
   )
legend(x=-1.49, y=1.2, legend = rownames(dat[-c(1,2),]), 
  bty = "n", pch=20 , col=colors_border , text.col = "black", 
  cex=1.1, pt.cex=3)
dev.off()


##################################################################################
##### 5. 建立城市功能可持续性/平衡性的指标；
# 参数的使用取决于它的相对数值或不同参数之间的可比关系，而不是它的绝对量
# 数值越大越优渥，富裕（经济），基建便利（基建），医疗条件好（社会服务），居住环境好（LivingSpace, Green）
# 数值越平均越平衡。
#     不能用1/sd，因为当sd为0时1/sd是正无穷，最好限制在[0,1]之间
#     但是用evenness的话，1/evenness也是正无穷，好像也没啥问题
#     发发推荐了：拿矩形为例，面积跟边长比，在边长一定的情况下，各边长分布越均匀，面积越大
# 能反映整体的正负
# 比较residual时，是不是应该除以人口或人口^beta？

### ANOVA
###############################################
###############################################

suml$theory = NA
suml[suml$type=='Socio-economic','theory']=7/6
suml[suml$type=='Basic Services','theory']=1
suml[suml$type=='Infrastructure','theory']=5/6
suml[suml$type=='Land Use','theory']=2/3
suml$Betadiff = -abs(suml$Beta-suml$theory)

AOVW = function(dat=suml, yearrange=1985:2018, method='Greedy',sampling='weighted'){
	mean.w = c()
	SSTw = c()
	SSEw = c()
	SSAw = c()
	for (i in 1:length(yearrange)) {
		sumly = dat[dat$year == yearrange[i],]
		n = nrow(sumly)
		ni = table(sumly$type)
		k = length(ni)
		
		if (method=='Greedy'){
			Dev = 'Beta'
		}else if(method=='Ideal'){
			Dev = 'Betadiff'
		}else if(method=='Theory'){
			Dev = 'theory'
		}else{print('opps')}
		
		xi = c()
		xij = c()
		df0 = split(sumly,sumly$type)
		for (di in 1:length(df0)){
			df1 = df0[[di]]
			xij = c(xij,df1[,Dev])
			xi = c(xi, rep(mean(df1[,Dev]),nrow(df1)))
		}
		
		if (sampling=='unweighted'){
			w = rep(1, nrow(dfmat))
		}else if(sampling=='weighted'){
			w = dfmat$weight
		}else{print('opps')}		
		
		
		### variance
		mean.w[i] <- sum(xij * w) / sum(w)
		SSTw[i] <- sum(w * (xij - mean.w[i])^2) / sum(w)
		SSAw[i] <- sum(w * (xi - mean.w[i])^2) / sum(w)
		SSEw[i] <- sum(w * (xij - xi)^2) / sum(w)
		
		#SSTw[i] <- (sum.w / (sum.w^2 - sum.w2)) * sum(w * (xij - mean.w)^2)
		#pw = k/(k^2-sum(1/ni))
		#TSD = pw * sum((unique(xi)-mean.w)^2)
		
		# if (sampling=='unweighted'){
			# xbar=mean(sumly[,Dev])
		# }else if(sampling=='weighted'){
			# xbar=mean(unique(xi))
		# }else{print('opps')}
		
		
		# SST[i] = sum((xij - xbar)^2)
		# SSA[i] = sum((xi - xbar)^2)
		# SSE[i] = sum((xij - xi)^2)
		
		# MST[i] = SST[i]/(n-1)
		# MSE[i] = SSE[i]/(n-k)
		# MSA[i] = SSA[i]/(k-1)
	}
	aovdf = data.frame(
			Year=1984:2017,
			WB=c(rep('Inter-dimension',2018-1984),
				 rep('intra-dimension',2018-1984)),
			Variance=c(SSAw,SSEw),
			Version=method
	)
	gdf = data.frame(
			Year=1984:2017,
			G = mean.w,
			Version=method
	)
	aovdfv = data.frame(
			Year=1984:2017,
			Interdimension=SSAw,
			Intradimension=SSEw,
			Overall=SSEw+SSAw,
			p = SSAw/(SSEw+SSAw),
			Version=method
	)
	allaovdf = list(gdf,aovdf,aovdfv)
	return(allaovdf)
}

### G + imbalance
av1 = AOVW(dat=suml, yearrange=1985:2018, method='Greedy',sampling='unweighted')
av2 = AOVW(dat=suml, yearrange=1985:2018, method='Greedy',sampling='weighted') # my choice
av3 = AOVW(dat=suml, yearrange=1985:2018, method='Ideal',sampling='unweighted')
av4 = AOVW(dat=suml, yearrange=1985:2018, method='Ideal',sampling='weighted')

av5 = AOVW(dat=dfmat, yearrange='ANY',method='Theory',sampling='unweighted')
av6 = AOVW(dat=dfmat, yearrange='ANY',method='Theory',sampling='weighted')

### for imbalance~development (type)	  
gv = aggregate(suml$Beta, by=list(suml$year,suml$type),mean)
gv = split(gv, gv$Group.2)

################################
#### defining development as per capita amount
cap1 = aggregate(suml$Intercept.s, by=list(suml$year,suml$type),mean)
cap3 = split(cap1, cap1$Group.2)
cap2 = aggregate(cap1$x, by=list(cap1$Group.1),mean)
colnames(cap2) = c('Year','G')

### for imbalance~development
bv = split(av2[[2]],av2[[2]]$WB)
cv = data.frame(Inter=bv[[1]]$Variance,
		Intra=bv[[2]]$Variance,
		Overall=bv[[1]]$Variance + bv[[2]]$Variance,
		Develop = av2[[1]]$G,
		GDP = subset(suml, suml$yIndex=='GDP')$Beta,
		Economic = gv[[1]]$x,
		Cap = cap2$G,
		CapGDP = subset(suml, suml$yIndex=='GDP')$Intercept.s,
		CapEconomic = cap3[[1]]$x,
		year = 1984:2017
	  )


png(filename=paste0(home, '/Results/',modelname,balof,'developmentIndex.png'),
		  width=13,height=12, units='cm',res=180)
p = ggplot(data=av2[[1]], aes(x=Year,y=G,color=Version)) + 
	geom_line(size=1,alpha=1, na.rm=T) + 
	geom_point(size=2, na.rm=T) +
	geom_hline(yintercept=av5[[1]][1,2], size=1,col="grey52") +
	labs(x = 'Year(1984-2017)', y='Development') +
	scale_x_continuous(breaks=seq(1985,2018, by=5)) +
	theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_blank(),
	  panel.grid.major = element_blank(),
	  legend.position = 'none',
	  legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  #plot.background = element_rect(fill = "transparent",colour = NA),
	  legend.title=element_blank(),
	  axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
    )
print(p)
dev.off()

### for single imbalance 【one pick】
png(filename=paste0(home, '/Results/',modelname,balof,'ImbalanceIndexCompare.png'),
		  width=27,height=12, units='cm',res=180)
p = ggplot(data=av2[[2]], mapping=aes(x=Year,y=Variance,fill=WB))+
  geom_col(width=0.5,position='stack')+
  geom_hline(yintercept=c(av5[[2]][1,3]), size=1,col="grey52")+
  labs(x = 'Year(1984-2017)', y='Imbalance') +
  scale_x_continuous(breaks=seq(1985,2018, by=5))+
  theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_blank(),
	  panel.grid.major = element_blank(),
	  legend.position = c(0.3,0.89),
	  legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  #plot.background = element_rect(fill = "transparent",colour = NA),
	  legend.title=element_blank(),
	  axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
	  )
print(p)
dev.off()


### for imbalance~development
fdo = lm(Develop~I(Overall^2)+Overall,data=cv)
fdol = lm(Develop~Overall,data=cv)

### Imb ~ Dev
li = c(rep(NA,3),av5[[1]][1,2],1.15,1.15)
ylabi = c(rep(NA,3),'Development', 'Development (GDP)','Development (Economy)')
for (i in 4:6){
png(filename=paste0(home, '/Results/',modelname,balof,'ImbDev',names(cv)[i],'.png'),
		  width=13,height=12, units='cm',res=180)
p = ggplot(data=cv, aes(x=Overall,y=cv[,i],col=year)) +
	geom_point(size=3) +
	geom_smooth(method = lm,formula = y~I(x^2)+x,se=F) +
	geom_hline(yintercept=li[i], size=1,col="grey52") +
	geom_vline(xintercept=av5[[2]][1,3], size=1,col="grey52") +
	labs(y = ylabi[i], x='Imbalance') +
	theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_blank(),
	  panel.grid.major = element_blank(),
	  legend.position = c(0.87,0.2),
	  legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  #plot.background = element_rect(fill = "transparent",colour = NA),
	  legend.title=element_blank()
	  #,
	  #axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
	  )
print(p)
dev.off()
}

### Dev ~ Imb
li = c(rep(NA,3),av5[[1]][1,2],1.15,1.15)
xlabi = c(rep(NA,3),'Development (Overall)', 'Development (GDP)','Development (Economy)')
for (i in 4:6){
png(filename=paste0(home, '/Results/',modelname,balof,'DevImb',names(cv)[i],'.png'),
		  width=13,height=12, units='cm',res=180)
p = ggplot(data=cv, aes(y=Overall,x=cv[,i],col=year)) +
	geom_point(size=3) +
	geom_smooth(method = lm,formula = y~I(x^2)+x,se=F) +
	geom_vline(xintercept=li[i], size=1,col="grey52") +
	geom_hline(yintercept=av5[[2]][1,3], size=1,col="grey52") +
	labs(x = xlabi[i], y='Imbalance (Overall)') +
	theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_blank(),
	  panel.grid.major = element_blank(),
	  legend.position = c(0.2,0.8),
	  legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  #plot.background = element_rect(fill = "transparent",colour = NA),
	  legend.title=element_blank()
	  #,
	  #axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
	  )
print(p)
dev.off()
}

#plot(cv$Overall,cv$Develop)
#identify(cv$Overall,cv$Develop,label=cv$Overall)

# ### for facet imbalance
# av = rbind(av2[[2]],av4[[2]])

# png(filename=paste0(home, '/Results/',modelname,balof,'ImbalanceIndex.png'),
		  # width=24,height=12, units='cm',res=180)
# p = ggplot(data=av, mapping=aes(x=Year,y=Variance,fill=WB))+
  # geom_col(width=0.5,position='dodge')+
  # labs(x = 'Year(1984-2017)', y='Imbalance') +
  # scale_x_continuous(breaks=seq(1985,2018, by=5))+
  # facet_wrap(~Version,scales='free')+
  # theme(
	  # text = element_text(size=18),
	  # panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  # panel.grid.minor = element_blank(),
	  # panel.grid.major = element_blank(),
	  # legend.position = c(0.13,0.85),
	  # legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  # #plot.background = element_rect(fill = "transparent",colour = NA),
	  # legend.title=element_blank(),
	  # axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
	  # )
# print(p)
# dev.off()


# ### for facet development
# mv = rbind(av2[[1]],av4[[1]])

# png(filename=paste0(home, '/Results/',modelname,balof,'developmentIndex.png'),
		  # width=24,height=12, units='cm',res=180)
# p = ggplot(data=mv, aes(x=Year,y=G,color=Version,group=Version)) + 
	# geom_line(size=1,alpha=1, na.rm=T) + 
	# geom_point(size=2, na.rm=T) +
	# facet_wrap(~Version,nrow=1,scales='free') +
	# labs(x = 'Year(1984-2017)', y='Development') +
	# scale_x_continuous(breaks=seq(1985,2018, by=5)) +
	# theme(
	  # text = element_text(size=18),
	  # panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  # panel.grid.minor = element_blank(),
	  # panel.grid.major = element_blank(),
	  # legend.position = 'none',
	  # legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  # #plot.background = element_rect(fill = "transparent",colour = NA),
	  # legend.title=element_blank(),
	  # axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
    # )
# print(p)
# dev.off()

# png(filename=paste0(home, '/Results/',modelname,balof,'ImbalanceP.png'),
		  # width=13,height=12, units='cm',res=180)
# p = ggplot(data=av2[[3]], aes(x=Year,y=p))+
  # geom_point(size=2)+
  # geom_line()+
  # labs(x = 'Year(1984-2017)', y='Proportion') +
  # #scale_x_continuous(breaks=seq(1985,2018, by=5))+
  # theme(
	  # text = element_text(size=18),
	  # panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  # panel.grid.minor = element_blank(),
	  # panel.grid.major = element_blank(),
	  # legend.position = c(0.3,0.89),
	  # legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  # #plot.background = element_rect(fill = "transparent",colour = NA),
	  # legend.title=element_blank(),
	  # axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
	  # )
# print(p)
# dev.off()





# development - year
png(filename=paste0(home, '/Results/',modelname,balof,'1DevelopmentBaseline.png'),
		  width=13,height=12, units='cm',res=180)
p = ggplot(data=cap2, aes(x=Year,y=G)) + 
	geom_line(size=1,alpha=1, na.rm=T) + 
	geom_point(size=2, na.rm=T) +
	#geom_hline(yintercept=av5[[1]][1,2], size=1,col="grey52") +
	labs(x = 'Year(1984-2017)', y='Development Baseline') +
	scale_x_continuous(breaks=seq(1985,2018, by=5)) +
	theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_blank(),
	  panel.grid.major = element_blank(),
	  legend.position = 'none',
	  legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  #plot.background = element_rect(fill = "transparent",colour = NA),
	  legend.title=element_blank(),
	  axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
    )
print(p)
dev.off()



### for imbalance~development
fdo = lm(Cap~I(Overall^2)+Overall,data=cv)
fdol = lm(Cap~Overall,data=cv)

### Imb ~ Dev
li = c(rep(NA,6),av5[[1]][1,2],1.15,1.15)
ylabi = c(rep(NA,6),'Development Baseline', 'Development Baseline (GDP)','Development Baseline (Economy)')
for (i in 7:9){
png(filename=paste0(home, '/Results/',modelname,balof,'1ImbDevBase',names(cv)[i],'.png'),
		  width=13,height=12, units='cm',res=180)
p = ggplot(data=cv, aes(x=Overall,y=cv[,i],col=year)) +
	geom_point(size=3) +
	geom_smooth(method = lm,formula = y~I(x^2)+x,se=F) +
	#geom_hline(yintercept=li[i], size=1,col="grey52") +
	geom_vline(xintercept=av5[[2]][1,3], size=1,col="grey52") +
	labs(y = ylabi[i], x='Imbalance') +
	theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_blank(),
	  panel.grid.major = element_blank(),
	  legend.position = c(0.87,0.2),
	  legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  #plot.background = element_rect(fill = "transparent",colour = NA),
	  legend.title=element_blank()
	  #,
	  #axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
	  )
print(p)
dev.off()
}

### Dev ~ Imb
li = c(rep(NA,6),av5[[1]][1,2],1.15,1.15)
ylabi = c(rep(NA,6),'Development Baseline', 'Development Baseline (GDP)','Development Baseline (Economy)')
for (i in 7:9){
png(filename=paste0(home, '/Results/',modelname,balof,'1DevBaseImb',names(cv)[i],'.png'),
		  width=13,height=12, units='cm',res=180)
p = ggplot(data=cv, aes(y=Overall,x=cv[,i],col=year)) +
	geom_point(size=3) +
	geom_smooth(method = lm,formula = y~I(x^2)+x,se=F) +
	#geom_vline(xintercept=li[i], size=1,col="grey52") +
	geom_hline(yintercept=av5[[2]][1,3], size=1,col="grey52") +
	labs(x = xlabi[i], y='Imbalance (Overall)') +
	theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_blank(),
	  panel.grid.major = element_blank(),
	  legend.position = c(0.2,0.8),
	  legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  #plot.background = element_rect(fill = "transparent",colour = NA),
	  legend.title=element_blank()
	  #,
	  #axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
	  )
print(p)
dev.off()
}
