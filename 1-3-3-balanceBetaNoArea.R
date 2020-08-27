
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
col = c("#F8766D", "grey52", "#619CFF", "#00BA38") #红，灰，蓝，绿
mark = c(7/6, 1, 5/6, 2/3)

sumlmHorizontal = na.omit(sumlmHorizontal)
sumlmHorizontal = sumlmHorizontal[!is.na(sumlmHorizontal$type),]

dir.create(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/BalanceNoArea/"),showWarnings = F)
setwd(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/BalanceNoArea/"))
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
#areadf = c('Area', 'AreaBuilt')
	

dfmat = data.frame(
	yname = c(economoicdf,needdf,infrasdf,areadf),
	type = c(rep('1economy', length(economoicdf)), rep('2survice', length(needdf)), 
			 rep('3infras', length(infrasdf)), rep('4landuse', length(areadf))),
	weight = c(rep(1/length(economoicdf),length(economoicdf)), rep(1/length(needdf),length(needdf)),
			   rep(1/length(infrasdf),length(infrasdf)), rep(1/length(areadf),length(areadf))),
	theory = c(rep(7/6, length(economoicdf)), rep(1, length(needdf)), 
			 rep(5/6, length(infrasdf)), rep(2/3, length(areadf))),
	stringsAsFactors = FALSE
)

suml = sumlmHorizontal[order(sumlmHorizontal$year),]
radardat = data.frame(year = 1985:2018)
for (yi in 1:length(dfmat$yname)){
	radardat[dfmat$yname[yi]] = suml[suml$yIndex==dfmat$yname[yi],'Beta']
}
rownames(radardat) = radardat$year - 1
radardat = radardat[,-1]

#################################################################################
### 2. 可视化：雷达图。选择一种模型(lm/bm)画图展示一个/几个城市单年份多维度功能图
### 去量纲 standardize
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

png(filename=paste0(home, '/Results/',modelname,'/BalanceNoArea/balanceBeta.png'),
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
a = aggregate(sumlmHorizontal$Beta, by=list(sumlmHorizontal$type,sumlmHorizontal$year),mean)
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

png(filename=paste0(home, '/Results/',modelname,'/BalanceNoArea/balanceBeta1.png'),
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

sustain = data.frame(
	year = as.numeric(rownames(radardat))
)
for (ri in 1:nrow(radardat)){
	# 原始beta
	ci = as.numeric(radardat[ri,])
	# beta和理论值的差值
	ci.t = abs(as.numeric(radardat[ri,])-dfmat$theory)
	
	# 权重
	w = dfmat$weight	
	sum.w <- sum(w)
    sum.w2 <- sum(w^2)
	
	# 距离的权重
	wei = w %*% t(w)
	wei[upper.tri(wei,diag=T)] = 0
	#wei = wei/sum(wei)*(nrow(dfmat)*(nrow(dfmat)-1)/2)
	wei = wei/sum(wei)
	
	### sd
	# 原始beta, 加权mean, sd, cv
    mean.w <- sum(ci * w) / sum(w)
    sd.w <- sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (ci - mean.w)^2))
	cv.w = sd.w/mean.w
	
	# 理论值差值, 加权mean, sd, cv
	#mean.w.t = sqrt(sum(ci.t^(2)* (w/sum.w)))
    mean.w.t = sum(ci.t * w) / sum(w)
    sd.w.t <- sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (ci.t - mean.w.t)^2))
	cv.w.t = sd.w.t/mean.w.t
	
	### dist
	# 原始beta
	b.dist = as.matrix(dist(ci,diag=T,upper=T))
	w.dist = b.dist * wei
	ti = c(0,table(dfmat$type))
	type.dist = unlist(lapply(1:(length(ti)-1), function(x){sum(w.dist[(sum(ti[1:x])+1):sum(ti[1:x+1]),(sum(ti[1:x])+1):sum(ti[1:x+1])])}))
	
	# 理论值差值 距离
	b.t.dist = as.matrix(dist(ci.t,diag=T,upper=T))
	# 理论值差值 加权距离
	w.t.dist = b.t.dist * wei
	# 理论值差值 组内距离
	ti = c(0,table(dfmat$type))
	type.t.dist = unlist(lapply(1:(length(ti)-1), function(x){sum(w.t.dist[(sum(ti[1:x])+1):sum(ti[1:x+1]),(sum(ti[1:x])+1):sum(ti[1:x+1])])}))
	
	sustain$weighted.mean[ri] = mean.w
	sustain$weighted.sd[ri] = sd.w
	sustain$weighted.cv[ri] = cv.w
	
	sustain$weighted.mean.diff[ri] = -mean.w.t
	sustain$weighted.sd.diff[ri] = sd.w.t
	sustain$weighted.cv.diff[ri] = cv.w.t
	
	#sustain$mean.b.dist[ri] = sum(b.dist) / (nrow(dfmat)*(nrow(dfmat)-1)/2)
	sustain$weighted.average.dist[ri] = sum(w.dist)
	sustain$within.dist[ri] = sum(type.dist) 
	
	#sustain$mean.b.t.dist[ri] = sum(b.t.dist) / (nrow(dfmat)*(nrow(dfmat)-1)/2)
	sustain$weighted.average.dist.diff[ri] = sum(w.t.dist)
	sustain$within.dist.diff[ri] = sum(type.t.dist)
}
write.csv(sustain, file=paste0(home,'/Results/', modelname,'/BalanceNoArea/imbalanceIndex.csv'))


f1 = data.frame()
for (i in 2:ncol(sustain)){
	f1 = rbind(f1, data.frame(year=sustain[,1], 
							  value=sustain[,i], 
							  index=colnames(sustain)[i]
							  )
			   )
}
f2 = f1[f1$index %in% c('weighted.mean.diff','weighted.mean'),]
png(filename=paste0(home, '/Results/',modelname,'/BalanceNoArea/developmentIndex.png'),
		  width=20,height=10, units='cm',res=180)
p = ggplot(data=f2, aes(x=year,y=value,colour=index,group=index)) + 
	geom_line(size=1,alpha=1, na.rm=T) + 
	geom_point(size=2, na.rm=T) +
	facet_wrap(.~index,nrow=1,scales='free') +
	labs(x = 'Year(1984-2017)', y='Development Index') +
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


f3 = f1[f1$index %in% c('weighted.sd.diff','weighted.sd'),]
png(filename=paste0(home, '/Results/',modelname,'/BalanceNoArea/imbalanceIndex.png'),
		  width=20,height=10, units='cm',res=180)
p = ggplot(data=f3, aes(x=year,y=value,colour=index,group=index)) + 
	geom_line(size=1,alpha=1, na.rm=T) + 
	geom_point(size=2, na.rm=T) +
	facet_wrap(.~index,nrow=1,scales='free') +
	labs(x = 'Year(1984-2017)', y='Imbalance Index') +
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



png(filename=paste0(home, '/Results/',modelname,'/BalanceNoArea/balanceIndex3.png'),
		  width=16,height=15, units='cm',res=180)
p = ggplot(data=sustain, aes(x=year,y=weighted.mean)) + 
	geom_line(size=1,alpha=1, na.rm=T) + 
	geom_point(size=2, na.rm=T) +
	geom_errorbar(aes(ymin=weighted.mean-weighted.sd, ymax=weighted.mean+weighted.sd), width=.1, colour='#FF6600') +
	labs(x = 'Year(1984-2017)', y='Imbalance Index') +
	scale_x_continuous(breaks=seq(1985,2018, by=5)) +
	theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_blank(),
	  panel.grid.major = element_blank(),
	  legend.position = c(0.25,0.9),
	  legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  #plot.background = element_rect(fill = "transparent",colour = NA),
	  legend.title=element_blank()
    )
print(p)
dev.off()

png(filename=paste0(home, '/Results/',modelname,'/BalanceNoArea/balanceIndex4.png'),
		  width=16,height=15, units='cm',res=180)
p = ggplot(data=sustain, aes(x=year,y=weighted.mean.diff)) + 
	geom_line(size=1,alpha=1, na.rm=T) + 
	geom_point(size=2, na.rm=T) +
	geom_errorbar(aes(ymin=weighted.mean.diff-weighted.sd.diff, ymax=weighted.mean.diff+weighted.sd.diff), width=.1, colour='#FF6600') +
	labs(x = 'Year(1984-2017)', y='Imbalance Index') +
	scale_x_continuous(breaks=seq(1985,2018, by=5)) +
	theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_blank(),
	  panel.grid.major = element_blank(),
	  legend.position = c(0.25,0.9),
	  legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  #plot.background = element_rect(fill = "transparent",colour = NA),
	  legend.title=element_blank()
    )
print(p)
dev.off()



b = aggregate(a$x,by=list(a$Group.2),dist)[-1]
b1 = apply(b,1,sum)
plot(sustain$weighted.average.dist,b1)

F = c()
Pv = c()
for (yeari in 1985:2018) {
sumly = suml[suml$year == yeari,]
#sumly = suml[suml$year == yeari&!suml$yIndex%in%c('Area'),]
#sumly = suml[suml$year == yeari&!suml$yIndex%in%c('Area','AreaBuilt'),]
av = anova(lm(Beta~type, data=sumly))
F = c(F,av[1,4])
Pv = c(Pv,av[1,5])
}
plot(1984:2017,F)
#plot(1984:2017,Pv)


