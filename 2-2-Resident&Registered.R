library(ggplot2)

RB0 = read.csv(file='C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq_R/sumlmHorizontal_Districts.csv',header=T,stringsAsFactors=F)
PB0 = read.csv(file='C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq/sumlmHorizontal_Districts.csv',stringsAsFactors=F)
RB = RB0[,c(2,3,4,10,11,12,13)]
PB = PB0[,c(2,3,4,10,11,12,13)]
RB$R = 'Resident'
PB$R = 'Registered'

PRBr = rbind(RB,PB)

foo = merge(RB,PB,by=c('yIndex','year'),all=T)
#foo = foo[which(foo$yIndex!='Passenger'),]

col = c("#619CFF", "#00BA38", "grey52", "#F8766D")
economoicdf = c('DepositHousehold','GDP','Retail','Salary')
#'Loan'数据少不要了 #,'Water'R2奇怪 #,'FixedAssets','Electricity'形状一般，不要也罢 #'ForeignCapital'R2低
infrasdf = c('UrbanRoadArea','Green')
#,'Bus','WaterSupply.Length') #'Sewage.Length'数据少不要了？#,'WaterResident'R2奇怪 #,'ElectricityResident'有点牵强
needdf = c('Hospital','HospitalBeds','PrimarySchool','PrimaryTeacher','School')
#,'Employee','ExpendEdu','ExpendScience','PrimaryStudent') #Doctor波动大, 'LivingSpace'数据少直接不要了算啦，聚类省去插补步骤
areadf = c('Area', 'AreaBuilt')
	

foo$type= NA
foo[foo$yIndex %in% economoicdf,]$type = 'Socio-economic'
foo[foo$yIndex %in% infrasdf,]$type = 'Infrastructure'
foo[foo$yIndex %in% needdf,]$type = 'Basic Services'
foo[foo$yIndex %in% areadf,]$type = 'Land Use'
foo = na.omit(foo)


a = lm(foo$Beta.x~foo$Beta.y) #x是常住人口，y是户籍人口， x常住人口~y户籍人口
aa = summary(a)

foo <- within(foo, yIndex <- factor(yIndex, levels = c(economoicdf, needdf, infrasdf, areadf)))
with(foo, levels(yIndex))

foo <- within(foo, type <- factor(type, levels = c('Socio-economic', 'Basic Services', 'Infrastructure', 'Land Use')))
with(foo, levels(type))

#text = paste0('y=', round(aa$coefficients[1,1],3), 'x, R^2=', round(aa$r.squared,3))
text = paste0('y=', round(aa$coefficients[2,1],3), 'x', round(aa$coefficients[1,1],3), ', R^2=', round(aa$r.squared,3))

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq_R/Registered&Resident_beta1.png'),
    width=15,height=15, units='cm',res=150)
p1 = ggplot(data=foo, aes(x=Beta.y, y=Beta.x, color=type)) + 
  #scale_colour_manual(values = col) +
  #geom_abline(size=1,col='grey') +
  geom_abline(slope=aa$coefficients[2,1],intercept=aa$coefficients[1,1], size=1) +
  #geom_abline(slope=aa$coefficients[1,1],size=1) +
  geom_point(size = 3.5) +
  labs(x='β (Registered)', y='β (Resident)') +
  scale_colour_hue(text) + 
  theme(text = element_text(size=20),
        legend.title=element_text(size = 14, face='italic'),
        legend.position = c(0.73,0.15),
        legend.text=element_text(size=14),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 0, hjust = 0.7, vjust = 0.5))
print(p1)
dev.off()

b = lm(foo$Intercept.x~foo$Intercept.y)
bb = summary(b)


#text = paste0('y=', round(bb$coefficients[1,1],3), 'x, R^2=', round(bb$r.squared,3))
text = paste0('y=', round(bb$coefficients[2,1],3), 'x+', round(bb$coefficients[1,1],3), ', R^2=', round(bb$r.squared,3))

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq_R/Registered&Resident_Intercept1.png'),
    width=15,height=15, units='cm',res=150)
p2 = ggplot(data=foo, aes(x=Intercept.y, y=Intercept.x, color=type)) + 
  #scale_colour_manual(values = col) +
  #geom_abline(slope=1,intercept=c,size=1,col='grey') +
  geom_abline(slope=bb$coefficients[2,1],intercept=bb$coefficients[1,1], size=1) +
  #geom_abline(slope=bb$coefficients[1,1], size=1) +
  geom_point(size = 3.5) +
  labs(x='Intercept (Registered)', y='Intercept (Resident)') +
  scale_colour_hue(text) + 
  theme(text = element_text(size=20),
        legend.title=element_text(size = 14, face='italic'),
        legend.position = c(0.73,0.15),
        legend.text=element_text(size=14),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 0, hjust = 0.7, vjust = 0.5))
print(p2)
dev.off()


r = lm(foo$Rsquare.x~foo$Rsquare.y)
rr = summary(r)
# rc = mean(na.omit(foo$Rsquare.x))-mean(na.omit(foo$Rsquare.y))
# Ry = foo$Rsquare.y+rc
# r1 = lm(foo$Rsquare.x~Ry-1)
# rr1 = summary(r1)


#text = paste0('y=', round(rr$coefficients[1,1],3), 'x, R^2=', round(rr$r.squared,3))
text = paste0('y=', round(rr$coefficients[2,1],3), 'x+', round(rr$coefficients[1,1],3), ', R^2=', round(rr$r.squared,3))

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq_R/Registered&Resident_R21.png'),
    width=15,height=15, units='cm',res=150)
p3 = ggplot(data=foo, aes(x=Rsquare.y, y=Rsquare.x, color=type)) + 
  #scale_colour_manual(values = col) +
  #geom_abline(slope=1,intercept=rc,size=1,col='grey') +
  geom_abline(slope=rr$coefficients[2,1],intercept=rr$coefficients[1,1], size=1) +
  #geom_abline(slope=rr$coefficients[1,1], size=1) +
  geom_point(size = 3.5) +
  labs(x='R-square (Registered)', y='R-square (Resident)') +
  scale_colour_hue(text) + 
  theme(text = element_text(size=20),
        legend.title=element_text(size = 14, face='italic'),
        legend.position = c(0.73,0.15),
        legend.text=element_text(size=14),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 0, hjust = 0.7, vjust = 0.5))
print(p3)
dev.off()


#########################################################
#########################################################
#########################################################


library(ggplot2)

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
for (yi in 1:length(dflist0)){
  dfname = dflist0[yi]
  df = get(dfname)
  df$year = as.integer(df$year)
  df$value = as.numeric(df$value)
  df$value[which(df$value==0)] = NA
  assign(dfname, df)
  #eval(parse(text = paste0('return(',dfname, ')')))
}

POP$year = POP$year-1

# Resident$value = Resident$value/10000
# Resident[which(Resident$city=='六盘水市'&Resident$year==2000),]$value = 99.5055
# Resident[which(Resident$city=='海口市'&Resident$year==1990),]$value = 41.0050
# Resident[which(Resident$city=='三亚市'&Resident$year==1990),]$value = 37.0244
# save(Resident, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/POPResident.Rdata')

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


xdfname = 'POPResident'
ydfname = 'POP'
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
rangeStat = rangeStatList[1] #按照指标对应的区域更改

xdf = get(xdfname)
delcity = '三沙市'
#delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市','哈密市','吐鲁番市')
xdf = xdf[which(!(xdf$city %in% delcity)),]
ydf = get(ydfname)
ORII = xdf[grepl(rangeStat, xdf$index),]
CORR = ydf[grepl(rangeStat, ydf$index),]
cordf = SearchCorValue(ORII, CORR)

data = data.frame(Y = log(cordf$yindex), X = log(cordf$xindex), city = cordf$city, year = cordf$year)

a = lm(log(cordf$xindex)~log(cordf$yindex))
aa = summary(a)
a1 = lm(log(cordf$xindex)~log(cordf$yindex)-1)
aa1 = summary(a1)


#text = paste0('y=', round(aa1$coefficients[1,1],3), 'x', ', R^2=', round(aa1$r.squared,3))
text = paste0('y=', round(aa$coefficients[2,1],3), 'x', round(aa$coefficients[1,1],3), ', R^2=', round(aa$r.squared,3))


png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq_R/ResidentVSPOP1.png'),width=15,height=15, units='cm',res=150)
p0 = ggplot(data=cordf, aes(x=log(yindex), y=log(xindex), color=as.factor(year))) + 
  #scale_colour_manual(values = col) +
  #geom_abline(slope=1,intercept=rc,size=1,col='grey') +
  geom_abline(slope=aa$coefficients[2,1],intercept=aa$coefficients[1,1], size=1) +
  #geom_abline(slope=aa1$coefficients[1,1], size=1) +
  geom_point(size = 3.5, alpha=0.6) +
  labs(x='Registered Population (log)', y='Resident Population (log)') +
  scale_colour_hue(text) + 
  theme(text = element_text(size=20),
        legend.title=element_text(size = 14, face='italic'),
        legend.position = c(0.73,0.15),
        legend.text=element_text(size=14),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        #axis.title.x = element_text(vjust = -2, hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.7, vjust = 0.5))
print(p0)
dev.off()

cordf1 = na.omit(cordf)
respoint = data.frame(city=cordf1$city, year=cordf1$year, Resident=cordf1$xindex, Registered=cordf1$yindex, res=aa$residuals)
resorder = respoint[order(respoint$res,decreasing=T),]
respoint1 = data.frame(city=cordf1$city, year=cordf1$year, Resident=cordf1$xindex, Registered=cordf1$yindex, res=aa1$residuals)
resorder1 = respoint1[order(respoint1$res,decreasing=T),]
write.csv(resorder1, file='C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq_R/ResOrder.csv')
write.csv(resorder, file='C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq_R/ResOrder2.csv')

library(ggpubr)

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq_R/arrange.png'),width=30,height=30, units='cm',res=300)
ggarrange(p0,p1,p2,p3,ncol=2,nrow=2,labels=c("A","B","C","D"))
dev.off()

#ggplot2.multiplot(p0, p1,p2,p3, cols=2, row=2)


#############################################################
#############################################################
#############################################################

cordf1df = split(cordf1, cordf1$year)
f1990 = lm(log(cordf1df[[1]]$xindex) ~ log(cordf1df[[1]]$yindex) - 1)
summary(f1990)
f2000 = lm(log(cordf1df[[2]]$xindex) ~ log(cordf1df[[2]]$yindex) - 1)
summary(f2000)
f2010 = lm(log(cordf1df[[3]]$xindex) ~ log(cordf1df[[3]]$yindex) - 1)
summary(f2010)

exp(0.072/1.024348) #1990, 1.0096
exp(0.072/1.117447) #2000, 1.0189
exp(0.072/1.219515) #2010, 1.0236

at = t.test(foo$Beta.x, foo$Beta.y, paired=T)
bt = t.test(foo$Intercept.x,foo$Intercept.y,paired=T,alternative = 'less')
rt = t.test(foo$Rsquare.x,foo$Rsquare.y,paired=T,alternative = 'greater')
pt = t.test(log(cordf1$xindex), log(cordf1$yindex), paired=T,alternative = 'two.sided')

#at = t.test(foo$Beta.x, foo$Beta.y, paired=T)
#bt = t.test(foo$Intercept.x,foo$Intercept.y,paired=T)
#rt = t.test(foo$Rsquare.x,foo$Rsquare.y,paired=T)
#pt = t.test(cordf1$xindex, cordf1$yindex, paired=T)
at
bt
rt
pt


colnames(foo) = gsub('\\bx\\b','Resident',colnames(foo))
colnames(foo) = gsub('\\by\\b','Registered',colnames(foo))


######################################
# Radar Chart

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

library(fmsb)
RAD = function(yeari, para){ 
	#1990,2000,2010; 'Beta','Intercept';

	foo1 = subset(foo, foo$year==yeari)
	dat1 = as.data.frame(t(foo1[,grep(para,colnames(foo1))]))
	colnames(dat1)=foo1$yIndex
	dat1 = dat1[,dfmat$yname]
	
	if (para=='Beta'){
	up = 1.3
	down = 0.3
	}else if(para=='Intercept'){
	up = 9.4
	down = -0.5
	}else if(para=='Rsquare'){
	up = 0.95
	down = 0.1
	}else{print('opps!')}
	
	dat = rbind(rep(up, ncol(dat1)), 
				rep(down, ncol(dat1)), dat1)

	### radar chart
	colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
	colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.2,0.5,0.2) , rgb(0.7,0.5,0.1,0.2) )

	png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq_R/Radar',para,yeari,'.png'),
			  width=15,height=15, units='cm',res=180)
	radarchart( dat  , axistype=1 , pty=32, 
	   pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1, seg=5,
	   cglcol="grey", cglty=1, axislabcol="darkgrey",  cglwd=0.8, 
	   vlcex=0.7, centerzero=T, caxislabels=seq(down, up, length.out=6),
	   )
	legend(x=0.78, y=1.35, legend = c('Resident', 'Registered'), #x=0.75, y=1.3
		   bty = "n", pch=20 , col=colors_border , text.col = "black", title=paste(yeari,para),
		   cex=1, pt.cex=2)
	dev.off()
}
for(yeari in c(1990,2000,2010)){RAD(yeari, 'Beta')}
for(yeari in c(1990,2000,2010)){RAD(yeari, 'Intercept')}
for(yeari in c(1990,2000,2010)){RAD(yeari, 'Rsquare')}


####################
# POP density 

cordf2 = data.frame(POP = log(c(cordf1$xindex, cordf1$yindex)), 
					R = c(rep('Resident',nrow(cordf1)),rep('Registered',nrow(cordf1))),
					year = rep(cordf1$year,2)
					)



for (yeari in c(1990,2000,2010)){

cordf3 = subset(cordf2,cordf2$year==yeari)
a = aggregate(cordf3$POP, by=list(cordf3$year,cordf3$R),mean)

pt = t.test(log(cordf3[cordf3$R=='Resident','POP']), log(cordf3[cordf3$R=='Registered','POP']), paired=T,alternative = 'two.sided')
zf = round(pt$conf.int[2] - pt$estimate,3)
print(paste0(yeari, ': log(POP.Resident)-log(POP.Registered)=', round(pt$estimate,3),'±',zf))

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_DJS_sxq_R/POPdensity',yeari,'.png'),
			  width=15,height=15, units='cm',res=180)
p = ggplot(cordf3, aes(x=POP)) +
	geom_density(aes(color = R),lwd=1.3) +
	geom_vline(data=a, aes(xintercept=x, color=Group.2),
             linetype="dashed") +
	labs(x='Population size (log)',y='Density') +
	guides(color=guide_legend(paste0('Δx = ', round(pt$estimate,3),' ± ',zf,' \n \n',yeari)))+
	theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_blank(),
	  panel.grid.major = element_blank(),
	  legend.key = element_rect(fill = "transparent", color = "transparent"), 
	  legend.position=c(0.8,0.7),
	  #plot.background = element_rect(fill = "transparent",colour = NA),
	  legend.title=element_text(size=14)
	  #,
	  #axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1)
	  )
print(p)
dev.off()
}



