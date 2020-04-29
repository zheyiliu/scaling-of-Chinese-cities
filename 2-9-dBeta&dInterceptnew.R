# mydf$financial = rgb(0,0,0,150,maxColorValue=255)
# mydf$financial[which(mydf$d.deposit.rate > 0)] = rgb(225,0,0,150,maxColorValue=255)
# mydf$financial[which(mydf$d.deposit.rate < 0)] = rgb(0,0,255,150,maxColorValue=255)
# 
# plot(mydf$dBeta, mydf$dIntercept, type='n',xlim=c(-0.1,0.1), col=mydf$financial)
# text(mydf$dBeta, mydf$dIntercept, labels=mydf$year, col=mydf$financial,cex=(range01(mydf1$d.deposit.rate)+0.6))
# #plot(dBeta, dIntercept,xlim=c(-0.1,0.1), col=drate,cex=(range01(mydf$d.deposit.rate)*2+0.5))
# abline(v=0)
# abline(h=0)

library(ggplot2)
home = 'C:/Sync/CoolGirl/Fhe'
modelname='OLS1_DJS_origin'
setwd(paste0(home, '/Results/',modelname))
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
#setwd('C:/Sync/CoolGirl/Fhe/Results/OLS1_DJS/fromPC/DJS')
load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))

sumlmHorizontal = sumlmHorizontal[which(sumlmHorizontal$Observation>=200),]
del = sumlmHorizontal$yIndex=='FixedAssets' & sumlmHorizontal$year %in% c(1998,1999)
sumlmHorizontal = sumlmHorizontal[!del,]
del = sumlmHorizontal$yIndex=='GDP' & sumlmHorizontal$year < 1991
sumlmHorizontal = sumlmHorizontal[!del,]

#load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))

gdpdf = sumlmHorizontal[which(sumlmHorizontal$yIndex=='GDP'),]
gdpdf = na.omit(gdpdf)
gdpdf = gdpdf[,c('Beta', 'Intercept', 'year')]
gdpdf$year = gdpdf$year - 1

gdpall = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/GDPall.csv', header=T)
urbanization = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/urbanization.csv', header=T)
rate = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/DepositRate.csv', header=T)

upyear = 1984:2016
upyear0 = c(gdpdf$year[gdpdf$year %in% upyear])
upyear0 = 1984:2016

dIntercept = vector()
dBeta = vector()
dyear = vector()
for (i in 1:length(upyear0)){
  gdp1a = gdpdf[gdpdf$year==upyear0[i],]$Intercept
  gdp2a = gdpdf[gdpdf$year==(upyear0[i]+1),]$Intercept
  if (length(gdp2a)!=0){
    da = gdp2a - gdp1a
    gdp1b = gdpdf[gdpdf$year==upyear0[i],]$Beta
    gdp2b = gdpdf[gdpdf$year==upyear0[i]+1,]$Beta
    db = gdp2b - gdp1b
    dIntercept[i] = da
    dBeta[i] = db
    dyear[i] = upyear0[i] + 1
  }
}

dd = data.frame(year=dyear, dBeta=dBeta, dIntercept=dIntercept)

mydf = merge(gdpdf, gdpall, by='year')
mydf = merge(mydf, urbanization, by='year')
mydf = merge(mydf, rate, by='year')
mydf = merge(mydf, dd, by='year')

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
mydf1 = mydf
for (n in 2:ncol(mydf1)){mydf1[,n] = range01(mydf1[,n])}

plot(mydf1$year, mydf1$Beta, col=1)
lines(mydf1$year, mydf1$Beta, col=1)

points(mydf1$year, mydf1$deposit.rate, col=3)
lines(mydf1$year, mydf1$deposit.rate, col=3)

points(mydf1$year, mydf1$gdp.growth.rate,col=4)
lines(mydf1$year, mydf1$gdp.growth.rate,col=4)

points(mydf1$year, mydf1$d.gdp.rate, col=2)
lines(mydf1$year, mydf1$d.gdp.rate, col=2)

plot(mydf$year,mydf$d.gdp.rate)
lines(mydf$year,mydf$d.gdp.rate)
abline(h=0)

####################################################################
mydf$up = 'a'
mydf$up[which(mydf$year %in% c(1998:2004))] = 'b'
mydf$up[which(mydf$year %in% c(2005:2008))] = 'c'
mydf2 = subset(mydf, mydf$up!='a')

png(filename=paste0(home,'/Results/dBetadIntercept/TotalPolicy.png'),
    width=17,height=15, units='cm',res=180)
p = ggplot(data=mydf2, aes(x=dBeta, y=dIntercept, color=up,cex=2)) + 
  geom_text(#vjust = 0, nudge_y = 0.01, 
    aes(label=year, color=up)) +
  scale_color_manual(values=alpha(c('red','blue'), 1)) +
  xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x ='Beta', y='Intercept') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)
dev.off()

####################################################################
mydf$speed = 'a' #GDP增速没有变化
mydf$speed[which(mydf$d.gdp.rate > 0)] ='b'  #GDP增速加速
mydf$speed[which(mydf$d.gdp.rate < 0)] ='c'  #GDP增速减缓

png(filename=paste0(home,'/Results/dBetadIntercept/GDPgrowth.png'),
    width=17,height=15, units='cm',res=180)
p = ggplot(data=mydf, aes(x=dBeta, y=dIntercept, color=speed)) + 
  geom_text(#vjust = 0, nudge_y = 0.01, 
    aes(label=year, color=speed,size=(range01(mydf1$d.gdp.rate)*2+0.5))) +
  scale_color_manual(values=alpha(c('red','blue'), 0.6)) +
  xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x ='Beta', y='Intercept') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)
dev.off()


####################################################################

png(filename=paste0(home,'/Results/dBetadIntercept/GDPgrowth1.png'),
    width=17,height=15, units='cm',res=180)
p = ggplot(data=mydf, aes(x=dBeta, y=dIntercept)) + 
  geom_point(cex=(mydf$gdp.growth.rate^1.2)/2, color=alpha('red',0.5)) +
  #geom_text(#vjust = 0, nudge_y = 0.01, 
  #  aes(label=year), size=(mydf$gdp.growth.rate^1.2)/2) +
  #scale_color_manual(values=alpha('red',0.5)) +
  xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x ='Beta', y='Intercept') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)
dev.off()

#####################################################################
mydf$financial = 'a' #没有宏观操作
mydf$financial[which(mydf$d.deposit.rate > 0)] ='b'  #积极货币政策
mydf$financial[which(mydf$d.deposit.rate < 0)] ='c'  #紧缩性货币政策

png(filename=paste0(home,'/Results/dBetadIntercept/MonetaryPolicy.png'),
    width=17,height=15, units='cm',res=180)
p = ggplot(data=mydf, aes(x=dBeta, y=dIntercept, color=financial)) + 
  geom_text(#vjust = 0, nudge_y = 0.01, 
    aes(label=year, color=financial,size=(range01(mydf1$d.deposit.rate)*2+0.5))) +
  scale_color_manual(values=alpha(c('black','red','blue'), 0.6)) +
  xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x ='Beta', y='Intercept') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)
dev.off()


# 
# Area$range = 'T'
# Area$range[which(grepl('市辖区', Area$index))] = 'D'
# POP$range = 'T'
# POP$range[which(grepl('市辖区', POP$index))] = 'D'
# GDP$range = 'T'
# GDP$range[which(grepl('市辖区', GDP$index))] = 'D'
# a = merge(GDP, POP[,-1],by=c('city','year','range'))
# b = subset(a, a$range=='T' & a$year==2016)
# plot(b$value.y, b$value.x)
# plot(log(b$value.y), log(b$value.x))
# f = lm(log(b$value.x) ~ log(b$value.y))
# summary(f)
# b


gdpdf = sumlmHorizontal[which(sumlmHorizontal$yIndex=='Area'),]
gdpdf = na.omit(gdpdf)
gdpdf = gdpdf[,c('Beta', 'Intercept', 'year')]
gdpdf$year = gdpdf$year - 1

upyear = 1984:2016
upyear0 = c(gdpdf$year[gdpdf$year %in% upyear])


dIntercept = vector()
dBeta = vector()
dyear = vector()
for (i in 1:length(upyear0)){
  gdp1a = gdpdf[gdpdf$year==upyear0[i],]$Intercept
  gdp2a = gdpdf[gdpdf$year==(upyear0[i]+1),]$Intercept
  if (length(gdp2a)!=0){
    da = gdp2a - gdp1a
    gdp1b = gdpdf[gdpdf$year==upyear0[i],]$Beta
    gdp2b = gdpdf[gdpdf$year==upyear0[i]+1,]$Beta
    db = gdp2b - gdp1b
    dIntercept[i] = da
    dBeta[i] = db
    dyear[i] = upyear0[i] + 1
  }
}

dd = data.frame(year=dyear, dBeta=dBeta, dIntercept=dIntercept)

mydf = merge(mydf, dd, by='year')

mydf$up = 'a'
mydf$up[which(mydf$year %in% c(1998:2004))] = 'b'
mydf$up[which(mydf$year %in% c(2005:2008))] = 'c'
mydf2 = subset(mydf, mydf$up!='a')

png(filename=paste0(home,'/Results/dBetadIntercept/TotalPolicy.png'),
    width=17,height=15, units='cm',res=180)
p = ggplot(data=mydf2, aes(x=dBeta, y=dIntercept, color=up,cex=2)) + 
  geom_text(#vjust = 0, nudge_y = 0.01, 
    aes(label=year, color=up)) +
  scale_color_manual(values=alpha(c('red','blue'), 1)) +
  xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x ='Beta', y='Intercept') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)
dev.off()




########################
############ 4 years
#######################
load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))
sumlmHorizontal = sumlmHorizontal[which(sumlmHorizontal$Observation>=200),]
del = sumlmHorizontal$yIndex=='FixedAssets'
sumlmHorizontal = sumlmHorizontal[!del,]

#yearrange = c(1986,1996,2006,2016)
yearrange = 1984:2016
forclus = sumlmHorizontal[,c('yIndex','Beta','Intercept','year')]
forclus$year = forclus$year - 1
forclus = forclus[forclus$year %in% yearrange,]

forclus$stage = NA
forclus[forclus$year %in% 1984:1991,]$stage = 1
forclus[forclus$year %in% 1992:1998,]$stage = 2
forclus[forclus$year %in% 1999:2004,]$stage = 3
forclus[forclus$year %in% 2005:2016,]$stage = 4
stagenum = unique(forclus$stage)
forclus$indexstage = paste(forclus$yIndex,forclus$stage)
#forclus = aggregate(forclus[,c('Beta','Intercept')], by=list(yIndex=forclus$yIndex,stage=forclus$stage), mean)


######## year
x2 = split(forclus, f=forclus$yIndex)
yearcom = data.frame(year=yearrange)
x1 = lapply(x2, FUN = function(x){merge(yearcom, x, by='year', all=T)})
#x1 = lapply(x2, FUN = function(x){merge(stagecom, x, by='stage', all=T)})

xall = data.frame()
for (i in 1:length(x1)){
  x0 = x1[[i]]
  diffbeta = data.frame(x0[-1,])
  for(n in 3:4){
    diffbeta[,n] = diff(x0[,n])
  }
  xall = rbind(xall, diffbeta)
}

col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','DepositHousehold','Electricity','FixedAssets','GDP','Loan','PostTele','Retail','Salary','WasteWater','Water')
infrasdf = c('Cinema', 'CityRoadArea','Doctor','Gas.Length','Green','GreenBuilt','Hospital','HospitalBerth','School','PavedRoad.Length','PrimarySchool','PrimaryTeacher','Sewage.Length','WaterSupply.Length')
needdf = c('ElectricityResident','LivingSpace','WaterResident')
areadf = c('Area', 'AreaBuilt')

col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','DepositHousehold','Electricity','FixedAssets','GDP','Loan','PostTele','Retail','Salary','WasteWater','Water')
infrasdf = c('Cinema', 'CityRoadArea','Doctor','Green','HospitalBerth','Sewage.Length')
needdf = c('ElectricityResident','WaterResident')
areadf = c('AreaBuilt')

xall$type=NA
xall[xall$yIndex %in% economoicdf,]$type = 'Socio-economic'
xall[xall$yIndex %in% infrasdf,]$type = 'Infrastructure'
xall[xall$yIndex %in% needdf,]$type = 'Basic Services'
xall[xall$yIndex %in% areadf,]$type = 'Land Use'
xall = xall[!is.na(xall$type),]

xall$color=NA
xall[xall$yIndex %in% economoicdf,]$color = "#F8766D"
xall[xall$yIndex %in% infrasdf,]$color = "#00BA38"
xall[xall$yIndex %in% needdf,]$color = "#619CFF"
xall[xall$yIndex %in% areadf,]$color = "grey52"
xall = xall[!is.na(xall$color),]

# xall$type=NA
# xall[xall$yIndex %in% economoicdf,]$type = 'socio-economic'
# xall[xall$yIndex %in% infrasdf,]$type = 'infrastructure'
# xall[xall$yIndex %in% needdf,]$type = 'individual need'
# xall[xall$yIndex %in% areadf,]$type = 'Area'
# xall = xall[!is.na(xall$type),]
# 
# xall$color=NA
# xall[xall$yIndex %in% economoicdf,]$color = "#F8766D"
# xall[xall$yIndex %in% infrasdf,]$color = "#00BA38"
# xall[xall$yIndex %in% needdf,]$color = "grey52"
# xall[xall$yIndex %in% areadf,]$color = "#619CFF"
# xall = xall[!is.na(xall$color),]

xall = na.omit(xall)

xall0 = xall
#xall = xall[xall$year!=1994,]

a = aggregate(xall, list(xall$year), mean)

png(filename=paste0(home, '/Results/',modelname,'/dbetadintercept/try1.png'),
    width=23,height=9, units='cm',res=180)
par(mfrow=c(1,3),oma=c(0,0,0,0))
plot(xall$year, xall$Beta,xlab='Year',ylab='Δβ',cex.axis=1,cex.lab=1.5,cex=1,col=xall$color,pch=20)
points(a$year,a$Beta,col=1,pch=20,cex=2.5)
abline(h=0)
abline(v=c(1992.5, 1998.5, 2004.5),lty=2)
plot(xall$year, xall$Intercept,xlab='Year',ylab='Δα',cex.axis=1,cex.lab=1.5,cex=1,col=xall$color,pch=20)
points(a$year,a$Intercept,col=1,pch=20,cex=2.5)
abline(h=0)
abline(v=c(1992.5,1998.5, 2004.5),lty=2)
plot(a$Beta, a$Intercept,xlab='Δβ',ylab='Δα',col=a$stage,cex.axis=1,cex.lab=1.5,cex=1.3,pch=19,xlim=c(-0.05,0.05))
abline(h=0)
abline(v=0)
dev.off()


p = ggplot(data=xall, aes(x=year, y=Intercept, color=type, cex=2)) + 
  scale_colour_manual(values = alpha(col,0.4)) +
  geom_point(cex=2) + #,pch=1 空心圆
  #geom_point(aes(x=Group.1,y=x),data=a, color=1) +
  #geom_text(aes(label=substr(as.character(yIndex),1,4), color=type),cex=3) +
  facet_wrap(.~type, nrow=2, scales='fixed') +
  #xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=c(1992.5,1998.5,2004.5),lty=2) +
  labs(x ='Year', y='Δα') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)


a$stage = as.factor(a$stage)
p = ggplot(data=a, aes(x=Beta, y=Intercept, color=stage, cex=2)) + 
  scale_colour_manual(values = alpha(c(1,2,3,4),1)) +
  #geom_point(cex=2) + #,pch=1 空心圆
  #geom_point(aes(x=Group.1,y=x),data=a, color=1) +
  geom_text(aes(label=year, color=stage),cex=5) +
  #facet_wrap(.~type, nrow=2, scales='fixed') +
  xlim(-0.1,0.1) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x ='Δβ', y='Δα') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)


xall1 = xall[xall$year!=1994,]
a = aggregate(xall1$Intercept, list(xall1$year), mean)
b = aggregate(xall1$Beta, list(xall1$year), mean)
p = ggplot(data=xall1, aes(x=year, y=Beta, color=type, cex=2)) + 
  scale_colour_manual(values = alpha(col,0.4)) +
  geom_point(cex=2) +
  #geom_point(aes(x=Group.1,y=x),data=b, color=1) +
  #geom_text(aes(label=substr(as.character(yIndex),1,4), color=type),cex=3) +
  facet_wrap(.~type, nrow=2, scales='fixed') +
  #xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=c(1992.5,1998.5,2004.5),lty=2) +
  labs(x ='Year', y='Δβ') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)

#####################

#########################################################
########### 4 stages
####################
yearrange = 1985:2017
forclus = sumlmHorizontal[,c('yIndex','Beta','Intercept','year')]
forclus$year = forclus$year - 1
#liudong = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialData/原始数据/POPmobility.csv',header=T)[,c(1,3)]
#forlm = merge(forclus, liudong, all=T)
#x2 = split(forlm, f=forlm$yIndex)
#f = lm(x2$GDP$Beta~x2$GDP$FloatingPOP)
#summary(f)

forclus$year = forclus$year - 1
forclus$stage = NA
forclus[forclus$year %in% 1984:1991,]$stage = 1
forclus[forclus$year %in% 1992:1998,]$stage = 2
forclus[forclus$year %in% 1999:2004,]$stage = 3
forclus[forclus$year %in% 2005:2016,]$stage = 4
stagenum = unique(forclus$stage)
forclus$indexstage = paste(forclus$yIndex,forclus$stage)
forclus = aggregate(forclus[,c('Beta','Intercept')], by=list(yIndex=forclus$yIndex,stage=forclus$stage), mean)


x2 = split(forclus, f=forclus$yIndex)
stagecom = data.frame(stage=1:length(stagenum))
x1 = lapply(x2, FUN = function(x){merge(stagecom, x, by='stage', all=T)})

xall = data.frame()
for (i in 1:length(x1)){
  x0 = x1[[i]]
  diffbeta = data.frame(x0[-1,])
  for(n in c(3,4)){
    diffbeta[,n] = diff(x0[,n])
  }
  xall = rbind(xall, diffbeta)
}

col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','DepositHousehold','Electricity','FixedAssets','GDP','Loan','PostTele','Retail','Salary','WasteWater','Water')
infrasdf = c('Cinema', 'CityRoadArea','Doctor','Gas.Length','Green','GreenBuilt','Hospital','HospitalBerth','School','PavedRoad.Length','PrimarySchool','PrimaryTeacher','Sewage.Length','WaterSupply.Length')
needdf = c('ElectricityResident','LivingSpace','WaterResident')
areadf = c('Area', 'AreaBuilt')

col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','DepositHousehold','Electricity','FixedAssets','GDP','Loan','PostTele','Retail','Salary','WasteWater','Water')
infrasdf = c('Cinema', 'CityRoadArea','Doctor','Green','HospitalBerth','Sewage.Length')
needdf = c('ElectricityResident','WaterResident')
areadf = c('AreaBuilt')

xall$type=NA
xall[xall$yIndex %in% economoicdf,]$type = 'Socioeconomics'
xall[xall$yIndex %in% infrasdf,]$type = 'Infrastructure'
xall[xall$yIndex %in% needdf,]$type = 'Basic individual service'
xall[xall$yIndex %in% areadf,]$type = 'Area'
xall = xall[!is.na(xall$type),]

xall$stage = as.factor(xall$stage)
p = ggplot(data=xall, aes(x=Beta, y=Intercept, color=stage,cex=2)) + 
  #scale_colour_manual(values = alpha(col,1)) +
  #geom_point() +
  #geom_text(aes(label=paste0(substr(as.character(yIndex),1,3),stage), color=type), cex=3) +
  geom_text(aes(label=paste0(yIndex,stage), color=stage), cex=3) +
  #xlim(-0.25,0.25) +
  facet_wrap(. ~ type, ncol=2) +
  #xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x ='dBeta', y='dIntercept') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)



#############################
#############################
yearrange = 1985:2017
forclus = sumlmHorizontal[,c('yIndex','Beta','Intercept','year')]

forclus$year = forclus$year - 1
forclus$stage = NA
forclus[forclus$year %in% 1984:1987,]$stage = 0
forclus[forclus$year %in% 1991:1994,]$stage = 1
forclus[forclus$year %in% 2002:2005,]$stage = 2
forclus[forclus$year %in% 2007:2010,]$stage = 3
forclus[forclus$year %in% 2013:2016,]$stage = 4
stagenum = unique(forclus$stage)
forclus$indexstage = paste(forclus$yIndex,forclus$stage)
forclus = aggregate(forclus[,c('Beta','Intercept')], by=list(yIndex=forclus$yIndex,stage=forclus$stage), mean)

x2 = split(forclus, f=forclus$yIndex)
stagecom = data.frame(stage=1:length(stagenum))
x1 = lapply(x2, FUN = function(x){merge(stagecom, x, by='stage', all=T)})

xall = data.frame()
for (i in 1:length(x1)){
  x0 = x1[[i]]
  diffbeta = data.frame(x0[-1,])
  for(n in c(3,4)){
    diffbeta[,n] = diff(x0[,n])
  }
  xall = rbind(xall, diffbeta)
}

col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','DepositHousehold','Electricity','FixedAssets','GDP','Loan','PostTele','Retail','Salary','WasteWater','Water')
infrasdf = c('Cinema', 'CityRoadArea','Doctor','Gas.Length','Green','GreenBuilt','Hospital','HospitalBerth','School','PavedRoad.Length','PrimarySchool','PrimaryTeacher','Sewage.Length','WaterSupply.Length')
needdf = c('ElectricityResident','LivingSpace','WaterResident')
areadf = c('Area', 'AreaBuilt')


col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','DepositHousehold','Electricity','FixedAssets','GDP','Loan','PostTele','Retail','Salary','WasteWater','Water')
infrasdf = c('Cinema', 'CityRoadArea','Doctor','Green','HospitalBerth','Sewage.Length')
needdf = c('ElectricityResident','WaterResident')
areadf = c('AreaBuilt')

xall$type=NA
xall[xall$yIndex %in% economoicdf,]$type = 'Socioeconomics'
xall[xall$yIndex %in% infrasdf,]$type = 'Infrastructure'
xall[xall$yIndex %in% needdf,]$type = 'Basic individual service'
xall[xall$yIndex %in% areadf,]$type = 'Area'
xall = xall[!is.na(xall$type),]

xall$stage = as.factor(xall$stage)
p = ggplot(data=xall, aes(x=Beta, y=Intercept, color=stage,cex=2)) + 
  #scale_colour_manual(values = alpha(col,1)) +
  #geom_point() +
  #geom_text(aes(label=paste0(substr(as.character(yIndex),1,3),stage), color=type), cex=3) +
  geom_text(aes(label=paste0(yIndex,stage), color=stage), cex=3) +
  #geom_text(aes(label=stage, color=stage), cex=5) +
  xlim(-0.25,0.25) +
  facet_wrap(. ~ type, ncol=2) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x ='Δβ', y='Δα') +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color=NA), 
    panel.grid.major = element_line(color=NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position="none"
  )
print(p)
