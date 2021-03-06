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
modelname='OLS_DJS_sxq'
setwd(paste0(home, '/Results/',modelname))
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
#setwd('C:/Sync/CoolGirl/Fhe/Results/OLS1_DJS/fromPC/DJS')
#load(paste0('200_sumlmHorizontal_',rangeStatList[2],'.Rdata'))
load(paste0('sumlmHorizontal_Districts_cluster.Rdata'))


dir.create(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/dbetadintercept/"),showWarnings = F)
setwd(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/dbetadintercept"))
file.remove(dir())

#load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))
#del = sumlmHorizontal$yIndex %in% c('FixedAssets')
#sumlmHorizontal = sumlmHorizontal[!del,]

# load('x3best.Rdata')
# 
# methodimp = 'norm'
# forclus = sumlmHorizontal[,c('yIndex','Intercept', 'year')]
# head(forclus)
# x1 = split(forclus, f=forclus$yIndex)
# x0 = x1[[1]][,c(2,3)]
# for (i in 2:length(x1)){
#   x0 = merge(x0, x1[[i]][,c(2,3)], by='year',all=T)  
# }
# colnames(x0) = c('year',names(x1) )
# tt = apply(is.na.data.frame(x0), 2, sum)
# x0 = x0[,c(names(tt[tt<20]))]
# 
# png(filename=paste0(home, '/Results/',modelname,'/clustering/imputation.png'),
#     width=15,height=15, units='cm',res=150)
# md.pattern(x0,rotate.name=T)
# dev.off()
# #fit = with(x0, lm(Sewage.Length~year))
# #summary(fit)
# 
# ini <- mice(x0, pred=quickpred(x0, mincor=.9), print=F)
# set.seed(777)
# imp <- mice(x0, pred=ini$pred, print=F, method=methodimp)
# #imp40 <- mice.mids(imp, print=F, maxit=35)
# #plot(imp40)
# 
# #fit = with(imp, lm(Sewage.Length~year))
# fit = with(imp, lm(Loan~year))
# summary(fit)
# pooled = pool(fit)
# summary(pooled)
# 
# x4 = complete(imp,action=3)
# stripplot(imp, Sewage.Length~.imp, pch=20, cex=2)
# plot(x0$year, x0$Sewage.Length)
# points(x4$year,x4$Sewage.Length,col=2)
# save(x4,file='x4best.Rdata')
# 
# 
# newyindex = rep(colnames(x3)[-1],each=nrow(x3))
# newBeta = unlist(c(x3[,c(2:ncol(x3))]))
# newIntercept = unlist(c(x4[,c(2:ncol(x3))]))
# newyear = rep(x3$year,ncol(x3)-1)
# newsum = data.frame(yIndex=newyindex, Beta=newBeta, Intercept=newIntercept, year=newyear)

########################
############ 4 years
#######################

yearrange = 1984:2017
forclus = na.omit(sumlmHorizontal[,c('yIndex','Beta','Intercept','year')])
#forclus = newsum
for1 = unique(sumlmHorizontal[,c('yIndex','type','clusMember')])
forclus$year = forclus$year - 1
forclus = forclus[forclus$year %in% yearrange,]

forclus$stage = NA
forclus[forclus$year %in% 1984:1991,]$stage = 1
forclus[forclus$year %in% 1992:1998,]$stage = 2
forclus[forclus$year %in% 1999:2004,]$stage = 3
forclus[forclus$year %in% 2005:2017,]$stage = 4

# forclus$stage = NA
# forclus[forclus$year %in% 1984:1987,]$stage = 0
# forclus[forclus$year %in% 1991:1994,]$stage = 1
# forclus[forclus$year %in% 2002:2005,]$stage = 2
# forclus[forclus$year %in% 2007:2010,]$stage = 3
# forclus[forclus$year %in% 2013:2017,]$stage = 4

stagenum = unique(forclus$stage)
forclus$indexstage = paste(forclus$yIndex,forclus$stage)
# forclus1 = aggregate(forclus[,c('Beta','Intercept')], by=list(yIndex=forclus$yIndex,stage=forclus$stage), mean)
# for1 = unique(forclus[,c(1,5,6)])
# forclus = merge(forclus1, for1, by.x='yIndex', by.y='yIndex')


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

xall = merge(xall, for1, by.x='yIndex', by.y='yIndex', all.x=T)
xall = na.omit(xall)

############################
#' economoicdf = c('Book','DepositHousehold','GDP','Loan','Retail','Salary')
#' #'Passenger', 'BusPassenger', 'Cinema', 'PostTele', 'Crash','Fire','Deposit','FixedAssets','Electricity','Water','WasteWater'
#' infrasdf = c('CityRoadArea','Green')
#' #'Gas.Length','PavedRoad.Length','WaterSupply.Length','Bus','Sewage.Length'
#' needdf = c('Doctor','Hospital','HospitalBerth','PrimarySchool','PrimaryTeacher')
#' #'ElectricityResident','WaterResident','School','LivingSpace'
#' areadf = c('Area', 'AreaBuilt')
#' 
############################
# col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
# economoicdf = c('Book','DepositHousehold','Electricity','FixedAssets','GDP','Loan','PostTele','Retail','Salary','WasteWater','Water','Bus','Cinema')
# infrasdf = c('CityRoadArea','Green','Sewage.Length')
# needdf = c('ElectricityResident','WaterResident','Doctor','HospitalBerth')
# areadf = c('AreaBuilt')
# 
# xall$type=NA
# xall[xall$yIndex %in% economoicdf,]$type = 'Socio-economic'
# xall[xall$yIndex %in% infrasdf,]$type = 'Infrastructure'
# xall[xall$yIndex %in% needdf,]$type = 'Basic Services'
# xall[xall$yIndex %in% areadf,]$type = 'Land Use'
# xall = xall[!is.na(xall$type),]
# 
# xall$color=NA
# xall[xall$yIndex %in% economoicdf,]$color = "#F8766D"
# xall[xall$yIndex %in% infrasdf,]$color = "#00BA38"
# xall[xall$yIndex %in% needdf,]$color = "#619CFF"
# xall[xall$yIndex %in% areadf,]$color = "grey52"
# xall = xall[!is.na(xall$color),]
# 
# xall = na.omit(xall)

###############################
xall$color=NA
xall[xall$type == 'Socio-economic',]$color = "#F8766D"
xall[xall$type == 'Infrastructure',]$color = "#00BA38"
xall[xall$type == 'Basic Services',]$color = "#619CFF"
xall[xall$type == 'Land Use',]$color = "grey52"
xall = xall[!is.na(xall$color),]

###################################

xall$typeclus = paste(xall$type,xall$clusMember)

#xall = xall[xall$year!=1994,]
xall0 = xall
xall = xall0

alltype = split(xall, f=as.factor(xall$clusMember))
xall = alltype[[2]]
a = aggregate(xall, list(xall$year), mean)


# png(filename=paste0(home, '/Results/',modelname,'/dbetadintercept/4stages1.png'),
#     width=24.5,height=9, units='cm',res=180)
# par(mfrow=c(1,3),oma=c(0,0,0,0),mar=c(5,3.3,1,1)+0.1,mgp=c(2.3,1,0))
# plot(xall$year, xall$Beta,xlab='Year',ylab='Δβ',cex.axis=1,cex.lab=1.5,cex=1.5,col=xall$color,pch=20)
# points(a$year,a$Beta,col=1,pch=20,cex=2)
# abline(h=0)
# abline(v=c(1992.5, 1998.5, 2004.5),lty=2)
# plot(xall$year, xall$Intercept,xlab='Year',ylab='Δα',cex.axis=1,cex.lab=1.5,cex=1.5,col=xall$color,pch=20)
# points(a$year,a$Intercept,col=1,pch=20,cex=2)
# abline(h=0)
# abline(v=c(1992.5,1998.5, 2004.5),lty=2)
# plot(a$Beta, a$Intercept,xlab='Δβ',ylab='Δα',col=a$stage,cex.axis=1,cex.lab=1.5,cex=1.3,pch=19,xlim=c(-0.05,0.05), ylim=c(min(a$Intercept)-0.05, max(a$Intercept)+0.05))
# abline(h=0)
# abline(v=0)
# dev.off()

#col = c("#619CFF", "#00BA38", "grey52", "#F8766D")
col = c("#00BA38", "grey52", "#F8766D")
##### beta阶段图
png(filename=paste0(home, '/Results/',modelname,'/dbetadintercept/4stages1.png'),
    width=15,height=15, units='cm',res=180)
p = ggplot(data=xall, aes(x=year, y=Beta, color=type, cex=2)) + 
  scale_colour_manual(values = alpha(col,0.4)) +
  geom_point(cex=2) + #,pch=1 空心圆
  geom_point(aes(x=Group.1,y=Beta),data=a, color=1) +
  #geom_text(aes(label=substr(as.character(yIndex),1,4), color=type),cex=3) +
  #facet_wrap(.~type, nrow=2, scales='fixed') +
  #xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=c(1991.5,1998.5,2004.5),lty=2) +
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
dev.off()

##### alpha阶段图
png(filename=paste0(home, '/Results/',modelname,'/dbetadintercept/4stages2.png'),
    width=15,height=15, units='cm',res=180)
p = ggplot(data=xall, aes(x=year, y=Intercept, color=type, cex=2)) + 
  scale_colour_manual(values = alpha(col,0.4)) +
  geom_point(cex=2) + #,pch=1 空心圆
  geom_point(aes(x=Group.1,y=Intercept),data=a, color=1) +
  #geom_text(aes(label=substr(as.character(yIndex),1,4), color=type),cex=3) +
  #facet_wrap(.~type, nrow=2, scales='fixed') +
  #xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=c(1991.5,1998.5,2004.5),lty=2) +
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
dev.off()

##### 年份四分图
png(filename=paste0(home, '/Results/',modelname,'/dbetadintercept/4stages3.png'),
    width=15,height=15, units='cm',res=180)
lim = max(abs(a$Beta))+0.02
a$stage = as.factor(a$stage)
p = ggplot(data=a, aes(x=Beta, y=Intercept, color=stage, cex=2)) + 
  scale_colour_manual(values = alpha(c(1,2,3,4),1)) +
  #geom_point(cex=2) + #,pch=1 空心圆
  #geom_point(aes(x=Group.1,y=x),data=a, color=1) +
  geom_text(aes(label=year, color=stage),cex=5) +
  #facet_wrap(.~type, nrow=2, scales='fixed') +
  xlim(-lim,lim) +
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
dev.off()

##### 线性-全部-diff
png(filename=paste0(home, '/Results/',modelname,'/dbetadintercept/linearAll.png'),
    width=13,height=14.5, units='cm',res=100)
par(mgp=c(2.3,1,0))
plot(xall$Beta, xall$Intercept,xlab='Δβ',ylab='Δα',cex.axis=1,cex.lab=1.5,cex=1.5,col=xall$color,pch=20)
points(a$Beta,a$Intercept,col=1,pch=20,cex=2.5)
abline(h=0)
abline(v=0)
dev.off()

  

##### 线性-全部
png(filename=paste0(home, '/Results/',modelname,'/dbetadintercept/linearCheck.png'),
    width=18,height=14.5, units='cm',res=100)
ggplot(data=sumlmHorizontal, aes(x=Beta,y=Intercept,color=yIndex)) + 
  geom_point(alpha=0.5) +
  geom_smooth(se=F,span=0.9)
dev.off()


##################
##################
##### 分类阶段图
xall$forclass = paste0(xall$year, xall$type)
a = aggregate(xall, list(xall$forclass), mean)
ayear = substr(a$Group.1, 1,4)
atype = substr(a$Group.1, 5,nchar(a$Group.1))
a$type = atype

#a = a[!a$year %in% c(1994),]

png(filename=paste0(home, '/Results/',modelname,'/dbetadintercept/4stagesTypeIntercept.png'),
    width=17,height=17, units='cm',res=180)
p = ggplot(data=xall, aes(x=year, y=Intercept, color=type, cex=2)) + 
  scale_colour_manual(values = alpha(col,0.4)) +
  geom_point(cex=2) + #,pch=1 空心圆
  geom_point(aes(x=year,y=Intercept),data=a, color=1, cex=2) +
  #geom_text(aes(label=substr(as.character(yIndex),1,4), color=type),cex=3) +
  facet_wrap(.~type, nrow=2, scales='fixed') +
  #xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=c(1991.5,1998.5,2004.5),lty=2) +
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
dev.off()

#####
png(filename=paste0(home, '/Results/',modelname,'/dbetadintercept/4stagesTypeBeta.png'),
    width=17,height=17, units='cm',res=180)
p = ggplot(data=xall, aes(x=year, y=Beta, color=type, cex=2)) + 
  scale_colour_manual(values = alpha(col,0.4)) +
  geom_point(cex=2) + #,pch=1 空心圆
  geom_point(aes(x=year,y=Beta),data=a, color=1, cex=2) +
  #geom_text(aes(label=substr(as.character(yIndex),1,4), color=type),cex=3) +
  facet_wrap(.~type, nrow=2, scales='fixed') +
  #xlim(-0.15,0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=c(1991.5,1998.5,2004.5),lty=2) +
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
dev.off()


##############
png(filename=paste0(home, '/Results/',modelname,'/dbetadintercept/4stagesTypeYear.png'),
    width=17,height=17, units='cm',res=180)
lim = max(abs(a$Beta))+0.02
a$stage = as.factor(a$stage)
p = ggplot(data=a, aes(x=Beta, y=Intercept, color=stage, cex=2)) + 
  scale_colour_manual(values = alpha(c(1,2,3,4),1)) +
  #geom_point(cex=2) + #,pch=1 空心圆
  #geom_point(aes(x=Group.1,y=x),data=a, color=1) +
  geom_text(aes(label=year, color=stage),cex=3) +
  facet_wrap(.~type, nrow=2, scales='fixed') +
  xlim(-lim,lim) +
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
dev.off()





# ####### 分类-四分图
# xall$stage = as.factor(xall$stage)
# p = ggplot(data=xall, aes(x=Beta, y=Intercept, color=stage,cex=2)) +
#   #scale_colour_manual(values = alpha(col,1)) +
#   #geom_point() +
#   #geom_text(aes(label=paste0(substr(as.character(yIndex),1,3),stage), color=type), cex=3) +
#   geom_text(aes(label=paste0(yIndex,stage), color=stage), cex=3) +
#   #xlim(-0.25,0.25) +
#   facet_wrap(. ~ type, ncol=2) +
#   #xlim(-0.15,0.15) +
#   geom_hline(yintercept=0) +
#   geom_vline(xintercept=0) +
#   labs(x ='dBeta', y='dIntercept') +
#   theme(
#     text = element_text(size=18),
#     panel.background = element_rect(fill = "transparent",colour = 'black'),
#     panel.grid.minor = element_line(color=NA),
#     panel.grid.major = element_line(color=NA),
#     plot.background = element_rect(fill = "transparent",colour = NA),
#     legend.position="none"
#   )
# print(p)



yearrange = 1984:2017
forclus = na.omit(sumlmHorizontal[,c('yIndex','Beta','Intercept','year')])
#forclus = newsum
for1 = unique(sumlmHorizontal[,c('yIndex','type','clusMember')])
forclus$year = forclus$year - 1
forclus = forclus[forclus$year %in% yearrange,]

forclus$stage = NA
forclus[forclus$year %in% 1984:1991,]$stage = 1
forclus[forclus$year %in% 1992:1998,]$stage = 2
forclus[forclus$year %in% 1999:2004,]$stage = 3
forclus[forclus$year %in% 2005:2017,]$stage = 4

stagenum = unique(forclus$stage)
forclus$indexstage = paste(forclus$yIndex,forclus$stage)

######## year
x2 = split(forclus, f=forclus$yIndex)
yearcom = data.frame(year=yearrange)
x11 = lapply(x2, FUN = function(x){merge(yearcom, x, by='year', all=T)})
#x1 = lapply(x2, FUN = function(x){merge(stagecom, x, by='stage', all=T)})
lapply(x11, function(x){
  #x$Beta<-scale(x$Beta, center=T, scale=F)
  x$Intercept<-scale(x$Intercept, center=T, scale=F)
  return(x)
}) -> x12


#x1 = x11 #没有scale
x1 = x12 #scale
#########################
xall = data.frame()
for (i in 1:length(x1)){
  x0 = x1[[i]]
  diffbeta = data.frame(x0[c(-1),])
  for(n in 3:4){
    diffbeta[,n] = diff(x0[,n],lag=1)
  }
  xall = rbind(xall, diffbeta)
} #diff

x13 = data.frame()
for (i in 1:length(x12)){
  x13 = rbind(x13,x12[[i]])
}
xall1 = x13 #orgin

################################
################################
################################

xall = merge(xall, for1, by.x='yIndex', by.y='yIndex', all.x=T)
xall = na.omit(xall)

xall$color=NA
xall[xall$type == 'Socio-economic',]$color = "#F8766D"
xall[xall$type == 'Infrastructure',]$color = "#00BA38"
xall[xall$type == 'Basic Services',]$color = "#619CFF"
xall[xall$type == 'Land Use',]$color = "grey52"
xall = xall[!is.na(xall$color),]

###################################

xall$typeclus = paste(xall$type,xall$clusMember)

#xall = xall[xall$year!=1994,]
xall0 = xall
xall = xall0

alltype = split(xall, f=as.factor(xall$clusMember))
xall = alltype[[2]]
a = aggregate(xall, list(xall$year), mean)
outa = table(xall$year)
out = names(outa[outa < 10])
a = a[!a$year %in% out,]

####################################
####################################
####################################

xall1 = merge(xall1, for1, by.x='yIndex', by.y='yIndex', all.x=T)
xall1 = na.omit(xall1)

xall1$color=NA
xall1[xall1$type == 'Socio-economic',]$color = "#F8766D"
xall1[xall1$type == 'Infrastructure',]$color = "#00BA38"
xall1[xall1$type == 'Basic Services',]$color = "#619CFF"
xall1[xall1$type == 'Land Use',]$color = "grey52"
xall1 = xall1[!is.na(xall1$color),]

###################################

xall1$typeclus = paste(xall1$type,xall1$clusMember)

#xall1 = xall1[xall1$year!=1994,]
xall10 = xall1
xall1 = xall10

alltype = split(xall1, f=as.factor(xall1$clusMember))
xall1 = alltype[[2]]
a = aggregate(xall1, list(xall1$year), mean)
outa = table(xall1$year)
out = names(outa[outa < 10])
a = a[!a$year %in% out,]

#############################################

df = xall #diff
df = xall1 #orgin

ggplot(data=df, aes(x=Beta, y=Intercept, color=year)) +
  geom_point(alpha=0.5)

ggplot(data=df, aes(x=Beta,y=Intercept,color=yIndex)) + 
  geom_point(alpha=0.5) +
  geom_smooth(se=F,span=0.9)

ggplot(data=df, aes(x=year,y=Beta,color=yIndex)) + 
  geom_point(alpha=0.5) +
  geom_smooth(se=F,span=0.9)

ggplot(data=df, aes(x=year,y=Intercept,color=yIndex)) + 
  geom_point(alpha=0.5) +
  geom_smooth(se=F,span=0.9)


# alltype = split(sumlmHorizontal, f=as.factor(sumlmHorizontal$clusMember))
# sumlmHorizontal = alltype[[2]]
# sumlmHorizontal = sumlmHorizontal[!is.na(sumlmHorizontal$type),]