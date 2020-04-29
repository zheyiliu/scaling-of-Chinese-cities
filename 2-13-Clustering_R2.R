library(ggplot2)
library(mice)

home = 'C:/Sync/CoolGirl/Fhe'
modelname='OLS1_DJS'
setwd(paste0(home, '/Results/',modelname))
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
#setwd('C:/Sync/CoolGirl/Fhe/Results/OLS1_DJS/fromPC/DJS')
load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))

sumlmHorizontal = sumlmHorizontal[which(sumlmHorizontal$Observation>=200),]
#load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))

#经济 #基建 #个人需要
## 删掉了BusPassenger,Passenger,Bus,Crash,Fire
col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','DepositHousehold','Electricity','FixedAssets','GDP','Loan','PostTele','Retail','Salary','WasteWater','Water')
infrasdf = c('Cinema', 'CityRoadArea','Doctor','Gas.Length','Green','GreenBuilt','Hospital','HospitalBerth','School','PavedRoad.Length','PrimarySchool','PrimaryTeacher','Sewage.Length','WaterSupply.Length')
needdf = c('ElectricityResident','LivingSpace','WaterResident')
areadf = c('Area', 'AreaBuilt')

sumlmHorizontal$type=NA
sumlmHorizontal[sumlmHorizontal$yIndex %in% economoicdf,]$type = 'socio-economic'
sumlmHorizontal[sumlmHorizontal$yIndex %in% infrasdf,]$type = 'infrastructure'
sumlmHorizontal[sumlmHorizontal$yIndex %in% needdf,]$type = 'individual need'
sumlmHorizontal[sumlmHorizontal$yIndex %in% areadf,]$type = 'Area'
sumlmHorizontal = sumlmHorizontal[!is.na(sumlmHorizontal$type),]


###### clustering ###############################
#################################################
forclus = sumlmHorizontal[,c('yIndex','Rsquare', 'year')]
head(forclus)
x1 = split(forclus, f=forclus$yIndex)
x0 = x1[[1]][,c(2,3)]
for (i in 2:length(x1)){
  x0 = merge(x0, x1[[i]][,c(2,3)], by='year',all=T)  
}
colnames(x0) = c('year',names(x1) )
tt = apply(is.na.data.frame(x0), 2, sum)
x0 = x0[,c(names(tt[tt<33]))]

imp<-mice(x0)
fit = with(imp, lm(PostTele~year))
pooled = pool(fit)
summary(pooled)
x1 = complete(imp,actions=3)
plot(x1$year,x1$PostTele)

tt = apply(is.na.data.frame(x1), 2, sum)
x2 = x1[,c(names(tt[tt<1]))]

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
x3 = apply(x2, 2, range01)


load('x3best.Rdata')


covv <- cov(x3[,-1])
corr <- cor(x3[,-1])
#聚类并将聚类信息存储在数据框里
d <- as.dist(1-corr)
hc <- hclust(d)

hcd = as.dendrogram(hc)

col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','Deposit','DepositHousehold','Electricity','FixedAssets','GDP','Loan','PostTele','Retail','Salary','WasteWater','Water')
infrasdf = c('Cinema', 'CityRoadArea','Doctor','Gas.Length','Green','GreenBuilt','Hospital','HospitalBerth','PavedRoad.Length','PrimarySchool','PrimaryTeacher','School','Sewage.Length','WaterSupply.Length')
needdf = c('ElectricityResident','LivingSpace','WaterResident')
areadf = c('Area', 'AreaBuilt')

col1 = rep(4,length(economoicdf))
names(col1) = economoicdf
col2 = rep(3,length(infrasdf))
names(col2) = infrasdf
col3 = rep(2,length(needdf))
names(col3) = needdf
col4 = rep(1,length(areadf))
names(col4) = areadf
collist = c(col1,col2,col3,col4)

labelColors = col
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[collist[which(names(collist) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
# using dendrapply
clusDendro = dendrapply(hcd, colLab)
# make plot
png(filename=paste0(home, '/Results/',modelname,'/R2','hClustering.png'),
    width=13,height=10, units='cm',res=150)
par(mar=c(7,2,1,1)+0.1,mgp=c(3,1,0))
plot(clusDendro)
rect.hclust(hc,k=4)
dev.off()

#save(x3,file='x3best.Rdata')

col1 = rep(col[4],length(economoicdf))
names(col1) = economoicdf
col2 = rep(col[3],length(infrasdf))
names(col2) = infrasdf
col3 = rep(col[2],length(needdf))
names(col3) = needdf
col4 = rep(col[1],length(areadf))
names(col4) = areadf
collist = c(col1,col2,col3,col4)

clusMember = cutree(hc, 4)

ylist1 = names(clusMember[clusMember==1])
ylist2 = names(clusMember[clusMember==2])
ylist3 = names(clusMember[clusMember==3])
ylist4 = names(clusMember[clusMember==4])
i = 0
for (ylist in list(ylist1, ylist2,ylist3,ylist4)){
  i = i + 1
  dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist),]
  dfBeta$yIndex = as.character(dfBeta$yIndex)
  png(filename=paste0(home, '/Results/',modelname,'/R2_',i,'Clustering_Temporal.png'),
      width=32,height=13, units='cm',res=180)
  p = ggplot(data=dfBeta, aes(x=year-1, y=Rsquare, color=yIndex)) + 
    scale_colour_manual(values = collist[ylist]) +
    geom_line(size=1,alpha=0.4) + geom_point(size=2) +
    #geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
    #geom_hline(yintercept = c(2/3,1,5/6,7/6),alpha=0.4, color=col, lwd=1.5) +
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

########### facet

ylist1 = names(clusMember[clusMember==1])
ylist2 = names(clusMember[clusMember==2])
sumlmHorizontal$ylist = NA
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist1,]$ylist = 'b'
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist2,]$ylist = 'a'
dfBeta = sumlmHorizontal[!is.na(sumlmHorizontal$ylist),]
  dfBeta$yIndex = as.character(dfBeta$yIndex)
  png(filename=paste0(home, '/Results/',modelname,'/Clustering_BetaTemporal.png'),
      width=31,height=23, units='cm',res=180)
  p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, col=yIndex)) + 
    facet_wrap(. ~ ylist, ncol=1,scales='free') +
    scale_colour_manual(values=collist[dfBeta$yIndex], guide=guide_legend(ncol=1)) +
    geom_line(size=1,alpha=0.4) + geom_point(size=2) +
    geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
    geom_hline(yintercept = c(2/3,1,5/6,7/6),alpha=0.4, color=rep(col,2), lwd=1.5) +
    #geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
    labs(x = 'Year(1984-2016)', y='β') +
    theme(
      text = element_text(size=30),
      panel.background = element_rect(fill = "transparent",colour = 'black'), 
      panel.grid.minor = element_line(color='azure3'), 
      panel.grid.major = element_line(color='azure3'),
      legend.key = element_rect(fill = "transparent", color = "transparent"), 
      #plot.background = element_rect(fill = "transparent",colour = NA),
      legend.title=element_blank()
    )
  print(p)
  dev.off()


##################################
d <- dist(t(x2[,-1]))
hc <- hclust(d)

hcd = as.dendrogram(hc)

col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','Deposit','DepositHousehold','Electricity','FixedAssets','GDP','Loan','PostTele','Retail','Salary','WasteWater','Water')
infrasdf = c('Cinema', 'CityRoadArea','Doctor','Gas.Length','Green','GreenBuilt','Hospital','HospitalBerth','PavedRoad.Length','PrimarySchool','PrimaryTeacher','School','Sewage.Length','WaterSupply.Length')
needdf = c('ElectricityResident','LivingSpace','WaterResident')
areadf = c('Area', 'AreaBuilt')

col1 = rep(4,length(economoicdf))
names(col1) = economoicdf
col2 = rep(3,length(infrasdf))
names(col2) = infrasdf
col3 = rep(2,length(needdf))
names(col3) = needdf
col4 = rep(1,length(areadf))
names(col4) = areadf
collist = c(col1,col2,col3,col4)

labelColors = col
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[collist[which(names(collist) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
# using dendrapply
clusDendro = dendrapply(hcd, colLab)
# make plot
png(filename=paste0(home, '/Results/',modelname,'/R2','hClustering1.png'),
    width=13,height=10, units='cm',res=150)
par(mar=c(7,2,1,1)+0.1,mgp=c(3,1,0))
plot(clusDendro)
rect.hclust(hc,k=4)
dev.off()

#save(x3,file='x3best.Rdata')

col1 = rep(col[4],length(economoicdf))
names(col1) = economoicdf
col2 = rep(col[3],length(infrasdf))
names(col2) = infrasdf
col3 = rep(col[2],length(needdf))
names(col3) = needdf
col4 = rep(col[1],length(areadf))
names(col4) = areadf
collist = c(col1,col2,col3,col4)

clusMember = cutree(hc, 4)

ylist1 = names(clusMember[clusMember==1])
ylist2 = names(clusMember[clusMember==2])
ylist3 = names(clusMember[clusMember==3])
ylist4 = names(clusMember[clusMember==4])
i = 0
for (ylist in list(ylist1, ylist2,ylist3,ylist4)){
  i = i + 1
  dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist),]
  dfBeta$yIndex = as.character(dfBeta$yIndex)
  png(filename=paste0(home, '/Results/',modelname,'/R2_',i,'Clustering1_Temporal.png'),
      width=32,height=13, units='cm',res=180)
  p = ggplot(data=dfBeta, aes(x=year-1, y=Rsquare, color=yIndex)) + 
    scale_colour_manual(values = collist[ylist]) +
    geom_line(size=1,alpha=0.4) + geom_point(size=2) +
    #geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
    #geom_hline(yintercept = c(2/3,1,5/6,7/6),alpha=0.4, color=col, lwd=1.5) +
    #geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
    labs(x = 'Year(1984-2016)', y='R-square') +
    theme(
      text = element_text(size=18),
      panel.background = element_rect(fill = "transparent",colour = 'black'), 
      panel.grid.minor = element_line(color='azure3'), 
      panel.grid.major = element_line(color='azure3'),
      legend.key = element_rect(fill = "transparent", color = "transparent"), 
      #plot.background = element_rect(fill = "transparent",colour = NA),
      legend.title=element_blank()
      #, legend.position = c(0.1,0.9)
    )
  print(p)
  dev.off()
}

######### facet
ylist1 = names(clusMember[clusMember==1])
ylist2 = names(clusMember[clusMember==2])
ylist3 = names(clusMember[clusMember==3])
ylist4 = names(clusMember[clusMember==4])
sumlmHorizontal$ylist = NA
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist1,]$ylist = 'd'
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist2,]$ylist = 'c'
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist3,]$ylist = 'a'
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist4,]$ylist = 'b'
dfBeta = sumlmHorizontal[!is.na(sumlmHorizontal$ylist),]
dfBeta$yIndex = as.character(dfBeta$yIndex)
png(filename=paste0(home, '/Results/',modelname,'/Clustering1_BetaTemporal.png'),
    width=32,height=23, units='cm',res=180)
p = ggplot(data=dfBeta, aes(x=year-1, y=Rsquare, col=yIndex)) +
  facet_wrap(. ~ ylist, ncol=2) +
  scale_colour_manual(values=collist[dfBeta$yIndex], guide=guide_legend(ncol=1)) +
  geom_line(size=1,alpha=0.4) + geom_point(size=2) +
  #geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
  #geom_hline(yintercept = c(2/3,1,5/6,7/6),alpha=0.4, color=rep(col,4), lwd=1.5) +
  #geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
  labs(x = 'Year(1984-2016)', y='β') +
  theme(
    text = element_text(size=30),
    panel.background = element_rect(fill = "transparent",colour = 'black'),
    panel.grid.minor = element_line(color='azure3'),
    panel.grid.major = element_line(color='azure3'),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    #plot.background = element_rect(fill = "transparent",colour = NA),
    legend.title=element_blank()
  )
print(p)
dev.off()