library(ggplot2)
library(mice)

home = 'C:/Sync/CoolGirl/Fhe'
modelname='OLS1_DJS'
setwd(paste0(home, '/Results/',modelname))
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
#setwd('C:/Sync/CoolGirl/Fhe/Results/OLS1_DJS/fromPC/DJS')
load(paste0('200_sumlmHorizontal_type_',rangeStatList[2],'.Rdata'))
sumlmHorizontal = sumlmHorizontal[!is.na(sumlmHorizontal$type),]

########### density ########################
############################################

#### variance
### 对于连续型变量，首先进行正态性分布检验，对于符合正态性分布的数据采用均数±标准差表示，
### 两组单独计量资料的比较采用独立样本t检验，多组之间的比较采用完全随机设计单因素的方差分析(one-way ANOVA)，
### 方差齐时整体的比较采用F检验，多重比较采用Bonferroni法，
### 方差不齐时采用Welch近似F检验，多重比较采用Dunnett’s T3法；当P ＜0.05时认为差异具有统计学意义。

# bartlett.test(Beta~type,data=sumlmHorizontal) #方差不齐
sumlmHorizontal$type = as.factor(sumlmHorizontal$type)
kruskal.test(Beta~type,data=sumlmHorizontal)
library(PMCMR)
posthoc.kruskal.nemenyi.test(Beta~type,data=sumlmHorizontal)


#### density0: facet_wrap density
col = c("#619CFF", "#00BA38", "grey52", "#F8766D")
png(filename=paste0(home, '/Results/',modelname,'/Density0_BetaTemporal.png'),
    width=18,height=18, units='cm',res=180)
m = ggplot(sumlmHorizontal, aes(x=Beta)) + 
  facet_wrap(. ~ type) +
  geom_density(aes(fill=factor(type)), size=2, alpha=.4) +
  scale_fill_manual(values = col) +
  geom_vline(xintercept = 1,alpha=0.4, color=1, lwd=1.5) +
  geom_vline(xintercept=c(2/3,1,5/6,7/6),alpha=0.6, lwd=1.5,
             col = c( c(col[1],NA,NA,NA),
                      c(NA,col[2],NA,NA),
                      c(NA,NA,col[3],NA),
                      c(NA,NA,NA,col[4]))) +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color='azure3'), 
    panel.grid.major = element_line(color='azure3'),
    legend.key = element_rect(fill = "transparent", color = "transparent"), 
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.title=element_blank(),
    legend.position="none"
  )
print(m)
dev.off()

### density: 4 in 1
png(filename=paste0(home, '/Results/',modelname,'/Density_BetaTemporal.png'),
    width=18,height=13, units='cm',res=180)
m = ggplot(sumlmHorizontal, aes(x=Beta)) + 
  geom_density(aes(fill=factor(type)), size=2, alpha=.4) +
  scale_fill_manual(values = col) +
  geom_vline(xintercept = c(2/3,1,5/6,7/6),alpha=0.4, color=col, lwd=1.5) +
  theme(
    text = element_text(size=18),
    panel.background = element_rect(fill = "transparent",colour = 'black'), 
    panel.grid.minor = element_line(color='azure3'), 
    panel.grid.major = element_line(color='azure3'),
    legend.key = element_rect(fill = "transparent", color = "transparent"), 
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.title=element_blank()
  )
print(m)
dev.off()


###### clustering ###############################
#################################################

###################
####### Imputation
###################

methodimp = 'norm'
#methodimp = 'pmm'

forclus = sumlmHorizontal[,c('yIndex','Beta', 'year')]
head(forclus)
x1 = split(forclus, f=forclus$yIndex)
x0 = x1[[1]][,c(2,3)]
for (i in 2:length(x1)){
  x0 = merge(x0, x1[[i]][,c(2,3)], by='year',all=T)  
}
colnames(x0) = c('year',names(x1) )
tt = apply(is.na.data.frame(x0), 2, sum)
x0 = x0[,c(names(tt[tt<20]))]

png(filename=paste0(home, '/Results/',modelname,'/clustering/imputation.png'),
    width=15,height=15, units='cm',res=150)
md.pattern(x0,rotate.name=T)
dev.off()
#fit = with(x0, lm(Sewage.Length~year))
#summary(fit)

ini <- mice(x0, pred=quickpred(x0, mincor=.9), print=F)
set.seed(123)
imp <- mice(x0, pred=ini$pred, print=F, method=methodimp)
#imp40 <- mice.mids(imp, print=F, maxit=35)
#plot(imp40)

#fit = with(imp, lm(Sewage.Length~year))
fit = with(imp, lm(Loan~year))
summary(fit)
pooled = pool(fit)
summary(pooled)

x3 = complete(imp,action=5)
stripplot(imp, Sewage.Length~.imp, pch=20, cex=2)
plot(x0$year, x0$Sewage.Length)
points(x3$year,x3$Sewage.Length,col=2)
save(x3,file='x3best.Rdata')
##########################################

#load('x3best.Rdata')

#聚类并将聚类信息存储在数据框里

##################
######## Pearson 
###################
covv <- cov(x3[,-1])
corr <- cor(x3[,-1])
d <- as.dist(1-corr)
hc <- hclust(d)

hcd = as.dendrogram(hc)

col = c("#619CFF", "#00BA38", "grey52", "#F8766D") 
economoicdf = unique(sumlmHorizontal[sumlmHorizontal$type=='Socio-economic',]$yIndex)
infrasdf = unique(sumlmHorizontal[sumlmHorizontal$type=='Infrastructure',]$yIndex)
needdf = unique(sumlmHorizontal[sumlmHorizontal$type=='Basic Services',]$yIndex)
areadf = unique(sumlmHorizontal[sumlmHorizontal$type=='Land Use',]$yIndex)

col1 = rep(4,length(economoicdf))
names(col1) = economoicdf
col2 = rep(3,length(areadf))
names(col2) = areadf
col3 = rep(2,length(infrasdf))
names(col3) = infrasdf
col4 = rep(1,length(needdf))
names(col4) = needdf
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
png(filename=paste0(home, '/Results/',modelname,'/clustering/',methodimp,'_P_hClustering.png'),
    width=11,height=10, units='cm',res=150)
par(mar=c(7,2,1,1)+0.1,mgp=c(3,1,0))
plot(clusDendro)
dev.off()

col1 = rep(col[4],length(economoicdf))
names(col1) = economoicdf
col2 = rep(col[3],length(areadf))
names(col2) = areadf
col3 = rep(col[2],length(infrasdf))
names(col3) = infrasdf
col4 = rep(col[1],length(needdf))
names(col4) = needdf
collist = c(col1,col2,col3,col4)

clusMember = cutree(hc, 2)
ylist1 = names(clusMember[clusMember==1])
ylist2 = names(clusMember[clusMember==2])

sumlmHorizontal$clusMember=NA
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist1,]$clusMember = 1
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist2,]$clusMember = 2
sumlmHorizontal = sumlmHorizontal[!is.na(sumlmHorizontal$clusMember),]
save(sumlmHorizontal, file='C:/Sync/CoolGirl/Fhe/Results/OLS1_DJS/200_sumlmHorizontal_Districts_cluster.Rdata')
write.csv(sumlmHorizontal, file='C:/Sync/CoolGirl/Fhe/Results/OLS1_DJS/200_sumlmHorizontal_Districts_cluster.csv')


for (ylist in list(ylist1, ylist2)){
  dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist),]
  dfBeta$yIndex = as.character(dfBeta$yIndex)
  png(filename=paste0(home, '/Results/',modelname,'/clustering/',methodimp,'_P_',ylist[1],'Clustering_BetaTemporal.png'),
      width=32,height=13, units='cm',res=180)
  p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, color=yIndex)) + 
    scale_colour_manual(values = collist[ylist]) +
    geom_line(size=1,alpha=0.4) + geom_point(size=2) +
    geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
    geom_hline(yintercept = c(1,5/6,2/3,7/6),alpha=0.4, color=col, lwd=1.5) +
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

# facet
ylist1 = names(clusMember[clusMember==1])
ylist2 = names(clusMember[clusMember==2])
sumlmHorizontal$ylist = NA
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist1,]$ylist = 'b'
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist2,]$ylist = 'a'
dfBeta = sumlmHorizontal[!is.na(sumlmHorizontal$ylist),]
  dfBeta$yIndex = as.character(dfBeta$yIndex)
  png(filename=paste0(home, '/Results/',modelname,'/clustering/',methodimp,'_P_Clustering_BetaTemporal.png'),
      width=31,height=23, units='cm',res=180)
  p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, col=yIndex)) + 
    facet_wrap(. ~ ylist, ncol=1,scales='free') +
    scale_colour_manual(values=collist[dfBeta$yIndex], guide=guide_legend(ncol=1)) +
    geom_line(size=1,alpha=0.4) + geom_point(size=2) +
    geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
    geom_hline(yintercept = c(1,5/6,2/3,7/6),alpha=0.4, color=rep(col,2), lwd=1.5) +
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
###########

  
###############################
########## Euclidean distance
##################################
#x2 = x0
d <- dist(t(x3[,-1]))
hc <- hclust(d)

hcd = as.dendrogram(hc)

col1 = rep(4,length(economoicdf))
names(col1) = economoicdf
col2 = rep(3,length(areadf))
names(col2) = areadf
col3 = rep(2,length(infrasdf))
names(col3) = infrasdf
col4 = rep(1,length(needdf))
names(col4) = needdf
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
png(filename=paste0(home, '/Results/',modelname,'/clustering/',methodimp,'_E_hClustering.png'),
    width=13,height=10, units='cm',res=150)
par(mar=c(7,2,1,1)+0.1,mgp=c(3,1,0))
plot(clusDendro)
dev.off()

#save(x3,file='x3best.Rdata')

col1 = rep(col[4],length(economoicdf))
names(col1) = economoicdf
col2 = rep(col[3],length(areadf))
names(col2) = areadf
col3 = rep(col[2],length(infrasdf))
names(col3) = infrasdf
col4 = rep(col[1],length(needdf))
names(col4) = needdf
collist = c(col1,col2,col3,col4)
clusMember = cutree(hc, 4)

ylist1 = names(clusMember[clusMember==1])
ylist2 = names(clusMember[clusMember==2])
ylist3 = names(clusMember[clusMember==3])
ylist4 = names(clusMember[clusMember==4])
for (ylist in list(ylist1, ylist2,ylist3,ylist4)){
  dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist),]
  dfBeta$yIndex = as.character(dfBeta$yIndex)
  png(filename=paste0(home, '/Results/',modelname,'/clustering/',methodimp,'_E_',ylist[1],'Clustering_BetaTemporal.png'),
      width=32,height=13, units='cm',res=180)
  p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, color=yIndex)) + 
    scale_colour_manual(values = collist[ylist]) +
    geom_line(size=1,alpha=0.4) + geom_point(size=2) +
    geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
    geom_hline(yintercept = c(1,5/6,2/3,7/6),alpha=0.4, color=col, lwd=1.5) +
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
png(filename=paste0(home, '/Results/',modelname,'/clustering/',methodimp,'_E_Clustering_BetaTemporal.png'),
    width=31,height=23, units='cm',res=180)
p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, col=yIndex)) +
  facet_wrap(. ~ ylist, ncol=2) +
  scale_colour_manual(values=collist[dfBeta$yIndex], guide=guide_legend(ncol=1)) +
  geom_line(size=1,alpha=0.4) + geom_point(size=2) +
  geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
  geom_hline(yintercept = c(1,5/6,2/3,7/6),alpha=0.4, color=rep(col,4), lwd=1.5) +
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
###############################


# ###########################
# ######## without imputation, 10, 把存在NA值的年份直接删掉
# ###########################
# xt = x0[,c(names(tt[tt<10]))]
# xt0 = na.omit(xt)
# x3=xt0
# covv <- cov(x3[,-1])
# corr <- cor(x3[,-1])
# #聚类并将聚类信息存储在数据框里
# d <- as.dist(1-corr)
# hc <- hclust(d)
# 
# hcd = as.dendrogram(hc)
# 
# col1 = rep(4,length(economoicdf))
# names(col1) = economoicdf
# col2 = rep(3,length(areadf))
# names(col2) = areadf
# col3 = rep(2,length(infrasdf))
# names(col3) = infrasdf
# col4 = rep(1,length(needdf))
# names(col4) = needdf
# collist = c(col1,col2,col3,col4)
# 
# labelColors = col
# # function to get color labels
# colLab <- function(n) {
#   if (is.leaf(n)) {
#     a <- attributes(n)
#     labCol <- labelColors[collist[which(names(collist) == a$label)]]
#     attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
#   }
#   n
# }
# # using dendrapply
# clusDendro = dendrapply(hcd, colLab)
# # make plot
# png(filename=paste0(home, '/Results/',modelname,'/clustering/10hClustering.png'),
#     width=13,height=10, units='cm',res=150)
# par(mar=c(7,2,1,1)+0.1,mgp=c(3,1,0))
# plot(clusDendro)
# dev.off()
# 
# col1 = rep(col[4],length(economoicdf))
# names(col1) = economoicdf
# col2 = rep(col[3],length(areadf))
# names(col2) = areadf
# col3 = rep(col[2],length(infrasdf))
# names(col3) = infrasdf
# col4 = rep(col[1],length(needdf))
# names(col4) = needdf
# collist = c(col1,col2,col3,col4)
# 
# clusMember = cutree(hc, 2)
# 
# ylist1 = names(clusMember[clusMember==1])
# ylist2 = names(clusMember[clusMember==2])
# for (ylist in list(ylist1, ylist2)){
#   dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist),]
#   dfBeta$yIndex = as.character(dfBeta$yIndex)
#   png(filename=paste0(home, '/Results/',modelname,'/clustering/','/10',ylist[1],'Clustering_BetaTemporal.png'),
#       width=32,height=13, units='cm',res=180)
#   p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, color=yIndex)) + 
#     scale_colour_manual(values = collist[ylist]) +
#     geom_line(size=1,alpha=0.4) + geom_point(size=2) +
#     geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
#     geom_hline(yintercept = c(1,5/6,2/3,7/6),alpha=0.4, color=col, lwd=1.5) +
#     #geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
#     labs(x = 'Year(1984-2016)', y='β') +
#     theme(
#       text = element_text(size=18),
#       panel.background = element_rect(fill = "transparent",colour = 'black'), 
#       panel.grid.minor = element_line(color='azure3'), 
#       panel.grid.major = element_line(color='azure3'),
#       legend.key = element_rect(fill = "transparent", color = "transparent"), 
#       #plot.background = element_rect(fill = "transparent",colour = NA),
#       legend.title=element_blank()
#     )
#   print(p)
#   dev.off()
# }
# 
# # facet
# ylist1 = names(clusMember[clusMember==1])
# ylist2 = names(clusMember[clusMember==2])
# sumlmHorizontal$ylist = NA
# sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist1,]$ylist = 'b'
# sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist2,]$ylist = 'a'
# dfBeta = sumlmHorizontal[!is.na(sumlmHorizontal$ylist),]
# dfBeta$yIndex = as.character(dfBeta$yIndex)
# png(filename=paste0(home, '/Results/',modelname,'/clustering/10Clustering_BetaTemporal.png'),
#     width=31,height=23, units='cm',res=180)
# p = ggplot(data=dfBeta, aes(x=year-1, y=Beta, col=yIndex)) + 
#   facet_wrap(. ~ ylist, ncol=1,scales='free') +
#   scale_colour_manual(values=collist[dfBeta$yIndex], guide=guide_legend(ncol=1)) +
#   geom_line(size=1,alpha=0.4) + geom_point(size=2) +
#   geom_errorbar(aes(ymin=BetaLower, ymax=BetaUpper), width=.2, alpha=0.4) +
#   geom_hline(yintercept =c(1,5/6,2/3,7/6),alpha=0.4, color=rep(col,2), lwd=1.5) +
#   #geom_hline(yintercept =  c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
#   labs(x = 'Year(1984-2016)', y='β') +
#   theme(
#     text = element_text(size=30),
#     panel.background = element_rect(fill = "transparent",colour = 'black'), 
#     panel.grid.minor = element_line(color='azure3'), 
#     panel.grid.major = element_line(color='azure3'),
#     legend.key = element_rect(fill = "transparent", color = "transparent"), 
#     #plot.background = element_rect(fill = "transparent",colour = NA),
#     legend.title=element_blank()
#   )
# print(p)
# dev.off()
# ###########


###### clustering ###############################
#################################################

###################
####### Imputation
###################

methodimp = 'norm'
#methodimp = 'pmm'

forclus = sumlmHorizontal[,c('yIndex','Intercept', 'year')]
head(forclus)
x1 = split(forclus, f=forclus$yIndex)
x0 = x1[[1]][,c(2,3)]
for (i in 2:length(x1)){
  x0 = merge(x0, x1[[i]][,c(2,3)], by='year',all=T)  
}
colnames(x0) = c('year',names(x1) )
tt = apply(is.na.data.frame(x0), 2, sum)
x0 = x0[,c(names(tt[tt<20]))]

# png(filename=paste0(home, '/Results/',modelname,'/clustering/imputation.png'),
#     width=15,height=15, units='cm',res=150)
# md.pattern(x0,rotate.name=T)
# dev.off()
#fit = with(x0, lm(Sewage.Length~year))
#summary(fit)

ini <- mice(x0, pred=quickpred(x0, mincor=.9), print=F)
set.seed(123)
imp <- mice(x0, pred=ini$pred, print=F, method=methodimp)
#imp40 <- mice.mids(imp, print=F, maxit=35)
#plot(imp40)

#fit = with(imp, lm(Sewage.Length~year))
fit = with(imp, lm(Loan~year))
summary(fit)
pooled = pool(fit)
summary(pooled)

x3 = complete(imp,action=5)
stripplot(imp, Sewage.Length~.imp, pch=20, cex=2)
plot(x0$year, x0$Sewage.Length)
points(x3$year,x3$Sewage.Length,col=2)
save(x3,file='x3best.Rdata')
##########################################

#load('x3best.Rdata')

#聚类并将聚类信息存储在数据框里

##################
######## Pearson 
###################
covv <- cov(x3[,-1])
corr <- cor(x3[,-1])
d <- as.dist(1-corr)
hc <- hclust(d)

hcd = as.dendrogram(hc)

col = c("#619CFF", "#00BA38", "grey52", "#F8766D") 
economoicdf = unique(sumlmHorizontal[sumlmHorizontal$type=='Socio-economic',]$yIndex)
infrasdf = unique(sumlmHorizontal[sumlmHorizontal$type=='Infrastructure',]$yIndex)
needdf = unique(sumlmHorizontal[sumlmHorizontal$type=='Basic Services',]$yIndex)
areadf = unique(sumlmHorizontal[sumlmHorizontal$type=='Land Use',]$yIndex)

col1 = rep(4,length(economoicdf))
names(col1) = economoicdf
col2 = rep(3,length(areadf))
names(col2) = areadf
col3 = rep(2,length(infrasdf))
names(col3) = infrasdf
col4 = rep(1,length(needdf))
names(col4) = needdf
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
png(filename=paste0(home, '/Results/',modelname,'/clustering/',methodimp,'_P_hClusteringIntercept.png'),
    width=11,height=10, units='cm',res=150)
par(mar=c(7,2,1,1)+0.1,mgp=c(3,1,0))
plot(clusDendro)
dev.off()

col1 = rep(col[4],length(economoicdf))
names(col1) = economoicdf
col2 = rep(col[3],length(areadf))
names(col2) = areadf
col3 = rep(col[2],length(infrasdf))
names(col3) = infrasdf
col4 = rep(col[1],length(needdf))
names(col4) = needdf
collist = c(col1,col2,col3,col4)

clusMember = cutree(hc, 2)
ylist1 = names(clusMember[clusMember==1])
ylist2 = names(clusMember[clusMember==2])

sumlmHorizontal$clusMember=NA
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist1,]$clusMember = 1
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist2,]$clusMember = 2
sumlmHorizontal = sumlmHorizontal[!is.na(sumlmHorizontal$clusMember),]
save(sumlmHorizontal, file='C:/Sync/CoolGirl/Fhe/Results/OLS1_DJS/200_sumlmHorizontal_Districts_cluster.Rdata')
write.csv(sumlmHorizontal, file='C:/Sync/CoolGirl/Fhe/Results/OLS1_DJS/200_sumlmHorizontal_Districts_cluster.csv')


for (ylist in list(ylist1, ylist2)){
  dfBeta = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% ylist),]
  dfBeta$yIndex = as.character(dfBeta$yIndex)
  png(filename=paste0(home, '/Results/',modelname,'/clustering/',methodimp,'_P_',ylist[1],'Clustering_InterceptTemporal.png'),
      width=32,height=13, units='cm',res=180)
  p = ggplot(data=dfBeta, aes(x=year-1, y=Intercept, color=yIndex)) + 
    scale_colour_manual(values = collist[ylist]) +
    geom_line(size=1,alpha=0.4) + geom_point(size=2) +
    geom_errorbar(aes(ymin=InterceptLower, ymax=InterceptUpper), width=.2, alpha=0.4) +
    #geom_hline(yintercept = c(1,5/6,2/3,7/6),alpha=0.4, color=col, lwd=1.5) +
    labs(x = 'Year(1984-2016)', y='α') +
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

# facet
ylist1 = names(clusMember[clusMember==1])
ylist2 = names(clusMember[clusMember==2])
sumlmHorizontal$ylist = NA
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist1,]$ylist = 'b'
sumlmHorizontal[sumlmHorizontal$yIndex %in% ylist2,]$ylist = 'a'
dfBeta = sumlmHorizontal[!is.na(sumlmHorizontal$ylist),]
dfBeta$yIndex = as.character(dfBeta$yIndex)
png(filename=paste0(home, '/Results/',modelname,'/clustering/',methodimp,'_P_Clustering_InterceptTemporal.png'),
    width=31,height=23, units='cm',res=180)
p = ggplot(data=dfBeta, aes(x=year-1, y=Intercept, col=yIndex)) + 
  facet_wrap(. ~ ylist, ncol=1,scales='free') +
  scale_colour_manual(values=collist[dfBeta$yIndex], guide=guide_legend(ncol=1)) +
  geom_line(size=1,alpha=0.4) + geom_point(size=2) +
  geom_errorbar(aes(ymin=InterceptLower, ymax=InterceptUpper), width=.2, alpha=0.4) +
  #geom_hline(yintercept = c(1,5/6,2/3,7/6),alpha=0.4, color=rep(col,2), lwd=1.5) +
  #geom_hline(yintercept = c(7/6,1,5/6),alpha=0.7,size=1,color='darkorange') +
  labs(x = 'Year(1984-2016)', y='α') +
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
###########
