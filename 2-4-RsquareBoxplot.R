library(ggplot2)


dir.create(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/R2plot/"),showWarnings = F)
setwd(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/R2plot/"))
file.remove(dir())

#####################################################################
#####################################################################

library(ggplot2)
home = 'C:/Sync/CoolGirl/Fhe'
modelname='OLS_DJS_sxq'
setwd(paste0(home, '/Results/',modelname))
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
sumlmHorizontal = read.csv(file=paste0('sumlmHorizontal_type_',rangeStatList[2],'.csv'),stringsAsFactors=F)

sumlmHorizontal = na.omit(sumlmHorizontal)
sumlmHorizontal = sumlmHorizontal[!is.na(sumlmHorizontal$type),]

gdp = c(0.72, 0.76,0.92,0.89, 0.77,0.93,0.96,0.91,0.96,0.64,0.94,0.88,0.76,0.89)
ins = c(0.93,0.94,0.75,0.87)
need = c(0.99,0.98,0.88,0.91,0.96)
area = c(0.87,0.62,0.74,0.84)
all0 = c(gdp, ins, need, area)
alldf = data.frame(Rsquare=all0, year='Previous', yIndex=NA,
                   type=c(rep('Socio-economic',length(gdp)),rep('Infrastructure',length(ins)),
                          rep('Basic Services',length(need)),rep('Land Use',length(area))))
col1 = c(rep(NA,4),'red')


chooseyear = c(1987,1997,2007,2017)

PB0 = sumlmHorizontal[,c('Rsquare','year','type','yIndex')]
PB1 = subset(PB0, PB0$year %in% chooseyear)
PB1$year = PB1$year - 1
PB2 = rbind(PB1, alldf)

a = aggregate(sumlmHorizontal$Rsquare, by=list(sumlmHorizontal$type),mean)

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/R2plot/R2Boxplot1facet33.png'),
    width=24,height=8, units='cm',res=180)
p = ggplot(data = PB2,aes(x = as.factor(year), y = Rsquare)) +
  #geom_boxplot(fill=col1) +
  geom_boxplot(fill=rep(col1,4)) +
  geom_hline(yintercept=a$x,alpha=1, lwd=1,
             col = c( c(col[1],NA,NA,NA),
                      c(NA,col[2],NA,NA),
                      c(NA,NA,col[3],NA),
                      c(NA,NA,NA,col[4]))) +
  facet_wrap(.~type,nrow=1) +
  labs(x = NULL, y='R-square') +
  theme(text = element_text(size=16),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1))
        #axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
print(p)
dev.off()

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/R2plot/R2Boxplot2facet33.png'),
    width=16,height=15, units='cm',res=180)
p = ggplot(data = PB2,aes(x = as.factor(year), y = Rsquare)) +
  #geom_boxplot(fill=col1) +
  geom_boxplot(fill=rep(col1,4)) +
  geom_hline(yintercept=a$x,alpha=1, lwd=1,
             col = c( c(col[1],NA,NA,NA),
                      c(NA,col[2],NA,NA),
                      c(NA,NA,col[3],NA),
                      c(NA,NA,NA,col[4]))) +
  facet_wrap(.~type,nrow=2) +
  labs(x = NULL, y='R-square') +
  theme(text = element_text(size=16),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1))
#axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
print(p)
dev.off()

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/R2plot/R2Boxplot2facet333.png'),
    width=16,height=15, units='cm',res=180)
p = ggplot(data = PB2,aes(x = as.factor(year), y = Rsquare)) +
  #geom_boxplot(fill=col1) +
  geom_boxplot(fill=rep(col1,4)) +
  geom_hline(yintercept=rep(0.5,4),alpha=1, lwd=1, lty=2) +
  facet_wrap(.~type,nrow=2) +
  labs(x = NULL, y='R-square') +
  theme(text = element_text(size=18),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1))
#axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
print(p)
dev.off()

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/R2plot/R2Boxplot1facet333.png'),
    width=24,height=8, units='cm',res=180)
p = ggplot(data = PB2,aes(x = as.factor(year), y = Rsquare)) +
  #geom_boxplot(fill=col1) +
  geom_boxplot(fill=rep(col1,4)) +
  geom_hline(yintercept=rep(0.5,4),alpha=1, lwd=1) +
  facet_wrap(.~type,nrow=1) +
  labs(x = NULL, y='R-square') +
  theme(text = element_text(size=16),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1))
#axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
print(p)
dev.off()

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/R2plot/R2Boxplot1333.png'),
    width=16,height=15, units='cm',res=180)
p = ggplot(data = PB2,aes(x = as.factor(year), y = Rsquare)) +
  geom_boxplot(fill=col1) +
  #geom_boxplot(fill=rep(col1,4)) +
  #geom_hline(yintercept=rep(0.5,4),alpha=1, lwd=1) +
  #facet_wrap(.~type,nrow=1) +
  labs(x = NULL, y='R-square') +
  theme(text = element_text(size=16),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1))
#axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
print(p)
dev.off()

col = c("#619CFF", "#00BA38", "grey52", "#F8766D") 
png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/R2plot/R2Boxplot2.png'),
    width=16,height=15, units='cm',res=180)
p = ggplot(data = foo,aes(x = type, y = Rsquare.y)) +
  geom_boxplot(fill=col) +
  labs(x = 'Variables classifications', y='R-square') +
  theme(text = element_text(size=18),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"))  #默认主题
        #axis.line = element_line(colour = "black"),
        #axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
print(p)
dev.off()

a = aggregate(sumlmHorizontal$Rsquare, by=list(sumlmHorizontal$year),mean)
#a = aggregate(sumlmHorizontal$Observation/sumlmHorizontal$ObservationAll, by=list(sumlmHorizontal$year),mean)

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/R2plot/R2points.png'),
    width=16,height=15, units='cm',res=180)
p = ggplot(data = a,aes(x = Group.1, y = x)) +
  geom_point(size=3) +
  #geom_hline(yintercept=0.5,lwd=1,lty=2) +
  labs(x = 'Year', y='R-square') +
  theme(text = element_text(size=18),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"))  #默认主题
#axis.line = element_line(colour = "black"),
#axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
print(p)
dev.off()

ty = unique(PB1$type)
PB2 = PB1[PB1$type==ty[2],]
sp = split(PB2,PB2$yIndex)
r2df = data.frame(year=chooseyear-1)
for (i in 1:length(sp)){
	r2df = cbind(r2df, sp[[i]][1])
}
colnames(r2df) = names(sp)



#####################################
# R2 lines facet

png(filename=paste0(home, '/Results/',modelname,'/R2plot/R2indexfacet1.png'),
	width=32,height=16, units='cm',res=180)
sumlmHorizontal1 = sumlmHorizontal[sumlmHorizontal$year %in% chooseyear, ]
pn4 = ggplot(data=sumlmHorizontal1, aes(x=year-1, y=Rsquare, color=yIndex)) + 
geom_line(size=3,alpha=0.4, na.rm=T) + 
geom_point(size=2, na.rm=T) +
geom_hline(yintercept = 0.5, alpha=0.4, color=1, lwd=1, lty=2) +
facet_wrap(.~type,nrow=1) +
scale_x_continuous(breaks=chooseyear-1) +
ylim(0, 1) +
labs(x = 'Year', y='R-square') +
	theme(
	  text = element_text(size=18),
	  panel.background = element_rect(fill = "transparent",colour = 'black'), 
	  panel.grid.minor = element_line(color=NA), 
	  panel.grid.major = element_line(color=NA),
	  legend.position = "none",
	  axis.text.x = element_text(angle = 90, hjust = 1)
	)
print(pn4)
dev.off()

# R2 lines type
for (tyi in ty){
	png(filename=paste0(home, '/Results/',modelname,'/R2plot/R2index2',tyi,'.png'),
		width=16,height=16, units='cm',res=180)
	PBi = sumlmHorizontal[sumlmHorizontal$type==tyi & sumlmHorizontal$year %in% chooseyear, ]
	pn4 = ggplot(data=PBi, aes(x=year-1, y=Rsquare, color=yIndex)) + 
	geom_line(size=3,alpha=0.4, na.rm=T) + 
	geom_point(size=2, na.rm=T) +
	geom_hline(yintercept = 0.5, alpha=0.4, color=1, lwd=1, lty=2) +
	facet_wrap(.~type,nrow=1) +
	scale_x_continuous(breaks=chooseyear-1) +
	ylim(0, 1) +
	labs(x = 'Year', y='R-square') +
		theme(
		  text = element_text(size=18),
		  panel.background = element_rect(fill = "transparent",colour = 'black'), 
		  panel.grid.minor = element_line(color=NA), 
		  panel.grid.major = element_line(color=NA),
		  legend.key = element_rect(fill = "transparent", color = "transparent"), 
		  #plot.background = element_rect(fill = "transparent",colour = NA),
		  legend.title=element_blank(),
		)
	print(pn4)
	dev.off()
}
	
