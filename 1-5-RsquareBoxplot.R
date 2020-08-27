
library(ggplot2)


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

dir.create(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/R2plot/"),showWarnings = F)
setwd(paste0("C:/Sync/CoolGirl/Fhe/Results/",modelname,"/R2plot/"))
file.remove(dir())



### 1. boxplot, previous
gdp = c(0.72, 0.76,0.92,0.89, 0.77,0.93,0.96,0.91,0.96,0.64,0.94,0.88,0.76,0.89)
ins = c(0.93,0.94,0.75,0.87)
need = c(0.99,0.98,0.88,0.91,0.96)
area = c(0.87,0.62,0.74,0.84)
all0 = c(gdp, ins, need, area)
alldf = data.frame(Rsquare=all0, year='Previous', yIndex=NA,
                   type=c(rep('Socio-economic',length(gdp)),rep('Infrastructure',length(ins)),
                          rep('Basic Services',length(need)),rep('Land Use',length(area))))
col1 = c(rep(NA,5),'red')

#chooseyear = c(1987,1997,2007,2017)
chooseyear = c(1985,1993,2002,2010,2018)

PB0 = sumlmHorizontal[,c('Rsquare','year','type','yIndex')]
PB1 = subset(PB0, PB0$year %in% chooseyear)
PB1$year = PB1$year - 1
PB2 = rbind(PB1, alldf)

a = aggregate(sumlmHorizontal$Rsquare, by=list(sumlmHorizontal$type),mean)



am = aggregate(alldf$Rsquare, by=list(alldf$type), mean)
png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/R2plot/R2Boxplot2facet444.png'),
    width=15,height=15, units='cm',res=180)
p = ggplot(data = PB1,aes(x = as.factor(year), y = Rsquare)) +
  #geom_boxplot(fill=col1) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.25)) +
  geom_hline(yintercept=rep(0.5,4),alpha=.4, lwd=1, lty=2) +
  geom_hline(yintercept=am$x,alpha=.5, lwd=2,
             col = c( c(col[1],NA,NA,NA),
                      c(NA,col[2],NA,NA),
                      c(NA,NA,col[3],NA),
                      c(NA,NA,NA,col[4]))) +
  facet_wrap(.~type,nrow=2) +
  labs(x = 'Year', y='R-square') +
  theme(text = element_text(size=23),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1))
#axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
print(p)
dev.off()


png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/R2plot/R2Boxplot2.png'),
    width=18,height=17, units='cm',res=180)
p = ggplot(data = PB2,aes(x = type, y = Rsquare)) +
  geom_boxplot(fill=col) +
  labs(x = 'Variables classifications', y='R-square') +
  theme(text = element_text(size=18),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"))  #默认主题
        #axis.line = element_line(colour = "black"),
print(p)
dev.off()



a1 = aggregate(sumlmHorizontal$Rsquare, by=list(sumlmHorizontal$type,sumlmHorizontal$year),mean)
a = aggregate(a1$x, by=list(a1$Group.2),mean)
#a = aggregate(sumlmHorizontal$Observation/sumlmHorizontal$ObservationAll, by=list(sumlmHorizontal$year),mean)

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname,'/R2plot/R2points.png'),
    width=16,height=16, units='cm',res=180)
p = ggplot(data = a,aes(x = Group.1-1, y = x)) +
  geom_point(size=3) +
  geom_smooth(se=T,span=0.7,size=1,alpha=0.4, ) +
  #geom_hline(yintercept=0.5,lwd=1,lty=2) +
  labs(x = 'Year (1984-2017)', y='R-square') +
  scale_x_continuous(breaks=seq(1985,2018, by=5)) +
  #scale_y_continuous(limits=c(0.48,0.66),breaks=seq(0.48,0.66,by=0.02)) +
  theme(text = element_text(size=23),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
		axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1))
#axis.line = element_line(colour = "black"),
#axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
print(p)
dev.off()


#####################################
# R2 lines facet
png(filename=paste0(home, '/Results/',modelname,'/R2plot/R2indexfacet1.png'),
	width=16,height=16, units='cm',res=180)
sumlmHorizontal1 = sumlmHorizontal[sumlmHorizontal$year %in% 1985:2018, ]
pn4 = ggplot(data=sumlmHorizontal1, aes(x=year-1, y=Rsquare, color=yIndex)) + 
	#geom_line(size=3,alpha=0.4, na.rm=T) + 
	geom_smooth(size = 1, span=0.6, se=F, method = "loess", na.rm=T) +
	#geom_line(size = 2, alpha = .4, stat = "smooth",se=T, method = "loess", na.rm=T) +
	geom_point(size=1, na.rm=T) +
	#geom_hline(yintercept = 0.5, alpha=0.4, color=1, lwd=1, lty=2) +
	geom_hline(yintercept=am$x,alpha=.3, lwd=1.5,lty=1,
             col = c( c(1,NA,NA,NA),
                      c(NA,1,NA,NA),
                      c(NA,NA,1,NA),
                      c(NA,NA,NA,1))) +
	facet_wrap(.~type,nrow=2) +
	scale_x_continuous(breaks=seq(1985,2018, by=5)) +
	ylim(0, 1) +
	labs(x = 'Year (1984-2017)', y='R-square') +
		theme(
		  text = element_text(size=23),
		  panel.background = element_rect(fill = "transparent",colour = 'black'), 
		  panel.grid.minor = element_line(color=NA), 
		  panel.grid.major = element_line(color=NA),
		  legend.position = "none",
		  axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1))
print(pn4)
dev.off()




png(filename=paste0(home, '/Results/',modelname,'/R2plot/R2indexfacet2.png'),
	width=40,height=13, units='cm',res=180)
sumlmHorizontal1 = sumlmHorizontal[sumlmHorizontal$year %in% 1985:2018, ]
pn4 = ggplot(data=sumlmHorizontal1, aes(x=year-1, y=Rsquare, color=yIndex)) + 
	#geom_line(size=3,alpha=0.4, na.rm=T) + 
	geom_smooth(size = 1, span=0.6, se=T, method = "loess", na.rm=T) +
	#geom_line(size = 2, alpha = .4, stat = "smooth",se=T, method = "loess", na.rm=T) +
	geom_point(size=1, na.rm=T) +
	#geom_hline(yintercept = 0.5, alpha=0.4, color=1, lwd=1, lty=2) +
	geom_hline(yintercept=am$x,alpha=.5, lwd=1.5,lty=1,
             col = c( c(1,NA,NA,NA),
                      c(NA,1,NA,NA),
                      c(NA,NA,1,NA),
                      c(NA,NA,NA,1))) +
	facet_wrap(.~type,nrow=1) +
	scale_x_continuous(breaks=seq(1985,2018, by=5)) +
	ylim(0, 1) +
	labs(x = 'Year (1984-2017)', y='R-square') +
		theme(
		  text = element_text(size=25),
		  panel.background = element_rect(fill = "transparent",colour = 'black'), 
		  panel.grid.minor = element_line(color=NA), 
		  panel.grid.major = element_line(color=NA),
		  legend.position = "none",
		  axis.text.x = element_text(angle = 45, vjust=1.1, hjust=1))
print(pn4)
dev.off()

# R2 lines type
ty = unique(sumlmHorizontal$type)
for (tyi in ty){
	png(filename=paste0(home, '/Results/',modelname,'/R2plot/R2index2',tyi,'.png'),
		width=22,height=16, units='cm',res=180)
	PBi = sumlmHorizontal[sumlmHorizontal$type==tyi & sumlmHorizontal$year %in% chooseyear, ]
	pn4 = ggplot(data=PBi, aes(x=year-1, y=Rsquare, color=yIndex)) + 
	geom_point(size=2, na.rm=T) +
	#geom_smooth(se=F,span=0.7,size=3,alpha=0.4, )+
	geom_line(size = 3, alpha = .4, stat = "smooth", method = "loess", na.rm=T) +
	geom_hline(yintercept = 0.5, alpha=0.4, color=1, lwd=1, lty=2) +
	#facet_wrap(.~type,nrow=1) +
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
		  axis.text.x = element_text(angle = 90, vjust=1.1, hjust=1)
		)
	print(pn4)
	dev.off()
}
