library(ggplot2)

RB0 = read.csv(file='C:/Sync/CoolGirl/Fhe/Results/OLS_R/sumlmHorizontal_Districts.csv',header=T)
PB0 = read.csv(file='C:/Sync/CoolGirl/Fhe/Results/OLS_130/sumlmHorizontal_Districts.csv',header=T)
RB = RB0[which(RB0$Observation > 180),c(2,3,4,10,12)]
PB = PB0[which(PB0$year %in% unique(RB0$year) & PB0$Observation > 180),c(2,3,4,10,12)]
RB$R = 'Resident'
PB$R = 'Registered'
foo = merge(RB,PB,by=c('yIndex','year'),all=T)
foo = foo[which(foo$yIndex!='Passenger'),]

col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','BusPassenger','DepositHousehold','GDP','Loan','Salary','PostTele','WasteWater','Bus')
infrasdf = c('CityRoadArea','Hospital','School','Cinema','Green','HospitalBerth','GreenBuilt')
needdf = c('Electricity','Water')
areadf = c('Area', 'AreaBuilt')

foo$type=NA
foo[foo$yIndex %in% economoicdf,]$type = 'socio-economic'
foo[foo$yIndex %in% infrasdf,]$type = 'infrastructure'
foo[foo$yIndex %in% needdf,]$type = 'individual need'
foo[foo$yIndex %in% areadf,]$type = 'Area'

a = lm(foo$Beta.x~foo$Beta.y-1) #x是常住人口，y是户籍人口， x常住人口~y户籍人口
aa = summary(a)
text = paste0('y=', round(aa$coefficients[1,1],3), 'x, R^2=', round(aa$r.squared,3))
#text = paste0('y=', round(aa$coefficients[2,1],3), 'x+', round(aa$coefficients[1,1],3), ', R^2=', round(aa$r.squared,3))

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_R/Registered&Resident_beta.png'),
    width=15,height=15, units='cm',res=150)
p1 = ggplot(data=foo, aes(x=Beta.y, y=Beta.x, color=type)) + 
  #scale_colour_manual(values = col) +
  #geom_abline(size=1,col='grey') +
  #geom_abline(slope=aa$coefficients[2,1],intercept=aa$coefficients[1,1], size=1) +
  geom_abline(slope=aa$coefficients[1,1],size=1) +
  geom_point(size = 3.5) +
  labs(x='β (Registered)', y='β (Resident)') +
  scale_colour_hue(text) + 
  theme(text = element_text(size=20),
        legend.title=element_text(size = 14, face='italic'),
        legend.position = c(0.75,0.15),
        legend.text=element_text(size=14),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 0, hjust = 0.7, vjust = 0.5))
print(p1)
dev.off()

b = lm(foo$Intercept.x~foo$Intercept.y-1)
bb = summary(b)

text = paste0('y=', round(bb$coefficients[1,1],3), 'x, R^2=', round(bb$r.squared,3))
#text = paste0('y=', round(bb$coefficients[2,1],3), 'x+', round(bb$coefficients[1,1],3), ', R^2=', round(bb$r.squared,3))

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_R/Registered&Resident_Intercept.png'),
    width=15,height=15, units='cm',res=150)
p2 = ggplot(data=foo, aes(x=Intercept.y, y=Intercept.x, color=type)) + 
  #scale_colour_manual(values = col) +
  #geom_abline(slope=1,intercept=c,size=1,col='grey') +
  #geom_abline(slope=bb$coefficients[2,1],intercept=bb$coefficients[1,1], size=1) +
  geom_abline(slope=bb$coefficients[1,1], size=1) +
  geom_point(size = 3.5) +
  labs(x='Intercept (Registered)', y='Intercept (Resident)') +
  scale_colour_hue(text) + 
  theme(text = element_text(size=20),
        legend.title=element_text(size = 14, face='italic'),
        legend.position = c(0.75,0.15),
        legend.text=element_text(size=14),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 0, hjust = 0.7, vjust = 0.5))
print(p2)
dev.off()


r = lm(foo$Rsquare.x~foo$Rsquare.y-1)
rr = summary(r)
# rc = mean(na.omit(foo$Rsquare.x))-mean(na.omit(foo$Rsquare.y))
# Ry = foo$Rsquare.y+rc
# r1 = lm(foo$Rsquare.x~Ry-1)
# rr1 = summary(r1)

text = paste0('y=', round(rr$coefficients[1,1],3), 'x, R^2=', round(rr$r.squared,3))
#text = paste0('y=', round(rr$coefficients[2,1],3), 'x+', round(rr$coefficients[1,1],3), ', R^2=', round(rr$r.squared,3))

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_R/Registered&Resident_R2.png'),
    width=15,height=15, units='cm',res=150)
p3 = ggplot(data=foo, aes(x=Rsquare.y, y=Rsquare.x, color=type)) + 
  #scale_colour_manual(values = col) +
  #geom_abline(slope=1,intercept=rc,size=1,col='grey') +
  #geom_abline(slope=rr$coefficients[2,1],intercept=rr$coefficients[1,1], size=1) +
  geom_abline(slope=rr$coefficients[1,1], size=1) +
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

#ggplot2.multiplot(p1,p2,p3, cols=3)


#########################################################
#########################################################
#########################################################


library(ggplot2)

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

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


xdfname = 'Resident'
ydfname = 'POP'
if (!grepl('Built', ydfname)){
  rangeStatList = c('市辖区', 'Districts', 'BetaD/')
}else{
  rangeStatList = c('建成区', 'Districts', 'BetaD/')
}
rangeStat = rangeStatList[1] #按照指标对应的区域更改
xdf = get(xdfname)
delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市','哈密市','吐鲁番市')
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

text = paste0('y=', round(aa1$coefficients[1,1],3), 'x', ', R^2=', round(aa1$r.squared,3))
#text = paste0('y=', round(aa$coefficients[2,1],3), 'x', round(aa$coefficients[1,1],3), ', R^2=', round(aa$r.squared,3))


png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_R/ResidentVSPOP.png'),width=15,height=15, units='cm',res=150)
p0 = ggplot(data=cordf, aes(x=log(yindex), y=log(xindex), color=year)) + 
  #scale_colour_manual(values = col) +
  #geom_abline(slope=1,intercept=rc,size=1,col='grey') +
  #geom_abline(slope=aa$coefficients[2,1],intercept=aa$coefficients[1,1], size=1) +
  geom_abline(slope=aa1$coefficients[1,1], size=1) +
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
        axis.text.x = element_text(angle = 0, hjust = 0.7, vjust = 0.5))
print(p0)
dev.off()

cordf1 = na.omit(cordf)
respoint = data.frame(city=cordf1$city, year=cordf1$year, Resident=cordf1$xindex, Registered=cordf1$yindex, res=aa$residuals)
resorder = respoint[order(respoint$res,decreasing=T),]
respoint1 = data.frame(city=cordf1$city, year=cordf1$year, Resident=cordf1$xindex, Registered=cordf1$yindex, res=aa1$residuals)
resorder1 = respoint1[order(respoint1$res,decreasing=T),]
write.csv(resorder1, file='C:/Sync/CoolGirl/Fhe/Results/OLS_R/ResOrder.csv')
write.csv(resorder, file='C:/Sync/CoolGirl/Fhe/Results/OLS_R/ResOrder2.csv')

library(ggpubr)

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_R/arrange1.png'),width=30,height=30, units='cm',res=300)
ggarrange(p0,p1,p2,p3,ncol=2,nrow=2,labels=c("A","B","C","D"))
dev.off()
