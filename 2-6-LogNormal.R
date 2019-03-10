SearchCorValue = function(ORI, COR){ #找到某指标对应的另一指标的值
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

############################################################
setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

dfname='GDP'
yeari=2017
rangeStat='市辖区'

xmin = 511.13
xsmid = seq(0,xmin, length.out = 5)[2:4]
bin = xmin/5
xsup = round(xsmid + bin)
xslow = round(xsmid - bin)

dfi = get(dfname)
delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市')
dfi = dfi[which(!(dfi$city %in% delcity)),]
dfi = na.omit(dfi)
dfi$value = dfi$value/10000
dat = dfi[which(dfi$year == yeari & grepl(rangeStat,dfi$index)),]
NY1 = dat[which(dat$value>xslow[1] & dat$value<xsup[1]),]
NY2 = dat[which(dat$value>xslow[2] & dat$value<xsup[2]),]
NY3 = dat[which(dat$value>xslow[3] & dat$value<xsup[3]),]

datPOP = POP[which(POP$year == yeari & grepl(rangeStat,POP$index)),]

cordf1 = SearchCorValue(NY1, datPOP)
ny1 = log(cordf1$yindex)

cordf2 = SearchCorValue(NY2, datPOP)
ny2 = log(cordf2$yindex)

cordf3 = SearchCorValue(NY3, datPOP)
ny3 = log(cordf3$yindex)

ny=ny2
st = shapiro.test(ny)

par(mfrow=c(1,2))

hist(ny, freq=F,
     main=paste0('Density of P(N|Y), ',dfname,' (',yeari-1,')'), 
     xlab='log(N)',
     ylab=paste0('P(logN|Y∈','[',round(xslow[1]),', ',round(xsup[1]),'])'))

qqnorm(ny,main=paste0("QQ-plot of P(N|Y), ",dfname,' (',yeari-1,')'))
legend('topleft', seg.len=0, x.intersp=0,
       paste0('Shapiro-Wilk normality test: p=',round(st$p.value,2)))
qqline(ny)

