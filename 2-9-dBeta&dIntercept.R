modelname='OLS'
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
###rangeStatList = c('建成区', 'Built', 'BetaB/')
setwd(paste0('C:/Sync/CoolGirl/Fhe/Results/',modelname))
load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))

gdpdf = sumlmHorizontal[which(sumlmHorizontal$yIndex=='GDP'),]
gdpdf = na.omit(gdpdf)
gdpdf$year = as.numeric(gdpdf$year)

#upyear = c(1992:1996,1999:2004)
upyear = 1984:2016
upyear = upyear + 1
upyear0 = gdpdf$year[gdpdf$year %in% upyear]

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
    dyear[i] = upyear0[i]
  }
}
plot(dBeta, dIntercept, type='n',xlim=c(-0.1,0.1))
text(dBeta, dIntercept, labels=dyear, cex=0.8)
abline(v=0)
abline(h=0)

########################################################################
gdpdf = sumlmHorizontal[which(sumlmHorizontal$yIndex=='CityRoadArea市辖区'),]
gdpdf = na.omit(gdpdf)
gdpdf$year = as.numeric(gdpdf$year)

upyear = c(1998:2004,2005:2008)
#upyear = 1984:2016
upyear = upyear + 1
upyear0 = gdpdf$year[gdpdf$year %in% upyear]

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
    dyear[i] = upyear0[i]
  }
}
plot(dBeta, dIntercept, type='n',xlim=c(-0.1,0.1))
text(dBeta, dIntercept, labels=dyear, cex=0.8)
abline(v=0)
abline(h=0)