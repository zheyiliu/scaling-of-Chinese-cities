
xdfname = 'POP'
ydfname = 'GDP'
rangeStat = rangeStatList[1] #按照指标对应的区域更改

all = data.frame()
for (yeari in 1985:2017){
year = yeari
xdf = get(xdfname)
delcity = c('三沙市')
#delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市','哈密市','吐鲁番市','重庆市')
xdf = xdf[which(!(xdf$city %in% delcity)),]
ydf = get(ydfname)
ORII = xdf[grepl(rangeStat, xdf$index) & xdf$year==year,]
CORR = ydf[grepl(rangeStat, ydf$index) & ydf$year==year,]
cordf = SearchCorValue(ORII, CORR)
all = rbind(all, cordf)
}

data = data.frame(Y = log(all$yindex), X = log(all$xindex), year=all$year, city=all$city)
flm = lm(Y~X, data=data) #1.22 ##LM比GLM的AIC都小

f2 = lm(Y~X+I(X^2), data=data)
fsum = summary(f2)
a = fsum$coefficients[1,1]
b = fsum$coefficients[2,1]
c = fsum$coefficients[3,1]
plot(data$X,data$Y)
curve(a + b * x + c * (x^2), from=log(min(POP$value)), to=log(max(POP$value)), add=T, col='red')

tst = data.frame()
xxx = split(data, data$year)
tstdf = lapply(xxx, function(x){
    xsim = x$X
    ysim = a + b*xsim + c* (xsim^2)
    #ysim = x$Y
    lmi = summary(lm(ysim~xsim))
    betai = lmi$coefficients[2,1]
    alphai = lmi$coefficients[1,1]
    tst = data.frame(betai=betai, alphai=alphai)
    return(tst)
  })
tst = data.frame()
for (i in 1:length(tstdf)){
  tst = rbind(tst,tstdf[[i]])
}
tst$year = names(tstdf)


rangeStatList = c('市辖区', 'Districts', 'BetaD/')
setwd('C:/Sync/CoolGirl/Fhe/Results/OLS1_DJS_origin/fromPC/DJS')
load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))

sumlmHorizontal = sumlmHorizontal[which(sumlmHorizontal$Observation>=200),]

betaest = sumlmHorizontal[sumlmHorizontal$yIndex=='GDP',c('Beta','Intercept','year')]
final = merge(betaest, tst, by='year')
plot(final$betai, final$Beta)
plot(final$alphai, final$Intercept)

plot(final$betai, final$alphai)
plot(final$Beta, final$Intercept)

summary(lm(final$betai~final$Beta))
summary(lm(final$alphai~final$Intercept))

plot(diff(final$alphai),diff(final$betai))
plot(diff(final$Intercept), diff(final$Beta))
