popall = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/人口普查常住人口/2000POPall.csv',header=T)
head(popall)
hist(popall$市辖区常住人口/10000,breaks=1000)



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



setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL1')
for (rdat in dir()){load(rdat)}

dfname='GDP'
dfi = get(dfname)
yeari=2000
rangeStat = '市辖区'
popdis = na.omit(POP[which(POP$year==yeari & grepl(rangeStat, POP$index)),])
ydis = na.omit(dfi[which(dfi$year==yeari & grepl(rangeStat, dfi$index)),])
ydis$value = ydis$value/10000
andis = na.omit(CityRoadArea[which(CityRoadArea$year==yeari & grepl(rangeStat, CityRoadArea$index)),])


popdis = POP[which(grepl(rangeStat, POP$index)),]
ydis = dfi[which(grepl(rangeStat, dfi$index)),]
ydis$value = ydis$value/10000
andis = CityRoadArea[which(grepl(rangeStat, CityRoadArea$index)),]



cordfy = SearchCorValue(popdis, ydis)
colnames(cordfy)[1:2] = c('N', 'Y')
cordfan = SearchCorValue(popdis, andis)
colnames(cordfan)[1:2] = c('N', 'An')

df = cbind(cordfy, cordfan$An)
df1 = merge(cordfy, cordfan, by='N')
df1$G = (df1$Y*df1$An)/(df1$N*df1$N)
df1$e = ((df1$An/df1$N^(5/6))/df1$G^(1/3))^(1/3)
df1$a = df1$G^2*df1$e

fg = lm(log(df1$G)~log(df1$N))
ffg = summary(fg)

plot(log(df1$N), log(df1$G))
plot(df1$N, df1$G)

plot(log(df1$N), log(df1$a))

fa = lm(log(df1$a)~log(df1$N))
ffa = summary(fa)

ddd = sort(df1$a,decreasing = T)[c(-1:-10)]
plot(ddd)
hist(ddd[c(-1:-10)],breaks=100)
