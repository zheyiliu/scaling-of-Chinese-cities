yeari = 2017
ydfname = 'GDP'
xdfname = 'POP'
if (!grepl('Built', ydfname)){
  rangeStatList = c('市辖区', 'Districts', 'BetaD/')
}else{
  rangeStatList = c('建成区', 'Districts', 'BetaD/')
}
rangeStat = rangeStatList[1] #按照指标对应的区域更改
year = yeari
cityi = '沈阳市'
city = cityi
xdf = get(xdfname)
delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市','哈密市','吐鲁番市')
xdf = xdf[which(!(xdf$city %in% delcity)),]
ydf = get(ydfname)
if (rangeStat=='建成区'){
  ORII = xdf[grepl('市辖区', xdf$index) & xdf$city==cityi,]
}else{
  ORII = xdf[grepl(rangeStat, xdf$index) & xdf$city==cityi,]
}
CORR = ydf[grepl(rangeStat, ydf$index) & ydf$city==cityi,]
cordf = SearchCorValue(ORII, CORR)

cordf1 = cordf[order(cordf$xindex),]
a = cordf1[1:80,]
b = cordf1[101:180,]
c = cordf1[201:280,]
summary(lm(log(cordf1$yindex)~log(cordf1$xindex)))
summary(lm(log(a$yindex)~log(a$xindex)))
summary(lm(log(b$yindex)~log(b$xindex)))
summary(lm(log(c$yindex)~log(c$xindex)))
plot(log(a$xindex),log(a$yindex))
plot(log(b$xindex),log(b$yindex))
plot(log(c$xindex),log(c$yindex))
plot(log(cordf1$xindex),log(cordf1$yindex))
plot(cordf1$xindex,cordf1$yindex)

r1 = rnorm(300, 100, 50)
r2 = rnorm(300, 100, 1000)
x = 51:350 + r1
y = x^1.5 + r2
plot(x,y)
cordf1 = data.frame(xindex=x, yindex=y)


##################################
library(readxl)
library(gdata)

year = 1995
setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',year))
fileall = dir()
f = "excel_filecode=N2005110398000010.xls"

lensh = length(readxl::excel_sheets(f))
readxl::excel_sheets(f)

for (i in 1:30) {
  sh = paste0('sheet',i)
  assign(sh, read_excel(f, sheet = i))
}

tst = read_excel(f,sheet=2)
for (m in 1:dim(tst)[1]){
  for (n in 1:dim(tst)[2]){
    if (is.na(tst[m,n])){
      tst[m,n]=''
    }
  }
}

dat = tst
mm = vector()
for (m in 1:10){if(sum(dat[m,]!='')==0){mm = c(mm, m)}}
if (length(mm)!=0){dat = dat[-mm,]}
for (p in 1:dim(dat)[2]){dat[,p] = gsub(' ', '', dat[,p])}
### 记录是否只含市辖区数据
for (q in 1:dim(dat)[2]){dat[,q] = gsub('市区', '市辖区', dat[,q])}
sxq = grep('市辖区|不包括市辖县',dat[,1])
if (length(sxq)>0){
  if (sxq[1] < 10){
    shixiaqu = T
    if (year <=1988){dat = dat[-sxq,]}
  }else{shixiaqu = F}
}else{shixiaqu = F}
### 
if ('序号' %in% dat[,1]){
  dat2001 = data.frame(城市=c(dat[,2],dat[,5]),index2001=c(dat[,3],dat[,6]),stringsAsFactors = FALSE)
  colnames(dat2001)[2] = dat[1,1]
  dat = dat2001
  mmm = vector()
  for (m in 1:10){if(sum(dat[m,]!='')==0){mmm = c(mmm, m)}}
  dat = dat[-mmm,]
  dat = dat[-1,]
  strangeformat = T
}else{strangeformat = F}
needtitle=''
if (year<=1992){ #year<=1992
  needtitle = dat[1,1]
  needtitle = gsub('表|\\d','', needtitle)
}
### 清理城市名（列名）
prov = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/省份列表.csv',header=F,stringsAsFactors = FALSE)[,1]
provfull = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/省份列表.csv',header=F,stringsAsFactors = FALSE)[,2]
dat = subset(dat, !grepl(pattern='合计|超大|特大|大城市|中等|小城市|东部|中部|西部|全部|沿海|特区|492|72|14|4|\\d个|人口|东都|中都', dat[,1]) & !(dat[,1] %in% c(prov,provfull)))
dat[,1] = gsub('城市名称|城市名', '城市', dat[,1])
### 对正常格式的表格清理指标名（行名）
if (!strangeformat){
  ### 删除表头大标题
  n = 1
  while(sum(dat[n,]!='')==1 & dat[n,1]!=''){dat = dat[-n,]}
  ### 记录表头标题占几行
  titleline = grep('北京|上海', dat[,1])[1]-1
  cn = dat[1:titleline,]
  cityline = grep('城市', cn[,1])
  if (length(cityline)!=0){
    if (cn[cityline[1],2]==''){
      cn[cityline[1],1] = ''
      cn[1,1] = '城市'}
  }else{cn[1,1] = '城市'}
  ### 根据标题所占行数处理标题
  for(t in 1:titleline){for(j in 2:dim(dat)[2]){if(cn[t,j]==''){cn[t,j]=cn[t,j-1]}}}
  cn1=data.frame()
  i = titleline
  if(i>=2){for(k in 1:i){
    cn1=paste(cn1, cn[k,],sep='')}
  }else{cn1 = cn}
  if (sum(grepl('其中', cn1))>0 & year>1987){
    for (k in grep('其中',cn1)){
      cn1[k] = gsub('其中', cn1[k-1], cn1[k])
    }}
  if (shixiaqu){cn1[-1] = paste(cn1[-1],'市辖区',sep='')}
  cn1[-1] = paste(needtitle,cn1[-1],sep='')
  if (year>2012 & grepl('存贷款', f)){
    cn1[6] = '年末金融机构人民币各项存款余额DepositsofNationalBankingSystematYear-end居民人民币储蓄存款余额HouseholdSavingDepositsatYear-end市辖区DistrictsunderCity'
  }
  colnames(dat) = cn1
}
#清洗表头 done!
dat1 = subset(dat, !(dat[,1]=='') & !grepl(pattern='注|\\d|表|单位|投资|计算|续|相等|\\b([a-z]+)\\b|地方|汇总|比例|\\b(城市)\\b', dat[,1]))
dat1[,1] = gsub('[[:punct:]]', '', dat1[,1])
if (!('北京市' %in% dat1[,1])){dat1[,1]=paste(dat1[,1],'市',sep='')}
if ('City' %in% colnames(dat1)){dat1 = dat1[,-2]}
colnames(dat1) = gsub('[[:punct:]]|[\n *)]|[a-z]|[A-Z]', '', colnames(dat1))
write.csv(dat1, paste(gsub('.csv','',f),'-new.csv',sep=''),row.names=F)
###清洗表格 done!



##################################
rangeStat = '市辖区'
dfname = 'Book'
ydf = get(dfname)
dat = ydf[grepl(rangeStat, ydf$index),]
table(dat$year)
          