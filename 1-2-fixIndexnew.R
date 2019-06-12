setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/')
load('SuperData/SuperDataAll_DelStrangeCities.Rdata')
#for (n in 1:ncol(df)){df[,n] = enc2utf8(as.character(df[,n]))}
#df$value = as.numeric(df$value)
#save(df, file='SuperData/SuperDataAll_DelStrangeCities.Rdata')
dfnew = df


#setwd('/home/zheyi/ecosocialData/')
#load('SuperData/SuperDataAll_DelStrangeCities.Rdata')
#dfnew = df


###查询命令的函数
FunRequest = function(yearR,indexR,cityR=dfnew$city){
  datR = subset(dfnew, dfnew$city %in% cityR & dfnew$year %in% yearR & dfnew$index %in% indexR)
  return(datR)
}



###改市辖区地区的函数
FunIndexName = function(dfR, indexname){
  idR3 = dfR[grep('县级市', dfR$index),]
  idR1 = dfR[grep('市辖区|市区|不包括市辖县', dfR$index),]
  idR2 = dfR[grep('市辖区|县级市', invert=T, dfR$index),]
  dfR$index[which(dfR$index %in% idR3$index)] = paste(indexname,'全市',sep='.')
  dfR = rbind(dfR, idR3)
  dfR$index[which(dfR$index %in% idR3$index)] = paste(indexname,'市辖区',sep='.')
  #dfR$index[which(dfR$index %in% idR3$index)] = paste(indexname,'全市.市辖区.县级市',sep='.')
  dfR$index[which(dfR$index %in% idR1$index)] = paste(indexname,'市辖区',sep='.')
  dfR$index[which(dfR$index %in% idR2$index)] = paste(indexname,'全市',sep='.')
  return(dfR)
}

### 去除同年同城市同指标的不同数值的情况
IDDELF = function(ddat){
  #ddat = GDP
  iddelf = vector()
  wield = paste0(ddat[,2], ddat[,3], ddat[,4])
  f= ddat[duplicated(wield),]
  if (nrow(f)>0){
	for (i in 1:dim(f)[1]){
		ff = subset(ddat,ddat$city==f$city[i] & ddat$year==f$year[i] & ddat$index==f$index[i])
		if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
		}else{iddelf = c(iddelf, ff$id[1])}
	}
  ddat = ddat[which(!ddat$id %in% iddelf),]
  }
  return(ddat)
}

uniindex = unique(dfnew$index)



##### 人口 #####
################
##### 总人口 #####
##################
#uniindex = unique(dfnew$index)
indexRequest = grep('.*人口.*', uniindex, value=T)
zongrenkou = grep('(年末.*人口)|(.*户籍.*)', indexRequest, value=T)
zongrenkou = grep('非农业|城镇人口', zongrenkou, invert=T, value=T)

feinongye = grep('非农业人口',indexRequest, value=T)
feinongye = grep('比重|行政级别_|\\d|供水',feinongye, invert=T, value=T)

chengzhen = grep('城镇人口',indexRequest, value=T)

#indexRequest[!indexRequest %in% c(zongrenkou,feinongye,feinongyebz)]

POP = FunRequest(1985:2017, zongrenkou, dfnew$city)

### 统一指标名称
POP = FunIndexName(POP, '年末总人口.万人')
tst = subset(POP, POP$city == '榆林市')[,-1]

POP = IDDELF(POP)
POP = subset(POP, POP$value<3500)
for (n in 1:ncol(POP)){POP[,n] = enc2utf8(as.character(POP[,n]))}
save(POP, file='SuperIndex/POP.Rdata')

##### 非农业人口 #####
######################
# 1985-2009 availabled
POPurban1 = FunRequest(1985:2017, feinongye, dfnew$city)
POPurban2 = FunRequest(1985:2017, chengzhen, dfnew$city)
POPurban = rbind(POPurban1, POPurban2)

### 统一指标名称
POPurban = FunIndexName(POPurban, '非农业人口.万人')
tst = subset(POPurban, POPurban$city == '榆林市')[,-1]

POPurban = IDDELF(POPurban)
POPurban = subset(POPurban, POPurban$value < 1500)
for (n in 1:ncol(POPurban)){POPurban[,n] = enc2utf8(as.character(POPurban[,n]))}
save(POPurban, file='SuperIndex/POPurban.Rdata')




################################################################################
############################     商业经济      #################################
################################################################################
##### 1.GDP #####
###############
indexRequest = grep('.*生产总值.*', uniindex, value=T)
indexRequest1 = grep('第一产业|第二产业|第三产业|人均|增长率|每万元|比重|构成|___|不变价格',indexRequest, invert=T, value=T)
GDP = FunRequest(1985:2017, indexRequest1, dfnew$city)

### 去除那些列有去年年份数据的重复值
gdpnum = grep('(\\d{5})',indexRequest1, invert=T, value=T)
gdpnum4 = grep('(\\d{4})',gdpnum, value=T)[-1]
for (num in gdpnum4){
  GDP[which(GDP$index==num),]$year = as.character(as.integer(gsub('\\w*(\\d{4})\\w*','\\1',num)[1])+1)
  GDP[which(GDP$index==num),]$index = gsub('\\d','',num)
}
### 统一指标名称
GDP = FunIndexName(GDP, '地区生产总值.万元')
tst = subset(GDP, GDP$city=='榆林市')

### 统一单位
i2000 = which(GDP$value < 2000)
GDP$value[i2000] = GDP$value[i2000] * 10000

### 去重
GDP = IDDELF(GDP)
for (n in 1:ncol(GDP)){GDP[,n] = enc2utf8(as.character(GDP[,n]))}
save(GDP, file='SuperIndex/GDP.Rdata')


#####第一产业GDP占比 #####
##########################
load('SuperIndex/GDP.Rdata')
GDP$value = as.numeric(GDP$value)
#uniindex = unique(dfnew$index)
indexRequest = grep('.*第一产业.*', uniindex, value=T)
indexRequest1 = grep('生产|经济', indexRequest, value=T)
indexRequest2 = grep('增加值|劳动', indexRequest1, invert=T, value=T)
indexRequest3 = grep('比重', indexRequest2, invert=T, value=T)
indexRequest4 = grep('比重', indexRequest2, value=T)
GDP1st = FunRequest(1985:2017, indexRequest3, dfnew$city)
GDP1stR = FunRequest(1985:2017, indexRequest4, dfnew$city)

### 统一指标名称
GDP1st = FunIndexName(GDP1st, '第一产业GDP占比.%')
tst = subset(GDP1st, GDP1st$city == '榆林市')[,-1]

### 统一单位
i100 = 1:dim(GDP1st)[1]
for (i100i in i100){
  cityL = GDP$city==GDP1st$city[i100i]
  yearL = GDP$year==GDP1st$year[i100i]
  indexL = substr(GDP$index,11,nchar(GDP$index)) == substr(GDP1st$index[i100i],13,nchar(GDP1st$index)[i100i])
  corGDP = GDP[which(cityL & yearL & indexL),]$value
  if (length(corGDP)!=1){
    #print(GDP[which(cityL & yearL & indexL),])
    corGDP = NA
  }
  GDP1st$value[i100i] = GDP1st$value[i100i]/corGDP * 100 * 10000
}

GDP1st$value[which(GDP1st$value>100)] = GDP1st$value[which(GDP1st$value>100)] / 10000
ss = GDP1st[which(GDP1st$value>100),]

### 统一指标名称
GDP1stR = FunIndexName(GDP1stR, '第一产业GDP占比.%')
tst = subset(GDP1stR, GDP1stR$city == '榆林市')[,-1]

GDP1st = rbind(GDP1st, GDP1stR)

### 去重
GDP1st = IDDELF(GDP1st)
for (n in 1:ncol(GDP1st)){GDP1st[,n] = enc2utf8(as.character(GDP1st[,n]))}
save(GDP1st, file='SuperIndex/GDP1st.Rdata')

#####第二产业GDP占比 #####
##########################
indexRequest = grep('.*第二产业.*', uniindex, value=T)
indexRequest1 = grep('生产|经济', indexRequest, value=T)
indexRequest2 = grep('增加值|劳动', indexRequest1, invert=T, value=T)
indexRequest3 = grep('比重', indexRequest2, invert=T, value=T)
indexRequest4 = grep('比重', indexRequest2, value=T)
GDP2nd = FunRequest(1985:2017, indexRequest3, dfnew$city)
GDP2ndR = FunRequest(1985:2017, indexRequest4, dfnew$city)

### 统一指标名称
GDP2nd = FunIndexName(GDP2nd, '第二产业GDP占比.%')
tst = subset(GDP2nd, GDP2nd$city == '榆林市')[,-1]

### 统一单位
i100 = 1:dim(GDP2nd)[1]
for (i100i in i100){
  cityL = GDP$city==GDP2nd$city[i100i]
  yearL = GDP$year==GDP2nd$year[i100i]
  indexL = substr(GDP$index,11,nchar(GDP$index)) == substr(GDP2nd$index[i100i],13,nchar(GDP2nd$index)[i100i])
  corGDP = GDP[which(cityL & yearL & indexL),]$value
  if (length(corGDP)!=1){
    corGDP = NA
  }
  GDP2nd$value[i100i] = GDP2nd$value[i100i]/corGDP * 100 * 10000
}

GDP2nd$value[which(GDP2nd$value>100)] = GDP2nd$value[which(GDP2nd$value>100)] / 10000
ss = GDP2nd[which(GDP2nd$value>100),]

### 统一指标名称
GDP2ndR = FunIndexName(GDP2ndR, '第二产业GDP占比.%')
tst = subset(GDP2ndR, GDP2ndR$city == '榆林市')[,-1]

GDP2nd = rbind(GDP2nd, GDP2ndR)

### 去重
GDP2nd = IDDELF(GDP2nd)
for (n in 1:ncol(GDP2nd)){GDP2nd[,n] = enc2utf8(as.character(GDP2nd[,n]))}
save(GDP2nd, file='SuperIndex/GDP2nd.Rdata')


#####第三产业GDP占比 #####
##########################
indexRequest = grep('.*第三产业.*', uniindex, value=T)
indexRequest1 = grep('生产|经济', indexRequest, value=T)
indexRequest2 = grep('增加值|劳动', indexRequest1, invert=T, value=T)
indexRequest3 = grep('比重', indexRequest2, invert=T, value=T)
indexRequest4 = grep('比重', indexRequest2, value=T)
GDP3rd = FunRequest(1985:2017, indexRequest3, dfnew$city)
GDP3rdR = FunRequest(1985:2017, indexRequest4, dfnew$city)

### 统一指标名称
GDP3rd = FunIndexName(GDP3rd, '第三产业GDP占比.%')
tst = subset(GDP3rd, GDP3rd$city == '榆林市')[,-1]

### 统一单位
i100 = 1:dim(GDP3rd)[1]
for (i100i in i100){
  cityL = GDP$city==GDP3rd$city[i100i]
  yearL = GDP$year==GDP3rd$year[i100i]
  indexL = substr(GDP$index,11,nchar(GDP$index)) == substr(GDP3rd$index[i100i],13,nchar(GDP3rd$index)[i100i])
  corGDP = GDP[which(cityL & yearL & indexL),]$value
  if (length(corGDP)!=1){
    corGDP = NA
  }
  GDP3rd$value[i100i] = GDP3rd$value[i100i]/corGDP * 100 * 10000
}

GDP3rd$value[which(GDP3rd$value>100)] = GDP3rd$value[which(GDP3rd$value>100)] / 10000
ss = GDP3rd[which(GDP3rd$value>100),]

### 统一指标名称
GDP3rdR = FunIndexName(GDP3rdR, '第三产业GDP占比.%')
tst = subset(GDP3rdR, GDP3rdR$city == '榆林市')[,-1]

GDP3rd = rbind(GDP3rd, GDP3rdR)

### 去重
GDP3rd = IDDELF(GDP3rd)
for (n in 1:ncol(GDP3rd)){GDP3rd[,n] = enc2utf8(as.character(GDP3rd[,n]))}
save(GDP3rd, file='SuperIndex/GDP3rd.Rdata')


#####第二三产业比重之和 #####
GDP23 = GDP1st
GDP23$id = paste0(GDP1st$id, 'lzy')
GDP23$index = gsub('一','二三',GDP1st$index)
GDP23$value = 100 - as.numeric(GDP1st$value)
for (n in 1:ncol(GDP23)){GDP23[,n] = enc2utf8(as.character(GDP23[,n]))}
save(GDP23, file='SuperIndex/GDP23.Rdata')




##### 2.总工资 #####
##################
indexRequest = grep('.*工资.*', uniindex, value=T)
gongzi = grep('所有制|合营|工业企业|平均工资|年末人数|经济类型|人均工资|均人数', indexRequest, invert=T, value=T)
###1985,1986只有各种所有制分别的工资数据
Salary = FunRequest(1985:2017, gongzi, dfnew$city)
tst = subset(Salary, Salary$city == '上海市')[,-1]

Salary = FunIndexName(Salary, '职工工资总额.万元')
Salary = IDDELF(Salary)
for (n in 1:ncol(Salary)){Salary[,n] = enc2utf8(as.character(Salary[,n]))}
save(Salary, file='SuperIndex/Salary.Rdata')


##########################
##### 3.固定资产投资 #####
########################
indexRequest = grep('(固定资产.*(投资额|投资总额|合计|总计|固定资产投资不))|固定资产投资情况|城镇固定资产', uniindex, value=T)
indexRequest1 = grep('所有制|规模以上|住宅|房地产|非生产性|个人|新增|城市|(\\.1)|(\\.2)', indexRequest, invert=T, value=T)
### 1985,1998,1999没有县级市数据
### 2002-2010年没有县级市的固定投资总额，只有重点分类完成额
FixedAssets = FunRequest(1985:2017, indexRequest1, dfnew$city)

### 去除那些列有去年年份数据的重复值
gdpnum = grep('(\\d{5})',indexRequest1, invert=T, value=T)
gdpnum4 = grep('(\\d{4})',gdpnum, value=T)
for (num in gdpnum4){
  FixedAssets[which(FixedAssets$index==num),]$year = as.character(as.integer(gsub('\\w*(\\d{4})\\w*','\\1',num)[1])+1)
  FixedAssets[which(FixedAssets$index==num),]$index = gsub('\\d','',num)
}
### 统一指标名称
FixedAssets = FunIndexName(FixedAssets, '固定资产投资额.万元')
tst = subset(FixedAssets, FixedAssets$city=='榆林市')

### 去重
FixedAssets = IDDELF(FixedAssets)
for (n in 1:ncol(FixedAssets)){FixedAssets[,n] = enc2utf8(as.character(FixedAssets[,n]))}
save(FixedAssets, file='SuperIndex/FixedAssets.Rdata')



##########################
######## 4.社会消费品 ######
##########################
### 直接载入万元
indexRequest = grep('社会消费品零售总额|社会商品零售|(商业经济__社会消费品零售额万元(全市_单位|市辖区_单位))', uniindex, value=T)
indexRequest1 = grep('限额以上|城乡集市|对',indexRequest, invert=T, value=T)
indexRequest2 = grep('万元', indexRequest1, value=T)
Retail1 = FunRequest(1985:2017, indexRequest2, dfnew$city)
### 统一单位，转亿元变万元
indexRequest3 = c('商业及外贸__社会商品零售总额亿元全市_单位','商业及外贸__社会商品零售总额亿元市辖区_单位')
Retail2 = FunRequest(1985:2017, indexRequest3, dfnew$city)
Retail2$value = Retail2$value * 10000

Retail = rbind(Retail1, Retail2)

### 统一指标名称
Retail = FunIndexName(Retail, '社会消费品零售总额.万元')
tst = subset(Retail, Retail$city=='上海市')

### 去重
Retail = IDDELF(Retail)
for (n in 1:ncol(Retail)){Retail[,n] = enc2utf8(as.character(Retail[,n]))}
save(Retail, file='SuperIndex/Retail.Rdata')


##### 5.客运量 #####
##################
ind1 = grep('.*客运总量.*', uniindex, value=T)
ind2 = grep('路|水|空|车|\\d', ind1, invert=T, value=T)

Passenger = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Passenger, Passenger$city == '上海市')[,-1]

Passenger = FunIndexName(Passenger, '客运总量.万人')
Passenger = IDDELF(Passenger)
Passenger = subset(Passenger, Passenger$value < 250000)
for (n in 1:ncol(Passenger)){Passenger[,n] = enc2utf8(as.character(Passenger[,n]))}
save(Passenger, file='SuperIndex/Passenger.Rdata')



##### 6.公共汽电车客运总数 #####
##############################
#uniindex = unique(dfnew$index)
ind1 = grep('.*公共.*汽电.*车.*客.*', uniindex, value=T)

BusPassenger = FunRequest(1985:2017, ind1, dfnew$city)
tst = subset(BusPassenger, BusPassenger$city == '北京市')[,-1]

BusPassenger$index = '公共汽电车客运总数.万人次.市辖区'
BusPassenger = IDDELF(BusPassenger)
for (n in 1:ncol(BusPassenger)){BusPassenger[,n] = enc2utf8(as.character(BusPassenger[,n]))}
save(BusPassenger, file='SuperIndex/BusPassenger.Rdata')


##### 7.藏书量 #####
ind1 = grep('.*藏.*', uniindex, value=T)
ind2 = grep('人|个', ind1, invert=T, value=T)

Book = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Book, Book$city == '深圳市')[,-1]

Book = FunIndexName(Book, '公共图书馆藏书量.千册')
Book = IDDELF(Book)
Book = subset(Book, Book$value < 85000)
for (n in 1:ncol(Book)){Book[,n] = enc2utf8(as.character(Book[,n]))}
save(Book, file='SuperIndex/Book.Rdata')

##### 8.全年用电量 ###
####################
#uniindex = unique(dfnew$index)
indexRequest = grep('.*全年用电|全社会用电.*', uniindex, value=T)
elec0 = grep('工业|居民|生活', indexRequest, invert=T, value=T)
elec1 = c(elec0, '用电量__年末邮电局所处全市_单位亿度')

Electricity = FunRequest(1985:2017, elec0, dfnew$city)
tst = subset(Electricity, Electricity$city == '上海市')[,-1]

### 统一单位
iyie = which(Electricity$year %in% 1985:1992)
Electricity$value[iyie] = Electricity$value[iyie] * 10000

### 统一指标名称
Electricity = FunIndexName(Electricity, '全年用电量.万千瓦时')

Electricity = IDDELF(Electricity)
for (n in 1:ncol(Electricity)){Electricity[,n] = enc2utf8(as.character(Electricity[,n]))}
save(Electricity, file='SuperIndex/Electricity.Rdata')


##### 9.全年用水量 #####
######################
ind1 = grep('.*[供|用]水.*(量|最).*[吨|立方米].*', uniindex, value=T)
ind2 = grep('生产|用电量|备水|生活|居民|煤气|石油气', ind1, invert=T, value=T)

Water = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Water, Water$city == '上海市')[,-1]

Water$index = '供水总量.万吨.市辖区'
Water = IDDELF(Water)
Water = subset(Water,Water$value < 400000)
for (n in 1:ncol(Water)){Water[,n] = enc2utf8(as.character(Water[,n]))}
save(Water, file='SuperIndex/Water.Rdata')

##### 10.居民存款 #####
####################
#uniindex = unique(dfnew$index)
ind1 = grep('.*储.*蓄.*余.*额.*', uniindex, value=T)
ind2 = grep('人均', ind1, invert=T, value=T)
ind3 = c('金融__年末金融机构各项贷款余额年末金融机构存款余额市辖区_单位万元.1',"金融__年末金融机构存款余额市辖区_单位万元.1")
ind4 = c(ind2, ind3)

DepositHousehold = FunRequest(1985:2017, ind4, dfnew$city)
tst = subset(DepositHousehold , DepositHousehold $city == '北京市')[,-1]

DepositHousehold = FunIndexName(DepositHousehold, '年末居民存款余额.万元')
DepositHousehold$index[which(DepositHousehold$year=='2001')] = gsub('全市', '市辖区', DepositHousehold$index[which(DepositHousehold$year=='2001')])

DepositHousehold = IDDELF(DepositHousehold)
for (n in 1:ncol(DepositHousehold)){DepositHousehold[,n] = enc2utf8(as.character(DepositHousehold[,n]))}
save(DepositHousehold, file='SuperIndex/DepositHousehold.Rdata')

##### 11.银行存款 #####
####################
#uniindex = unique(dfnew$index)
ind1 = grep('.*存款.*余.*额.*', uniindex, value=T)
ind2 = grep('居民|\\d', ind1, invert=T, value=T)

Deposit = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Deposit , Deposit $city == '北京市')[,-1]

Deposit = FunIndexName(Deposit, '年末金融机构存款余额.万元')

Deposit = IDDELF(Deposit)
for (n in 1:ncol(Deposit)){Deposit[,n] = enc2utf8(as.character(Deposit[,n]))}
save(Deposit, file='SuperIndex/Deposit.Rdata')

##### 12.贷款 #####
################
#uniindex = unique(dfnew$index)
ind1 = grep('.*贷.*款.*余.*额.*', uniindex, value=T)
ind2 = grep('固定|农业|流动|储蓄年末|存款余额|_城市|\\d', ind1, invert=T, value=T)

Loan= FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Loan, Loan$city == '北京市')[,-1]

Loan= FunIndexName(Loan, '年末金融机构贷款余额.万元')
Loan = IDDELF(Loan)
for (n in 1:ncol(Loan)){Loan[,n] = enc2utf8(as.character(Loan[,n]))}
save(Loan, file='SuperIndex/Loan.Rdata')


######  13.火灾 14.交通  ######
#############################
ind1 = grep('火灾起', uniindex, value=T)
ind2 = grep('交通.*件', uniindex, value=T)

Fire = FunRequest(1985:2017, ind1, dfnew$city)
Crash = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Crash, Crash$city == '榆林市')[,-1]

### 统一指标名称
Fire$index = '火灾.起.市辖区'
Crash$index = '交通事故.件.市辖区'

Fire = IDDELF(Fire)
for (n in 1:ncol(Fire)){Fire[,n] = enc2utf8(as.character(Fire[,n]))}
save(Fire, file='SuperIndex/Fire.Rdata')

Crash = IDDELF(Crash)
for (n in 1:ncol(Crash)){Crash[,n] = enc2utf8(as.character(Crash[,n]))}
save(Crash, file='SuperIndex/Crash.Rdata')


##### 15.工业废水排放量 ######
##############################
#uniindex = unique(dfnew$index)
ind1 = grep('.*废水.*', uniindex, value=T)
ind2 = grep('处理量|达标|产生量|二氧化硫排放量|投资|处理率', ind1, invert=T, value=T)

WasteWater = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(WasteWater , WasteWater $city == '深圳市')[,-1]

WasteWater = FunIndexName(WasteWater, '工业废水排放量.万吨')
WasteWater = IDDELF(WasteWater)
for (n in 1:ncol(WasteWater)){WasteWater[,n] = enc2utf8(as.character(WasteWater[,n]))}
save(WasteWater, file='SuperIndex/WasteWater.Rdata')

################################################################################
############################     基础建设      #################################
################################################################################
################################################################################
##### 1.2.3.4管道长度 #####
####################
indexRequest = grep('长度', uniindex, value=T)
indexRequest1 = grep('每万人', indexRequest, invert=T, value=T)
ps = grep('下水道长度|下水道总长度|排水管道长度', indexRequest1, value=T)
gs = grep('供水管道', indexRequest1, value=T)
mq = grep('煤气管道', indexRequest1, value=T)
dl = grep('铺装道路', indexRequest1, value=T)

SewagePipeline.Length = FunRequest(1985:2017, ps, dfnew$city)
WaterSupplyPipeline.Length = FunRequest(1985:2017, gs, dfnew$city)
GasPipeline.Length = FunRequest(1985:2017, mq, dfnew$city)
PavedRoad.Length = FunRequest(1985:2017, dl, dfnew$city)


### 统一指标名称、去重、保存
SewagePipeline.Length = FunIndexName(SewagePipeline.Length, '排水管道长度.公里')
SewagePipeline.Length = IDDELF(SewagePipeline.Length)
for (n in 1:ncol(SewagePipeline.Length)){SewagePipeline.Length[,n] = enc2utf8(as.character(SewagePipeline.Length[,n]))}
save(SewagePipeline.Length, file='SuperIndex/SewagePipeline.Length.Rdata')

WaterSupplyPipeline.Length = FunIndexName(WaterSupplyPipeline.Length, '排水管道长度.公里')
WaterSupplyPipeline.Length = IDDELF(WaterSupplyPipeline.Length)
for (n in 1:ncol(WaterSupplyPipeline.Length)){WaterSupplyPipeline.Length[,n] = enc2utf8(as.character(WaterSupplyPipeline.Length[,n]))}
save(WaterSupplyPipeline.Length, file='SuperIndex/WaterSupplyPipeline.Length.Rdata')

GasPipeline.Length = FunIndexName(GasPipeline.Length, '排水管道长度.公里')
GasPipeline.Length = IDDELF(GasPipeline.Length)
for (n in 1:ncol(GasPipeline.Length)){GasPipeline.Length[,n] = enc2utf8(as.character(GasPipeline.Length[,n]))}
save(GasPipeline.Length, file='SuperIndex/GasPipeline.Length.Rdata')

PavedRoad.Length = FunIndexName(PavedRoad.Length, '排水管道长度.公里')
PavedRoad.Length = IDDELF(PavedRoad.Length)
for (n in 1:ncol(PavedRoad.Length)){PavedRoad.Length[,n] = enc2utf8(as.character(PavedRoad.Length[,n]))}
save(PavedRoad.Length, file='SuperIndex/PavedRoad.Length.Rdata')


#########################
##### 5.道路面积 #####
####################
load('SuperIndex/POP.Rdata')
POP$value = as.numeric(POP$value)
indexRequest = grep('铺装道路面积|城市道路面积', uniindex, value=T)
indexRequest1 = grep('每万人|人均', indexRequest, invert=T, value=T)

indexRequest2 = grep('每万人|人均', indexRequest, value=T)

### 1985-2003是铺装道路面积，2004-2017改名为城市道路面积(或改变了统计口径)
### 人均和总的都有缺失年份，需要拼接

### 先处理人均，把人均改成总的
RoadArea = FunRequest(1985:2017, indexRequest2, dfnew$city)
RoadArea$index[which(RoadArea$year=='2001')] = paste0(RoadArea$index[which(RoadArea$year=='2001')],'市辖区')

### 统一指标名称
RoadArea = FunIndexName(RoadArea, '道路面积.平方米')
tst = subset(RoadArea, RoadArea$city == '上海市')[,-1]

### 统一单位
i100 = 1:dim(RoadArea)[1]
for (i100i in i100){
  cityL = POP$city==RoadArea$city[i100i]
  yearL = POP$year==RoadArea$year[i100i]
  indexL = substr(POP$index,10,nchar(POP$index)) == substr(RoadArea$index[i100i],10,nchar(RoadArea$index)[i100i])
  corPOP = POP[which(cityL & yearL & indexL),]$value
  if (length(corPOP)!=1){
    corPOP = NA
  }
  RoadArea$value[i100i] = RoadArea$value[i100i] * corPOP
}

### 现在处理现成的总的
RoadAreaT = FunRequest(1985:2017, indexRequest1, dfnew$city)
tst1 = subset(RoadAreaT, RoadAreaT$city == '上海市')[,-1]

### 如无意外
### 拼接，去重
CityRoadArea = rbind(RoadArea, RoadAreaT)
CityRoadArea$index = '道路面积.万平方米.市辖区'

### 去重
CityRoadArea = IDDELF(CityRoadArea)
for (n in 1:ncol(CityRoadArea)){CityRoadArea[,n] = enc2utf8(as.character(CityRoadArea[,n]))}
save(CityRoadArea, file='SuperIndex/CityRoadArea.Rdata')



##### 6.小学数量 #####
####################
indexRequest = grep('.*小学.*', uniindex, value=T)
indexRequest1 = grep('小学数|学校总数|学校数', indexRequest, value=T)
indexRequest2 = grep('中学数|三十一教育_1993_小学学校数个市辖区_单位', indexRequest1, invert=T, value=T)
PrimarySchool = FunRequest(1985:2017, indexRequest2, dfnew$city)
### 重庆很奇怪啊，1998年从飙到八千飙到
### 统一指标名称
PrimarySchool = FunIndexName(PrimarySchool, '小学数量.所')

PrimarySchool = IDDELF(PrimarySchool)
for (n in 1:ncol(PrimarySchool)){PrimarySchool[,n] = enc2utf8(as.character(PrimarySchool[,n]))}
save(PrimarySchool, file='SuperIndex/PrimarySchool.Rdata')

##### 7.小学教师 #####
####################
indexRequest = grep('.*小学.*', uniindex, value=T)
indexRequest1 = grep('教师', indexRequest, value=T)
PrimaryTeacher = FunRequest(1985:2017, indexRequest1, dfnew$city)
### 重庆很奇怪啊，1998年从飙到八千飙到
### 统一指标名称
PrimaryTeacher = FunIndexName(PrimaryTeacher, '小学教师数.人')

PrimaryTeacher = IDDELF(PrimaryTeacher)
PrimaryTeacher = subset(PrimaryTeacher, PrimaryTeacher$value < 400000)
for (n in 1:ncol(PrimaryTeacher)){PrimaryTeacher[,n] = enc2utf8(as.character(PrimaryTeacher[,n]))}
save(PrimaryTeacher, file='SuperIndex/PrimaryTeacher.Rdata')

##### 8.医院数量 #####
####################
ind1 = grep('.*医院.*', uniindex, value=T)
ind2 = grep('床位数和医生数__医院卫生院数', ind1, value=T)
ind3 = grep('床|医生|技术|每万人', ind1, invert=T, value=T)
ind4 = c(ind2, ind3)

Hospital = FunRequest(1985:2017, ind4, dfnew$city)
tst = subset(Hospital, Hospital$city == '上海市')[,-1]

Hospital = FunIndexName(Hospital, '医院.个')

Hospital = IDDELF(Hospital)
for (n in 1:ncol(Hospital)){Hospital[,n] = enc2utf8(as.character(Hospital[,n]))}
save(Hospital, file='SuperIndex/Hospital.Rdata')

##### 9.医院床位数 #####
######################
#uniindex = unique(dfnew$index)
ind1 = grep('.*床.*(张|床).*', uniindex, value=T)
ind2 = grep('每万人|福利|卫生机构床位|__床位', ind1, invert=T, value=T)
ind3 = c('卫生机构及人员__卫生机构医院床位数市辖区_单位','卫生机构及人员__卫生机构医院床位数全市_单位')
ind4 = c(ind2,ind3)

HospitalBerth = FunRequest(1985:2017, ind4, dfnew$city)
tst = subset(HospitalBerth, HospitalBerth$city == '上海市')[,-1]

HospitalBerth = FunIndexName(HospitalBerth, '医院床位数.张')
HospitalBerth = IDDELF(HospitalBerth)
HospitalBerth = subset(HospitalBerth, HospitalBerth$value<=80000)
for (n in 1:ncol(HospitalBerth)){HospitalBerth[,n] = enc2utf8(as.character(HospitalBerth[,n]))}
save(HospitalBerth, file='SuperIndex/HospitalBerth.Rdata')

##### 10.医生数量 #####
#######################
#uniindex = unique(dfnew$index)
ind1 = grep('.*医生.*', uniindex, value=T)
ind2 = grep('每|张|个', ind1, invert=T, value=T)

Doctor = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Doctor, Doctor$city == '上海市')[,-1]

Doctor = FunIndexName(Doctor, '医生数.人')
Doctor = IDDELF(Doctor)
Doctor = subset(Doctor, !(Doctor$value>60000 & Doctor$year=='1995'))
Doctor = subset(Doctor, Doctor$year != 1985)
for (n in 1:ncol(Doctor)){Doctor[,n] = enc2utf8(as.character(Doctor[,n]))}
save(Doctor, file='SuperIndex/Doctor.Rdata')


##### 11.影剧院数 ######
#####################
ind1 = grep('.*剧.*', uniindex, value=T)
ind2 = grep('电影放映单位|每|_公共|馆数个', ind1, invert=T, value=T)

Cinema = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Cinema, Cinema$city == '上海市')[,-1]

Cinema = FunIndexName(Cinema, '影剧院.个')
Cinema = IDDELF(Cinema)
Cinema = subset(Cinema, Cinema$value < 500)
for (n in 1:ncol(Cinema)){Cinema[,n] = enc2utf8(as.character(Cinema[,n]))}
save(Cinema, file='SuperIndex/Cinema.Rdata')


##### 12.公共汽电车辆数 #####
##########################
ind1 = grep('.*公共.*汽电.*车.*', uniindex, value=T)
ind2 = grep('万人', ind1, invert=T, value=T)
ind3 = grep('每万人', ind1, value=T)

Bus = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Bus, Bus$city == '北京市')[,-1]

Bus$index = '公共汽电车辆数.辆.市辖区'
Bus = IDDELF(Bus)
for (n in 1:ncol(Bus)){Bus[,n] = enc2utf8(as.character(Bus[,n]))}
save(Bus, file='SuperIndex/Bus.Rdata')




################################################################################
#############################    土地利用      #################################
################################################################################
################################################################################
##### 1.土地面积 #####
####################
#uniindex = unique(dfnew$index)
indexRequest = grep('.*土地面积.*', uniindex, value=T)
mianji = grep('人口密度人|绿|耕地|建成区|__城市|万立方米', indexRequest, invert=T, value=T)
mianji = c(mianji, '行政区域土地面积及建成区面积__行政区域土地面积_单位_县级市')
### 1998,1999缺少县级市数据
Area = FunRequest(1985:2017, mianji, dfnew$city)
tst = subset(Area, Area$city == '榆林市')[,-1]

### 统一指标名称
Area = FunIndexName(Area, '土地面积.平方公里')

Area = IDDELF(Area)
for (n in 1:ncol(Area)){Area[,n] = enc2utf8(as.character(Area[,n]))}
save(Area, file='SuperIndex/Area.Rdata')




#### 土地面积建成区 ###
##### 2.建成区面积 #####
######################
#uniindex = unique(dfnew$index)
ind1 = grep('建成区', uniindex, value=T)
ind2 = grep('比重|绿地面积|绿化覆盖|绿化面积|行业|_行政区域土地面积_', ind1, invert=T, value=T)

AreaBuilt = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(AreaBuilt, AreaBuilt$city == '榆林市')[,-1]

### 统一指标名称
AreaBuilt$index = '建成区土地面积.平方公里.全市.市辖区'

AreaBuilt = IDDELF(AreaBuilt)
outlier = c('丰镇市-1992-土地面积人口密度和城市绿化__土地面积平方公里市辖区建成区_单位-u', '百色市-2012-行政区域土地面积及人口密度__建成区面积平方公里市辖区_单位-e')
AreaBuilt = subset(AreaBuilt, !AreaBuilt$id %in% outlier)
for (n in 1:ncol(AreaBuilt)){AreaBuilt[,n] = enc2utf8(as.character(AreaBuilt[,n]))}
save(AreaBuilt, file='SuperIndex/AreaBuilt.Rdata')







################################################################################
#############################    个体需求      #################################
################################################################################
################################################################################
##### 1.居住用地面积 #####
########################
indexRequest = grep('(居住|建筑).*面积.*', uniindex, value=T)
indexRequest1 = grep('施工|竣工|商品房|人均',indexRequest, invert=T, value=T)
living = grep('居住', indexRequest1, value=T)

LivingSpace = FunRequest(1985:2017, living, dfnew$city)

LivingSpace = FunIndexName(LivingSpace, '居住面积.平方公里')
tst = subset(LivingSpace, LivingSpace$city=='榆林市')

### 去重
LivingSpace = IDDELF(LivingSpace)
for (n in 1:ncol(LivingSpace)){LivingSpace[,n] = enc2utf8(as.character(LivingSpace[,n]))}
save(LivingSpace, file='SuperIndex/LivingSpace.Rdata')

##### 2.居民用电量 #####
######################
load('SuperIndex/POP.Rdata')
POP$value = as.numeric(POP$value)
indexRequest = grep('.*用电.*', uniindex, value=T)
elec0 = grep('居民|生活', indexRequest, value=T)
elec1 = grep('用水量', elec0, invert=T, value=T)
elec2 = grep('每|人均|平均', elec1, invert=T, value=T)
elec3 = grep('每|人均|平均', elec1, value=T)

### 人均和总的都有缺失年份，需要拼接

### 先处理人均，把人均改成总的
ERA = FunRequest(1985:2017, elec3, dfnew$city)
ERA$index[which(ERA$year=='2001')] = paste0(ERA$index[which(ERA$year=='2001')],'市辖区')

### 统一指标名称
ERA = FunIndexName(ERA, '居民生活用电量.万千瓦时')
tst = subset(ERA, ERA$city == '上海市')[,-1]

### 统一单位
i100 = 1:dim(ERA)[1]
for (i100i in i100){
  cityL = POP$city==ERA$city[i100i]
  yearL = POP$year==ERA$year[i100i]
  indexL = substr(POP$index,10,nchar(POP$index)) == substr(ERA$index[i100i],14,nchar(ERA$index)[i100i])
  corPOP = POP[which(cityL & yearL & indexL),]$value
  if (length(corPOP)!=1){
    corPOP = NA
  }
  ERA$value[i100i] = ERA$value[i100i] * corPOP
}
ERA = subset(ERA, !(ERA$year %in% as.character(1985:1990) & ERA$value > 400000))

ER = FunRequest(1985:2017, elec2, dfnew$city)

ER = FunIndexName(ER, '居民生活用电量.万千瓦时')
tst = subset(ER, ER$city=='上海市')[,-1]
ER$value[which(ER$year %in% as.character(c(1985:1992)))] = ER$value[which(ER$year %in% as.character(c(1985:1992)))] * 10000


ElectricityRisident = rbind(ERA, ER)

### 去重
ElectricityRisident = IDDELF(ElectricityRisident)
for (n in 1:ncol(ElectricityRisident)){ElectricityRisident[,n] = enc2utf8(as.character(ElectricityRisident[,n]))}
save(ElectricityRisident, file='SuperIndex/ElectricityRisident.Rdata')


##### 3.居民用水量 #####
######################
load('SuperIndex/POP.Rdata')
POP$value = as.numeric(POP$value)
indexRequest = grep('.*用水.*', uniindex, value=T)
elec0 = grep('居民|生活|家庭', indexRequest, value=T)
elec1 = grep('用煤气|用液化|人口万|生活用电', elec0, invert=T, value=T)
elec2 = grep('每|人均|平均', elec1, invert=T, value=T)
elec3 = grep('每|人均|平均', elec1, value=T)

### 人均和总的都有缺失年份，需要拼接

### 先处理人均，把人均改成总的
ERA = FunRequest(1985:2017, elec3, dfnew$city)

### 统一指标名称
ERA = FunIndexName(ERA, '居民生活用水量.万吨')
tst = subset(ERA, ERA$city == '上海市')[,-1]

### 统一单位
i100 = 1:dim(ERA)[1]
for (i100i in i100){
  cityL = POP$city==ERA$city[i100i]
  yearL = POP$year==ERA$year[i100i]
  indexL = substr(POP$index,10,nchar(POP$index)) == substr(ERA$index[i100i],12,nchar(ERA$index)[i100i])
  corPOP = POP[which(cityL & yearL & indexL),]$value
  if (length(corPOP)!=1){
    corPOP = NA
  }
  ERA$value[i100i] = ERA$value[i100i] * corPOP
}

ER = FunRequest(1985:2017, elec2, dfnew$city)
tst = subset(ER, ER$city == '上海市')[,-1]

ER = FunIndexName(ER, '居民生活用水量.万吨')

WaterRisident = rbind(ERA, ER)
tst = subset(WaterRisident, WaterRisident$city=='上海市')[,-1]
WaterRisident$index[which(WaterRisident$year=='2001')] = gsub('全市', '市辖区', WaterRisident$index[which(WaterRisident$year=='2001')])


### 去重
WaterRisident = IDDELF(WaterRisident)
for (n in 1:ncol(WaterRisident)){WaterRisident[,n] = enc2utf8(as.character(WaterRisident[,n]))}
save(WaterRisident, file='SuperIndex/WaterRisident.Rdata')


#######################################################################################
#######################################################################################
#######################################################################################




#######################################################################################
#######################################################################################
#######################################################################################



# 
# 
# ##### 绿地面积 #####
# ####################
# ##### 绿地面积建成区 #####
# #uniindex = unique(dfnew$index)
# load('indexSQL/AreaBuilt.Rdata')
# ind0 = grep('.*绿.*', uniindex, value=T)
# ind1 = grep('.*比重|道路|平方公里|耕地|公园|人均.*', ind0, invert=T, value=T)
# green1 = grep('.*覆盖率.*', ind0, value=T)
# 
# GreenBuiltR = FunRequest(1985:2017, green1, dfnew$city)
# 
# ### 把建成区绿化覆盖率改成建成区绿化面积
# for (i100i in 1:dim(GreenBuiltR)[1]){
#   cityL = AreaBuilt$city==GreenBuiltR$city[i100i]
#   yearL = AreaBuilt$year==GreenBuiltR$year[i100i]
#   indexL = grepl('建成区', AreaBuilt$index)
#   corAreaBuilt = AreaBuilt[which(cityL & yearL & indexL),]$value
#   if (length(corAreaBuilt)==0){
#     corAreaBuilt = NA}
#   if (length(corAreaBuilt)>1){
#     print(AreaBuilt[which(cityL & yearL & indexL),])
#     break}
#   GreenBuiltR$value[i100i] = GreenBuiltR$value[i100i] * corAreaBuilt #单位公顷
# }
# tst = subset(GreenBuiltR, GreenBuiltR$city == '上海市')[,-1]
# 
# ### 现在处理现成的总的
# ind2 = grep('.*建成区.*', ind1, value=T)
# green2 = grep('.*率.*', ind2, invert=T, value=T)
# 
# GreenBuiltA = FunRequest(1985:2017, green2, dfnew$city)
# tst = subset(GreenBuiltA, GreenBuiltA$city == '北京市')[,-1]
# 
# ### 如无意外
# ### 拼接，去重
# GreenBuilt = rbind(GreenBuiltR, GreenBuiltA)
# GreenBuilt$index = '绿地覆盖面积.公顷.建成区'
# GreenBuilt = GreenBuilt[!duplicated(GreenBuilt[,-1]),]
# tst = subset(GreenBuilt, GreenBuilt$city == '北京市')[,-1]
# iddelf = vector()
# f= GreenBuilt[duplicated(GreenBuilt[,c(-1,-5)]),]
# for (i in 1:dim(f)[1]){
#   ff = subset(GreenBuilt,GreenBuilt$city==f$city[i] & GreenBuilt$year==f$year[i] & GreenBuilt$index==f$index[i])
#   if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
#   }else{iddelf = c(iddelf, ff$id[1])}
# }
# GreenBuilt = GreenBuilt[which(!GreenBuilt$id %in% iddelf),]
# 
# save(GreenBuilt, file='indexSQL/GreenBuilt.Rdata')
# 
# ##### 绿地面积市辖区 ##### 
# ind3 = grep('.*市辖区.*', ind1, value=T)
# green3 = c(grep('建成区|公共', ind3, invert=T, value=T),'土地面积土地面积平方公里园林绿地面积公顷','土地面积人口密度和城市绿化园林绿地面积公顷')
# 
# Green = FunRequest(1985:2017, green3, dfnew$city)
# tst = subset(Green, Green$city == '北京市')[,-1]
# 
# Green$index = '绿地覆盖面积.公顷.市辖区'
# save(Green, file='indexSQL/Green.Rdata')
# 
# 
# ##### 邮电业务总量 #####
# ########################
# uniindex = unique(dfnew$index)
# ### 2001年以后的邮政和电信业务总量是分开的，相加做好2001年后的邮电业务总量
# ind1 = grep('.*电信.*', uniindex, value=T)
# Tele = FunRequest(1985:2017, ind1, dfnew$city)
# tst = subset(Tele, Tele$city == '北京市')[,-1]
# Tele = FunIndexName(Tele, '电信业务总量.万元')
# 
# ind2 = grep('.*邮政业务.*', uniindex, value=T)
# Post = FunRequest(1985:2017, ind2, dfnew$city)
# tst = subset(Post, Post$city == '北京市')[,-1]
# Post = FunIndexName(Post, '邮政业务总量.万元')
# 
# PostTele2001 = Tele
# PostTele2001$value = Post$value + Tele$value
# PostTele2001$id = paste0(PostTele2001$id, 'lzy')
# PostTele2001$index = gsub('电信', '邮电', PostTele2001$index)
# tst = subset(PostTele2001, PostTele2001$city == '北京市')[,-1]
# 
# ### 199x:2001年的邮电业务总量是人均，算好总的
# ind3 = grep('.*人均邮电业务.*', uniindex, value=T)
# PostTeleA = FunRequest(1985:2017, ind3, dfnew$city)
# tst = subset(PostTeleA, PostTeleA$city == '北京市')[,-1]
# PostTeleA = FunIndexName(PostTeleA, '邮电业务总量.万元')
# 
# ### 把人均改成总的
# load('indexSQL/POP.Rdata')
# for (i100i in 1:dim(PostTeleA)[1]){
#   cityL = POP$city==PostTeleA$city[i100i]
#   yearL = POP$year==PostTeleA$year[i100i]
#   indexL = substr(POP$index,10,nchar(POP$index)) == substr(PostTeleA$index[i100i],11,nchar(PostTeleA$index)[i100i])
#   corPOP = POP[which(cityL & yearL & indexL),]$value
#   if (length(corPOP)==0){
#     corPOP = NA}
#   if (length(corPOP)>1){
#     print(POP[which(cityL & yearL & indexL),])
#     break}
#   PostTeleA$value[i100i] = PostTeleA$value[i100i] * corPOP
# }
# 
# 
# ### 再早些年的邮电业务总量
# ind4 = grep('.*邮电业务.*', uniindex, value=T)
# ind5 = grep('.*人均|用电量|电话机.*', ind4, invert=T, value=T)
# PostTele1985 = FunRequest(1985:2017, ind5, dfnew$city)
# tst = subset(PostTele1985, PostTele1985$city == '北京市')[,-1]
# PostTele1985 = FunIndexName(PostTele1985, '邮电业务总量.万元')
# 
# ### 拼接，去重
# PostTele = rbind(PostTele1985, PostTeleA, PostTele2001)
# tst = subset(PostTele, PostTele$city == '北京市')[,-1]
# 
# save(PostTele, file='indexSQL/PostTele.Rdata')
# 
# 
# 
# 
# 
# # 三废数据太少
# 
# # 科学 教育 就业
# 
# 
# 
# 
# ###### 人口密度 #######
# #######################
# load('indexSQL/POP.Rdata')
# load('indexSQL/Area.Rdata')
# ydata = Area
# for (i100i in 1:dim(ydata)[1]){
#   cityL = POP$city==ydata$city[i100i]
#   yearL = POP$year==ydata$year[i100i]
#   indexL = substr(POP$index,10,nchar(POP$index)) == substr(ydata$index[i100i],11,nchar(ydata$index)[i100i])
#   corPOP = POP[which(cityL & yearL & indexL),]$value
#   if (length(corPOP)==0){
#     corPOP = NA}
#   if (length(corPOP)>1){
#     print(POP[which(cityL & yearL & indexL),])
#     corPOP = corPOP[1]}
#   ydata$value[i100i] = corPOP / ydata$value[i100i] * 10000 #人/平方公里
# }
# let = letters[sample(26,dim(ydata)[1],replace=T)]
# ydata$index = gsub('土地面积.平方公里', '人口密度.人每平方公里', ydata$index)
# POPdensity = data.frame(
#   id = paste(ydata$city, ydata$year, ydata$index, let, sep='-'),
#   city = ydata$city,
#   year = ydata$year,
#   index = ydata$index,
#   value = ydata$value
# )
# save(POPdensity, file=paste0('POPdensity.Rdata'))
