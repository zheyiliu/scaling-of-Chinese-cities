setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/')
load('psudoSQLall.Rdata')

###查询命令的函数
FunRequest = function(yearR,indexR,cityR=dfnew$city){
	datR = subset(dfnew, dfnew$city %in% cityR & dfnew$year %in% yearR & dfnew$index %in% indexR)
	return(datR)
}
###改市辖区地区的函数
FunIndexName = function(dfR, indexname){
	idR1 = dfR[grepl('市辖区|市区', dfR$index),]
	idR2 = dfR[!grepl('市辖区', dfR$index),]
	dfR$index[which(dfR$index %in% idR1$index)] = paste(indexname,'市辖区',sep='.')
	dfR$index[which(dfR$index %in% idR2$index)] = paste(indexname,'全市',sep='.')
	return(dfR)
}

### 去除同年同城市同指标的不同数值的情况
IDDELF = function(ddat){
	ddat = GDP
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



##### GDP #####
###############
uniindex = unique(dfnew$index)
indexRequest = grep('.*生产总值.*', uniindex, value=T)
indexRequest1 = grep('第一产业|第二产业|第三产业|人均|增长率|每万元',indexRequest, invert=T, value=T)
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
GDP$value = as.numeric(gsub(',','',GDP$value))
i2000 = which(GDP$value < 2000)
GDP$value[i2000] = GDP$value[i2000] * 10000

### 去重
GDP = GDP[!duplicated(GDP[,-1]),]

## 处理淮安市
# a = GDP[GDP$city=='淮安市',] 
# b = 1988:2000 #names(table(a$year))[table(a$year)==4]
# c = a[a$year %in% b & a$index=='地区生产总值.万元.市辖区',]
# d = a[a$year %in% b & a$index=='地区生产总值.万元.全市',]
# iddelc = c$id[grepl('淮安市', c$id)]
# iddeld = d$id[grepl('淮安市', d$id)]
e = GDP[which(GDP$city=='菏泽市'),]
iddele = e$id[duplicated(e[,c(-1,-2)])]
iddelf = vector()
f= GDP[duplicated(GDP[,c(-1,-5)]),]
for (i in 1:dim(f)[1]){
	ff = subset(GDP,GDP$city==f$city[i] & GDP$year==f$year[i] & GDP$index==f$index[i])
	if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
	}else{iddelf = c(iddelf, ff$id[1])}
}
GDP = GDP[which(!GDP$id %in% c(iddele,iddelf)),]

iddelz = GDP[which(GDP$city=='淮安市' & GDP$year==2000),]$id[c(3,4)]
GDP = GDP[!GDP$id %in% iddelz, ]
save(GDP, file='indexSQL/GDP.Rdata')

#####第一产业GDP占比 #####
##########################
load('indexSQL/GDP.Rdata')
#uniindex = unique(dfnew$index)
indexRequest = grep('.*第一产业.*', uniindex, value=T)
GDP1st = FunRequest(1985:2017, indexRequest, dfnew$city)

### 统一指标名称
GDP1st = FunIndexName(GDP1st, '第一产业GDP占比.%')
tst = subset(GDP1st, GDP1st$city == '榆林市')[,-1]

### 统一单位
GDP1st$value = as.numeric(gsub(',','',GDP1st$value))
i1994 = which(GDP1st$year %in% 1989:1994)
GDP1st$value[i1994] = GDP1st$value[i1994] * 10000
i100 = which(GDP1st$value > 100)
for (i100i in i100){
	cityL = GDP$city==GDP1st$city[i100i]
	yearL = GDP$year==GDP1st$year[i100i]
	indexL = substr(GDP$index,11,nchar(GDP$index)) == substr(GDP1st$index[i100i],13,nchar(GDP1st$index)[i100i])
	corGDP = GDP[which(cityL & yearL & indexL),]$value
	if (length(corGDP)>1){
		print(GDP[which(cityL & yearL & indexL),])
		break}
	GDP1st$value[i100i] = GDP1st$value[i100i]/corGDP * 100
}
GDP1st = GDP1st[which(!GDP1st$value > 100),]

### 去重
GDP1st = GDP1st[!duplicated(GDP1st[,-1]),]
save(GDP1st, file='indexSQL/GDP1st.Rdata')

#####第二产业GDP占比 #####
##########################
load('indexSQL/GDP.Rdata')
#uniindex = unique(dfnew$index)
indexRequest = grep('.*第二产业.*', uniindex, value=T)
GDP2nd = FunRequest(1985:2017, indexRequest, dfnew$city)

### 统一指标名称
GDP2nd = FunIndexName(GDP2nd, '第二产业GDP占比.%')
tst = subset(GDP2nd, GDP2nd$city == '榆林市')[,-1]

### 统一单位
GDP2nd$value = as.numeric(gsub(',','',GDP2nd$value))
i1994 = which(GDP2nd$year %in% 1989:1994)
GDP2nd$value[i1994] = GDP2nd$value[i1994] * 10000
i100 = which(GDP2nd$value > 100)
for (i100i in i100){
	cityL = GDP$city==GDP2nd$city[i100i]
	yearL = GDP$year==GDP2nd$year[i100i]
	indexL = substr(GDP$index,11,nchar(GDP$index)) == substr(GDP2nd$index[i100i],13,nchar(GDP2nd$index)[i100i])
	corGDP = GDP[which(cityL & yearL & indexL),]$value
	if (length(corGDP)>1){
		print(GDP[which(cityL & yearL & indexL),])
		break}
	GDP2nd$value[i100i] = GDP2nd$value[i100i]/corGDP * 100
}
GDP2nd[which(GDP2nd$value > 100),]
ilinfen = which(GDP2nd$id=='临汾市-1989-综合经济指标国内生产总值按现行第二产业价格计算市辖区-k')
GDP2nd$value[ilinfen] = GDP2nd$value[ilinfen]/100

### 去重
GDP2nd = GDP2nd[!duplicated(GDP2nd[,-1]),]
save(GDP2nd, file='indexSQL/GDP2nd.Rdata')

#####第三产业GDP占比 #####
##########################
load('indexSQL/GDP.Rdata')
#uniindex = unique(dfnew$index)
indexRequest = grep('.*第三产业.*', uniindex, value=T)
GDP3rd = FunRequest(1985:2017, indexRequest, dfnew$city)
indexRequest

### 统一指标名称
GDP3rd = FunIndexName(GDP3rd, '第三产业GDP占比.%')
tst = subset(GDP3rd, GDP3rd$city == '榆林市')[,-1]

### 统一单位
GDP3rd$value = as.numeric(gsub(',','',GDP3rd$value))
i1994 = which(GDP3rd$year %in% 1989:1994)
GDP3rd$value[i1994] = GDP3rd$value[i1994] * 10000
i100 = which(GDP3rd$value > 100)
for (i100i in i100){
	cityL = GDP$city==GDP3rd$city[i100i]
	yearL = GDP$year==GDP3rd$year[i100i]
	indexL = substr(GDP$index,11,nchar(GDP$index)) == substr(GDP3rd$index[i100i],13,nchar(GDP3rd$index)[i100i])
	corGDP = GDP[which(cityL & yearL & indexL),]$value
	if (length(corGDP)>1){
		print(GDP[which(cityL & yearL & indexL),])
		break}
	GDP3rd$value[i100i] = GDP3rd$value[i100i]/corGDP * 100
}
GDP3rd[which(GDP3rd$value > 100),]

### 去重
GDP3rd = GDP3rd[!duplicated(GDP3rd[,-1]),]
save(GDP3rd, file='indexSQL/GDP3rd.Rdata')


#####第二三产业比重之和 #####
GDP23 = GDP1st
GDP23$id = paste0(GDP1st$id, 'lzy')
GDP23$index = gsub('一','二三',GDP1st$index)
GDP23$value = 100 - GDP1st$value
save(GDP23, file='indexSQL/GDP23.Rdata')


##### 人口 #####
################
#uniindex = unique(dfnew$index)
indexRequest = grep('.*人口.*', uniindex, value=T)
zongrenkou = grep('总人口|户籍人口', indexRequest, value=T)
zongrenkou = grep('非农业', zongrenkou, invert=T, value=T)
feinongye = grep('非农业人口',indexRequest, value=T)
nianpingjun = grep('年平均',indexRequest, value=T)
#indexRequest[!indexRequest %in% c(zongrenkou,hujirenkou,feinongye,nianpingjun)]

POP = FunRequest(1985:2017, zongrenkou, dfnew$city)

### 统一指标名称
POP = FunIndexName(POP, '年末总人口.万人')
tst = subset(POP, POP$city == '榆林市')[,-1]

### 统一单位
POP$value = as.numeric(gsub(',','',POP$value))

POP = POP[!duplicated(POP[,-1]),]
save(POP, file='indexSQL/POP.Rdata')

##### 市辖区人口占比 #####
##########################
load('indexSQL/POP.Rdata')
POPsxq = POP[grepl('市辖区',POP$index),]
POPsq = POP[!grepl('市辖区',POP$index),]
POPr = data.frame(POPsxq)
POPr$id = paste0(POPr$id, 'lzy')
POPr$index = '市辖区人口占比.全市'
for (i100i in 1:dim(POPsxq)[1]){
	cityL = POPsq$city==POPsxq$city[i100i]
	yearL = POPsq$year==POPsxq$year[i100i]
	corPOPsq = POPsq[which(cityL & yearL),]$value
	if (length(corPOPsq)==0){
		corPOPsq = NA}
	if (length(corPOPsq)>1){
		print(POPsq[which(cityL & yearL),])
		break}
	POPr$value[i100i] = POPsxq$value[i100i]/corPOPsq*100
}
tst = subset(POPr, POPr$city == '榆林市')[,-1] 
save(POPr, file='indexSQL/POPr.Rdata')





##### 总工资 #####
##################
#uniindex = unique(dfnew$index)
indexRequest = grep('.*工资.*', uniindex, value=T)
gongzi = grep('工业企业|所有制|经济|合营|平均|人均|人数', indexRequest, invert=T, value=T)

Salary = FunRequest(1985:2017, gongzi, dfnew$city)
tst = subset(Salary, Salary$city == '上海市')[,-1]

Salary = FunIndexName(Salary, '职工工资总额.万元')

Salary = Salary[!duplicated(Salary[,-1]),]
save(Salary, file='indexSQL/Salary.Rdata')


##### 土地面积 #####
####################
#uniindex = unique(dfnew$index)
indexRequest = grep('.*土地面积.*', uniindex, value=T)
mianji0 = grep('\\b(土地面积人口密度和城市绿化).*', indexRequest, value=T)
mianji1 = grep('\\b(土地面积人口密度和城市绿化).*[绿密耕建]', mianji0, invert=T, value=T)
mianji2 = grep('人口密度|绿|耕地|建成区', indexRequest, invert=T, value=T)
mianji = c(mianji1, mianji2)

Area = FunRequest(1985:2017, mianji, dfnew$city)
tst = subset(Area, Area$city == '榆林市')[,-1]

### 统一指标名称
Area = FunIndexName(Area, '土地面积.平方公里')

Area[duplicated(Area[,-1]),]
f= Area[duplicated(Area[,c(-1,-5)]),]
Area = Area[!duplicated(Area[,-1]),]
save(Area, file='indexSQL/Area.Rdata')



##### 土地面积建成区 #####
#uniindex = unique(dfnew$index)
ind1 = grep('.*土地面积.*建成区.*', uniindex, value=T)
ind2 = grep('人口密度|.*全市\\b|绿', ind1, invert=T, value=T)
ind3 = c(ind2, '土地面积人口密度和城市绿化士地面积平方公里建成区', '土地面积人口密度和城市绿化土地面积平方公里建成区')

AreaBuilt = FunRequest(1985:2017, ind3, dfnew$city)
tst = subset(AreaBuilt, AreaBuilt$city == '榆林市')[,-1]

### 统一指标名称
AreaBuilt$index = '土地面积.平方公里.建成区'

AreaBuilt[duplicated(AreaBuilt[,-1]),]
f= AreaBuilt[duplicated(AreaBuilt[,c(-1,-5)]),]
AreaBuilt = AreaBuilt[!duplicated(AreaBuilt[,-1]),]
save(AreaBuilt, file='indexSQL/AreaBuilt.Rdata')




##### 道路面积 #####
####################
load('indexSQL/POP.Rdata')
#uniindex = unique(dfnew$index)
indexRequest = grep('.*道路面积.*', uniindex, value=T)

### 先处理人均（因为有些年份只有人均道路面积）
indexRequest1 = grep('.*人均.*', indexRequest, value=T)
#dfnew[which(dfnew$index=='年末实有铺装道路面积平方米市辖区'),-1]

RoadArea = FunRequest(1985:2017, indexRequest1, dfnew$city)
tst = subset(RoadArea, RoadArea$city == '榆林市')[,-1]

### 把人均改成总的
for (i100i in 1:dim(RoadArea)[1]){
	cityL = POP$city==RoadArea$city[i100i]
	yearL = POP$year==RoadArea$year[i100i]
	indexL = grepl('市辖区', POP$index)
	corPOP = POP[which(cityL & yearL & indexL),]$value
	if (length(corPOP)==0){
		corPOP = NA}
	if (length(corPOP)>1){
		print(POP[which(cityL & yearL & indexL),])
		break}
	RoadArea$value[i100i] = RoadArea$value[i100i] * corPOP
}

### 现在处理现成的总的
indexRequest2 = grep('.*人|车.*', indexRequest, invert=T, value=T)

RoadAreaT = FunRequest(1985:2017, indexRequest2, dfnew$city)
tst = subset(RoadAreaT, RoadAreaT$city == '上海市')[,-1]

### 如无意外
### 拼接，去重
CityRoadArea = rbind(RoadArea, RoadAreaT)
CityRoadArea$index = '道路面积.万平方米.市辖区'
CityRoadArea = CityRoadArea[!duplicated(CityRoadArea[,-1]),]
tst = subset(CityRoadArea, CityRoadArea$city == '榆林市')[,-1]
iddelf = vector()
f= CityRoadArea[duplicated(CityRoadArea[,c(-1,-5)]),]
for (i in 1:dim(f)[1]){
	ff = subset(CityRoadArea,CityRoadArea$city==f$city[i] & CityRoadArea$year==f$year[i] & CityRoadArea$index==f$index[i])
	if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
	}else{iddelf = c(iddelf, ff$id[1])}
}
CityRoadArea = CityRoadArea[which(!CityRoadArea$id %in% iddelf),]

save(CityRoadArea, file='indexSQL/CityRoadArea.Rdata')


##### 能源消耗 #####
####################
##### 全年用电量 ###
#uniindex = unique(dfnew$index)
indexRequest = grep('.*全年用电|全社会用电.*', uniindex, value=T)
elec0 = grep('工业|电话', indexRequest, invert=T, value=T)
elec1 = grep('用水', elec0, value=T)
elec2 = grep('居民|生活', elec0, invert=T, value=T)

Electricity = FunRequest(1985:2017, c(elec1,elec2), dfnew$city)
tst = subset(Electricity, Electricity$city == '上海市')[,-1]

### 统一单位
iyie = which(Electricity$year %in% 1985:1991)
Electricity$value[iyie] = Electricity$value[iyie] * 10000

### 统一指标名称
Electricity = FunIndexName(Electricity, '全年用电量.万千瓦时')

save(Electricity, file='indexSQL/Electricity.Rdata')


##### 用水量 #####
#uniindex = unique(dfnew$index)
ind1 = grep('.*[供|用]水.*量.*[吨|立方米].*', uniindex, value=T)
ind2 = grep('生产|用电量|均|备水|生活|居民', ind1, invert=T, value=T)

Water = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Water, Water$city == '上海市')[,-1]

Water$index = '供水总量.万吨.市辖区'

save(Water, file='indexSQL/Water.Rdata')



##### 创造力 #####
##################
##### 藏书量 #####
#uniindex = unique(dfnew$index)
ind1 = grep('.*藏.*', uniindex, value=T)
ind2 = grep('人|个', ind1, invert=T, value=T)

Book = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Book, Book$city == '深圳市')[,-1]

Book = FunIndexName(Book, '公共图书馆藏书量.千册')

save(Book, file='indexSQL/Book.Rdata')

##### 影剧院数 ######
#####################
#uniindex = unique(dfnew$index)
ind1 = grep('.*剧.*', uniindex, value=T)
ind2 = grep('公共图书馆|影剧院和图书馆电影放映单位个地区|影剧院和图书馆电影放映单位个市辖区', ind1, invert=T, value=T)

Cinema = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Cinema, Cinema$city == '深圳市')[,-1]

Cinema = FunIndexName(Cinema, '影剧院.个')

save(Cinema, file='indexSQL/Cinema.Rdata')


##### 学校数 #####
##################
#uniindex = unique(dfnew$index)
ind1 = grep('.*普通中学.*', uniindex, value=T)
ind2 = grep('教师|学生', ind1, invert=T, value=T)

School = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(School, School$city == '榆林市')[,-1]

School = FunIndexName(School, '普通中学.所')

save(School, file='indexSQL/School.Rdata')

##### 医院数 #####
##################
#uniindex = unique(dfnew$index)
ind1 = grep('.*医院.*', uniindex, value=T)
ind2 = grep('床|医生|技术|每万人', ind1, invert=T, value=T)

Hospital = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Hospital, Hospital$city == '上海市')[,-1]

Hospital = FunIndexName(Hospital, '医院.个')

save(Hospital, file='indexSQL/Hospital.Rdata')

##### 医院床位数 #####
######################
#uniindex = unique(dfnew$index)
ind1 = grep('.*床.*张.*', uniindex, value=T)
ind2 = grep('每万人|福利', ind1, invert=T, value=T)

HospitalBerth = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(HospitalBerth, HospitalBerth$city == '上海市')[,-1]

HospitalBerth = FunIndexName(HospitalBerth, '医院床位数.张')

save(HospitalBerth, file='indexSQL/HospitalBerth.Rdata')

##### 客运量 #####
##################
#uniindex = unique(dfnew$index)
ind1 = grep('.*客运总量.*', uniindex, value=T)
ind2 = grep('路|水|空|车', ind1, invert=T, value=T)

Passenger = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Passenger, Passenger$city == '上海市')[,-1]

Passenger = FunIndexName(Passenger, '客运总量.万人')

save(Passenger, file='indexSQL/Passenger.Rdata')

##### 公共汽电车辆数 #####
##########################
uniindex = unique(dfnew$index)
ind1 = grep('.*公共.*汽电.*车.*', uniindex, value=T)
ind2 = grep('万人', ind1, invert=T, value=T)

Bus = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Bus, Bus$city == '北京市')[,-1]

Bus$index = '公共汽电车辆数.辆.市辖区'

save(Bus, file='indexSQL/Bus.Rdata')

##### 公共汽电车客运总数 #####
##############################
#uniindex = unique(dfnew$index)
ind1 = grep('.*公共.*汽电.*车.*客.*', uniindex, value=T)

BusPassenger = FunRequest(1985:2017, ind1, dfnew$city)
tst = subset(BusPassenger, BusPassenger$city == '北京市')[,-1]

BusPassenger$index = '公共汽电车客运总数.万人次.市辖区'

save(BusPassenger, file='indexSQL/BusPassenger.Rdata')


##### 存款 #####
################
#uniindex = unique(dfnew$index)
ind1 = grep('.*储.*蓄.*余.*额.*', uniindex, value=T)
ind2 = grep('人均', ind1, invert=T, value=T)
ind3 = grep('贷款', ind2, invert=T, value=T)
ind4 = c(ind3, '年末金融机构各项贷款余额市辖区城乡居民储蓄年末余额全市城乡居民储蓄年末余额市辖区', '年末金融机构各项贷款余额市辖区城乡居民储蓄年末余额全市')

DepositHousehold = FunRequest(1985:2017, ind4, dfnew$city)
tst = subset(DepositHousehold , DepositHousehold $city == '北京市')[,-1]

DepositHousehold = FunIndexName(DepositHousehold, '年末居民存款余额.万元')

save(DepositHousehold , file='indexSQL/DepositHousehold.Rdata')

##### 贷款 #####
################
#uniindex = unique(dfnew$index)
ind1 = grep('.*贷.*款.*余.*额.*', uniindex, value=T)
ind2 = grep('固定|农业|流动', ind1, invert=T, value=T)[c(-10,-11)]

Loan= FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Loan, Loan$city == '北京市')[,-1]

Loan= FunIndexName(Loan, '年末银行贷款余额.万元')

save(Loan, file='indexSQL/Loan.Rdata')



##### 绿地面积 #####
####################
##### 绿地面积建成区 #####
#uniindex = unique(dfnew$index)
load('indexSQL/AreaBuilt.Rdata')
ind0 = grep('.*绿.*', uniindex, value=T)
ind1 = grep('.*比重|道路|平方公里|耕地|公园|人均.*', ind0, invert=T, value=T)
green1 = grep('.*率.*', ind1, value=T)

GreenBuiltR = FunRequest(1985:2017, green1, dfnew$city)

### 把建成区绿化覆盖率改成建成区绿化面积
for (i100i in 1:dim(GreenBuiltR)[1]){
	cityL = AreaBuilt$city==GreenBuiltR$city[i100i]
	yearL = AreaBuilt$year==GreenBuiltR$year[i100i]
	indexL = grepl('建成区', AreaBuilt$index)
	corAreaBuilt = AreaBuilt[which(cityL & yearL & indexL),]$value
	if (length(corAreaBuilt)==0){
		corAreaBuilt = NA}
	if (length(corAreaBuilt)>1){
		print(AreaBuilt[which(cityL & yearL & indexL),])
		break}
	GreenBuiltR$value[i100i] = GreenBuiltR$value[i100i] * corAreaBuilt #单位公顷
}
tst = subset(GreenBuiltR, GreenBuiltR$city == '上海市')[,-1]

### 现在处理现成的总的
ind2 = grep('.*建成区.*', ind1, value=T)
green2 = grep('.*率.*', ind2, invert=T, value=T)

GreenBuiltA = FunRequest(1985:2017, green2, dfnew$city)
tst = subset(GreenBuiltA, GreenBuiltA$city == '北京市')[,-1]

### 如无意外
### 拼接，去重
GreenBuilt = rbind(GreenBuiltR, GreenBuiltA)
GreenBuilt$index = '绿地覆盖面积.公顷.建成区'
GreenBuilt = GreenBuilt[!duplicated(GreenBuilt[,-1]),]
tst = subset(GreenBuilt, GreenBuilt$city == '北京市')[,-1]
iddelf = vector()
f= GreenBuilt[duplicated(GreenBuilt[,c(-1,-5)]),]
for (i in 1:dim(f)[1]){
	ff = subset(GreenBuilt,GreenBuilt$city==f$city[i] & GreenBuilt$year==f$year[i] & GreenBuilt$index==f$index[i])
	if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
	}else{iddelf = c(iddelf, ff$id[1])}
}
GreenBuilt = GreenBuilt[which(!GreenBuilt$id %in% iddelf),]

save(GreenBuilt, file='indexSQL/GreenBuilt.Rdata')

##### 绿地面积市辖区 ##### 
ind3 = grep('.*市辖区.*', ind1, value=T)
green3 = c(grep('建成区|公共', ind3, invert=T, value=T),'土地面积土地面积平方公里园林绿地面积公顷','土地面积人口密度和城市绿化园林绿地面积公顷')
														
Green = FunRequest(1985:2017, green3, dfnew$city)
tst = subset(Green, Green$city == '北京市')[,-1]

Green$index = '绿地覆盖面积.公顷.市辖区'
save(Green, file='indexSQL/Green.Rdata')


##### 邮电业务总量 #####
########################
uniindex = unique(dfnew$index)
### 2001年以后的邮政和电信业务总量是分开的，相加做好2001年后的邮电业务总量
ind1 = grep('.*电信.*', uniindex, value=T)
Tele = FunRequest(1985:2017, ind1, dfnew$city)
tst = subset(Tele, Tele$city == '北京市')[,-1]
Tele = FunIndexName(Tele, '电信业务总量.万元')

ind2 = grep('.*邮政业务.*', uniindex, value=T)
Post = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(Post, Post$city == '北京市')[,-1]
Post = FunIndexName(Post, '邮政业务总量.万元')

PostTele2001 = Tele
PostTele2001$value = Post$value + Tele$value
PostTele2001$id = paste0(PostTele2001$id, 'lzy')
PostTele2001$index = gsub('电信', '邮电', PostTele2001$index)
tst = subset(PostTele2001, PostTele2001$city == '北京市')[,-1]

### 199x:2001年的邮电业务总量是人均，算好总的
ind3 = grep('.*人均邮电业务.*', uniindex, value=T)
PostTeleA = FunRequest(1985:2017, ind3, dfnew$city)
tst = subset(PostTeleA, PostTeleA$city == '北京市')[,-1]
PostTeleA = FunIndexName(PostTeleA, '邮电业务总量.万元')

### 把人均改成总的
load('indexSQL/POP.Rdata')
for (i100i in 1:dim(PostTeleA)[1]){
	cityL = POP$city==PostTeleA$city[i100i]
	yearL = POP$year==PostTeleA$year[i100i]
	indexL = substr(POP$index,10,nchar(POP$index)) == substr(PostTeleA$index[i100i],11,nchar(PostTeleA$index)[i100i])
	corPOP = POP[which(cityL & yearL & indexL),]$value
	if (length(corPOP)==0){
		corPOP = NA}
	if (length(corPOP)>1){
		print(POP[which(cityL & yearL & indexL),])
		break}
	PostTeleA$value[i100i] = PostTeleA$value[i100i] * corPOP
}


### 再早些年的邮电业务总量
ind4 = grep('.*邮电业务.*', uniindex, value=T)
ind5 = grep('.*人均|用电量|电话机.*', ind4, invert=T, value=T)
PostTele1985 = FunRequest(1985:2017, ind5, dfnew$city)
tst = subset(PostTele1985, PostTele1985$city == '北京市')[,-1]
PostTele1985 = FunIndexName(PostTele1985, '邮电业务总量.万元')

### 拼接，去重
PostTele = rbind(PostTele1985, PostTeleA, PostTele2001)
tst = subset(PostTele, PostTele$city == '北京市')[,-1]

save(PostTele, file='indexSQL/PostTele.Rdata')

##### 污染 #####
################
##### 工业废水排放量 #####
#uniindex = unique(dfnew$index)
ind1 = grep('.*废水.*', uniindex, value=T)
ind2 = grep('处理量|达标', ind1, invert=T, value=T)

WasteWater = FunRequest(1985:2017, ind2, dfnew$city)
tst = subset(WasteWater , WasteWater $city == '深圳市')[,-1]

WasteWater = FunIndexName(WasteWater, '工业废水排放量.万吨')

save(WasteWater, file='indexSQL/WasteWater.Rdata')


# 煤气+液化石油气供气总量 ### 表头太复杂
# 教师数表头太复杂
# 居住用地面积是截面（属于城市建设用地面积），居住面积数据量太少，只有1985:1994
# 三废数据太少

# 科学 教育 就业
# 房屋建设面积 住宅建筑面积 住宅居住面积 城市居住人口？



head(dfnew[which(dfnew$index=='工业二氧化硫产生量吨'),-1])
unique(dfnew[which(dfnew$index%in%ind1),-1]$year)



###### 人口密度 #######
#######################
load('indexSQL/POP.Rdata')
load('indexSQL/Area.Rdata')
ydata = Area
for (i100i in 1:dim(ydata)[1]){
	cityL = POP$city==ydata$city[i100i]
	yearL = POP$year==ydata$year[i100i]
	indexL = substr(POP$index,10,nchar(POP$index)) == substr(ydata$index[i100i],11,nchar(ydata$index)[i100i])
	corPOP = POP[which(cityL & yearL & indexL),]$value
	if (length(corPOP)==0){
		corPOP = NA}
	if (length(corPOP)>1){
		print(POP[which(cityL & yearL & indexL),])
		corPOP = corPOP[1]}
	ydata$value[i100i] = corPOP / ydata$value[i100i] * 10000 #人/平方公里
}
let = letters[sample(26,dim(ydata)[1],replace=T)]
ydata$index = gsub('土地面积.平方公里', '人口密度.人每平方公里', ydata$index)
POPdensity = data.frame(
	id = paste(ydata$city, ydata$year, ydata$index, let, sep='-'),
	city = ydata$city,
	year = ydata$year,
	index = ydata$index,
	value = ydata$value
)
save(POPdensity, file=paste0('POPdensity.Rdata'))