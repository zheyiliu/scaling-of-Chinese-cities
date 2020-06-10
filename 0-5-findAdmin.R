
library(readxl)

prov0 = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/省份列表.csv',header=F,stringsAsFactors = FALSE)[,1]
prov = subset(prov0, !grepl('吉林',prov0))
provfull = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/省份列表.csv',header=F,stringsAsFactors = FALSE)[,2]


### 已整理好的OCR识别错误的表格，用于Clean函数，可能以后会更新，以后可以批量对adminxxxx文件Clean一次
OCR1 = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/OCRerror.csv',header=T,stringsAsFactors = FALSE)
OCR2 = OCR1
for (r in 1:ncol(OCR1)){
	OCR1[!OCR1[,r]%in%c('津市','沙市'),r] = gsub('市\\b','',OCR1[!OCR1[,r]%in%c('津市','沙市'),r])
	OCR1[,r] = gsub('\\b津\\b', '津市', OCR1[,r])
	OCR1[,r] = gsub('\\b沙\\b', '沙市', OCR1[,r])
}
for (r in 1:ncol(OCR2)){
	whichshi = !grepl('市\\b', OCR2[,r]) & OCR2[,r]!='' | (grepl('\\b(津市)\\b|\\b(沙市)\\b', OCR2[,r]))
	OCR2[whichshi,r] = paste0(OCR2[whichshi,r], '市')
	OCR2[,r] = gsub('\\b津\\b', '津市', OCR2[,r])
	OCR2[,r] = gsub('\\b沙\\b', '沙市', OCR2[,r])
}
newOCRerror = data.frame(noshi1=OCR1[,1], noshi2=OCR1[,2], shi1=OCR2[,1], shi2=OCR2[,2], stringsAsFactors=F)
newOCRerror = unique(newOCRerror)
write.csv(newOCRerror, 'C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/newOCRerror.csv', row.names=F)

OCR = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/newOCRerror.csv',header=T,stringsAsFactors = FALSE)
colnames(OCR) = c('from','to','from','to')
#OCR = rbind(OCR[,1:2], OCR[,3:4])
OCR = OCR[,3:4]



Clean2005 = function(dat=dat){
	### 替换NA为空格
    for (r in 1:dim(dat)[2]){dat[,r][is.na(dat[,r])] = ''}
    ### 删除空行
    mm = vector()
    for (m in 1:10){if(sum(dat[m,]!='')==0){mm = c(mm, m)}}
    if (length(mm)!=0){dat = data.frame(dat[-mm,],stringsAsFactors=F)}
    ### 删除空列
    ss = vector()
    for (s in 1:dim(dat)[2]){if(sum(dat[,s]!='')==0){ss = c(ss, s)}}
    if (length(ss)!=0){dat = dat[,-ss]}
    ### 删除空格
    for (p in 1:dim(dat)[2]){dat[,p] = gsub(' ', '', dat[,p])}
	### 特殊符号
	for (v in 1:dim(dat)[2]){dat[,v] = gsub('[[:punct:]]|[\n *)]|[a-z]|[A-Z]', '', dat[,v])}
	dat = subset(dat, !grepl('名单|城市名称|市名称|人口|人口数|万人|合计|市分|\\d|超大城市|大城市|特大城市|小城市|中等城市|省级单位|括号', dat[,1]))
	whichshi = (!grepl('市\\b', dat[,1])) | (grepl('\\b(津市)\\b|\\b(沙市)\\b', dat[,1]))
	dat[whichshi,1] = paste0(dat[whichshi,1], '市')
	for (g in 1:nrow(OCR)){
		dat[,1] = gsub(OCR[g,1],OCR[g,2],dat[,1])
	}
	return(dat)
}

  
Clean = function(dat=dat){
	
	dat = Clean2005(dat)
	
	for (q in 1:dim(dat)[2]){dat[,q] = gsub('\\b内蒙\\b', '内蒙古', dat[,q])}
    dat = subset(dat, !(dat[,1] %in% c(prov,provfull)))
    dat = subset(dat, dat[,1]!='')
	return(dat)
 }

Mule1995a14 = function(yeari, f){
	setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/unlock/'))

	y1993 = data.frame(read_excel(f, sheet = 1, col_names=F, col_types='text'),stringsAsFactors=F)
	colnames(y1993) = rep(c('城市名称',paste0('admin',yeari),'非农业人口'), 2)
	y1993 = rbind(y1993[,c(1:2)],y1993[,c(4:5)])
	if(sum(grepl('市',y1993[,2])) < 20){y1993[,2] = paste0(y1993[,2], '市')}
	y1993 = Clean(y1993)
	return(y1993)
}

Mule1995b135 = function(yeari, f){
	setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/unlock/'))

	yfocal = data.frame(read_excel(f, sheet = 1, col_names=F, col_types='text'),stringsAsFactors=F)
	colnames(yfocal) = rep('城市名称',ncol(yfocal))
	yfocal = rbind(yfocal[1], yfocal[3], yfocal[5])
	yfocal = Clean(yfocal)
	colnames(yfocal) = rep('城市名称',ncol(yfocal))
	return(yfocal)
}


Mule1987 = function(f,yeari){
	
	setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/unlock/'))

	yfocal = data.frame(read_excel(f, sheet = 1, col_names=F, col_types='text'),stringsAsFactors=F)
	colnames(yfocal) = rep('城市名称',6)
	yfocal = rbind(yfocal[1], yfocal[3], yfocal[5])
	yfocal = subset(yfocal, !grepl('\\d', yfocal[,1]))
	yfocal = Clean(yfocal)
	colnames(yfocal) = rep('城市名称',ncol(yfocal))

	lastyeari = yeari-1
	last = read.csv(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin',lastyeari,'.csv'),header=T)
	last = Clean(last)[,c(1:4)]
	colnames(last)[1:2] = c('城市名称','last')

	y1993 = read_excel('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/1993-1994/unlock/中国城市统计年鉴-1993-1994-二.按市区非农业人口排列的全部城市名单(按省，直辖市，自治区及行政级别排列)wkrg.xls', 
			sheet = 1, col_names=F, col_types='text')
	y1993 = data.frame(y1993[c(-1:-5),],stringsAsFactors=F)
	colnames(y1993) = c('城市名称','admin1993','非农业人口','城市名称','admin1993','非农业人口')
	y1993 = rbind(y1993[,c(1:2)],y1993[,c(4:5)])
	y1993 = Clean(y1993)


	y1 = merge(yfocal, last, by='城市名称', all.x=T)
	y2 = merge(y1, y1993, by='城市名称', all.x=T)

	for (r in 1:dim(y2)[2]){y2[,r][is.na(y2[,r])] = ''}
	y2 = subset(y2, y2[,1]!='' & y2[,2]!='省')
	y2$current = NA

	y2 = y2[,c('城市名称','current','更改年份','别名','last','admin1993')]

	for (r in 1:nrow(y2)){
		if(y2$last[r] %in% c('','县级市')&y2$admin1993[r]=='县级'){
			y2$current[r] = '县级市'
		}else if((y2$last[r]=='地级市')&y2$admin1993[r]=='地级'){
			y2$current[r] = '地级市'
		}else if((y2$last[r]=='直辖市')&y2$admin1993[r]=='直辖'){
			y2$current[r] = '直辖市'
		}else{
			y2$current[r] = '?'
		}
	}
	colnames(y2) = gsub('current', paste0('admin',yeari), colnames(y2))
	return(y2)
}







# 1987 - 1992，手动核查1985和1986年，再利用1993年减小核查工作量，规则见Mule函数
###########################################################################################
###########################################################################################
#中国城市统计年鉴-1987-按市区非农业人口数排列的全部城市名单qnhn.xls done

#中国城市统计年鉴-1988-按市区非农业人口数排列的全部城市名单(二)zjpm.xls done
#中国城市统计年鉴-1988-按市区非农业人口数排列的全部城市名单(一)zjpm.xls done

#中国城市统计年鉴-1989-按市区非农业人口数排列的全部城市名单(二)olhj.xls done
#中国城市统计年鉴-1989-按市区非农业人口数排列的全部城市名单(一)ttnp.xls done

#中国城市统计年鉴-1990-按市区非农业人口数排列的全部城市名单(二)ddts.xls done
#中国城市统计年鉴-1990-按市区非农业人口数排列的全部城市名单(一)ghnv.xls done

#中国城市统计年鉴-1991-表02按市区非农业人口数排列的全部城市名单wdgk.xls done
#中国城市统计年鉴-1991-表01按市区非农业人口数排列的全部城市名单vofo.xls done

#中国城市统计年鉴-1992-表01按市区非农业人口数排列的全部城市名单ezqk.xls done
#中国城市统计年鉴-1992-表02按市区非农业人口数排列的全部城市名单hzwm.xls done

yeari = 1992
a1 = Mule1987(f='中国城市统计年鉴-1992-表01按市区非农业人口数排列的全部城市名单ezqk.xls', yeari=yeari)
a2 = Mule1987(f='中国城市统计年鉴-1992-表02按市区非农业人口数排列的全部城市名单hzwm.xls', yeari=yeari)

a3 = rbind(a1,a2)
a4 = a3[!duplicated(a3[,1]),]

a4[!a4[,1]%in%a1[,1],]
a4[!a4[,1]%in%a2[,1],]

cat(length(unique(a1[,1])), length(unique(a2[,1])), length(unique(a4[,1])))

#write.csv(a4, paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin',yeari,'.csv'), row.names=F)






# 1993 - 1994，按照1993年改动的信息分开1993年和1994年
##########################################################################################################
##########################################################################################################

last = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin1992.csv',header=T,stringsAsFactors=F)
last = Clean(last)[,c(1,3,4)]
	
y1993 = read_excel('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/1993-1994/unlock/中国城市统计年鉴-1993-1994-二.按市区非农业人口排列的全部城市名单(按省，直辖市，自治区及行政级别排列)wkrg.xls', 
		sheet = 1, col_names=F, col_types='text')
y1993 = data.frame(y1993[c(-1:-5),],stringsAsFactors=F)
colnames(y1993) = c('城市名称','admin1993','非农业人口','城市名称','admin1993','非农业人口')
y1993 = rbind(y1993[,c(1:2)],y1993[,c(4:5)])
y1993$admin1993 = paste0(y1993$admin1993, '市')
y1993 = merge(y1993, last, by='城市名称', all.x=T)
y1993 = Clean(y1993)
write.csv(y1993, paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin1993.csv'), row.names=F)
#y1993[!y1993[,1]%in%a4[,1],]
## 手动改1993有变化的城市 





# 1995 - 1999，有行政级别的表格，也有只有城市名单的表格，交叉验证
############################################################################################################
############################################################################################################
#中国城市统计年鉴-1995-2.按市区非农业人口排列的全部城市名单(按省、直辖市、自治区及行政级别排列)ectq.xls
#中国城市统计年鉴-1995-3.按市区非农业人口排列的全部城市名单wmha.xls

#中国城市统计年鉴-1996-3.全部城市名单、行政级别及其市区非农业人口(按省、直辖市、自治区排列)wgur.xls
#中国城市统计年鉴-1996-3．按市区非农业人口排列的全部城市名单cvnw.xls

# yeari = 1997
# f1 = '中国城市统计年鉴-1997-3．按市区非农业人口排列的全部城市名单qkzj.xls'
# f2 = '中国城市统计年鉴-1997-3．按市区非农业人口排列的全部城市名单slme.xls'

# yeari = 1998
# f1 = '中国城市统计年鉴-1998-4．按省、自治区、直辖市及行政级别排列的全部城市名单lzlb.xls'
# f2 = '中国城市统计年鉴-1998-3．按市区非农业人口排序的全部城市名单pvkp.xls'

# yeari = 1999
# f1 = '中国城市统计年鉴-1999-4．按省、自治区、直辖市及行政级别排列的全部城市名单ddxa.xls'
# f2 = '中国城市统计年鉴-2000-2—3按市区非农业人口排序的全部城市名单hqxg.xls'

yeari = 1999
f1 = '中国城市统计年鉴-1999-4．按省、自治区、直辖市及行政级别排列的全部城市名单ddxa.xls'
f2 = '中国城市统计年鉴-2000-2—3按市区非农业人口排序的全部城市名单hqxg.xls'


a1 = Mule1995a14(yeari = yeari, f = f1)
a2 = Mule1995b135(yeari = yeari, f = f2)
a4 = merge(a1, a2, by='城市名称', all=T)

a4[!a4[,1]%in%a1[,1],]
a4[!a4[,1]%in%a2[,1],]

cat(length(unique(a1[,1])), length(unique(a2[,1])), length(unique(a4[,1])))

write.csv(a4, paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin',yeari,'.csv'), row.names=F)





# 2000 - 2004，有分列列出的行政级别表格
###########################################################################################################
###########################################################################################################


# 2000, 2004, 行政级别在非农业人口之前

# yeari = 2000
# f = '中国城市统计年鉴-2000-2—3按市区非农业人口排序的全部城市名单hqxg.xls'

# yeari = 2004
# f = '中国城市统计年鉴-2004-2—3按市辖区人口排列的城市名单zjuf.xls'


yeari = 2004
f = '中国城市统计年鉴-2004-2—3按市辖区人口排列的城市名单zjuf.xls'

setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/unlock/'))

y1993 = data.frame(read_excel(f, sheet = 1, col_names=F, col_types='text'),stringsAsFactors=F)
colnames(y1993) = rep(c('城市名称',paste0('admin',yeari),'非农业人口'), 3)
y1993 = rbind(y1993[,c(1:2)],y1993[,c(4:5)],y1993[,c(7:8)])
if(sum(grepl('市',y1993[,2])) < 20){y1993[,2] = paste0(y1993[,2], '市')}
y1993 = Clean(y1993)

write.csv(y1993, paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin',yeari,'.csv'), row.names=F)


# 2001 - 2003, 行政级别在非农业人口之后

# yeari = 2001
# f = '中国城市统计年鉴-2001-2-3按市区非农业人口排列的全部城市名单eeoz.xls'

# yeari = 2002
# f = '中国城市统计年鉴-2002-2—3按市区非农业人口排列的全部城市名单knjm.xls'

# yeari = 2003
# f = '中国城市统计年鉴-2003-2—3按市区非农业人口排列的全部城市名单srss.xls'

yeari = 2003
f = '中国城市统计年鉴-2003-2—3按市区非农业人口排列的全部城市名单srss.xls'

setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/unlock/'))

y1993 = data.frame(read_excel(f, sheet = 1, col_names=F, col_types='text'),stringsAsFactors=F)
colnames(y1993) = rep(c('城市名称','非农业人口',paste0('admin',yeari)), 3)
y1993 = rbind(y1993[,c(1,3)],y1993[,c(4,6)],y1993[,c(7,9)])
if(sum(grepl('市',y1993[,2])) < 20){y1993[,2] = paste0(y1993[,2], '市')}
y1993 = Clean(y1993)

write.csv(y1993, paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin',yeari,'.csv'), row.names=F)




# 2005 - 2014，有行政级别的树状表
###########################################################################################################
###########################################################################################################

yeari = 2005
f = 'super-中国城市统计年鉴-2005-1—32004年城市辖市情况一览表hpcl.csv'

yeari = 2006
f = 'super-中国城市统计年鉴-2006-1-3城市辖市情况一览表zdsm.csv'

yeari = 2007
f = 'super-中国城市统计年鉴-2007-1—3城市辖市情况一览表tvin.csv'

yeari = 2009
f = 'super-中国城市统计年鉴-2009-1-2城市辖市情况一览表nmre.csv'

yeari = 2010
f = 'super-中国城市统计年鉴-2010-1-2分地区城市情况一览表kcrq.csv'

cozy = read.csv(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCal/',f),header=T,stringsAsFactors=F)
cozy1 = Clean2005(cozy)
colnames(cozy1) = NA
zcity = data.frame(cityn=rbind(cozy1[1],cozy1[4]), admin='直辖市',stringsAsFactors=F)
zcity = subset(zcity, zcity!='吉林')
dcity = data.frame(cityn=rbind(cozy1[2],cozy1[5]), admin='地级市',stringsAsFactors=F)
xcity = data.frame(cityn=rbind(cozy1[3],cozy1[6]), admin='县级市',stringsAsFactors=F)
allcity = rbind(zcity,dcity,xcity)
allcity = Clean(allcity)
allcity[,1] = gsub('市\\b','',allcity[,1])
colnames(allcity) = c('城市名称', paste0('admin',yeari))
write.csv(allcity, paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin',yeari,'.csv'), row.names=F)

yeari = 2014
f = 'super-中国城市统计年鉴-2014-1-2分地区城市情况一览表nkqi.csv'

cozy = read.csv(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCal/',f),header=T,stringsAsFactors=F)
cozy1 = Clean2005(cozy)
colnames(cozy1) = NA
zcity = data.frame(cityn=cozy1[1], admin='直辖市',stringsAsFactors=F)
zcity = subset(zcity, zcity!='吉林')
dcity = data.frame(cityn=cozy1[2], admin='地级市',stringsAsFactors=F)
xcity = data.frame(cityn=cozy1[3], admin='县级市',stringsAsFactors=F)
allcity = rbind(zcity,dcity,xcity)
allcity = Clean(allcity)
allcity[,1] = gsub('市\\b','',allcity[,1])
colnames(allcity) = c('城市名称', paste0('admin',yeari))
write.csv(allcity, paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin',yeari,'.csv'), row.names=F)





######## 其他，没有专门展示行政级别的表格，但分开统计了地级市和县级市

load("C:\\Sync\\CoolGirl\\Fhe\\ecosocialData\\SuperData\\SuperDataAll.Rdata")

x1 = df[grep('人口',df$index),]
a1 = unique(df$index)
a2 = grep('人口',a1, value=T)
a3 = grep('平均|变动|覆盖|密度|行业|名单|面积|行政|居住|煤气|率|情况|比重|市辖区|非农业|总户数', a2, invert=T, value=T)
d1 = na.omit(subset(df, df$index %in% a3))

for (yeari in c(2008, 2011:2013, 2015:2018)){
	
	d2 = subset(d1, d1$year==yeari)
	inn = unique(d2$index)
	print(c(yeari,length(inn)))
	
	city1 = d2[d2$index==inn[1],'city']
	city2 = d2[d2$index==inn[2],'city']
	if ((sum(grepl('北京|上海', city1))==0) & (sum(grepl('辛集|临夏', city1))>0)){
		xcity = city1
		dcity = city2
    }else{
		xcity = city2
		dcity = city1
	}

	here = data.frame(
		'城市名称' = c(dcity, xcity),
		'admin' = c(rep('地级市', length(dcity)), rep('县级市', length(xcity))),
		stringsAsFactors=F
	)
	here = unique(Clean(here))
	colnames(here)[2] = paste0('admin',yeari)
	write.csv(here, paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin',yeari,'.csv'), row.names=F)
}



### 最后一起清洗，合并出所有年份的城市名单
adminallyear = data.frame()
for (yeari in 1985:2018){
	admindf = read.csv(file=paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/admin',yeari,'.csv'),header=T,stringsAsFactors = FALSE)
	adminyi = unique(Clean(admindf))
	write.csv(adminyi, paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/admin',yeari,'.csv'), row.names=F)
	
	for (r in 1:ncol(adminyi)){
		adminyi[!adminyi[,r]%in%c('津市','沙市'),r] = gsub('市\\b','',adminyi[!adminyi[,r]%in%c('津市','沙市'),r])
		allcityn = paste0(adminyi[,1],'市')
	}
	adminyinew = cbind(data.frame(allcityn,stringsAsFactors=F), adminyi[,c(1,2)])
	adminyinew$yeari = yeari
	colnames(adminyinew) = c('cityshi','city','admin','yeari')
	adminyinew = subset(adminyinew, !grepl('\\b省\\b',adminyinew$admin))
	adminyinew = subset(adminyinew, !grepl('吉林市', adminyinew$cityshi))
	adminyinew = rbind(adminyinew, data.frame(cityshi='吉林市',city='吉林',admin='地级',yeari=yeari))
	if (yeari > 2012 & !'三沙市' %in% adminyinew$cityshi) {adminyinew = rbind(adminyinew, data.frame(cityshi='三沙市',city='三沙',admin='地级',yeari=yeari))}
	adminyinew = subset(adminyinew, adminyinew$admin!='')
	adminyinew = unique(adminyinew)
	adminallyear = rbind(adminallyear, adminyinew)
	write.csv(adminyinew, paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminshi',yeari,'.csv'), row.names=F)
	
	print(c(yeari, nrow(admindf), nrow(adminyi)))
}
write.csv(adminallyear, 'C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminallyear.csv', row.names=F)


# 检查用
# dxjs = adminallyear[grep('县级',adminallyear$admin),]
# a = table(adminallyear[grep('临夏',adminallyear$cityshi),'yeari'])
# b = table(dxjs[grep('临夏',dxjs$cityshi),'yeari'])
# names(a)[!names(a) %in% names(b)]

# ddjs = adminallyear[grep('县级',adminallyear$admin,invert=T),]
# a = table(adminallyear[grep('北京',adminallyear$cityshi),'yeari'])
# b = table(ddjs[grep('北京',ddjs$cityshi),'yeari'])
# names(a)[!names(a) %in% names(b)]



#### 把应该连续的年份补成连续，方便筛掉插补之后多出来的数据
adminallyear = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminallyear.csv',header=T,stringsAsFactors=F)
ad = adminallyear
ad$cityshi = as.numeric(as.factor(ad$cityshi))
sadm = split(ad,ad$cityshi)

m = c()
ok = c()
for (n in 1:length(sadm)){
	lb = sort(sadm[[n]]$year)
	fy = lb[1]:(lb[1]+length(lb)-1)
	if(sum(lb == fy) != length(lb)){
		m = c(m, n)
	}else{ok = c(ok,n)}
}

for (mi in m){
	print(unique(sadm[[mi]]$city))
}
#4   5 130 139 213 220 286 309 443 470 487 523 553 556 565 566 596 597 618 619 647
#5 131 140 214 221 287 310 444 471 488 524 554 557 566 567 597 598 619 620 648

#sadm[[470]]

#1987年设立扶余市-县级
#1992年改松原市扶余区，1995年新设立扶余县，原扶余区更名宁江区
#2013年扶余县设立扶余市-县级
#结论：1987-1992的扶余市，和2013年后的扶余市，不是一个东西

#1969年抚州市-县级市,
#1987年8月抚州市与临川县合并为临川市-县级，但到1995年2月才正式合并为临川市。
#2000年撤销抚州地区和县级临川市，设立地级抚州市，原临川市改临川区
#结论：2000年后的抚州市更大

#del1 = adminallyear$cityshi=='扶余市' & adminallyear$year %in% 1985:2013
#del2 = adminallyear$cityshi=='抚州市' & adminallyear$year %in% 1985:2000

del3 = adminallyear$cityshi=='沙市市' & adminallyear$year %in% 1997
del4 = adminallyear$cityshi=='思茅市' & adminallyear$year %in% 2008:2018
del5 = adminallyear$cityshi=='通什市' & adminallyear$year %in% 2002:2018
del6 = adminallyear$cityshi=='襄樊市' & adminallyear$year %in% 2012

add14 = data.frame(cityshi=rep('阿拉尔市',1),city=rep('阿拉尔',1),admin=rep('县级',1),yeari=2004)
add15 = data.frame(cityshi=rep('阿拉山口市',2),city=rep('阿拉山口',2),admin=rep('县级',2),yeari=c(2015:2016))
add1 = data.frame(cityshi=rep('呼伦贝尔市',2),city=rep('呼伦贝尔',2),admin=rep('地级',2),yeari=2003:2004)
add2 = data.frame(cityshi=rep('华蓥市',4),city=rep('华蓥',4),admin=rep('县级',4),yeari=2001:2004)
add3 = data.frame(cityshi='荆州市',city='荆州',admin='地级',yeari=1997)
add4 = data.frame(cityshi=rep('拉萨市',3),city=rep('拉萨',3),admin=rep('地级',3),yeari=c(1999,2004,2011))
add5 = data.frame(cityshi=rep('普洱市',4),city=rep('普洱',4),admin=rep('地级',4),yeari=c(2008,2009,2011,2012))
add6 = data.frame(cityshi=rep('日喀则市',2),city=rep('日喀则',2),admin=rep('县级',2),yeari=2015:2016)
add7 = data.frame(cityshi=rep('铁门关市',2),city=rep('铁门关',2),admin=rep('县级',2),yeari=c(2015:2016))
add8 = data.frame(cityshi=rep('图木舒克市',1),city=rep('图木舒克',1),admin=rep('县级',1),yeari=2004)
add9 = data.frame(cityshi=rep('吐鲁番市',1),city=rep('吐鲁番',1),admin=rep('地级',1),yeari=2016)
add10 = data.frame(cityshi=rep('五家渠市',1),city=rep('五家渠',1),admin=rep('县级',1),yeari=2004)
add11 = data.frame(cityshi=rep('五指山市',1),city=rep('五指山',1),admin=rep('县级',1),yeari=2004)
add12 = data.frame(cityshi=rep('襄阳市',1),city=rep('襄阳',1),admin=rep('地级',1),yeari=2012)
add13 = data.frame(cityshi=rep('宣城市',1),city=rep('宣城',1),admin=rep('地级',1),yeari=2006)

adminallyear = adminallyear[!(del3|del4|del5|del6),]
adminallyear = rbind(adminallyear,add1,add2,add3,add4,add5,add6,add7,add8,add9,add10,add11,add12,add13,add14,add15)

adminallyear = unique(adminallyear)

# ad = adminallyear
# ad$cityshi = as.numeric(as.factor(ad$cityshi))
# sadm = split(ad,ad$cityshi)

# m = c()
# ok = c()
# for (n in 1:length(sadm)){
	# lb = sort(sadm[[n]]$year)
	# fy = lb[1]:(lb[1]+length(lb)-1)
	# if(sum(lb == fy) != length(lb)){
		# m = c(m, n)
	# }else{ok = c(ok,n)}
# }

write.csv(adminallyear, 'C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminallyear.csv', row.names=F)



### 改名版
adminallyear = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminallyear.csv',header=T,stringsAsFactors=F)
OCR1 = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/OCRerror.csv',header=T,stringsAsFactors = FALSE)
changeOCR = OCR1[OCR1[,3]!='',c(3,4)]

adminchange = na.omit(adminallyear)

for (g in 1:nrow(changeOCR)){adminchange$cityshi = gsub(changeOCR[g,1],changeOCR[g,2],adminchange$cityshi)}
a = paste0(adminchange$cityshi, adminchange$yeari)
adminchange = adminchange[!duplicated(a),]
write.csv(adminchange, 'C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminchange.csv', row.names=F)
