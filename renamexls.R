### 添加.xls后缀
setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/alldata/')
for (year in dir()){
  setwd(paste('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/alldata/',year,sep=''))
  #setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/alldata/2013/')
  oldname = as.character(dir())
  #newname = gsub('.xls','', oldname)
  newname = paste(oldname, '.xls',sep='')
  file.rename(oldname, newname)
}

### 在VBA中将xls文件转成csv文件

### 利用表头给csv文件重命名
setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/alldata/')
yearlist = dir()
for (year in yearlist){
	setwd(paste('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',year,'/csv',sep=''))
	for (f in dir()){
	tst = read.csv(f, header=F, colClasses='character')
	m=1
	while(sum(tst[m,]!='')==0){
		tst = tst[-m,]
}
newname = paste(tst[1,][which(tst[1,]!='')][1],'.csv',sep='')
newname = gsub(' ','',newname)
newname = paste('中国城市统计年鉴', year, newname,sep='-')
file.rename(f, newname)
}}

### 接下来在主脚本清洗数据、录入数据库

################ 六十年
### 不需要宏的代码
library(gdata)
setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/六十年/')
yearlist = dir()
for (year in yearlist){
	setwd(paste('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/六十年/',year,sep=''))
	for (f in dir()){
	tst = read.xls(f, blank.lines.skip=T, perl="C:/Strawberry/perl/bin/perl.exe",header=F,stringsAsFactors = FALSE, colClasses='character',fileEncoding='UTF-8')
	m=1
	while(sum(tst[m,]!='')==0){ #删除表头空行
		tst = tst[-m,]
}
newname = paste(tst[1,][which(tst[1,]!='')][1],'.csv',sep='')
newname = gsub(' ','',newname)
newname = paste(year, newname,sep='-')
colnames(tst)=NULL
write.csv(tst, newname,row.names = F)
#file.rename(f, newname)
}}
