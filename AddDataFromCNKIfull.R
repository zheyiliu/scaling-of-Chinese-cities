##### 用于给Rdata添加数据的代码
##### 数据来自"ecosocialDATA/知网地级市"
##### 包括所有可用地级市+所有可用年份
##### 仅包含全市数据
############

root = 'C:/Sync/CoolGirl/Fhe/ecosocialDATA/'
setwd(paste0(root, 'indexSQL'))
for (rdat in dir()){load(rdat)}

###
Rdataname = 'POP' #需要操作的Rdata
f = '知网地级市-总人口数.csv' #需要增添的数据文件
fname = paste0(root,'/知网地级市/',f)
indexname = unique(get(Rdataname)$index)[1]
year = 'all'
### Rdata里面year==1995的数据是1994年的数据，所以其实缺的是1994年的数据。



dat0 = read.csv(fname, header=F, stringsAsFactors = FALSE)
indexn = dat0[2,4]
yearrange = gsub('年', '', dat0[1,])[c(-1:-3)]
dat = dat0[c(-1,-2),c(-2,-3)]
colnames(dat) = c('city', yearrange)

plot(1:dim(dat)[2], dat[1,])
dat1 = apply(dat[,-1], MARGIN=2, FUN=as.numeric)
rownames(dat1) = dat$city
### 异常值怎么办？
### 悲惨呀戛然而止的代码
### 方法一：算出每个点i周围几个点的平均值和方差（不包括点i）
### 点i和周围几个点的平均值的差大于3倍的方差，就算做异常值
### 方法二：计算每个点删除后，拟合度的下降率
for (i in dim(dat1)[1]){
	for (j in dim(dat1)[2]){
		if (dat[i,j] != NA){
			ijbar = mean(dat1[i,c(j+1:j+5)])
			
		
######没写完，先写到这儿吧。太花时间了。	
		
centers<-k$centers[k$cluster,]
distances<-sqrt(rowSums(ij-mean(ij))^2)
outlier<-(order(distances,decreasing=TRUE))[1:5]
ij[outlier,]

dat$index = indexname
dat$year = year
let = letters[sample(26,dim(dat)[1],replace=T)]
dat$id = paste(dat$city, dat$year, dat$index, let, sep='-')

df = rbind(get(Rdataname), dat)

########### 接下来是处理重复值
df = subset(df, df$value!='')
ddff = df[duplicated(df[,-1]),]
df = df[!duplicated(df[,-1]),]
dim(df)


# citydf = unique(df$city)
# citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)
# citypre = subset(citylist, citylist$Administrative_level != 'county')$City_ch
# citycoun = subset(citylist, citylist$Administrative_level == 'county')$City_ch
# cityqu = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/ToDistrict.csv',stringsAsFactors=F)[,1]
# citydel = citydf[!citydf %in% citypre & citydf %in% c(citycoun,cityqu)]
# df = subset(df, !df$city %in% citydel)

### 去除同年同城市同指标的不同数值的情况
iddelf = vector()
f= df[duplicated(df[,c(-1,-5)]),]
for (i in 1:dim(f)[1]){
	ff = subset(df,df$city==f$city[i] & df$year==f$year[i] & df$index==f$index[i])
	if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
	}else{iddelf = c(iddelf, ff$id[1])}
}
df = df[which(!df$id %in% iddelf),]

a = table(subset(get(Rdataname), grepl('市辖区', get(Rdataname)$index))$year)
b = table(subset(df, grepl('市辖区', df$index))$year)

assign(Rdataname, df)

POP$value = as.numeric(POP$value)

save(POP, file=paste0(root, 'indexSQL/',Rdataname,'.Rdata'))
