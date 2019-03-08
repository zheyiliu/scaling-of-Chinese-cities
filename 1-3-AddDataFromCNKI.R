### 用于给Rdata添加数据的代码
### 数据来自"ecosocialDATA/原始数据/增添数据/"，包括所有地级市单个年份
root = 'C:/Sync/CoolGirl/Fhe/ecosocialDATA/'
setwd(paste0(root, 'indexSQL'))
for (rdat in dir()){load(rdat)}

Rdataname = 'Area' #需要操作的Rdata
rangeStat = '市辖区'
year = 2007
fname = paste0(year,'-1',rangeStat,Rdataname,'.csv') #需要增添的数据文件
indexname = unique(get(Rdataname)$index)[grep(rangeStat, unique(get(Rdataname)$index))]
### Rdata里面year==1995的数据是1994年的数据，所以其实缺的是1994年的数据。


dat0 = read.csv(paste0(root,'/原始数据/增添数据/',fname), header=F, stringsAsFactors = FALSE)
dat = dat0[c(-1,-2),-2]
colnames(dat) = c('city', 'value')
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

a = table(subset(get(Rdataname), grepl(rangeStat, get(Rdataname)$index))$year)
b = table(subset(df, grepl(rangeStat, df$index))$year)

df$value = as.numeric(df$value)
df = df[which(grepl('市',df$city)),]

assign(Rdataname, df)

eval(parse(text=paste0('save(', Rdataname, ",file='", root, 'indexSQL/',Rdataname,".Rdata')")))
