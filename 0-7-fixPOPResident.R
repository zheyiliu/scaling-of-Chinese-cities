load(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperIndex/POPResidentOrigin.Rdata')
df = POPResidentOrigin

whichshi = (!grepl('市\\b', df$city)) | (grepl('\\b(津市)\\b|\\b(沙市)\\b', df$city))
df[whichshi,'city'] = paste0(df[whichshi,'city'], '市')



### 改了之后再检查一次
adminallyear = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminallyear.csv',header=T,stringsAsFactors=F)

delcity = data.frame()
pb <- txtProgressBar(style = 3)
s=Sys.time()
yearii = 1985
while(yearii < 2019){
	adminyi = subset(adminallyear, adminallyear$yeari==yearii)
	dfyi = subset(df, df$year==yearii & !df$city %in% adminyi$cityshi)
	if(nrow(dfyi)!=0){
		delcityi = data.frame(city=table(dfyi$city),yeari=yearii)
		colnames(delcityi)=c('city','num','yeari')
		delcity = rbind(delcity, delcityi)
	}
	setTxtProgressBar(pb, (yearii-1984)/(2018-1984))
	yearii = yearii + 1
}
e=Sys.time()
close(pb)
e-s


#### 针对每年不符合城市名单的城市，该改名的改名，摸不准的异常城市名数量极少，直接去除
sp = split(delcity, delcity$city)
sp

df$city[df$city=='池州市' & df$year %in% c(2000)] = '贵池市'
df$city[df$city=='抚州市' & df$year %in% c(2000)] = '临川市'
df$city[df$city=='晋中市' & df$year %in% c(2000)] = '榆次市'
df$city[df$city=='宣城市' & df$year %in% c(2000)] = '宣州市'

dfs = df[df$city=='眉山市' & df$year %in% c(2000),]

df = subset(df, !df$id %in% dfs$id)



#### 把改了名称的城市，一概全部改到最新名称
OCR1 = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/OCRerror.csv',header=T,stringsAsFactors = FALSE)
changeOCR = OCR1[OCR1[,3]!='',c(3,4)]

pb <- txtProgressBar(style = 3)
s=Sys.time()
for (g in 1:nrow(changeOCR)){
	df$city = gsub(changeOCR[g,1],changeOCR[g,2],df$city)
	setTxtProgressBar(pb, g/nrow(changeOCR))
}
e=Sys.time()
close(pb)
e-s


#load('C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll_DSCchange.Rdata')
### 再次检查一下改名后的城市，和改名后的城市名单，为后面df加入admin行政级别做准备
#adminallyear = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminallyear.csv',header=T,stringsAsFactors=F)
adminchange = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminchange.csv',header=T,stringsAsFactors=F)

delcity = data.frame()
risk = data.frame()
pb <- txtProgressBar(style = 3)
s=Sys.time()
yearii = 1985
while(yearii < 2019){
	adminyi = subset(adminchange, adminchange$yeari==yearii)
	if (sum(duplicated(adminyi$cityshi))>0){risk = rbind(risk, adminyi[duplicated(adminyi$cityshi),])}
	dfyi = subset(df, df$year==yearii & !df$city %in% adminyi$cityshi)
	if(nrow(dfyi)!=0){
		delcityi = data.frame(city=table(dfyi$city),yeari=yearii)
		colnames(delcityi)=c('city','num','yeari')
		delcity = rbind(delcity, delcityi)
	}
	setTxtProgressBar(pb, (yearii-1984)/(2018-1984))
	yearii = yearii + 1
}
e=Sys.time()
close(pb)
e-s




yearrange = 1985:2018
adminchange = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminchange.csv',header=T,stringsAsFactors=F)


df$admin = 0
dfadmin = data.frame()

strangeadmin = data.frame()

for(yearii in yearrange){
	dfyi = subset(df, df$year==yearii)
	adminyi = subset(adminchange, adminchange$yeari==yearii)
	
	#测试用
	#dfyi = dfyi[3000:5000,]
	if (nrow(dfyi)<2){
		next
	}
	a = lapply(1:nrow(dfyi), FUN=function(i){
		adminyi[grepl(paste0('\\b',dfyi$city[i],'\\b'),adminyi$cityshi),'admin']
	})
	deli = c()
	#morei = c()
	for(i in 1:length(a)){ 
		if( length(a[[i]])==0 ) {
			print(dfyi[i,])
			strangeadmin = rbind(strangeadmin, dfyi[i,])
			deli = c(deli, i)
		}else if (length(a[[i]])>1) {
			print(dfyi[i,])
			strangeadmin = rbind(strangeadmin, dfyi[i,])
			a[i] = unlist(a[i])[1]
			#morei = c(morei, i)
		}
	}	
	if (length(deli)>0){dfyi = dfyi[-deli,]}
	
	dfyi$admin = unlist(a)
	dfadmin = rbind(dfadmin, dfyi)
}
	
POPResident = dfadmin


save(POPResident, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperIndex/POPResident.Rdata')

