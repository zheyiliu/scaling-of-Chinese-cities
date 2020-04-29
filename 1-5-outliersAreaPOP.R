
#home = '/home/zheyi'
home = 'C:/Sync/CoolGirl/Fhe'
setwd(paste0(home,'/ecosocialData/indexSQL'))
for (rdat in dir()){load(rdat)}

citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)
citypre = subset(citylist, citylist$Administrative_level != 'county')$City_ch
citycoun = subset(citylist, citylist$Administrative_level == 'county')$City_ch
cityqu = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/ToDistrict.csv',stringsAsFactors=F)[,1]
mass = c('内蒙市', '胡南省')


dflist0 = gsub('.Rdata', '', dir(paste0(home,'/ecosocialData/indexSQL')))
for (yi in 1:length(dflist0)){
  dfname = dflist0[yi]
  df = get(dfname)
  df$year = as.integer(df$year)
  df$value = as.numeric(df$value)
  df$value[which(df$value==0)] = NA
  
  citydf = unique(df$city)
  citydel = citydf[!citydf %in% citypre & citydf %in% c(citycoun,cityqu,mass)]
  df = subset(df, !df$city %in% citydel)
  
  assign(dfname, df)
  eval(parse(text = paste0("save(",dfname, ",file='",dfname,".Rdata')")))
  print(yi)
}


# dflist = gsub('.Rdata', '', dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL'))
# 
# 
# citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)
# citypre = subset(citylist, citylist$Administrative_level != 'county')$City_ch
# n = 15
# par(mfrow=c(4,5))
# for (cityi in citypre[(n*20-19):(n*20)]){
#   dat = dfi[which(dfi$city == cityi & grepl(rangeStat,dfi$index)),]
#   dat = na.omit(dat)
#   if(dim(dat)[1]>0){
#     plot(as.numeric(dat$year)-1, dat$value, main=cityi,xlab='year',ylab=dfname,pch=1, xlim=c(1985,2017))
#   } else {
#     plot(1:5,1:5,main=cityi,xlab='year',ylab=dfname,cex=0)
#   }
# }


Outliers = function(cityi0, yeari0, valuei0){
  dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index) & dfi$year %in% yeari0),]$value = valuei0
  dfi
}

###############################################################
###############################################################
###############################################################

### Area has been done!

rangeStat = '市辖区'
dfname = 'Area'
dfi = get(dfname)
dfi = dfi[which(grepl('市',dfi$city)),]

dfi = Outliers('巴中市', 2014, 2560)
dfi = Outliers('白城市', 1989, 978)
dfi = Outliers('白山市', 2001, 1373)
dfi = Outliers('包头市', 1992, 2153)
dfi = Outliers('本溪市', 1992, 1308)
dfi = Outliers('本溪市', 1994, 1308)
dfi = Outliers('北京市', 1994, 4568)

dfi = Outliers('朝阳市', 1992, 555)
dfi = Outliers('成都市', 1996, 1418)
dfi = Outliers('承德市', 2007, 708)
dfi = Outliers('承德市', 2008, 708)
dfi = Outliers('承德市', 2009, 708)
dfi = Outliers('赤峰市', 1992, 7017)
dfi = Outliers('崇左市', 2005, 2951)
dfi = Outliers('大连市', 1992, 2415)
dfi = Outliers('丹东市', 1992, 526)
dfi = Outliers('防城港市', 2007, 2822)
dfi = Outliers('抚顺市', 1992, 675)
dfi = Outliers('阜新市', 1992, 448)
dfi = Outliers('阜阳市', 1997, 1796)
dfi = Outliers('阜阳市', 1998, 1796)

dfi = Outliers('广安市', 2009, 1536)
dfi = Outliers('合肥市', 2012, 839)
dfi = Outliers('鹤岗市', 1989, 4551)

dfi = Outliers('呼和浩特市', 1992, 2054)
dfi = Outliers('呼伦贝尔市', 1992, 1440)
dfi = Outliers('葫芦岛市', 1992, 2271)
dfi = Outliers('淮安市', 2010, 3171)
dfi = Outliers('淮安市', 1989, 347)
dfi = Outliers('淮安市', 1995, 347)
dfi = Outliers('黄冈市', 2012, 362)
dfi = Outliers('黄石市', 1987, 179)
dfi = Outliers('佳木斯市', 2008, 1875)

dfi = Outliers('金昌市', 1991, 2080)
dfi = Outliers('金昌市', 1992, 2080)
dfi = Outliers('克拉玛依市', 1997, 9500)
dfi = Outliers('金昌市', 1991, 2080)
dfi = Outliers('荆州市', 1990, 692)

dfi = Outliers('辽阳市', 1992, 560)

dfi = Outliers('上海市', 2006, 5299)
dfi = Outliers('金昌市', 1991, 2080)
dfi = Outliers('汕尾市', 1999, 401)
dfi = Outliers('汕尾市', 2002, 432)
dfi = Outliers('汕尾市', 2005, 432)
dfi = Outliers('汕尾市', 2006, 432)
dfi = Outliers('韶关市', 2007, 2870)

dfi = Outliers('邵阳市', 2006, 436)
dfi = Outliers('邵阳市', 2007, 436)
dfi = Outliers('沈阳市', 1992, 3495)
dfi = Outliers('双鸭山市', 1990, 1614)
dfi = Outliers('遂宁市', 1996, 1849)
dfi = Outliers('铁岭市', 1992, 307)
dfi = Outliers('通辽市', 1992, 3518)

dfi = Outliers('新余市', 1998, 1776)
dfi = Outliers('新余市', 1999, 1776)
dfi = Outliers('新余市', 2000, 1776)
dfi = Outliers('阳江市', 1989, 2488)

dfi = Outliers('营口市', 1992, 648)
dfi = Outliers('云浮市', 2009, 762)

dfi = Outliers('中卫市', 2010, 6876)
dfi = Outliers('新余市', 1998, 1776)

dfi = Outliers('滁州市', 1994, 1399)
dfi = Outliers('滁州市', 1995, 1399)

dfi = Outliers('随州市', 1989, 6989)
dfi = Outliers('亳州市', 1989, 2226)
dfi = Outliers('池州市', 2016, 2532)

cityi0 = '咸宁市'
dat0 = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
dat0[order(dat0$year),2:5]

for (n in 1:ncol(dfi)){dfi[,n] = enc2utf8(as.character(dfi[,n]))}

assign(dfname, dfi)
eval(parse(text=paste0('save(',dfname,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/",dfname,".Rdata')")))




###############################################################
###############################################################
###############################################################

### POP has been done!

rangeStat = '市辖区'
dfname = 'POP'
dfi = get(dfname)
dfi = dfi[which(grepl('市',dfi$city)),]

dfi = Outliers('白城市', 1991, 33)
dfi = Outliers('东莞市', 2006, 164)
dfi = Outliers('淮安市', 1995, 46.5)
dfi = Outliers('丽水市', 2012, 39)
dfi = Outliers('随州市', 1989, 135)
dfi = Outliers('湛江市', 1988, 148)
dfi = Outliers('益阳市', 2002, 126.67)
dfi = Outliers('百色市', 2012, 34.2)
dfi = Outliers('湛江市', 1988, 96.8)
dfi = Outliers('湛江市', 2009, 148.16)
dfi = Outliers('龙岩市', 2011, 48.03)
dfi = Outliers('宿州市', 2003, 170.1)
dfi = Outliers('宿州市', 2004, 171.8)


cityi0 = '宿州市'
dat0 = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
dat0[order(dat0$year),2:5]

for (n in 1:ncol(dfi)){dfi[,n] = enc2utf8(as.character(dfi[,n]))}

assign(dfname, dfi)
eval(parse(text=paste0('save(',dfname,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/",dfname,".Rdata')")))


###############################################################
###############################################################
###############################################################

### GDP doesn't need to.


###############################################################
###############################################################
###############################################################

### City Road Area has been done!

rangeStat = '市辖区'
dfname = 'CityRoadArea'
dfi = get(dfname)
dfi = dfi[which(grepl('市',dfi$city)),]

dfi = Outliers('衢州市', 1998, 147)
dfi = Outliers('宁波市', 2003, NA)
dfi = Outliers('丽江市', 2007:2009, NA)
dfi = Outliers('拉萨市', 2014, 372)
dfi = Outliers('乐山市', 1987, NA)
dfi = Outliers('内江市', 1994, 70)
dfi = Outliers('遂宁市', 1994, 50)
dfi = Outliers('雅安市', c(1994:1997,2004), NA)
dfi = Outliers('长治市', 2014, 581)
dfi = Outliers('聊城市', 2005, 1306)
dfi = Outliers('宝鸡市', 2011, 1241)
dfi = Outliers('安康市', 2007, 400)
dfi = Outliers('商洛市', 2017, 187)
dfi = Outliers('商洛市', 2007, 135)
dfi = Outliers('石嘴山市', 2008, 603)
dfi = Outliers('石嘴山市', 2009, 614)
dfi = Outliers('石嘴山市', 2010, 614)
dfi = Outliers('石嘴山市', 2011, 664)
dfi = Outliers('沈阳市', 2017, 9129)
dfi = Outliers('鞍山市', 2001, 726)
dfi = Outliers('鞍山市', 2001, 838)
dfi = Outliers('通化市', c(1999,2015), NA)
dfi = Outliers('四平市', 2015, NA)
dfi = Outliers('吉林市', 2015, 1460)
dfi = Outliers('白山市', 2015, 411)
dfi = Outliers('乌兰察布市', 2013, NA)
dfi = Outliers('常德市', 2017, NA)
dfi = Outliers('衡阳市', 2012, NA)
dfi = Outliers('怀化市', 2004, 200)
dfi = Outliers('郑州市', 1988, NA)
dfi = Outliers('信阳市', 2014, NA)
dfi = Outliers('鹤壁市', 2013, NA)
dfi = Outliers('齐齐哈尔市', 1989, 350)
dfi = Outliers('齐齐哈尔市', 1994:1995, 388)
dfi = Outliers('齐齐哈尔市', 1994:1995, 388)
dfi = Outliers('双鸭山市', 2013, 358)
dfi = Outliers('承德市', 1994, 95)
dfi = Outliers('承德市', 2014, 720)
dfi = Outliers('海口市', 2011:2013, 1350)
dfi = Outliers('遵义市', 2008:2009, 385)
dfi = Outliers('南宁市',2010, 2800)
dfi = Outliers('桂林市', 2001, 400)
dfi = Outliers('北海市', 2007:2008, 600)
dfi = Outliers('百色市', 2014, 429)
dfi = Outliers('贺州市', 2005, 235)
dfi = Outliers('钦州市', 2009, 608)
dfi = Outliers('梧州市', 2004, 200)
dfi = Outliers('梧州市', 2008, 271)
dfi = Outliers('深圳市', 2007, 7700)
dfi = Outliers('佛山市', 2003, 1619)
dfi = Outliers('潮州市', 2002, 199)
dfi = Outliers('揭阳市', 2016, 700)
dfi = Outliers('湛江市', 2013, NA)
dfi = Outliers('河源市', 2001, 128)
dfi = Outliers('梅州市', 1997, 258)
dfi = Outliers('梅州市', 1995, 200)
dfi = Outliers('韶关市', 2003, 425)
dfi = Outliers('金昌市', 1996, 102.418)
dfi = Outliers('陇南市', 2008, 41)
dfi = Outliers('平凉市', 2016, 641)
dfi = Outliers('天水市', 2007, NA)
dfi = Outliers('南平市', c(1987,1990), 38)
dfi = Outliers('亳州市', 2017, NA)
dfi = Outliers('马鞍山市', 2017, NA)
dfi = Outliers('宿州市', 2002, 360)
dfi = Outliers('宿州市', 2011, 900)
dfi = Outliers('宣城市', 2016, 1030)
dfi = Outliers('舟山市', 1990, 120)
dfi = Outliers('丽水市', 1987:1990, NA)

cityi0 = '菏泽市'
dat0 = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
dat0[order(dat0$year),2:5]

for (n in 1:ncol(dfi)){dfi[,n] = enc2utf8(as.character(dfi[,n]))}

assign(dfname, dfi)
eval(parse(text=paste0('save(',dfname,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/",dfname,".Rdata')")))


###############################################################
###############################################################
###############################################################

### 随便搞了一下医院床位，Hospital和School都太乱，不弄了

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

rangeStat = '市辖区'
dfname = 'HospitalBerth'
dfi = get(dfname)
dfi = dfi[which(grepl('市',dfi$city)),]

dfi = Outliers('黑河市', 1995, 1106)
dfi = Outliers('百色市', 2009, 2702)
dfi = Outliers('廊坊市', 2006, 2279)
dfi = Outliers('秦皇岛市', c(1987,1992), NA)
dfi = Outliers('玉林市', 1999, 2771)

cityi0 = '玉林市'
dat0 = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
dat0[order(dat0$year),2:5]

for (n in 1:ncol(dfi)){dfi[,n] = enc2utf8(as.character(dfi[,n]))}

assign(dfname, dfi)
eval(parse(text=paste0('save(',dfname,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/",dfname,".Rdata')")))



###############################################################
###############################################################
###############################################################

### 随便搞了绿地面积 Green

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

rangeStat = '市辖区'
dfname = 'Green'
dfi = get(dfname)
dfi = dfi[which(grepl('市',dfi$city)),]

dfi = Outliers('玉溪市', 2017, NA)
dfi = Outliers('齐齐哈尔市', c(2008:2009), NA)
dfi = Outliers('珠海市', c(1996:2000), NA)
dfi = Outliers('汕尾市', 1999, NA)
dfi = Outliers('韶关市', 2009, NA)
dfi = Outliers('云浮市', 2015, NA)
dfi = Outliers('湛江市', c(2006,2010), NA)
dfi = Outliers('河源市', 2010, NA)
dfi = Outliers('重庆市', c(2009:2010), NA)
dfi = Outliers('日照市', 2014, NA)
dfi = Outliers('咸阳市', 2012, 2169)
dfi = Outliers('长春市', 1985:1987, NA)
dfi = Outliers('白山市', 2007, 530)
dfi = Outliers('白城市', 2006, 1000)
dfi = Outliers('娄底市', 2006, 1600)
dfi = Outliers('白山市', 2007, 530)
dfi = Outliers('呼和浩特市', 2010:2012, 6000)
dfi = Outliers('秦皇岛市', 2015, 5700)
dfi = Outliers('北海市', 2015, 2454)
dfi = Outliers('河池市', 2004, 453)
dfi = Outliers('梧州市', 2002, 1838)

cityi0 = '梧州市'
dat0 = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
dat0[order(dat0$year),2:5]

for (n in 1:ncol(dfi)){dfi[,n] = enc2utf8(as.character(dfi[,n]))}

assign(dfname, dfi)
eval(parse(text=paste0('save(',dfname,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/",dfname,".Rdata')")))


###############################################################
###############################################################
###############################################################

### Salary

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

rangeStat = '市辖区'
dfname = 'Salary'
dfi = get(dfname)
dfi = dfi[which(grepl('市',dfi$city)),]

dfi = Outliers('临沧市', 2013, 120850)
dfi = Outliers('朔州市', 2017, 573502)
dfi = Outliers('潍坊市', 2015, 1866244)
dfi = Outliers('汉中市', 2006, 93137)
dfi = Outliers('延安市', 2014, 416024)
dfi = Outliers('永州市', 2016, 437750)
dfi = Outliers('永州市', 2001, 58693)
dfi = Outliers('张家界市', 2009, 84421)
dfi = Outliers('黄冈市', 2004, 42794)
dfi = Outliers('黑河市', 1996, 19160)
dfi = Outliers('衡水市', 2016, 563801)
dfi = Outliers('遵义市', 2016, 828620)
dfi = Outliers('北海市', 2011, 247250)
dfi = Outliers('钦州市', 2011, 257965)
dfi = Outliers('揭阳市', 2008, 94746)
dfi = Outliers('天水市', 2007, 153749)
dfi = Outliers('龙岩市', 2015, 663588)
dfi = Outliers('漳州市', 2014, 680310)
dfi = Outliers('马鞍山市', 2014, 836019)
dfi = Outliers('六安市', 2011, 253444)
dfi = Outliers('宿州市', 2010:2013, NA)

cityi0 = '宿州市'
dat0 = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
dat0[order(dat0$year),2:5]

for (n in 1:ncol(dfi)){dfi[,n] = enc2utf8(as.character(dfi[,n]))}

assign(dfname, dfi)
eval(parse(text=paste0('save(',dfname,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/",dfname,".Rdata')")))


###############################################################
###############################################################
###############################################################

### DepositHousehold 年末居民存款

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}

rangeStat = '市辖区'
dfname = 'DepositHousehold'
dfi = get(dfname)
dfi = dfi[which(grepl('市',dfi$city)),]

iddelf = vector()
f= dfi[duplicated(dfi[,c(-1,-5)]),]
for (i in 1:dim(f)[1]){
  ff = subset(dfi,dfi$city==f$city[i] & dfi$year==f$year[i] & dfi$index==f$index[i])
  if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
  }else{iddelf = c(iddelf, ff[order(ff$value),]$id[2])}
}
dfi = dfi[which(!dfi$id %in% iddelf),]

dfi = Outliers('宁波市', 2009, 14096303)
dfi = Outliers('昆明市', 2010, 14109469)
dfi = Outliers('昆明市', 2012, 21831247)
#dfi = Outliers('朝阳市', 2012, NA)
dfi = Outliers('通化市', 2007, 816240)
dfi = Outliers('荆门市', 2009:2010, NA)
dfi = Outliers('十堰市', 2016:2017, NA)
dfi = Outliers('黑河市', 2005, 282790)
dfi = Outliers('南宁市', 2009, 7286542)
dfi = Outliers('南宁市', 2012, 12441640)
dfi = Outliers('钦州市', 2012, 1794110)
dfi = Outliers('汕尾市', 2002, 184582)
#dfi = Outliers('阳江市', 2010, 1501446)
dfi = Outliers('阳江市', 2013, 2029313)
dfi = Outliers('河源市', 1999, 270188)

cityi0 = '河源市'
dat0 = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
dat0[order(dat0$year),2:5]

for (n in 1:ncol(dfi)){dfi[,n] = enc2utf8(as.character(dfi[,n]))}

assign(dfname, dfi)
eval(parse(text=paste0('save(',dfname,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/",dfname,".Rdata')")))


###############################################################
###############################################################
###############################################################

### Electricity 全年用电量

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL')
for (rdat in dir()){load(rdat)}
dflist = gsub('.Rdata', '', dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL'))

rangeStat = '市辖区'
dfname = 'Electricity'
dfi = get(dfname)
dfi = dfi[which(grepl('市',dfi$city)),]

iddelf = vector()
f= dfi[duplicated(dfi[,c(-1,-5)]),]
for (i in 1:dim(f)[1]){
  ff = subset(dfi,dfi$city==f$city[i] & dfi$year==f$year[i] & dfi$index==f$index[i])
  if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
  }else{iddelf = c(iddelf, ff[order(ff$value),]$id[2])}
}
dfi = dfi[which(!dfi$id %in% iddelf),]

dfi = Outliers('吐鲁番市', 1988, 4890)
dfi = Outliers('天津市', 1992, 1148200)
dfi = Outliers('绵阳市', 2004, 200000)
dfi = Outliers('广元市', 1998, 90463)
dfi = Outliers('太原市', 1990, 462100)
dfi = Outliers('榆林市', 2015, 446080)
dfi = Outliers('榆林市', 2009, NA)
dfi = Outliers('长春市', 2004:2005, NA)
dfi = Outliers('松原市', 1995, NA)
dfi = Outliers('呼和浩特市', 2011:2012, NA)
dfi = Outliers('鄂尔多斯市', 2011:2012, NA)
dfi = Outliers('乌兰察布市', 2016, 153350)
dfi = Outliers('益阳市', 1994, 29540)
dfi = Outliers('嘉峪关市', 2016, NA)
dfi = Outliers('梧州市', 2012, NA)
dfi = Outliers('滁州市', 2006, 57707)
dfi = Outliers('克拉玛依市', 2005, 224800)
dfi = Outliers('嘉峪关市', 2016, NA)
dfi = Outliers('梧州市', 2012, NA)
dfi = Outliers('滁州市', 2006, 57707)
dfi = Outliers('克拉玛依市', 2005, 224800)
dfi = Outliers('曲靖市', 2010:2012, NA)
dfi = Outliers('朔州市', 2010, NA)
dfi = Outliers('咸宁市', 2011, NA)

cityi0 = '池州市'
dat0 = dfi[which(dfi$city == cityi0 & grepl(rangeStat,dfi$index)),]
dat0[order(dat0$year),2:5]

for (n in 1:ncol(dfi)){dfi[,n] = enc2utf8(as.character(dfi[,n]))}

assign(dfname, dfi)
eval(parse(text=paste0('save(',dfname,",file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/",dfname,".Rdata')")))
