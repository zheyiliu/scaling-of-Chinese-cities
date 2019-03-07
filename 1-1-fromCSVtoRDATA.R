###清洗表格
#indexclass = '建设用地'
#filename = paste(book, year, indexclass, sep='-')
##如果是存款贷款，一定要复制2016年的‘-new.csv’的表头，直接打开-new
#清洗表头
##客运量，表头加万人二字
##公共财政-市辖区，加市辖区万元
##电话互联网，万户
for (year in 1985:2017){
  setwd(paste('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/中国城市年鉴/',year,sep=''))
  
  df = data.frame()
  save(df, file=paste('C:/Sync/CoolGirl/Fhe/ecosocialDATA/psudoSQLall/psudoSQL',year,'.Rdata',sep=''))
  
  
  fileall = dir()
  fileyear = fileall[grep(as.character(year),fileall)]
  fileyearraw = fileyear[!grepl('new',fileyear)]
  for (f in fileyearraw){
    #f = '中国城市统计年鉴-2004-3—44环境状况(四)：城市绿化(市辖区).csv'
    #f = '中国城市统计年鉴-2000-3—34城市道路与交通(不包括市辖县).csv'
    dat = read.csv(f, header=F, stringsAsFactors = FALSE, colClasses='character')
    #dat = read.csv(paste(filename,'.csv',sep=''), header=F, colClasses='character')
    ### 删除空行和空格
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
  }
  
  
  ###输入假的数据库
  fileall = dir()
  fileyear = fileall[grep(as.character(year),fileall)]
  fileyearnew = fileyear[grepl('new',fileyear)]
  for (f in fileyearnew){
    dat1 = read.csv(f,header=T,stringsAsFactors = FALSE)
    cn = colnames(dat1)
    city = rep(dat1$城市,(dim(dat1)[2]-1))
    #city_en = rep(dat1$City, (dim(dat1)[2]-2)) ###2011没有城市英文
    index = unlist(lapply(cn[-1], function(x){rep(x,dim(dat1)[1])}))
    value = unlist(c(dat1[,-1]))
    names(value)= NULL
    let = letters[sample(26,length(city),replace=T)]
    id = paste(city, year, index, let, sep='-')
    sql = data.frame(id=id, city=city, year=year, index=index, value=value,stringsAsFactors = FALSE)
    load(file=paste('C:/Sync/CoolGirl/Fhe/ecosocialDATA/psudoSQLall/psudoSQL',year,'.Rdata',sep=''))
    df = rbind(df,sql)
    ###输入假的数据库 done!
    #print(length(unique(df$index)))
    #df$index = gsub('[().*]', '', df$index)
    save(df, file=paste('C:/Sync/CoolGirl/Fhe/ecosocialDATA/psudoSQLall/psudoSQL',year,'.Rdata',sep=''))
  }
  
  df[duplicated(df[,-1]),]
  df = df[!duplicated(df[,-1]),]
  dim(df)
  
  ##更改错误信息
  df$city = gsub('乌兰察市市', '乌兰察布市', df$city) 
  df$city = gsub('两安市', '西安市', df$city)
  df$city = gsub('洒泉市', '酒泉市', df$city) 
  df$city = gsub('玉溪市-', '玉溪市', df$city)
  df$city = gsub('白贡市', '自贡市', df$city)
  df$city = gsub('景徳镇市', '景德镇市', df$city)
  df$city = gsub('揄林市', '榆林市', df$city)
  df$city = gsub('·阳泉市', '阳泉市', df$city)
  df$city = gsub('衝州市', '衢州市', df$city)
  df$city = gsub('毫州市', '亳州市', df$city)
  df$city = gsub('张家门市', '张家口市', df$city)
  df$city = gsub('张家n市', '张家口市', df$city)
  df$city = gsub('思茅市', '普洱市', df$city)
  df$city = gsub('襄樊市', '襄阳市', df$city)
  df$city = gsub('荊州市', '荆州市', df$city)
  df$city = gsub('秦阜岛市', '秦皇岛市', df$city)
  df$city = gsub('同原市', '固原市', df$city)
  df$city = gsub('鸡两市', '鸡西市', df$city)
  df$city = gsub('宿辽市', '宿迁市', df$city)
  df$city = gsub('四安市', '西安市', df$city)
  df$city = gsub('周门市', '周口市', df$city)
  df$city = gsub('H照市', '日照市', df$city)
  df$city = gsub('秦皂岛市', '秦皇岛市', df$city)
  df$city = gsub('擖阳市', '揭阳市', df$city)
  df$city = gsub('定四市', '定西市', df$city)
  df$city = gsub('包投市', '包头市', df$city)
  df$city = gsub('三业市', '三亚市', df$city)
  df$city = gsub('营n市', '营口市', df$city)
  df$city = gsub('厦fJ市', '厦门市', df$city)
  df$city = gsub('灭水市', '天水市', df$city)
  df$city = gsub('大连市.', '大连市', df$city)
  df$city = gsub('东芜市', '东莞市', df$city)
  df$city = gsub('赤蜂市', '赤峰市', df$city)
  df$city = gsub('包头市.', '包头市', df$city)
  df$city = gsub('唇山市', '眉山市', df$city)
  df$city = gsub('成宁市', '咸宁市', df$city)
  df$city = gsub('准北市', '淮北市', df$city)
  df$city = gsub('准安市', '淮安市', df$city)
  df$city = gsub('衙州市', '衢州市', df$city)
  df$city = gsub('成阳市', '咸阳市', df$city)
  df$city = gsub('宅鸡市', '宝鸡市', df$city)
  df$city = gsub('广东市', '广州市', df$city)
  df$city = gsub('准南市', '淮南市', df$city)
  df$city = gsub('长冶市', '长治市', df$city)
  df$city = gsub('长样市', '长春市', df$city)
  df$city = gsub('攀枝化市', '攀枝花市', df$city)
  df$city = gsub('沪州市', '泸州市', df$city)
  df$city = gsub('乌再木齐市', '乌鲁木齐市', df$city)
  df$city = gsub('台帅市', '台州市', df$city)
  df$city = gsub('来寅市', '来宾市', df$city)
  df$city = gsub('胡芦岛市', '葫芦岛市', df$city)
  df$city = gsub('褊州市', '福州市', df$city)
  df$city = gsub('廓坊市', '廊坊市', df$city)
  df$city = gsub('宿迂市', '宿迁市', df$city)
  df$city = gsub('鹰渖市', '鹰潭市', df$city)
  df$city = gsub('平顺山市', '平顶山市', df$city)
  df$city = gsub('皂州市', '泉州市', df$city)
  df$city = gsub('宦宾市', '宜宾市', df$city)
  df$city = gsub('奉溪市', '本溪市', df$city)
  df$city = gsub('杨州市', '扬州市', df$city)
  df$city = gsub('演州市', '滨州市', df$city)
  df$city = gsub('半顶山市', '平顶山市', df$city)
  df$city = gsub('剂门市', '荆门市', df$city)
  df$city = gsub('格州市', '梧州市', df$city)
  df$city = gsub('媪州市', '温州市', df$city)
  df$city = gsub('大水市', '天水市', df$city)
  df$city = gsub('柳外市', '柳州市', df$city)
  df$city = gsub('漂河市', '漯河市', df$city)
  df$city = gsub('青林市', '吉林市', df$city)
  df$city = gsub('石家成市', '石家庄市', df$city)
  df$city = gsub('岛海市', '乌海市', df$city)
  df$city = gsub('恩茅市', '普洱市', df$city)
  df$city = gsub('青安市', '吉安市', df$city)
  df$city = gsub('威宁市', '咸宁市', df$city)
  df$city = gsub('海拉尔市', '呼伦贝尔市', df$city)
  df$city = gsub('占安市', '吉安市', df$city)
  df$city = gsub('白水市', '天水市', df$city)
  df$city = gsub('淮阴市', '淮安市', df$city)
  df$city = gsub('克垃玛依市', '克拉玛依市', df$city)
  df$city = gsub('清镇一市', '清镇市', df$city)
  df$city = gsub('荷泽市', '菏泽市', df$city)
  df$city = gsub('荆沙市', '荆州市', df$city)
  df$city = gsub('黄岗市', '黄冈市', df$city)
  df$city = gsub('盘石市', '磐石市', df$city)
  df$city = gsub('扎兰电市', '扎兰屯市', df$city)
  df$city = gsub('井岗山市', '井冈山市', df$city)
  df$city = gsub('幸集市', '辛集市', df$city)
  df$city = gsub('夏门市', '厦门市', df$city)
  df$city = gsub('钢陵市', '铜陵市', df$city)
  df$city = gsub('锦西市', '葫芦岛市', df$city)
  df$city = gsub('鹰谭市', '鹰潭市', df$city)
  df$city = gsub('大庸市', '张家界市', df$city)
  df$city = gsub('张家日市', '张家口市', df$city)
  df$city = gsub('图门市', '图们市', df$city)
  df$city = gsub('泠水江市', '冷水江市', df$city)
  df$city = gsub('汩罗市', '汨罗市', df$city)
  df$city = gsub('油头市', '汕头市', df$city)
  df$city = gsub('福J州市', '福州市', df$city)
  df$city = gsub('华鑫市', '华蓥市', df$city)
  df$city = gsub('水安市', '永安市', df$city)
  df$city = gsub('咸海市', '威海市', df$city)
  df$city = gsub('刑门市', '荆门市', df$city)
  df$city = gsub('吉片市', '吉首市', df$city)
  df$city = gsub('和用市', '和田市', df$city)
  df$city = gsub('愉次市', '榆次市', df$city)
  df$city = gsub('电新市', '阜新市', df$city)
  df$city = gsub('南半市', '南平市', df$city)
  df$city = gsub('满州里市', '满洲里市', df$city)
  df$city = gsub('天永市', '天水市', df$city)
  df$city = gsub('白碱市', '白城市', df$city)
  df$city = gsub('丹江日市', '丹江口市', df$city)
  df$city = gsub('梅河门市', '梅河口市', df$city)
  df$city = gsub('\\b(江口市)', '江门市', df$city)
  df$city = gsub('昌占市', '昌吉市', df$city)
  df$city = gsub('肇队市', '肇庆市', df$city)
  df$city = gsub('仪片市', '仪征市', df$city)
  df$city = gsub('格尔森市', '格尔木市', df$city)
  df$city = gsub('营门市', '营口市', df$city)
  df$city = gsub('丹江门市', '丹江口市', df$city)
  df$city = gsub('源江市', '沅江市', df$city)
  df$city = gsub('尔台市', '东台市', df$city)
  df$city = gsub('株州市', '株洲市', df$city)
  df$city = gsub('\\b(连浩特市)', '二连浩特市', df$city)
  df$city = gsub('刺门市', '荆门市', df$city)
  df$city = gsub('韶天市', '韶关市', df$city)
  df$city = gsub('克托玛依市', '克拉玛依市', df$city)
  df$city = gsub('隹木斯市', '佳木斯市', df$city)
  df$city = gsub('石咀山市', '石嘴山市', df$city)
  df$city = gsub('通江市', '通辽市', df$city)
  df$city = gsub('格尔本市', '格尔木市', df$city)
  df$city = gsub('霍林格勒市', '霍林郭勒市', df$city)
  df$city = gsub('渡口市', '攀枝花市', df$city)
  df$city = gsub('制门市', '荆门市', df$city)
  df$city = gsub('临次市', '临汾市', df$city)
  df$city = gsub('强家口市', '张家口市', df$city)
  df$city = gsub('阜阳巿', '阜阳市', df$city)
  df$city = gsub('东方自治市', '东方市', df$city)
  df$city = gsub('二连市', '二连浩特市', df$city)
  df$city = gsub('益州市', '盖州市', df$city)
  df$city = gsub('百城市', '白城市', df$city)
  df$city = gsub('荣城市', '荣成市', df$city)
  df$city = gsub('门城市', '白城市', df$city)
  df$city = gsub('枝城市', '宜都市', df$city)
  df$index = gsub('年末总人口万人市辖区.1', '年末总人口万人非农业人口市辖区', df$index)
  df$index = gsub('末金融机构存款余额市辖区.1', '年末金融机构存款余额城乡居民储蓄年末余额市辖区', df$index)
  #df$city = gsub('', '', df$city)
  
  #去除在城市列表内的县级市
  citydf = unique(df$city)
  citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)
  citypre = subset(citylist, citylist$Administrative_level != 'county')$City_ch
  citycoun = subset(citylist, citylist$Administrative_level == 'county')$City_ch
  cityqu = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/ToDistrict.csv',stringsAsFactors=F)[,1]
  citydel = citydf[!citydf %in% citypre & citydf %in% c(citycoun,cityqu)]
  df = subset(df, !df$city %in% citydel)
  if (year <= 1999){
    #去除错误写进数据表里的县级市
    county1 = table(df$city)
    county2 = names(county1[which(county1==10)])
    df = subset(df,!(df$city %in% county2))
    #找出OCR错误的城市
    citydf1 = unique(df$city)
    cityerror = citydf1[!citydf1 %in% citypre]
    #df = subset(df,!(df$city %in% cityerror))
  }else{
    citydf1 = unique(df$city)
    cityerror = citydf1[!citydf1 %in% citypre]
  }
  cityerror
  df = subset(df,!(df$city %in% c('h市','龙门市','几台市','海域市','F市','凯墨市','内蒙市','大唐市','')))
  df = subset(df,!grepl('省|村|\\b(城市)\\b|城市市|\\d|[a-z]|[A-Z]',df$city))
  #'毫州市' %in% citylist
  #unique(subset(df, df$city == '准北市')$city_en)
  #unique(subset(df, df$city_en == 'Baotou')$city)
  #a = which(df$city == '临次市')
  #df[(a-5):(a+5),]
  #a = which(unique(df$index)=='城市全市.1')
  #unique(df$index)[(a-5):(a+5)]
  
  save(df, file=paste('C:/Sync/CoolGirl/Fhe/ecosocialDATA/psudoSQLall/psudoSQL',year,'.Rdata',sep=''))
  print(year)
}
#sort(table(df$city))
#sort(table(df$index))

###查询命令
FunRequest = function(yearR,indexR,cityR=df$city){
  datR = subset(df, df$city %in% cityR & df$year %in% yearR & df$index %in% indexR)
  return(datR)
}
uniindex = unique(df$index)
cityRequest = c('北京市','上海市')
yearRequest = 2010
indexRequest = grep('.*公路货运.*', uniindex, value=T)
datRequest = FunRequest(yearRequest, indexRequest, cityRequest)
#write.csv(datRequest, 'sansha.csv',row.names=F)

###出错的补救
#unique(df$index)
#det = as.character(unique(df$index)[72:74])
#df1 = subset(df, !(df$index %in% det))
#df = df1


setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/psudoSQLall')
df = data.frame()
dfnew = data.frame()
for (fr in dir()){
  rm(df)
  load(fr)
  dfnew = rbind(dfnew, df)
}

### 去除同年同城市同指标的不同数值的情况
iddelf = vector()
f= dfnew[duplicated(dfnew[,c(-1,-5)]),]
for (i in 1:dim(f)[1]){
  ff = subset(dfnew,dfnew$city==f$city[i] & dfnew$year==f$year[i] & dfnew$index==f$index[i])
  if (sum(is.na(ff$value))==1){iddelf = c(iddelf, ff$id[is.na(ff$value)])
  }else{iddelf = c(iddelf, ff$id[1])}
}
### 处理淮安市
a = dfnew[dfnew$city=='淮安市',] 
b = 1988:2000 #names(table(a$year))[table(a$year)==4]
c = a[a$year %in% b,]
iddelc = c$id[grepl('淮安市', c$id)]
dfnew = dfnew[which(!dfnew$id %in% c(iddelc,iddelf)),]

### 更改错误的指标名
dfnew$index = gsub('综合经济指标国内生产总值按现行第二产业价格计算市辖区1','国内生产总值第三产业市辖区',dfnew$index)
dfnew$index = gsub('年末部人口万人', '年末总人口万人',dfnew$index)
dfnew$index = gsub('总景', '总量',dfnew$index)
dfnew$index = gsub('总最', '总量',dfnew$index)
dfnew$index = gsub('供水总量万吨10000万吨10000市辖区', '供水总量生活用水量万吨市辖区',dfnew$index)

dfnew$value = as.numeric(gsub(',','',dfnew$value))
dfnew[which(dfnew$value==0),]$value = NA

### 
dfnew1 = dfnew
dfnew1$value[is.na(dfnew1$value)] = 0
gzqs1 = subset(dfnew1,dfnew1$index=='职工工资总额全民所有制全市' & dfnew1$year==1986)
gzqs2 = subset(dfnew1,dfnew1$index=='职工工资总额城镇集体所有制全市' & dfnew1$year==1986)
gzqs3 = subset(dfnew1,dfnew1$index=='职工工资总额各种合营全市' & dfnew1$year==1986)
gongzi1986qs = gzqs1$value + gzqs2$value +gzqs3$value
gongzi1986qs[gongzi1986qs==0] = NA
gongzi1986qsdf = gzqs1
gongzi1986qsdf$index = rep('职工工资总额.万元.全市', dim(gzqs1)[1])
gongzi1986qsdf$value = gongzi1986qs
gongzi1986qsdf$id = paste0(gzqs1$id,'lzy')

gzxsq1 = subset(dfnew1,dfnew1$index=='职工工资总额全民所有制市辖区' & dfnew1$year==1986)
gzxsq2 = subset(dfnew1,dfnew1$index=='职工工资总额城镇集体所有制市辖区' & dfnew1$year==1986)
gzxsq3 = subset(dfnew1,dfnew1$index=='职工工资总额单位万元各种合营市辖区' & dfnew1$year==1986)
gongzi1986xsq = gzxsq1$value + gzxsq2$value +gzxsq3$value
gongzi1986xsq[gongzi1986xsq==0] = NA
gongzi1986xsqdf = gzxsq1
gongzi1986xsqdf$index = rep('职工工资总额.万元.市辖区', dim(gzxsq1)[1])
gongzi1986xsqdf$value = gongzi1986xsq
gongzi1986xsqdf$id = paste0(gzxsq1$id,'lzy')
dfnew = rbind(dfnew, gongzi1986qsdf, gongzi1986xsqdf)


save(dfnew, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/psudoSQLall.Rdata')