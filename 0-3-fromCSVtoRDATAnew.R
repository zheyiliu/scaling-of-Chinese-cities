# #####处理常住人口数据
# df2000 = read.csv('C:\\Sync\\CoolGirl\\Fhe\\ecosocialDATA\\原始数据\\人口普查常住人口\\2000POPall.csv', header=T, stringsAsFactors = FALSE, colClasses='character')
# df2010 = read.csv('C:\\Sync\\CoolGirl\\Fhe\\ecosocialDATA\\原始数据\\人口普查常住人口\\2010POPall.csv', header=T, stringsAsFactors = FALSE, colClasses='character')
# 
# #for (p in 2:dim(dat)[2]){dat[,p] = gsub(' ', '', dat[,p])}
# #write.csv(dat, "C:\\Sync\\CoolGirl\\Fhe\\ecosocialdata\\原始数据\\人口普查常住人口\\1990人口普查分县资料_按户口分1.csv")
# love = function(yeari){
#   df1990 = read.csv(file=paste0('C:\\Sync\\CoolGirl\\Fhe\\ecosocialDATA\\原始数据\\人口普查常住人口\\',yeari,'POPall.csv'), 
#                     header=T, stringsAsFactors = FALSE, colClasses='character')
#   df1990$city = gsub('浑江市', '白山市', df1990$city)
#   for (p in 1:dim(df1990)[2]){df1990[,p] = gsub(' ', '', df1990[,p])}
#   citydf = unique(df1990$city)
#   citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)
#   citypre = subset(citylist, citylist$Administrative_level != 'county')$City_ch
#   citycoun = subset(citylist, citylist$Administrative_level == 'county')$City_ch
#   cityqu = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/ToDistrict.csv',stringsAsFactors=F)[,1]
#   citydel = citydf[!citydf %in% citypre & citydf %in% c(citycoun,cityqu)]
#   df = subset(df1990, !df1990$city %in% citydel & df1990$level=='地级市')[,1:2]
#   colnames(df) = c('city','value')
#   df$index = '常住人口.人.市辖区'
#   df$year = yeari
#   let = letters[sample(26,dim(df)[1],replace=T)]
#   df$id = paste(df$city, df$year, df$index, let, sep='-')
#   return(df)
# }
# df1 = love(1990)
# df2 = love(2000)
# df3 = love(2010)
# 
# Resident = rbind(df1,df2,df3)
# Resident$value = as.numeric(Resident$value)
# Resident$year = as.character(Resident$year)
# save(Resident,file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQL/POPResident.Rdata')
# #write.csv(Resident, file="C:\\Sync\\CoolGirl\\Fhe\\ecosocialdata\\原始数据\\人口普查常住人口\\POPResident.csv",row.names=F)






###清洗表格
#indexclass = '建设用地'
#filename = paste(book, year, indexclass, sep='-')
##如果是存款贷款，一定要复制2016年的‘-new.csv’的表头，直接打开-new
#清洗表头
##客运量，表头加万人二字
##公共财政-市辖区，加市辖区万元
##电话互联网，万户
notdata = vector()
for (year in 1985:2017){
  #year = 1985
  print(year)
  setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCal/')

  fileall = dir()
  #fileall = fileall[!grepl('全国|分布|行政区划|区域变动|机制研究', fileall)]
  fileyear = fileall[grep(as.character(year),fileall)]
  fileyearraw = fileyear[!grepl('new',fileyear)]
  for (f in fileyearraw){
    #f = '中国城市统计年鉴-2004-3—44环境状况(四)：城市绿化(市辖区).csv'
    #f = '中国城市统计年鉴-2000-3—34城市道路与交通(不包括市辖县).csv'
    dat = read.csv(f, header=F, stringsAsFactors = FALSE, colClasses='character')
    #dat = read.csv(paste(filename,'.csv',sep=''), header=F, colClasses='character')
    ### 替换NA为空格
    for (r in 1:dim(dat)[2]){dat[,r][is.na(dat[,r])] = ''}
    if (dim(dat)[1]<10 | dim(dat)[2]<2){
      notdata = c(notdata, f)
      next
      }
    ### 删除空行
    mm = vector()
    for (m in 1:10){if(sum(dat[m,]!='')==0){mm = c(mm, m)}}
    if (length(mm)!=0){dat = dat[-mm,]}
    ### 删除空列
    ss = vector()
    for (s in 1:dim(dat)[2]){if(sum(dat[,s]!='')==0){ss = c(ss, s)}}
    if (length(ss)!=0){dat = dat[,-ss]}
    ### 删除空格
    for (p in 1:dim(dat)[2]){dat[,p] = gsub(' ', '', dat[,p])}
    
    
    ### 记录是否只含市辖区数据
    for (q in 1:dim(dat)[2]){dat[,q] = gsub('\\b市区\\b', '市辖区', dat[,q])}
    for (q in 1:dim(dat)[2]){dat[,q] = gsub('\\b地区\\b', '全市', dat[,q])}
    sxq = grep('市辖区|\\b不包括市辖县\\b',dat[,1])
    if (length(sxq)>0){
      if (sxq[1] < 10){
        shixiaqu = T
        #if (year <=1988){dat = dat[-sxq,]}
      }else{shixiaqu = F}
    }else{shixiaqu = F}
    qs = grep('\\b包括市辖县\\b',dat[,1])
    if (length(qs)>0){
      if (qs[1] < 10){
        quanshi = T
        #if (year <=1988){dat = dat[-sxq,]}
      }else{quanshi = F}
    }else{quanshi = F}
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
    needtitle = ''
    needtitle = dat[1,1]
    needtitle = gsub('表|\\d','', needtitle)
    needtitle = gsub('[[:punct:]]|[\n *)]|[a-z]|[A-Z]','',needtitle)
    
    needsubtitle = ''

    danwei = '单位'
    danweiindex = grep('\\b单位', dat[,1])
    danweiindex2 = grep('\\b单位', dat[,dim(dat)[2]])
    if (length(danweiindex)>0){
      danwei = dat[danweiindex[1],1]
      danwei = gsub('[[:punct:]]|[\n *)]|[a-z]|[A-Z]','',danwei)
    }
    if (length(danweiindex2)>0){
      danwei = dat[danweiindex2[1],dim(dat)[2]]
      danwei = gsub('[[:punct:]]|[\n *)]|[a-z]|[A-Z]','',danwei)
      dat[danweiindex2[1],dim(dat)[2]] = ''
    }
    
    
    ### 清理城市名（列名）
    prov = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/省份列表.csv',header=F,stringsAsFactors = FALSE)[,1]
    provfull = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/省份列表.csv',header=F,stringsAsFactors = FALSE)[,2]
    citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)[,1]
    citylistshi = gsub('市','',citylist)
    dat = subset(dat, !grepl(pattern='合计|总计|超大|特大|大城市|中等|小城市|东部|中部|人口|西部|全部|沿海|特区|492|72|14|4|\\d个|东都|中都|增值税', dat[,1]) & !(dat[,1] %in% c(prov,provfull)))
    dat[,1] = gsub('城市名称|城市名|地市|程市|城巾|城、市|市名称', '城市', dat[,1])
    ### 对正常格式的表格清理指标名（行名）
    findcities = which(dat[,1] %in% c(citylist,citylistshi))
    if (length(findcities) < 50){
      notdata = c(notdata, f)
      next
    }
    if (!strangeformat){
      ### 删除表头大标题
      oldtitleline = vector()
      n = 1
      while(sum(dat[n,]!='')==1 & dat[n,1]!=''){
        oldtitleline = c(oldtitleline,n)
        n = n + 1
      }
      if (length(oldtitleline)>1){
        needsubtitle = dat[oldtitleline[2],1]
        needsubtitle = gsub('[[:punct:]]|[\n *)]|[a-z]|[A-Z]','',needsubtitle)
        if (needsubtitle == danwei){needsubtitle = ''}
      }
      
      if (length(oldtitleline)>0){dat = dat[-oldtitleline,]}
      
      
      ### 记录表头标题占几行
      
      titleline = which(dat[,1] %in% c(citylist,citylistshi))[1]-1
      if (!is.na(titleline)){cn = dat[1:titleline,]}
 
      cityline = grep('\\b城市\\b|\\b城市名称\\b|\\b城市名\\b|\\b地市\\b|\\b程市\\b|\\b城巾\\b|\\b城、市\\b|\\b市名称\\b', dat[,1])
      if (length(cityline)==0){
        dat[titleline,1]='城市'
        cityline = titleline
        cn = dat[1:titleline,]
      }
      citycol = grep('\\b城市\\b|\\b城市名称\\b|\\b城市名\\b|\\b地市\\b|\\b程市\\b|\\b城巾\\b|\\b城、市\\b|\\b市名称\\b', dat)
      
      
      
      if (length(citycol)==2){
        names(dat) = NA
        datc = dat[,citycol[1]:citycol[2]-1]
        datd = dat[,citycol[2]:dim(dat)[2]]
        
        if (dim(datc)[2]==dim(datd)[2]){
          if (sum(datc[,1] %in% datd[,1])<50){
            dat = rbind(datc, datd)
          } else {
            dat = dat[,-citycol[2]]
            citycol = citycol[1]
          }
        } else {
          notdata = c(notdata, f)
          next
        }
        
      } else if (length(citycol) > 2){
        notdata = c(notdata, f)
        next
      }
      
      cn = dat[1:titleline,]
      
      
      # if (cn[cityline[1],2]==''){
      #   cn[cityline[1],1] = ''
      #   cn[1,1] = '城市'
      # } else{cn[1,1] = '城市'}
      
      
      ### 根据标题所占行数处理标题
      ###############################################
      if (titleline > 1){
        if (cityline[1] < titleline){
          cn[cityline[1], 1] = ''
          cn[titleline, 1] = '城市'
        }
        for(t in 2:titleline){
          for(j in 2:dim(dat)[2]){
            if(cn[t,j]=='' & cn[t-1,j]==''){
              cn[t,j]=cn[t,j-1]
            }}}
      } 
      for(j in 2:dim(dat)[2]){if(cn[1,j]==''){cn[1,j]=cn[1,j-1]}}
      ###############################################
      
      cn1=data.frame()
      i = titleline
      if(i>=2){for(k in 1:i){
        cn1=paste(cn1, cn[k,],sep='')}
      }else{cn1 = cn}
      if (sum(grepl('其中', cn1))>0){
        for (k in grep('其中',cn1)){
          cn1[k] = gsub('其中', '', cn1[k])
        }}
      if (shixiaqu){cn1[-1] = paste(cn1[-1],'市辖区',sep='')}
      if (quanshi){cn1[-1] = paste(cn1[-1],'全市',sep='')}
      # if (year>2012 & grepl('存贷款', f)){
      #   cn1[6] = '年末金融机构人民币各项存款余额DepositsofNationalBankingSystematYear-end居民人民币储蓄存款余额HouseholdSavingDepositsatYear-end市辖区DistrictsunderCity'
      # }
      cn1 = gsub('[[:punct:]]|[\n *)]|[a-z]|[A-Z]', '', cn1)
      cn1[-1] = paste(needtitle,needsubtitle, cn1[-1],danwei,sep='_')
      cn1 = gsub('不包括市辖县|包括市辖县', '', cn1)
      colnames(dat) = cn1
    }
    #清洗表头 done!
    dat1 = subset(dat, !(dat[,1]=='') & !grepl(pattern='注|\\d|表|单位|投资|计算|续|相等|\\b([a-z]+)\\b|地方|汇总|比例|\\b(城市)\\b', dat[,1]))
    
    dat1[,1] = gsub('[[:punct:]]', '', dat1[,1])
    if (sum(dat1[,1] %in% citylistshi)!=0){dat1[,1]=paste(dat1[,1],'市',sep='')}
    
    dat1 = subset(dat1, !(dat1[,1]=='') & !grepl(pattern='合计|总计|超大|特大|大城市|中等|小城市|东部|中部|人口|西部|全部|沿海|特区|492|72|14|4|\\d个|东都|中都|增值税|收入市|参考市|全市|市辖县|平均', dat1[,1]) & !(dat1[,1] %in% c(prov,provfull)))
    
    if ((sum(grepl('北京市|上海市', dat1[,1]))==0) & (sum(grepl('辛集市|临夏市', dat1[,1]))>0)){
      colnames(dat1)[-1]=paste(colnames(dat1)[-1],'县级市', sep='_')
    }
    if ('City' %in% colnames(dat1)){dat1 = dat1[,-2]}
    saveto = 'C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCalNew/'
    write.csv(dat1, paste0(saveto, gsub('.csv','',f),'-new.csv'),row.names=F)
    ###清洗表格 done!
  }
}



###################################################################################
###################################################################################
###################################################################################




# for (year in 1985:2017){
#   df = data.frame()
#   save(df, file=paste('C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperData',year,'.Rdata',sep=''))
# }



setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCalNew/')
###输入假的数据库
for (yeari in 1985:2017){
  print(yeari)
  fileall = dir()
  luck = paste0('\\b', as.character(yeari), '\\b')
  fileyear = fileall[grep(luck,fileall)]
  df = data.frame()
  save(df, file=paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperData',yeari,'.Rdata'))
  for (f in fileyear){
    dat1 = read.csv(f,header=T,stringsAsFactors = FALSE)
    cn = colnames(dat1)
    city = rep(dat1$城市,(dim(dat1)[2]-1))
    #city_en = rep(dat1$City, (dim(dat1)[2]-2)) ###2011没有城市英文
    index = unlist(lapply(cn[-1], function(x){rep(x,dim(dat1)[1])}))
    value = unlist(c(dat1[,-1]))
    value[which(value==0)] = ''
    value = as.numeric(gsub(',', '', value))
    names(value)= NULL
    stryear = regexpr('\\d', f)[[1]]
    year = substr(f, stryear, stryear+3)
    let = letters[sample(26,length(city),replace=T)]
    id = paste(city, year, index, let, sep='-')
    sql = data.frame(id=id, city=city, year=year, index=index, value=value,stringsAsFactors = FALSE)
    load(file=paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperData',yeari,'.Rdata'))
    df = rbind(df,sql)
    ###输入假的数据库 done!
    #print(length(unique(df$index)))
    #df$index = gsub('[().*]', '', df$index)
    save(df, file=paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperData',yeari,'.Rdata'))
  }
}





###################################################################################
###################################################################################
###################################################################################


####粗暴去除OCR识别错误的城市
setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/SuperData/')
df = data.frame()
dfnew = data.frame()
for (fr in dir()){
  rm(df)
  load(fr)
  dfnew = rbind(dfnew, df)
}
df = dfnew
save(df, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll.Rdata')
  
a = df[duplicated(df[,-1]),]
a$new = paste0(a$year,a$index)
biu = table(a$new)
biu = biu[order(biu, decreasing=T)]

df = df[!duplicated(df[,-1]),]
dim(df)

miu = table(df$city)
miu = miu[order(miu,decreasing=T)]

df$city = gsub('市市', '市', df$city)
df$city = gsub('\\b津市\\b', '津市市', df$city)


##更改错误信息
df$city = gsub('乌兰察市', '乌兰察布市', df$city) 
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
#df$index = gsub('年末总人口万人市辖区.1', '年末总人口万人非农业人口市辖区', df$index)
#df$index = gsub('末金融机构存款余额市辖区.1', '年末金融机构存款余额城乡居民储蓄年末余额市辖区', df$index)
#df$city = gsub('', '', df$city)

miu = table(df$city)
miiu = miu[miu<40]
dfs = subset(df, df$city %in% names(miiu))
save(dfs, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/StrangeCities.Rdata')
df = subset(df, !df$city %in% names(miiu))

df = subset(df,  !grepl(pattern='___', df$index))

save(df, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll_DelStrangeCities.Rdata')



