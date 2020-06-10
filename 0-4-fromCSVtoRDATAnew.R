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




dir.create("C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCalNew/")
setwd("C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCalNew/") 
file.remove(dir("C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCalNew/"))

###清洗表格
#indexclass = '建设用地'
#filename = paste(book, year, indexclass, sep='-')
##如果是存款贷款，一定要复制2016年的‘-new.csv’的表头，直接打开-new
#清洗表头
##客运量，表头加万人二字
##公共财政-市辖区，加市辖区万元
##电话互联网，万户
notdata = vector()
strangedata = vector()
for (year in 1985:2018){
  print(year)
  setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCal/')

  fileall = dir()
  #fileall = fileall[!grepl('全国|分布|行政区划|区域变动|机制研究', fileall)]
  fileyear = fileall[grep(as.character(year),fileall)]
  fileyearraw = fileyear[!grepl('new',fileyear)]
  for (f in fileyearraw){
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
    qs = grep('\\b包括市辖县\\b|全市',dat[,1])
    if (length(qs)>0){
      if (qs[1] < 10){
        quanshi = T
        #if (year <=1988){dat = dat[-sxq,]}
      }else{quanshi = F}
    }else{quanshi = F}
	
	### 处理序号格式的文件
    if ('序号' %in% dat[,1]){
      dat2001 = data.frame(城市=c(dat[,2],dat[,5]),index2001=c(dat[,3],dat[,6]),stringsAsFactors = FALSE)
      colnames(dat2001)[2] = dat[1,1]
      dat = dat2001
      mmm = vector()
      for (m in 1:10){if(sum(dat[m,]!='')==0){mmm = c(mmm, m)}}
      dat = dat[-mmm,]
      dat = dat[-1,]
	  if (shixiaqu){colnames(dat)[2] = paste(colnames(dat)[2],'市辖区',sep='')}
	  if (quanshi){colnames(dat)[2] = paste(colnames(dat)[2],'全市',sep='')}
	  strangedata = c(strangedata, f)
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
    for (q in 1:dim(dat)[2]){dat[,q] = gsub('\\b内蒙\\b', '内蒙古', dat[,q])}
    prov = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/省份列表.csv',header=F,stringsAsFactors = FALSE)[,1]
	prov = subset(prov, !grepl('吉林',prov))
    provfull = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/省份列表.csv',header=F,stringsAsFactors = FALSE)[,2]
    citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)[,1]
    citylistshi = gsub('市','',citylist)
	dat = subset(dat, !grepl(pattern='合计|总计|超大|特大|大城市|中等|小城市|东部|中部|人口|西部|全部|沿海|特区|492|72|14|4|\\d个|东都|中都|增值税', dat[,1]))
    rubbish = !(dat[,1] %in% c(prov,provfull))
    dat[!rubbish, 1] = ''
	dat[,1] = gsub('城市名称|城市名|地市|程市|城巾|城、市|市名称|\\b全市\\b|\\b地区\\b', '城市', dat[,1])
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
		
		cn0 = cn
        for(t in 2:titleline){
          for(j in 2:dim(dat)[2]){
            if(cn[t,j]=='' & cn[t-1,j]==''){
			  ref = ifelse( sum(cn[t, 1:j]!='')>0, max(which(cn[t, 1:j]!='')), j-1)
              if(cn[t-1,ref]!=''){
			    cn0[t,j]=cn[t,ref]
			  }else{
			    cn0[t,j]=cn[t,j-1]
			  }
            }
		  }
		}
		cn = cn0
	  } 
      for(j in 2:dim(dat)[2]){
		if(cn[1,j]==''){
			ref = ifelse( sum(cn[1, 1:j]!='')>0, max(which(cn[1, 1:j]!='')), j-1)
			cn[1,j]=cn[1,ref]
		}
	  }
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
    # if (sum(dat1[,1] %in% citylistshi)!=0){dat1[,1]=paste(dat1[,1],'市',sep='')}
    if (sum(grepl('\\B(市)\\b',dat1[,1]))-sum(grepl('\\B(城市)\\b',dat1[,1]))<5){dat1[,1]=paste(dat1[,1],'市',sep='')}
	
	
    dat1 = subset(dat1, !(dat1[,1]=='') & !grepl(pattern='合计|总计|超大|特大|大城市|中等|小城市|东部|中部|人口|西部|全部|沿海|特区|492|72|14|4|\\d个|东都|中都|增值税|收入市|参考市|全市|市辖县|平均', dat1[,1]) & !(dat1[,1] %in% c(prov,provfull)))
    
    if (grepl('ity', colnames(dat1)[2])){dat1 = dat1[,-2]}
	jl = which(dat1[,1]=='吉林市')
	if(length(jl) > 1){dat1 = dat1[-jl[1],]}
    saveto = 'C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCalNew/'
    write.csv(dat1, paste0(saveto, gsub('.csv','',f),'-new.csv'),row.names=F)
    ###清洗表格 done!
  }
}
d0 = dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCal/') 
d = dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCalNew/') 
d = gsub('-new','',d)
intersect(d,notdata)


###################################################################################
###################################################################################
###################################################################################




# for (year in 1985:2017){
#   df = data.frame()
#   save(df, file=paste('C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperData',year,'.Rdata',sep=''))
# }


dir.create("C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/")
setwd("C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/") 
file.remove(dir("C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/"))

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCalNew/')
###输入假的数据库
for (yeari in 1985:2018){
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


####拼接所有年份数据，统一排查问题
##################################

setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/SuperData/')
df = data.frame()
dfnew = data.frame()
for (fr in dir()){
  rm(df)
  load(fr)
  dfnew = rbind(dfnew, df)
}
df = dfnew
df = na.omit(df)

# 改掉后面发现的.1的指标
df$index = gsub("在校学生数__女生_单位人.1", "在校学生数__小学在校学生数女生_单位人", df$index)
df$index = gsub("在校学生数__女生_单位人", "在校学生数__普通中学在校学生数女生_单位人", df$index)

# 后面发现这个指标需要手动处理，但不想再动表格了，就这样改一下
ind = grep('.*三十一教育__.*', df$index)
df$index[ind] = paste0(df$index[ind], '_市辖区')

save(df, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll.Rdata')






#### 排查重复值来找出奇怪的问题，现在无必要运行
###############################################

load(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll.Rdata')
# 除了id之外，city, index, value都一样的条目（已排查）
a = df[duplicated(df[,-1]),]
a$new = paste0(a$year,a$index)
biu = table(a$new)
biu = biu[order(biu, decreasing=T)]
#subset(a,a$new=='1987工业总产值_3按行业分按1980年不变价格计算续2_有色金属冶炼及压延加工业市辖区_单位')
#### 2008表头city造成重复（去除NA可解决）
#### 2007通信续表3意外出现两次造成重复（去除重复值可解决），
#### 1987按行业分不一样的表头被合并造成重复（把xls文件手动拆好）
#### 1998，99，96，95城市名单因为没有识别到两列“城市名称”而造成重复（名单数据不重要）
#### 1999综合经济1意外重复衡水、日照、宜宾数据（识别NA去除重复值可解决）
#### 其他都是一两个城市意外重复（1986，1991）
#### 还有一个吉林省和吉林市需要注意
#### 2005-3-5辛集市市
#### 结论：处理吉林市、辛集市市


# 除了id和value之外，city, index都一样的条目（已排查）
b = df[duplicated(df[,c(-1,-5)]),]
b$new = paste0(b$year,b$index)
b_a = b[!b$id%in%a$id,]
miu = table(b_a$new)
miu = miu[order(miu, decreasing=T)]
#subset(b, b$new=='1987工业总产值_3按行业分按1980年不变价格计算续2_有色金属冶金属制品业市辖区_单位')
#sum(miu[miu<6])
table(b_a$city)[order(table(b_a$city),decreasing=T)]
#### 除了1987行业和1995保险（已处理）之外，每个指标的重复数量≤5，有512个条目
#### 出现重复条目的城市，大户是榆林市182个条目，第二大户银川43个条目
#### 结论：不管了，当作随机误差了


# 看看.1的指标，可能是表头出错，也可能是算法的问题（？）
df$new = paste0(df$year,df$index)
grep('\\b\\d\\b', unique(df$new), value=T)
#head(subset(df, df$new=="1995固定资产投资__国有经济固定资产投资住宅建设市辖区_单位万元.1"))
#### ​1989综合经济第二产业，表头出错，是第三产业。等等一系列类似错误，手动改了alldata的xls文件。
#### "2012在校学生数__女生_单位人.1" ？






### 这里需要用到0-5-findAdmin
### 1 制作OCR识别错误记录表
### 2 确定每年所有城市的名单和行政级别，可以用来检测异常城市名，也可以用来后续分出县级市


###粗暴去除OCR识别错误的城市
############################

load(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll.Rdata')

whichshi = (!grepl('市\\b', df$city)) | (grepl('\\b(津市)\\b|\\b(沙市)\\b', df$city))
df[whichshi,'city'] = paste0(df[whichshi,'city'], '市')

OCR = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/newOCRerror.csv',header=T,stringsAsFactors = FALSE)
colnames(OCR) = c('from','to','from','to')
#OCR = rbind(OCR[,1:2], OCR[,3:4])
OCR = OCR[,3:4]


adminallyear = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminallyear.csv',header=T,stringsAsFactors=F)
#### 检查OCR识别错误，补充记录在文件中，等待转换，9min
delcity = data.frame()
pb <- txtProgressBar(style = 3)
s=Sys.time()
yearii = 1985
while(yearii < 2019){
	adminyi = subset(adminallyear, adminallyear$yeari==yearii)
	dfyi = subset(df, df$year==yearii & !df$city %in% c(OCR[,1],OCR[,2],adminyi$cityshi,adminyi$shi))
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

write.csv(delcity, 'C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/strangecity.csv', row.names=F)
#### 确认已经几乎没有奇怪的城市名




#### 把异常城市名转换成正确的
adminallyear = read.csv('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminallyear.csv',header=T,stringsAsFactors=F)

# 本来想用apply函数族优化速度，但是不太会写
# 试了textclean包、mgsub包、qdap包，但它们都只能替换等长字符
# 所以最后还是用了for循环
pb <- txtProgressBar(style = 3)
s=Sys.time()
for (g in 1:nrow(OCR)){
	df$city = gsub(OCR[g,1],OCR[g,2],df$city)
	setTxtProgressBar(pb, g/nrow(OCR))
}
e=Sys.time()
close(pb)
e-s

#miu = table(df$city)
#miu = miu[order(miu,decreasing=T)]

### 找出经过OCR文件改名之后，仍然没有覆盖到的错误名称
miu = unique(df$city)
miiu = miu[!miu %in% adminallyear$cityshi]

dfs0 = subset(df, df$city %in% miiu)
df = subset(df, !df$city %in% miiu)


### 改了之后再检查一次
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

df$city[df$city=='赤壁市' & df$year %in% c(1999)] = '蒲圻市'
df$city[df$city=='海门市' & df$year %in% c(1987,1990)] = '海口市'
df$city[df$city=='荆州市' & df$year %in% c(1987,1989,1990)] = '沙市市'
df$city[df$city=='思茅市' & df$year %in% c(2008:2013)] = '普洱市'
df$city[df$city=='通什市' & df$year %in% c(2002,2004)] = '五指山市'
df$city[df$city=='襄樊市' & df$year %in% c(2012)] = '襄阳市'

dfs1 = df[df$city=='冀州市' & df$year %in% c(1990),]
dfs2 = df[df$city=='沙市市' & df$year %in% c(1997,1998),]
dfs3 = df[df$city=='台山市' & df$year %in% c(1988,1992),]
dfs4 = df[df$city=='宜州市' & df$year %in% c(1988,1989,1991,1992),]
dfs5 = df[df$city=='即墨市' & df$year %in% c(1987),]

dfs = rbind(dfs0, dfs1, dfs2, dfs3, dfs4, dfs5)
save(dfs, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/StrangeCities.Rdata')

df = subset(df, !df$id %in% dfs$id)

save(df, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll_DelStrangeCities.Rdata')








# #### 把行政级别加入df里面
# #########################

# library(pbapply)
# load('C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll_DelStrangeCities.Rdata')

# df$admin = 0
# dfadmin = data.frame()

# # 1.5h
# s=Sys.time()
# for(yeari in 1985:2018){
	# print(c(yeari,nrow(dfadmin),nrow(df)))
	# dfyi = subset(df, df$year==yeari)
	# adminyi = read.csv(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/AdminLevel/adminshi',yeari,'.csv'),header=T,stringsAsFactors=F)
	
	# #测试用
	# #dfyi = dfyi[3000:5000,]
	# a = pblapply(1:nrow(dfyi), FUN=function(i){
		# adminyi[grepl(paste0('\\b',dfyi$city[i],'\\b'),adminyi$cityshi),'admin']
	# })
	# #w = c()
	# #for(i in 1:length(a)){ if( length(a[[i]])!=1 ) { w = c(w, i)} }
	# #a[[w[1]]]
	
	# dfyi$admin = unlist(a)

	# dfadmin = rbind(dfadmin, dfyi)
# }
# e=Sys.time()
# e-s

# save(dfadmin, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll_DSCadmin.Rdata')




#### 把改了名称的城市，一概全部改到最新名称
load('C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll_DelStrangeCities.Rdata')
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
save(df, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll_DSCchange.Rdata')






load('C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperData/SuperDataAll_DSCchange.Rdata')
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

