########## 添加.xls后缀，因为原来是在Linux下载的，没有后缀 ########
# setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/alldata/')
# for (year in dir()){
# # setwd(paste('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/alldata/',year,sep=''))
#   setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/alldata/2013/')
# # oldname = as.character(dir())
#   newname = gsub('.xls','', oldname)
# # newname = paste(oldname, '.xls',sep='')
# # file.rename(oldname, newname)
# }


################ From .xls to .csv #################
library(readxl)

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/')
yearall = dir()
yearall = subset(yearall, yearall!='1993-1994')

#### 以标题名重命名
for (yeari in yearall){

  setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/unlock/'))
  fileall = dir()
  fileall = fileall[grepl('.xls',fileall)]
  
  for (f in fileall){
    
    tst = read_excel(f, sheet = 1, col_names=F, col_types='text')
    m=1
    if (nrow(tst)==0){break}
    while(sum(!is.na(tst[m,]))==0){
      tst = tst[-m,]
    }
    l = paste0(sample(letters,1),sample(letters,1),sample(letters,1),sample(letters,1))
    newname = paste(tst[1,][which(!is.na(tst[1,]))][1], l, '.xls', sep='')
    newname = gsub(' ','',newname)
	newname = gsub('\\*','',newname)
    newname = paste('中国城市统计年鉴', yeari, newname, sep='-')
    
    file.rename(f, newname)
  }
}

dir.create("C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCal/")
setwd("C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCal/") 
file.remove(dir("C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCal/"))

#### 找出多个sheet的xls, 以及 只有一个sheet的xls
troublelist = vector() 
safelist = vector()            
for (yeari in yearall){
  setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/unlock/'))
  fileall = dir()
  fileall = fileall[grepl('.xls',fileall)]
  
  for (f in fileall){
    shname = readxl::excel_sheets(f)
    cnki = grepl(".*[A-Za-z]+.*",shname)
    shname1 = shname[!cnki]
    shlen = length(shname1)
    hyphen = grepl('-', shname1)
    if (shlen > 1) { #& sum(hyphen)<=0
      troublelist = c(troublelist, paste(yeari, f, shname1[hyphen],sep=','))
    }else if(shlen == 1) { #& sum(hyphen)<=0
      safelist = c(safelist, paste(yeari, f,sep=';'))
    }
  }
}

#### 把没问题的xls保存为csv格式
for (t in 1:length(safelist)){
  tt = strsplit(safelist[t],';')[[1]]
  yeari = tt[1]
  ft = tt[2]
  setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/unlock/'))
  shname = excel_sheets(ft)
  shname1 = shname[!grepl(".*[A-Za-z]+.*",shname)]
  shname2 = paste0('sheet',shname1)
  
  tst = read_excel(ft, sheet = 1,col_names=F, col_types='text')
  colnames(tst)=NA
  fn = gsub('xls','csv',ft) 
  write.csv(tst, file=paste0('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCal/super-',fn),row.names=F)
}



#### 找出有问题的xls - sheet1中有多张表格的xls
#### 如果不同sheet列数一样，直接拼接（列名？）
#### 如果不一样，这些xls文件需要人工处理
#### 共有10个，其中6个是有用的
#### 我也不知道为啥我要重新跑1985-2017的，反正这6个文件我是直接另存为当初处理过的过来了...


#### 如果不同sheet列数一样，直接拼接（列名？）
troublelist1 = vector() # rbind报错，每个sheet的ncol不一样
for (t in 1:length(troublelist)){
  tt = strsplit(troublelist[t],',')[[1]]
  yeari = tt[1]
  ft = tt[2]
  setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/unlock/'))
  shname = excel_sheets(ft)
  shname1 = shname[!grepl(".*[A-Za-z]+.*",shname)]
  shname2 = paste0('sheet',shname1)
  
  shcol = vector()
  tst = data.frame()
  for (i in 1:length(shname2)) {
    shtst = read_excel(ft, sheet = i,col_names=F, col_types='text')
	shcol = c(shcol, dim(shtst)[2])
	if(max(shcol) == min(shcol)){tst = rbind(tst, shtst)}
  }
  if(max(shcol) != min(shcol)){
    troublelist1 = c(troublelist1,paste(yeari, ft, sep=','))
	next
  }
  colnames(tst)=NA
  fn = gsub('xls','csv',ft) 
  write.csv(tst, file=paste0('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCal/super-',fn),row.names=F)
}

troublelist1




#### 记得拆分1993-1994年
f93 = dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCal-9394/')
fromdir = paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/DataForCal-9394/',f93)
todir = paste0('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCal/super-',f93)
file.copy(fromdir, todir)



######################################
######################################
######################################
######################################
## 方便测试，0-4代码复制了一部分过来










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
    dat = subset(dat, !grepl(pattern='合计|总计|超大|特大|大城市|中等|小城市|东部|中部|人口|西部|全部|沿海|特区|492|72|14|4|\\d个|东都|中都|增值税', dat[,1]) & !(dat[,1] %in% c(prov,provfull)))
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
    
	
    if ((sum(grepl('北京市|上海市', dat1[,1]))==0) & (sum(grepl('辛集市|临夏市', dat1[,1]))>0)){
      colnames(dat1)[-1]=paste(colnames(dat1)[-1],'县级市', sep='_')
    }
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
