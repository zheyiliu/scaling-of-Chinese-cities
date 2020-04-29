########### 添加.xls后缀，因为原来是在Linux下载的，没有后缀 ########
setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/alldata/')
for (year in dir()){
  setwd(paste('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/alldata/',year,sep=''))
  #setwd('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/alldata/2013/')
  oldname = as.character(dir())
  #newname = gsub('.xls','', oldname)
  newname = paste(oldname, '.xls',sep='')
  file.rename(oldname, newname)
}


################ From .xls to .csv #################
library(readxl)

setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/')
yearall = dir()
yearall = yearall[yearall!=2018]


#### 以标题名重命名
for (yeari in yearall){

  setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/csv/'))
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
    newname = paste('中国城市统计年鉴', yeari, newname, sep='-')
    
    file.rename(f, newname)
  }
}

#### 找出多个sheet的xls
troublelist = vector()             
for (yeari in yearall){
  setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/csv/'))
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
    }
  }
}

### 只有一个sheet的xls
safelist = vector()
for (yeari in yearall){
  setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/csv/'))
  fileall = dir()
  fileall = fileall[grepl('.xls',fileall)]
  
  for (f in fileall){
    shname = readxl::excel_sheets(f)
    cnki = grepl(".*[A-Za-z]+.*",shname)
    shname1 = shname[!cnki]
    shlen = length(shname1)
    hyphen = grepl('-', shname1)
    if (shlen == 1) { #& sum(hyphen)<=0
      safelist = c(safelist, paste(yeari, f,sep=';'))
    }
  }
}

# collist = vector()
# for (t in 1:length(troublelist)) {
#   tt = strsplit(troublelist[t],',')[[1]]
#   yeari = tt[1]
#   ft = tt[2]
#   sh = tt[3]
#   tst = read_excel(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/csv/',ft),
#                    col_names=F, col_types='text',sheet=2)
#   collist = c(collist, dim(tst)[2])
# }


#### 找出sheet1中有多张表格的xls
tellme = vector()
for (t in 1:length(troublelist)){
  tt = strsplit(troublelist[t],',')[[1]]
  yeari = tt[1]
  ft = tt[2]
  setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/csv/'))
  
  if (length(excel_sheets(ft)<4)){
    tst1 = read_excel(ft, sheet = 1, col_names=F, col_types='text')
    tst2 = read_excel(ft, sheet = 2, col_names=F, col_types='text')
    if (dim(tst1)[2] != dim(tst2)[2]){tellme = c(tellme, ft)}
  } else {
    tst1 = read_excel(ft, sheet = 1, col_names=F, col_types='text')
    tst2 = read_excel(ft, sheet = 2, col_names=F, col_types='text')
    tst3 = read_excel(ft, sheet = 3, col_names=F, col_types='text')
    
    if (!(dim(tst1)[2] == dim(tst2)[2] & dim(tst2)[2] == dim(tst3)[2])){
      tellme = c(tellme, ft)
    }
  }
}

#### 把多个sheet的xls的sheet都拼成sheet1
for (t in 1:length(troublelist)){
  tt = strsplit(troublelist[t],',')[[1]]
  yeari = tt[1]
  ft = tt[2]
  setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/csv/'))
  shname = excel_sheets(ft)
  shname1 = shname[!grepl(".*[A-Za-z]+.*",shname)]
  shname2 = paste0('sheet',shname1)
  
  tst = data.frame()
  for (i in 1:length(shname2)) {
    shtst = read_excel(ft, sheet = i,col_names=F, col_types='text')
    tst = rbind(tst, shtst)
  }
  colnames(tst)=NA
  fn = gsub('xls','csv',ft) 
  write.csv(tst, file=paste0('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCal/super-',fn),row.names=F)
}

#### 把其他xls也保存为csv格式
for (t in 1:length(safelist)){
  tt = strsplit(safelist[t],';')[[1]]
  yeari = tt[1]
  ft = tt[2]
  setwd(paste0('C:/Sync/CoolGirl/Fhe/ecosocialDATA/原始数据/alldata/',yeari,'/csv/'))
  shname = excel_sheets(ft)
  shname1 = shname[!grepl(".*[A-Za-z]+.*",shname)]
  shname2 = paste0('sheet',shname1)
  
  tst = read_excel(ft, sheet = 1,col_names=F, col_types='text')
  colnames(tst)=NA
  fn = gsub('xls','csv',ft) 
  write.csv(tst, file=paste0('C:/Sync/CoolGirl/Fhe/ecosocialdata/原始数据/DataForCal/super-',fn),row.names=F)
}



