index = '医疗床位数'
province = '内蒙古'
setwd(paste('C:/Sync/Coolgirl/Fhe/ecosocialdata/指标数据/', index, sep=''))
oldfile = paste(index, '_', province, '.csv', sep='')
mimi = read.csv(oldfile, header=T)

###分割指标
m = 3
rawcities = colnames(mimi)
cities = c('呼和浩特市', rawcities[seq(7,by=5,length=7)])
mimi = mimi[,c(1,1+(seq(m,by=5,length=8)))]
colnames(mimi) = c(rawcities[m+1], cities)

###删除空格
for (i in 1:9){mimi[,i]=as.numeric(gsub(" ", "", mimi[,i]))}

###补全年份
for (i in 1949:2016){
  if (!(i %in% mimi[,1])){
    row = c(i, rep(NA, ncol(mimi)-1))
    if (i > max(mimi[,1])){
      mimi = rbind(mimi, row)
    }else{
    mimi = rbind(mimi[1:(i-1949+1), ], row, mimi[(i-1949+2):nrow(mimi), ])
    }
  }
}
str(mimi)

###万元变亿元
mimi[,-1] = mimi[,-1]/10000

newfile = paste(colnames(mimi)[1], '_', province, '.csv', sep='')
write.csv(mimi, newfile, row.names=F, na="")
