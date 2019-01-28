setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/知网县级市')
filelist = dir()
dat = read.csv(filelist[1], header=F, stringsAsFactors = FALSE, colClasses='character')
val = dat[c(-1,-2),c(-1,-2,-3)]
for (i in 3:dim(dat)[1]){
	for (j in 4:dim(dat)[2]){
		cityname = dat[i, 1]
		indexname = dat[2, j]