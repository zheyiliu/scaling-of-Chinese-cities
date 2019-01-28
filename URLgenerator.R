f = 'shanxi.txt'
l = 'N2010010103000'
setwd('C:/Sync/temp/url')
for (i in 1:9){ 
	link = paste0("http://data.cnki.net/download/excel?filecode=",l,"00",i)
	write.table(link, f, quote = FALSE, row.names = FALSE, col.names = FALSE, append=T)
	}
for (i in 1:99){ 
	link = paste0("http://data.cnki.net/download/excel?filecode=",l,"0",i)
	write.table(link, f, quote = FALSE, row.names = FALSE, col.names = FALSE, append=T)
	}
for (i in 1:500){ 
	link = paste0("http://data.cnki.net/download/excel?filecode=",l,i)
	write.table(link, f, quote = FALSE, row.names = FALSE, col.names = FALSE, append=T)
	}