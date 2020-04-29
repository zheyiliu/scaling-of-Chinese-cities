library(ggplot2)

home = 'C:/Sync/CoolGirl/Fhe'
setwd(paste0(home,'/ecosocialData/indexSQL'))
for (rdat in dir()){load(rdat)}

dflist0 = gsub('.Rdata', '', dir(paste0(home,'/ecosocialData/indexSQL'))) #全部都需要预处理
dflist = grep('POP|\\d', dflist0, invert=T, value=T) #用来算OLS的

for (yi in 1:length(dflist0)){
  dfname = dflist0[yi]
  df = get(dfname)
  df$year = as.integer(df$year)
  df$value = as.numeric(df$value)
  df$value[which(df$value==0)] = NA
  assign(dfname, df)
  #eval(parse(text = paste0('return(',dfname, ')')))
}
modelname = 'OLS1_DJS'
rangeStatList = c('市辖区', 'Districts', 'BetaD/', 'FigD/')

citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/city_info.csv',stringsAsFactors=F)
citypre = subset(citylist, citylist$Administrative_level != 'county')$City_ch
citycoun = subset(citylist, citylist$Administrative_level == 'county')$City_ch
cityqu = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/ToDistrict.csv',stringsAsFactors=F)[,1]
mass = c('内蒙市', '胡南省')
for (yi in 1:length(dflist0)){
  dfname = dflist0[yi]
  df = get(dfname)
  citydf = unique(df$city)
  citydel = citydf[!citydf %in% citypre & citydf %in% c(citycoun,cityqu,mass)]
  df = subset(df, !df$city %in% citydel)
  
  assign(dfname, df)
  #eval(parse(text = paste0('return(',dfname, ')')))
  print(yi)
}

IDDELF = function(ddat){
  #ddat = GDP
  iddelf = vector()
  wield = paste0(ddat[,2], ddat[,3], ddat[,4])
  f= ddat[duplicated(wield),]
  if (nrow(f)>0){
    for (i in 1:dim(f)[1]){
      ff = subset(ddat,ddat$city==f$city[i] & ddat$year==f$year[i] & ddat$index==f$index[i])
      iddelf = c(iddelf, ff$id[-1])
    }
    ddat = ddat[which(!ddat$id %in% iddelf),]
  }
  return(ddat)
}


SearchCorValue = function(ORI, COR){ #找到某指标对应的另一指标的值
  ORI = IDDELF(ORI)
  COR = IDDELF(COR)
  corValue = vector()
  for (i100i in 1:dim(ORI)[1]){
    cityL = COR$city==ORI$city[i100i]
    yearL = COR$year==ORI$year[i100i]
    corrr = COR[which(cityL & yearL),]
    corV = corrr$value
    if (length(corV)==0){
      corV = NA}
    if (length(corV)>1){
      print(corrr)
      corV = corV[1]}
    corValue[i100i] = corV
  }
  if (length(ORI$value)==0){
    corValueDF = data.frame(NA)
  }else{
    corValueDF = data.frame(xindex = ORI$value, yindex = corValue, city=ORI$city, year=ORI$year)
  }
  return(corValueDF)
}






#dflist = gsub('.Rdata', '', dir('C:/Sync/CoolGirl/Fhe/ecosocialDATA/indexSQLforCal'))

#modelname = 'OLS'
#rangeStatList = c('全市', 'Total','BetaT/')


  for (yi in 1:length(dflist)){
    xdfname = 'POP'
    ydfname = dflist[yi]
    #ydfname = 'Loan'
    rangeStat = rangeStatList[1] #按照指标对应的区域更改
    xdf = get(xdfname)
    #delcity = c('三沙市')
    delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市','哈密市','吐鲁番市','重庆市')
    xdf = xdf[which(!(xdf$city %in% delcity)),]
    ydf = get(ydfname)
    ORII = xdf[grepl(rangeStat, xdf$index),]
    CORR = ydf[grepl(rangeStat, ydf$index),]
    cordf = SearchCorValue(ORII, CORR)
    
    data = data.frame(Y = log(cordf$yindex), X = log(cordf$xindex), city = cordf$city, year = cordf$year)
    xlab = paste0(unique(ORII$index),' (log)')
    ylab = paste0(unique(CORR$index),' (log)')
    png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/RankSpace/',ydfname,rangeStat,'RankSpace.png'),width=18,height=15, units='cm',res=150)
    dat = cordf[which(cordf$year %in% seq(1985,2017,5)),]
    g = ggplot(data=cordf, aes(x=log(xindex), y=log(yindex), group=year,color=year)) + 
      geom_point(size = 0.5, alpha=0.3) + 
      #geom_smooth(se=F,method = "loess", span=0.7) +
      geom_smooth(se=F,method = "lm") +
      labs(x=xlab, y=ylab)+
      theme(
        text = element_text(size=18),
        panel.background = element_rect(fill = "transparent",colour = 'black'), 
        #panel.grid.minor = element_line(color='azure3'), 
        #panel.grid.major = element_line(color='azure3'),
        #plot.background = element_rect(fill = "transparent",colour = NA),
        legend.title=element_blank()
      )
    print(g)
    dev.off()
  }
