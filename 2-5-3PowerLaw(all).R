
library(ggplot2)

setwd('C:/Sync/CoolGirl/Fhe/Results/3powerlaw/pars/')
for (rdat in dir()){load(rdat)}

###
rangeStat = '市辖区'
dfname = paste0('5 urban indicators of 1984-2016')
dflist = gsub('.Rdata', '', dir())
Ydflist = dflist[!grepl('POP|sumlmHorizontal', dflist)]
Ydflist = Ydflist[!grepl('HospitalBerth|Green', Ydflist)]

dftau1 = data.frame()
for (i in 1:length(Ydflist)){
  dftau1 = rbind(dftau1, get(Ydflist[i]))
}
yeari = 1985:2017
dftau1 = dftau1[which(dftau1$year==yeari),]

###
#dfname='GDP'
#dftau2 = get(paste0(dfname,'conpl'))

#############################################
taus = dftau1[,c(5,2,1)]
#############################################

taus$name = 'tau'
alphas = POPconpl[which(POPconpl$year %in% yeari),c(5,2,1)]
alphas$name = 'alpha'
betas = sumlmHorizontal[which(sumlmHorizontal$yIndex %in% taus$yIndex & sumlmHorizontal$year %in% yeari),c(9,2,1)]
betas$name = 'beta'
colnames(betas)[2] = 'Alpha'

#df0 = rbind(taus, alphas, betas)
#POPalpha = df0[which(df0$name=='alpha'),]
#Ytau = df0[which(df0$name=='tau'),]
#Ybeta = df0[which(df0$name=='beta'),]

df1 = merge(taus, alphas, by='year')
colnames(df1)[3] = 'yIndex'
df2 = merge(df1, betas, by=c('yIndex','year'))
df2 = na.omit(df2)[,c(-4,-6, -7,-9)]
colnames(df2)[c(3,4,5)] = c('tau','alpha', 'beta')

df2$estYbeta = (df2$alpha - 1) / (df2$tau - 1)
df2$Yr = df2$beta/df2$estYbeta
df2[order(df2$Yr,decreasing=F),][1:20,]

normYtau = (1/df2$tau-min(1/df2$tau))/((max(1/df2$tau)-min(1/df2$tau)))
normPOPalpha = (df2$alphaa-min(df2$alpha))/((max(df2$alpha)-min(df2$alpha)))
normYbeta = (df2$beta-min(df2$beta))/((max(df2$beta)-min(df2$beta)))
normestYbeta = (df2$estYbeta-min(df2$estYbeta))/((max(df2$estYbeta)-min(df2$estYbeta)))

#############################################
f = lm(beta~estYbeta-1, data=df2)
ff = summary(f)

### 绘制β和β'的关系
png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/3PowerLaw/figs/',dfname, rangeStat,'αβτ.png'),
    width=1600,height=540*2, units='px',res=200)
plot(df2$estYbeta, df2$beta, 
     ann=F, xaxt="n",yaxt="n", mgp=c(2,0,0),
     col=rgb(0, 0, 0, 80, maxColorValue=255),pch=19)
axis(1, las=0, tck=0.03, mgp=c(0,0.3,0))
axis(2, las=1, tck=0.03,mgp=c(0,0.3,0))
title(xlab="Analytical exponent (β')", mgp=c(1.5,0,0),cex.lab=1.2)
title(ylab="Empirical exponent (β)", mgp=c(2.5,0,0),cex.lab=1.2)
abline(a=0,b=ff$coefficients[1,1],lwd=2)
legend("bottomright", c(paste0('R-square=', round(ff$r.squared,3)), 
                        paste0('λ =', round(ff$coefficients[1,1],3)), 
                     paste0('P-value=', round(ff$coefficients[1,4],3))),
       title=paste0(dfname,":"),title.adj=0.2,x.intersp=0.4,y.intersp=0.8, seg.len=0,
       horiz=F, col=rgb(0, 0, 0, 80, maxColorValue=255))
dev.off()

############################################
fn = lm(normYbeta~normestYbeta-1)
ffn = summary(fn)

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/3PowerLaw/figs/',dfname, rangeStat,'αβτ.png'),
    width=1600,height=540*2, units='px',res=200)
plot(normestYbeta, normYbeta, 
     ann=F, xaxt="n",yaxt="n", mgp=c(2,0,0),
     col=rgb(0, 0, 0, 80, maxColorValue=255),pch=19)
axis(1, las=0, tck=0.03, mgp=c(0,0.3,0))
axis(2, las=1, tck=0.03,mgp=c(0,0.3,0))
title(xlab="Analytical exponent (β')", mgp=c(1.5,0,0),cex.lab=1.2)
title(ylab="Empirical exponent (β)", mgp=c(2.5,0,0),cex.lab=1.2)
abline(a=0,b=ffn$coefficients[1,1],lwd=2)
legend("bottomright", c(paste0('R-square=', round(ffn$r.squared,3)), 
                        paste0('P-value=', round(ffn$coefficients[1,4],3))),
       title=paste0(dfname,":"),title.adj=0.2,x.intersp=0.4,y.intersp=0.8, seg.len=0,
       horiz=F, col=rgb(0, 0, 0, 80, maxColorValue=255))
dev.off()

