
library(ggplot2)

setwd('C:/Sync/CoolGirl/Fhe/Results/3powerlaw/pars/')
for (rdat in dir()){load(rdat)}
rangeStat = '市辖区'

#dfname='GDP'
#dfname='Area'
#dfname='CityRoadArea'
#dfname='HospitalBerth'
dfname='Green'


#############################################
taus = get(paste0(dfname,'conpl'))[,c(5,2,1)]
taus$name = 'tau'
taus$yIndex = gsub('市辖区', '', taus$yIndex)
alphas = get('POPconpl')[,c(5,2,1)]
alphas$name = 'alpha'
betas = sumlmHorizontal[which(sumlmHorizontal$yIndex==as.character(taus$yIndex[1])),c('year','Beta','yIndex')]
betas$name = 'beta'
colnames(betas)[2] = 'Alpha'
df0 = rbind(taus, alphas, betas)
df0 = na.omit(df0)

df = merge(alphas, taus, by='year')
df1 = merge(df, betas, by='year')
#df1 = df1[which(df1$year!=1997),]

POPalpha = df1$Alpha.x
Ytau = df1$Alpha.y
Ybeta = df1$Alpha

estYbeta = (POPalpha - 1) / (Ytau - 1)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
normYtau = range01(1/Ytau)
normPOPalpha = range01(POPalpha)
normYbeta = range01(Ybeta)
normestYbeta = range01(estYbeta)

#############################################
f = lm(Ybeta~estYbeta-1)
ff = summary(f)


png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/3PowerLaw/figs/',dfname, rangeStat,'αβτ.png'),
    width=1600,height=540*2, units='px',res=200)

par(mfrow=c(2,1),mar=c(3,4,1,1))

### 绘制β和β'的关系
plot(estYbeta, Ybeta, 
     ann=F, xaxt="n",yaxt="n", mgp=c(2,0,0),
     col=rgb(0, 0, 0, 80, maxColorValue=255),pch=19)
axis(1, las=0, tck=0.03, mgp=c(0,0.3,0))
axis(2, las=1, tck=0.03,mgp=c(0,0.3,0))
title(xlab="Analytical exponent (β')", mgp=c(1.5,0,0),cex.lab=1.2)
title(ylab="Empirical exponent (β)", mgp=c(2.5,0,0),cex.lab=1.2)
#abline(a=ff$coefficients[1,1],b=ff$coefficients[2,1],lwd=2)
abline(a=0,b=ff$coefficients[1,1],lwd=2)
legend("bottomright", c(paste0('R-square=', round(ff$r.squared,3)), 
                        paste0('λ =', round(ff$coefficients[1,1],3)), 
                     paste0('P-value=', round(ff$coefficients[1,4],3))),
       title=paste0(dfname,":"),title.adj=0.2,x.intersp=0.4,y.intersp=0.8, seg.len=0,
       horiz=F, col=rgb(0, 0, 0, 80, maxColorValue=255))

# ### 绘制α和τ的时间动态图
# #col2rgb('#FF6666')
# col1 = rgb(0, 150, 0, 100, maxColorValue=255)
# col2 = rgb(150, 0, 0, 100, maxColorValue=255)
# #normPOPalpha, normYtau
# plot(df1$year-1, POPalpha,
#      type='o',lwd=2, col=col1, pch=19,
#      ann=F, xaxt="n",yaxt="n", mgp=c(2,0,0),
#      ylim=c(min(Ytau,POPalpha),max(Ytau,POPalpha))) #绿色
# lines(df1$year-1, Ytau,type='o',lwd=2, col=col2, pch=19) #橙色
# axis(1, las=0, tck=0.03,  at=1984:2016, labels=1984:2016,mgp=c(0,0.3,0))
# axis(2, las=1, tck=0.03, mgp=c(0,0.3,0))
# title(xlab="Year (1984-2016)", mgp=c(1.5,0,0),cex.lab=1.2)
# title(ylab="Value", mgp=c(2.5,0,0),cex.lab=1.2)
# legend("topright", horiz=F, col=c(col1,col2), lty=c(1,1), lwd=2, pch=c(19,19),
#        title=paste0(dfname,":"),title.adj=0.2,x.intersp=0.4,y.intersp=0.8, seg.len=1.5,
#        c('α (Population)', paste0('τ (', dfname,')')))

### 绘制标准化的α, τ, β的时间动态图
plot((df1$year)-1, normPOPalpha,type='n',lwd=2, col=1, ylim=c(0,1.2),
     ann=F, xaxt="n",yaxt="n", mgp=c(2,0,0))
lines((df1$year)-1, normYbeta,lty=1,lwd=2)
lines((df1$year)-1, normPOPalpha,lty=3,lwd=2, col=2)#绿色
lines((df1$year)-1, normYtau,lty=3,lwd=2, col=4) #橙色
axis(1, las=0, tck=0.03,  at=1984:2016, labels=1984:2016, mgp=c(0,0.3,0))
axis(2, las=1, tck=0.03, mgp=c(0,0.3,0))
title(xlab="Year (1984-2016)", mgp=c(1.5,0,0),cex.lab=1.2)
title(ylab="Normalized value", mgp=c(2.5,0,0),cex.lab=1.2)
legend("topright", horiz=T, col=c(1,2,4), lty=c(1,3,3), lwd=2,
       title=paste0(dfname,":"),title.adj=0.2,x.intersp=0.4,y.intersp=0.8, seg.len=1.5,
       c('β','α', '1/τ'))
dev.off()

# ### 绘制实际值的α, τ, β的时间动态图
# p = ggplot(data=df0, aes(x=year-1, y=Alpha, color=name,shape=name)) +
#   geom_line(size=1) + geom_point(size=2) +
#   labs(x = 'Year(1984-2016)', y='Value') +
#   theme(
#     text = element_text(size=18),
#     panel.background = element_rect(fill = "transparent",colour = 'black'),
#     panel.grid.minor = element_line(color='azure3'),
#     panel.grid.major = element_line(color='azure3'),
#     #plot.background = element_rect(fill = "transparent",colour = NA),
#     legend.title=element_blank()
#   )
# print(p)

