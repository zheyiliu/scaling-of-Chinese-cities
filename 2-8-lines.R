N = 1:100
png(file='C:/Sync/CoolGirl/Fhe/Pre/line.png',width=20,height=20, units='cm',res=150)
par(mfrow=c(2,2), mar=c(6,4,2,4))

#1
plot(N,N, type='n', xaxt="n",yaxt="n",ann=F)
title(xlab="log(N)", mgp=c(0.5,0,0),cex.lab=2)
title(ylab="log(Y)", mgp=c(0.4,0,0),cex.lab=2)
abline(a=0,b=1,lwd=4,col=2)
abline(a=20, b=0.5, lwd=4,col=2, lty=3)

#2
plot(N,N, type='n', xaxt="n",yaxt="n",ann=F)
title(xlab="log(N)", mgp=c(0.5,0,0),cex.lab=2)
title(ylab="log(Y)", mgp=c(0.4,0,0),cex.lab=2)
abline(a=0,b=1,lwd=4,col=2)
abline(a=20, b=1.8, lwd=4,col=2, lty=3)

#3
plot(N,N, type='n', xaxt="n",yaxt="n",ann=F)
title(xlab="log(N)", mgp=c(0.5,0,0),cex.lab=2)
title(ylab="log(Y)", mgp=c(0.4,0,0),cex.lab=2)
abline(a=0,b=1,lwd=4,col=2)
abline(a=-20, b=0.5, lwd=4,col=2, lty=3)

#4
plot(N,N, type='n', xaxt="n",yaxt="n",ann=F)
title(xlab="log(N)", mgp=c(0.5,0,0),cex.lab=2)
title(ylab="log(Y)", mgp=c(0.4,0,0),cex.lab=2)
abline(a=0,b=1,lwd=4,col=2)
abline(a=-20, b=1.8, lwd=4,col=2, lty=3)



dev.off()