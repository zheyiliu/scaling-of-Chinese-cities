library(poweRlaw)
data('moby')
#首先建立幂律对象，xmin被设为1，尺度参数被设为空
m_m=displ$new(moby)
m_m$getXmin()#返回预设的Xmin,其值为1

#Xmin与alpha参数的调节方法
#m_m$setXmin(5)
#m_m$setPars(2)

#通过实际分布函数与理论分布之间的距离最小化，求出Xmin
est = estimate_xmin(m_m)
m_m$setXmin(est)
est = estimate_pars(m_m)

plot(m_m)
lines(m_m,col=2,lw=3)

#如何得到图像点的数据
dd = plot(m_m) #返回数据框
head(dd, 3)

##################################################################
#解决Xmin的不确定性：bootstrap方法
#N <- 数据集中的样本数
#for(i in 1:B){
#  sample(,N)
#  estx = estimate_xmin(m_m)
#  m_m$setXmin(estx)
#  estpar = estimate_pars(m_m)
#}
parallel::detectCores()#查看有几个线程
bs = bootstrap(m_m, no_of_sims=200, threads=4)
plot(jitter(bs$bootstraps[,2], factor=1.2), bs$bootstraps[,3])
###################################################################

###################################################################
#是否真的为幂律分布：bootstrap方法
#先计算Xmin与Pars
#ksd=原始数据集的ks距离
#ntail=大于xmin的样本个数
#for(i in 1:B){
#  从二项分布B(n,ntail/n)中抽一个样n1
#  从小于xmin的数中抽n-n1个样
#  从指数为pars的幂律分布中抽n1个样
#  计算ks统计量
#  if ks>ksd P=P+1
#}
#p=P/B
#p值<0.05则拒绝幂律分布
bs_p = bootstrap_p(m_m, no_of_sims=100, threads=4)
