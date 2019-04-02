
# 时间序列是将统一统计值按照时间发生的先后顺序来进行排列
# 时间序列分析的主要目的是根据已有数据对未来进行预测。
# 一个稳定的时间序列中常常包含两个部分：有规律的时间序列+噪声。
#所以，以下方法的主要目的就是去过滤噪声值，让我们的时间序列更加的有分析意义。

########### 平稳时间序列 ##########
###平稳：均值和方差为常数，并且具有与时间无关的自协方差。

########### 自回归系数 ############
###生成一个完全随机的时间序列
#example 1
set.seed(123)
yt = rnorm(50,0,1)#正态分布的序列
plot(yt,type='b');abline(h = 0)#plot绘制序列点，abline绘制中间那条直线，视觉上好一些。

###定义一个滞后函数，5,4,3,2,1变成NAN,5,4,3,2
L_ <- function(x,lag = 1,na.is = TRUE){#delay ont step function
  if(na.is)c(rep(NA,lag),x[1:length(x)-lag])
  else x[1:(length(x) - lag)]
}

###得到一个比刚才完全随机的时间序列滞后1个时间步的时间序列
#example 2
yt_1 = L_ (yt,na.is = T)
plot(yt,yt_1);abline(h = 0)
###原数据和时滞为1的数据之间的相关系数就是一阶自回归系数
cor(yt,yt_1,"complete")

###auto correlation function，自回归系数函数，就是不同"滞后阶"下的回归系数。
acf(yt)

###自回归系数的假设检验：是否至少存在某个cor()≠0，通常有两种：Box-Pierce 与 Box-Ljung（更适合小样本）
### p>0.05，不存在显著的自回归关系
Box.test(yt)
Box.test(yt,type = 'Ljung-Box')

############# AR模型（自回归模型） ###############
###生成一个一阶自回归序列，残差是白噪音，即正态分布
#example 5
set.seed(1234)#设置随机种子
n = 50#序列数量
y1 = rep(0,n);#初始化y1时间序列
for(t in 2:n){#根据自回归方程计算y1序列
  y1[t] = 0.8*y1[t-1] + rnorm(1)
}
plot(y1,type = 'o')#绘制序列图

###R语言内置的函数快速完成回归序列的生成
#example 6
y1  = arima.sim(n = 50,list(ar = 0.8))#R中自带函数，list中为各阶的自回归系数，由于我们只有一阶自回归，所以只有一个0.8
plot(y1,type = 'o')

acf(y1) ###超过蓝色虚线的自回归系数通常被认为是显著的
pacf(y1)

###AR模型估计
#其实就是估计线性回归系数，yulr-walker方法精读高于ols
#example 7
ar(y1,method = "yule-walker")
ar(y1,method = "ols")

###一个2阶自回归模型作为例子来演示R语言自带函数的demo
y2 = arima.sim(n = 100,list(ar = c(0.7,-0.5))) #产生2阶自回归序列
plot(y2,type = 'o')
acf(y2)
pacf(y2)$ac[1:5] #要区别acf与pacf函数，pacf用于多阶的AR，消除了yt-1对yt-2的间接影响之后来计算yt与yt-2之间的相关性
arima(y2,order = c(2,0,0)) #估计AR模型，加入include.mean = F，那么就不会有均值项，也就是显示中的intercept项


################## MA模型（移动平均模型）###################
# AR：yt用不同阶滞后的序列拟合，MA：yt用不同阶滞后的白噪音拟合，yt是白噪音的线性加权
#example 7
y3 = arima.sim(n=100,list(ma = 0.8))
plot(y3,type = 'o')
acf(y3)
pacf(y3)


################## ARMA模型（AR和MA的混合模型）###################
set.seed(12345)
y4 = arima.sim(n=100,list(ar = 0.8,ma = 0.6))
plot(y4,type = 'o')
###forecast包中的auto函数自动给出模型参数辨识
require(forecast)
auto.arima(y4)




################## 非平稳序列的平稳方法--差分 ####################
# 差分，就是后一时间点的值减去当前时间点，也就是yt-yt-1。
###生成一个自回归系数之和为1的非平稳时间序列
#example 10
set.seed(12345)
ut = rnorm(50,0,1.5)
xt = cumsum(ut)
plot(xt,type = 'o');abline(h = 0)

###两次差分后看上去就变得平稳了
#如果一个序列经过d次差分之后变成平稳序列，则称之为d阶单整序列
plot(diff(xt,d = 1),type = 'o');abline(h = 0)
plot(diff(xt,d = 2),type = 'o');abline(h = 0)

###根据单位根过程可以对平稳性进行假设检验，譬如DF与ADF检验
#Dickey-Fuller检验是测试一个自回归模型是否存在单位根:
#X(t) - X(t-1) = (Rho - 1) X(t - 1) + Er(t), 检验Rho–1=0是否显著，若显著等于0，则该时间序列并不平稳。 
#example 13
require(tseries)
adf.test(xt)
adf.test(diff(xt))
adf.test(diff(xt,d=2))

################# ARIMA模型 #################
# ARIMA模型能够用于齐次非平稳时间序列的分析，这里的齐次指的是原本不平稳的时间序列经过d次差分后成为平稳时间序列。
# 差分自回归移动平均模型写成ARIMA(p,d,q)。p代表自回归阶数；d代表差分次数；q代表移动平均阶数。


################# 协整 ###################
#平稳序列的线性组合还是平稳序列；平稳序列和一阶单整序列的线性组合是一阶单整，
#但是，两个一阶单整序列的线性组合不一定是一阶单整。于是我们得到了另外一种获得平稳序列的方法，就是两个一阶单整的序列的线性组合。
#ut=m+a*xt+b*yt，如果ut是平稳序列，xt与yt都是一阶单整序列，那么我们就称xt与yt是协整的，（a，b）称为协整向量。

###先构造两个自回归系数之和为1的非平稳时间序列
#example 14
set.seed(123)
n=100
u1 = rnorm(n)
xt = cumsum(u1)
u2 = rnorm(n)
yt = 0.6*xt + u2
plot(xt,type = 'l',ylab = '')
lines(yt,lty = 2)
legend('topleft',c('xt','yt'),lty = 1:2,bty = 'n')

###我们对xt于yt做一个回归，然后残差作为一个时间序列来分析，残差就是ut，adf检验可见ut是平稳的
#example 15
fm = lm(yt~xt)
et = resid(fm)
plot(et,type = 'o');abline(h=0)
adf.test(et)


################## 格兰杰因果关系检验 ###################
#example 16
grangertest(xt~yt)
grangertest(yt~xt)


################## 实证检验步骤 ##################
#先做单位根检验，看变量序列是否平稳序列
#若平稳，可构造回归模型等经典计量经济学模型
#若非平稳，进行差分，当进行到第i次差分时序列平稳，则服从i阶单整（注意趋势、截距不同情况选择，根据P值和原假设判定）。
#若所有检验序列均服从同阶单整，可构造VAR模型，做协整检验（注意滞后期的选择），判断模型内部变量间是否存在协整关系，即是否存在长期均衡关系。
#如果有，则可以构造VEC模型或者进行Granger因果检验（协整后对残差进行的），检验变量之间“谁引起谁变化”，即因果关系。

