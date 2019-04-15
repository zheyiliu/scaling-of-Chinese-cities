RB0 = read.csv(file='C:/Sync/CoolGirl/Fhe/Results/OLS_R/sumlmHorizontal_Districts.csv',header=T)
PB0 = read.csv(file='C:/Sync/CoolGirl/Fhe/Results/OLS_130/sumlmHorizontal_Districts.csv',header=T)
RB = RB0[which(RB0$Observation > 170),c(2,3,4,12)]
PB = PB0[which(PB0$year %in% unique(RB0$year) & PB0$Observation > 170),c(2,3,4,12)]
RB$R = 'Resident'
PB$R = 'Registered'
foo = merge(RB,PB,by=c('yIndex','year'),all=T)
foo = foo[which(foo$yIndex!='Passenger'),]

col = c("#619CFF", "grey52", "#00BA38", "#F8766D")
economoicdf = c('Book','BusPassenger','DepositHousehold','GDP','Loan','Salary','PostTele','WasteWater','Bus')
infrasdf = c('CityRoadArea','Hospital','School','Cinema','Green','HospitalBerth','GreenBuilt')
needdf = c('Electricity','Water')
areadf = c('Area', 'AreaBuilt')

foo$type=NA
foo[foo$yIndex %in% economoicdf,]$type = 'socio-economic'
foo[foo$yIndex %in% infrasdf,]$type = 'infrastructure'
foo[foo$yIndex %in% needdf,]$type = 'individual need'
foo[foo$yIndex %in% areadf,]$type = 'Area'

a = lm(foo$Beta.x~foo$Beta.y-1)
aa = summary(a)

png(filename=paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_R/Registered&Resident2.png'),
    width=20,height=15, units='cm',res=150)
p = ggplot(data=foo, aes(x=Beta.y, y=Beta.x, color=type)) + 
  scale_colour_manual(values = col) +
  geom_abline(size=1,col='grey') +
  geom_abline(slope=aa$coefficients[1,1],size=1) +
  geom_point(size = 3.5) +
  labs(x='β (Registered)', y='β (Resident)') +
  theme(text = element_text(size=18),
        legend.title=element_blank(),
        panel.grid =element_blank(),                                             #默认主题
        panel.background = element_rect(fill = "transparent",colour = 'black'),  #默认主题
        legend.key = element_rect(fill = "transparent", color = "transparent"),  #默认主题
        #axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 0.7, vjust = 0.5))
print(p)
dev.off()



