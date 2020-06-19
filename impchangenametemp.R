setwd('C:/Sync/CoolGirl/Fhe/ecosocialDATA/SuperIndex_origin/')

load('UrbanRoadArea.Rdata')
UrbanRoadArea$index = '道路面积.万平方米.市辖区'
save(UrbanRoadArea, file='UrbanRoadArea.Rdata')

load('Bus.Rdata')
Bus$index = '公共汽电车辆数.辆.市辖区'
save(Bus, file='Bus.Rdata')

load('AreaBuilt.Rdata')
AreaBuilt$index = '建成区土地面积.平方公里.市辖区'
save(AreaBuilt, file='AreaBuilt.Rdata')

load('AreaConstruct.Rdata')
AreaConstruct$index = '城市建设用地面积.平方公里.市辖区'
save(AreaConstruct, file='AreaConstruct.Rdata')

load('GreenBuilt.Rdata')
GreenBuilt$index = '建成区绿地覆盖面积.公顷.市辖区'
save(GreenBuilt, file='GreenBuilt.Rdata')

load('Green.Rdata')
Green$index = '绿地覆盖面积.公顷.市辖区'
save(Green, file='Green.Rdata')

load('BusPassenger.Rdata')
BusPassenger$index = '公共汽电车客运总数.万人次.市辖区'
save(BusPassenger, file='BusPassenger.Rdata')

load('Water.Rdata')
Water$index = '供水总量.万吨.市辖区'
save(Water, file='Water.Rdata')

load('Mobile.Rdata')
Mobile$index = '移动电话用户数.平方公里.全市'
save(Mobile, file='Mobile.Rdata')