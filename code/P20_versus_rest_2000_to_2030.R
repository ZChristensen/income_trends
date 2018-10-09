#Income gaps
list.of.packages <- c("data.table","readr","stringr","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


#Loading PovcalNet data
if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}
wd = paste0(prefix,"/git/income_trends/")
setwd(wd)


load("E:/git/poverty_trends/data/AGGPovcalScrapeSept2018.RData")


agg_total$ConsumptionFloor = agg_total$povertyLine*(1-(agg_total$p2/agg_total$pg))
agg_total$diff=abs(agg_total$hc-.20)
regional.extpov = subset(agg_total, povertyLine==1.90)
GlobalExtPov = subset(regional.extpov, regionTitle=="World Total")
names(GlobalExtPov)[which(names(GlobalExtPov)=="ConsumptionFloor")] <- "Global.Consumption.Floor"
names(GlobalExtPov)[which(names(GlobalExtPov)=="hc")] <- "Global.Ext.HC"

GlobalExtPov=GlobalExtPov[,c("requestYear","Global.Consumption.Floor","Global.Ext.HC")]
regional.p20 = data.table(agg_total)[,.SD[which.min(diff)],by=.(regionTitle,requestYear)]
WorldP20threshold = subset(regional.p20, regionTitle=="World Total")
WorldP20threshold$P20Threshold = WorldP20threshold$povertyLine
WorldP20threshold2=data.frame(WorldP20threshold)[,c("requestYear","P20Threshold")]


P20main= data.frame(regional.p20)
P20main$population=P20main$population * 1000000
P20main$P20pop = (P20main$hc)*(P20main$population)
P20main$P20average = P20main$povertyLine -((P20main$povertyLine*P20main$pg*P20main$pop)/P20main$P20pop)
P20main$restpop = P20main$population - P20main$P20pop
P20main$restaverage = (((P20main$mean/(365/12))*P20main$population)-(P20main$P20average * P20main$P20pop))/P20main$restpop



world=P20main[which(P20main$regionTitle=="World Total" & P20main$requestYear>=1999),]
P20_1999=world$P20average[which(world$requestYear==1999)]
P20_2015=world$P20average[which(world$requestYear==2015)]
Rest_1999=world$restaverage[which(world$requestYear==1999)]
Rest_2015=world$restaverage[which(world$requestYear==2015)]
P20_growth=(P20_2015-P20_1999)/(2015-1999)
Rest_growth=(Rest_2015-Rest_1999)/(2015-1999)
world$gap=world$restaverage-world$P20average
world=world[,c("requestYear","P20average","restaverage","gap")]
world2030=c("2030",P20_2015+((2030-2015)*P20_growth),Rest_2015+((2030-2015)*Rest_growth),(Rest_2015+((2030-2015)*Rest_growth)-(P20_2015+((2030-2015)*P20_growth))))
world=rbind(world,world2030)            

write.csv(world,"data/P20_Rest_Gap_2030_forecast.csv",row.names = F,na="")
