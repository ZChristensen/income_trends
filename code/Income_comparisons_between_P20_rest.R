#Income gaps
list.of.packages <- c("data.table","excel.link","ggplot2","readr","stringr","scales","Hmisc")
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


P20main <- read.csv("data/P20incometrends.csv")
P20main$Consumption=NA
P20main$Consumption[which(P20main$DataType=="X")]="Consumption"
P20main$Consumption[which(P20main$DataType=="Y")]="Income"



P20main2=P20main[,c(
                    "CountryName"
                    ,"RequestYear"
                    ,"P20average"
                    ,"restaverage"
                    ,"P20Headcount"
                    ,"P20pop"
                    ,"restpop"
                    ,"ExtPovHC"
                    ,"RegionCode"
                    ,"Consumption"
                    ,"NP20average"
                    ,"Nrestaverage"
)]

names(P20main2)[which(names(P20main2)=="P20Headcount")] <- "Percent.in.P20"
names(P20main2)[which(names(P20main2)=="ExtPovHC")] <- "Percent.in.Extreme.Poverty"
names(P20main2)[which(names(P20main2)=="RequestYear")] <- "Year"


regions=unique(P20main$RegionCode)


for(region in regions){
  print(region)
  filenam=paste0("data/P20versusRestIncomeTrends",region,".xlsx")
  P20main3=subset(P20main2, RegionCode==region)
  xl.workbook.add()
  countries=unique(P20main3$CountryName)
  for(country in countries){
    new=subset(P20main3, CountryName==country)
    maximums=max(new$restaverage,new$Nrestaverage)+(.05*(max(new$restaverage,new$Nrestaverage)))
    Consumption=unique(new$Consumption)
    new=new[,c("CountryName","P20average","restaverage","Year")]
    names(new)=c("CountryName","P20","Rest of population","Year")
    new.m=melt(new,id.vars=c("CountryName","Year"))
    title=paste(country, "\n",Consumption,"Trends")
    p=ggplot(data=new.m, aes(x=Year, y=value,group=variable,colour=variable))+
      geom_line()+
      labs(title=title,y=paste("Ave. daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year",colour="")+
      theme_classic()+
      theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+
      expand_limits(y=0)+ 
      scale_y_continuous(expand=c(0,0),limits=c(0,maximums),labels=dollar)
      # coord_cartesian(ylim=c(0,max))
    print(p)
    y.plot=current.graphics()
    xl.sheet.add(substr(country,1,30))
    xl[a1]=y.plot
    xl[i1]=t(names(new))
    xl[i2]=new
    
  }
  xl.sheet.delete("Sheet1")
  xl.workbook.save(filenam)
  xl.workbook.close()
}

for(region in regions){
  print(region)
  filenam=paste0("data/NP20versusRestIncomeTrends",region,".xlsx")
  P20main3=subset(P20main2, RegionCode==region)
  xl.workbook.add()
  countries=unique(P20main3$CountryName)
  for(country in countries){
    new=subset(P20main3, CountryName==country)
    max=max(new$restaverage,new$Nrestaverage)+(.05*(max(new$restaverage,new$Nrestaverage)))
    Consumption=unique(new$Consumption)
    new=new[,c("CountryName","NP20average","Nrestaverage","Year")]
    names(new)=c("CountryName","National P20","Rest of population","Year")
    new.m=melt(new,id.vars=c("CountryName","Year"))
    title=paste(country, "\n",Consumption,"Trends")
    # p=ggplot(data=new, aes(x=Year, y=Nrestaverage))+ geom_line(aes(y=NP20average, colour="National P20"))+ geom_line(aes(y=Nrestaverage, colour="Rest"))+labs(title=title,y=paste("Average daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year")+theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+scale_y_continuous(labels=dollar)
    p=ggplot(data=new.m, aes(x=Year, y=value,group=variable,colour=variable))+geom_line()+
      labs(title=title,y=paste("Ave. daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year",colour="")+
      theme_classic()+theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+
      expand_limits(y=0)+ 
      scale_y_continuous(expand=c(0,0),limits=c(0,max),labels=dollar)
    # +coord_cartesian(ylim=c(0,max))
    print(p)
    y.plot=current.graphics()
    xl.sheet.add(substr(country,1,30))
    xl[a1]=y.plot
    xl[i1]=t(names(new))
    xl[i2]=new
    
  }
  xl.sheet.delete("Sheet1")
  xl.workbook.save(filenam)
  xl.workbook.close()
}

#regions
for(region in regions){
  print(region)
  filenam=paste0("data/NP20versusRestIncomeTrends",region,".xlsx")
  P20main3=subset(P20main2, RegionCode==region)
  xl.workbook.add()
  countries=unique(P20main3$CountryName)
  for(country in countries){
    new=subset(P20main3, CountryName==country)
    maximums=max(new$restaverage,new$Nrestaverage)+(.05*(max(new$restaverage,new$Nrestaverage)))
    Consumption=unique(new$Consumption)
    new=new[,c("CountryName","NP20average","Nrestaverage","Year")]
    names(new)=c("CountryName","National P20","Rest of population","Year")
    new.m=melt(new,id.vars=c("CountryName","Year"))
    title=paste(country, "\n",Consumption,"Trends")
    # p=ggplot(data=new, aes(x=Year, y=Nrestaverage))+ geom_line(aes(y=NP20average, colour="National P20"))+ geom_line(aes(y=Nrestaverage, colour="Rest"))+labs(title=title,y=paste("Average daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year")+theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+scale_y_continuous(labels=dollar)
    p=ggplot(data=new.m, aes(x=Year, y=value,group=variable,colour=variable))+geom_line()+
      labs(title=title,y=paste("Ave. daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year",colour="")+
      theme_classic()+theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+
      expand_limits(y=0)+ 
      scale_y_continuous(expand=c(0,0),limits=c(0,maximums),labels=dollar)
    # +coord_cartesian(ylim=c(0,max))
    print(p)
    y.plot=current.graphics()
    xl.sheet.add(substr(country,1,30))
    xl[a1]=y.plot
    xl[i1]=t(names(new))
    xl[i2]=new
    
  }
  xl.sheet.delete("Sheet1")
  xl.workbook.save(filenam)
  xl.workbook.close()
}



##Regions
#Wealth Graph for App
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

regions=unique(P20main$regionCID)
countries=unique(P20main$regionTitle)
names(P20main)[2]="Year"


  filenam="data/P20versusRestIncomeTrendsREG.xlsx"
   xl.workbook.add()
 
  for(country in countries){
    new=subset(P20main, regionTitle==country)
    maximums=max(new$restaverage,new$Nrestaverage)+(.05*(max(new$restaverage,new$Nrestaverage)))
    Consumption="Consumption"
    new=new[,c("regionTitle","P20average","restaverage","Year")]
    names(new)=c("regionTitle","P20","Rest of population","Year")
    new.m=melt(new,id.vars=c("regionTitle","Year"))
    title=paste(country, "\n","Trends")
    p=ggplot(data=new.m, aes(x=Year, y=value,group=variable,colour=variable))+
      geom_line()+
      labs(title=title,y=paste("Ave. daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year",colour="")+
      theme_classic()+
      theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+
      expand_limits(y=0)+
      scale_y_continuous(expand=c(0,0),limits=c(0,max),labels=dollar)
    # coord_cartesian(ylim=c(0,max))
    print(p)
    y.plot=current.graphics()
    xl.sheet.add(substr(country,1,30))
    xl[a1]=y.plot
    xl[i1]=t(names(new))
    xl[i2]=new

  }
  xl.workbook.save(filenam)
  xl.workbook.close()



