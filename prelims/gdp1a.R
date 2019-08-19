# gdp1a.R
# File to generate graph of GDP evolution
# Also GINI 1996 to 2015
options(scipen=999) # to supress scientific notation

gdp89_19 <- read.csv("C:/Country/Russia/Data/Bin/gdp89_18.csv")
gdp89_19$GDP_mn=(gdp89_19$GDP)/1000000
gdp89_19$GDP_tn=(gdp89_19$GDP)/1000000000000

library(ggplot2)

ggplot(data=gdp89_19,aes(x=Year,y=GDP_tn)) + 
  geom_line(color="turquoise",size=1.15) +
 ylim(c(0.75,1.75))+
 scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015,2018))+
  ylab("GDP in constant 2010 trillion dollars")+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.text = element_text(color="blue",size=14))

gini96_15 <- read.csv("C:/Country/Russia/Data/Bin/gini96_15.csv")

ggplot(data=gini96_15,aes(x=Year,y=GINI))+
  geom_line(size=1.15) +
  scale_x_continuous(limits=c(2008,2015)) +
  scale_y_continuous(limits=c(37,43)) +
  stat_smooth(method="lm",se=FALSE,size=1.15,linetype="longdash")+
  ylab("GINI Index from World Bank")+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.text = element_text(color="blue",size=14))
  
  
