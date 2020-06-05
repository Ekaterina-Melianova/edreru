# mincer3a.R
# A file to generate the graph of returns and years of schooling

library(dplyr)
library(ggplot2)
library(reshape2)


wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1"
setwd(wd) 

hp <-

read.table(header = TRUE, text = "
Year	S	     r
1994	12.7	8.4
1995	12.7	7.8
1996	12.8	7.1
1998	12.7	8.8
2000	12.7	8.7
2001	12.8	9.3
2002	12.8	9.2
2003	12.8	9.3
2004	12.8	8.5
2005	12.8	8.1
2006	12.8	8.0
2007	12.8	6.6
2008	12.9	7.9
2009	12.9	7.6
2010	13.0	7.1
2011	13.0	6.7
2012	13.0	6.1
2013	13.1	6.5
2014	13.1	6.8
2015	13.2	5.7
2016	13.3	6.1
2017	13.3	5.4
2018	13.3	5.4
"
)

hp2 <- reshape2::melt(hp,id.vars=c("Year")) %>% mutate(Year=as.factor(Year))
str(hp2)

hp2$fvar[hp2$variable=="S"] <- "Number of years of Schooling"
hp2$fvar[hp2$variable=="r"] <- "Rate of Returns in percentage"

hp2$fvar <- factor(hp2$fvar,levels=c("Number of years of Schooling",
                                     "Rate of Returns in percentage"))

xb <- seq(1994,2018, by = 2) # has no effect as we are using facets

ggplot(data=hp2,aes(x=Year,y=value,group=fvar,color=fvar))+
  geom_line()+
  facet_wrap(~fvar,scales="free")+
  scale_x_discrete(xb)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1, size = 8),
        axis.text.y = element_text(size = 14, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+ 
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  scale_color_manual(values = c("purple", "brown"))+
  theme(legend.position = "none")
ggsave("hp_rs.png", width = 10, height = 7,
       units = "in")
      