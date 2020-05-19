# telez1a.R

# Graph in telezhkina presentation reproduced in ggplot2

library(reshape2)
library(ggplot2)
library(dplyr)

# Input the data
cards <-"YEAR en_m en_f en_all 
2000 19.7 26.4 23.0
2001  NA  NA   25.8
2002  NA  NA   27.8
2003  NA  NA   29.7
2004  NA  NA   31.2
2005 26.8 37.9 32.3
2006 27.9 39.5 33.6
2007 28.4 40.4 34.4
2008 29.5 41.2 35.3
2009 30.3 41.7 35.9
2010 29.9 41.1 35.4
2011 29.5 39.1 34.2
2012 29.8 38.2 33.9
2013 30.4 37.2 33.7
2014 29.6 36.2 32.9
2015 29.3 35.1 32.1
2016 28.9 34.8 31.8
2017 29.2 35.3 32.1
2018 29.8 35.7 32.7
  "
telez1a <-read.table(textConnection(cards),header=TRUE)

closeAllConnections()

telez1a$YEAR <- factor(telez1a$YEAR)

telez1b <- melt(data=telez1a, id.var="YEAR")

telez1b$variable <- factor(telez1b$variable,labels=c("males","females","all"))



ggplot(data=telez1b , aes(YEAR, value, group = variable, 
                          color = variable,
                            shape = variable)) +
  geom_point(aes(shape = variable), size = 4) +
  geom_smooth(se = F, method = 'loess') +
  geom_line() +
#  scale_y_continuous(limits = c(-1, 30), breaks = seq(0, 30, 2)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key = element_rect(size = 16)) +
  scale_color_manual(values = c("darkblue","darkmagenta","honeydew4")) +
  #scale_shape_manual(values=c(2,4)) +
#  scale_x_discrete(breaks = c(2000,20002,2004,2006,2008,2010,2012,2014,2016,2018)) +
  ylab("Percentage of 17-25 year olds enrolled") +
  xlab("Year")+
  annotate("text", x = 14, y = 21, label = "Source: Telezkhina 2019; HSE Education Year Book")

# Saving
ggsave("telez1a.png", width = 7, height = 7,
       units = "in")