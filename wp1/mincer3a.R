# mincer3a.R
# A file to generate the graph of returns and years of schooling

library(dplyr)
library(ggplot2)

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





p <- ggplot(obs, aes(x = Timestamp))
p <- p + geom_line(aes(y = air_temp))
p


ggplot(data=hp, aes(x = Year))+
       geom_line(aes(y=r,color="r"))+
       geom_line(aes(y=S*(10/14),color="S"))
                          


p <- ggplot(obs, aes(x = Timestamp))
p <- p + geom_line(aes(y = air_temp, colour = "Temperature"))

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = rel_hum/5, colour = "Humidity"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Air temperature [°C]",
              x = "Date and time",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.8, 0.9))
p




















                          
                          
geom_bar(stat="identity") +
  scale_fill_manual(values = c('grey', 'darkgreen', 'darkgreen')) +
  geom_text(aes(y = edu_ratio, label = edu_ratio, vjust = -0.5), color="black", size = 5) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle('1998')




