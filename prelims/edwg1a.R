# edwg1a.R
# A file to generate a graph of median lifecycle earnings based on RLMS

# Data: joined base

memory.limit(1e10)
dat_joined <- foreign::read.spss(file="USER_RLMS-HSE_IND_1994_2017_v2.sav",
                                 use.value.labels = F,
                                 use.missings=TRUE,
                                 to.data.frame = TRUE)

# Recoding education 
# 4 categories:
# 0 - lower than secondary
# 1 - secondary 
# 2 - specialized / vocational
# 3 - higher and above

descr::freq(dat_joined$EDUC)
dat_joined$edu_4_cat <- car::recode(dat_joined$EDUC, "0:7=0; 8:9=1; 10:11=2; 12=1; 13=2;
                                    14=1; 15:17=2; 18=2; 19:20=3; 21:23=3")
descr::freq(dat_joined$edu_4_cat)

# Filtering age

dat_joined_adlt <- dat_joined[dat_joined$AGE>=25&dat_joined$AGE<65,]
descr::freq(dat_joined_adlt$AGE)

# Selecting years 1998, 2008, 2017

dat_adlt_4w <- dat_joined_adlt[dat_joined_adlt$YEAR==1998|
                               dat_joined_adlt$YEAR==2008|
                               dat_joined_adlt$YEAR==2017,]

# Filtering 3 education levels

dat_adlt_4w <- dat_adlt_4w[dat_adlt_4w$edu_4_cat>0,]
descr::freq(dat_adlt_4w$edu_4_cat)

# Filtering employed

dat_adlt_4w <- dat_adlt_4w[is.na(dat_adlt_4w$J13.2)==F,]
hist(dat_adlt_4w$J13.2)

# Summarizing data for the graph:
# median wage by year by age for education levels

library(dplyr)

dat_wgdif <- dat_adlt_4w %>%
  group_by(edu_4_cat, AGE, YEAR) %>%
  summarise (med_wg = median(J13.2))

# Education as factor

dat_wgdif$edu_4_cat <- factor(dat_wgdif$edu_4_cat,
                            levels=c(1,2,3),
                            labels=c("Secondary",
                                     "Specialized / vocational",
                                     "Higher"))
# Ploting 

library(ggplot2)
library(lemon)
library(RColorBrewer)
library(gridExtra)

col <- brewer.pal(11, 'RdYlGn')

# 1998

p1 <- ggplot(data=dat_wgdif[dat_wgdif$YEAR==1998,],
       aes(x=AGE, y=med_wg, colour=edu_4_cat, linetype=edu_4_cat)) +
  geom_smooth(method = 'loess', se=F, size=1.8) +
  scale_linetype_manual(values = c("dashed", "solid", "solid")) +
  scale_colour_manual(values = c("darkgray", col[2], col[10])) +
  theme_bw() +
  theme(axis.text.y = element_text(size=14, face="bold", color="black"),
        axis.text.x = element_text(size=14, face="bold", color="black"),
        axis.title = element_text(size=14, face="bold"),
        axis.ticks = element_line(color="black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=17, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5, vjust = 1)) + 
  scale_x_continuous(breaks=seq(25, 64, 5)) +
  scale_y_continuous(limits = c(0, 1200))  +
  ylab("Wage, RUB per mounth") +
  xlab("Age") + ggtitle("1998") +
  theme(plot.margin = unit(c(2.5,1,1,1), "cm"))

# 2008

p2 <- ggplot(data=dat_wgdif[dat_wgdif$YEAR==2008,],
       aes(x=AGE, y=med_wg, colour=edu_4_cat, linetype=edu_4_cat)) +
  geom_smooth(method = 'loess', se=F, size=1.5) +
  scale_linetype_manual(values = c("dashed", "solid", "solid")) +
  scale_colour_manual(values = c("darkgray", col[2], col[10])) +
  theme_bw() +
  theme(axis.text.y = element_text(size=14, face="bold", color="black"),
        axis.text.x = element_text(size=14, face="bold", color="black"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y =element_blank(),
        axis.ticks = element_line(color="black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=17, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5)) + 
  scale_x_continuous(breaks=seq(25, 64, 5)) +
  scale_y_continuous(limits = c(0, 15000))  +
  ylab("Wage, RUB per mounth") +
  xlab("Age") + ggtitle("2008") +
  theme(plot.margin = unit(c(2.5,1,1,1), "cm"))

# 2017
  
p3 <- ggplot(data=dat_wgdif[dat_wgdif$YEAR==2017,],
       aes(x=AGE, y=med_wg, colour=edu_4_cat, linetype=edu_4_cat)) +
  geom_smooth(method = 'loess', se=F, size=1.5) +
  scale_linetype_manual(values = c("dashed", "solid", "solid")) +
  scale_colour_manual(values = c("darkgray", col[2], col[10])) +
  theme_bw() +
  theme(axis.text.y = element_text(size=14, face="bold", color="black"),
        axis.text.x = element_text(size=14, face="bold", color="black"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y =element_blank(),
        axis.ticks = element_line(color="black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=17, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5)) + 
  scale_x_continuous(breaks=seq(25, 64, 5)) +
  scale_y_continuous(limits = c(0, 30000))  +
    ylab("Wage, RUB per mounth") +
  xlab("Age") + ggtitle("2017") +
  theme(plot.margin = unit(c(2.5,1,1,1), "cm")) 

# grid_arrange_shared_legend(p1, p2, p3, ncol = 3, nrow = 1)

library(grid)
library(gridExtra)

mylegend <- g_legend(p3) 
# Zoom to see the graph
grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         p2 + theme(legend.position="none"),
                         p3 + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2, heights = c(2,0.7),
             top = textGrob("Median Lifecycle Earnings, source: RLMS",
                            gp=gpar(fontsize=25, hjust = 0.5, font=1)))

