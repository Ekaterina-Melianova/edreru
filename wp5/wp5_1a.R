# wp5_1a.R

# Just prelims with the data

options(scipen=999) # to supress scientific notation
Sys.setlocale("LC_ALL", "russian")

library(openxlsx)
library(frequency)
library(ggplot2)
library(tidyverse)
#library(xspliner)

setwd('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Tertiary')

df_col <- read.xlsx("dataframe_colleges.xlsx")
# check for duplicates
df_col2 <- df_col %>% group_by(inn) %>% filter(n()>1)
# there are 16 dupicates 

#we eliminate the duplicates
df_col2 <- df_col[!duplicated(df_col[,1]),]

# Mostly they are коледж (1068) or техникум (925)
blix <-as.data.frame(freq(df_col2$institutionType_name))
blix <- blix[order(as.numeric(blix$x..Freq),decreasing = TRUE),]

# exploring graduate salaries from 2013, 2014, 2015, 2016
# need to convert salaries into one variable
df_col3 <- df_col2 %>% select("graduate_salary_2013",
                              "graduate_salary_2014",
                              "graduate_salary_2015",
                              "graduate_salary_2016") %>%
                       gather("graduate_salary_2013",
                              "graduate_salary_2014",
                              "graduate_salary_2015",
                              "graduate_salary_2016",
                              key="year",value="salaries")

ggplot(data=df_col3, aes(x=year,y=salaries))+
  geom_boxplot()

# exploring first college on website 
df_col3 <- df_col2 %>% filter(region_name=="Республика Хакасия")
df_col3 <- df_col2 %>% filter(inn=="1901022643")

# exploring employment rate
par(mfrow=c(2,2))
fp <- function(x,y,z){hist(x,col=y,xlab=z)
  abline(v=mean(x),col="black",lty=2,lwd=3)}
fp(df_col2$employment_rate_2013,"blue",2013)
fp(df_col2$employment_rate_2014,"yellow",2014)
fp(df_col2$employment_rate_2015,"green",2015)
fp(df_col2$employment_rate_2016,"red",2016)

## Projecting data from four points - heroidc but potentially
## meaningful
par(mfrow=c(2,2))
y <- c(12041,14545,17713,18936)
x <- c(19,20,21,22)
plot(x,y, type='b', col='red')
xspline(x,y, shape=1)


y <- c(12041,14545,17713,18936,19333,23445,22345,18765,17654)
x <- c(19,20,21,22,23:27)
plot(x,y, type='b', col='red')
xspline(x,y, shape=1)


y <- c(12041,14545,17713,18936,19456,22234,21345,17432,16554)
x <- c(19,20,21,22,23:27)
plot(x,y, type='b', col='red')
xspline(x,y, shape=1)

y <- c(12041,14545,17713,18936,19456,18765,16232,12554,11987)
x <- c(19,20,21,22,23:27)
plot(x,y, type='b', col='red')
xspline(x,y, shape=1)

###

op <- par(mfrow=c(1,2))

(a <- spline(x, y, n = 3*length(x), method = "fmm",
       xmin = min(x), xmax = max(x), ties = mean))

plot(a,type='b', col='red')

(a <- splinefun(x, y))

f <- splinefun(x,y)
ls(envir = environment(f))
splinecoef <- get("z", envir = environment(f))
curve(f(x), 19, 23, col = "green", lwd = 1.5)
points(splinecoef, col = "purple", cex = 2)
curve(f(x, deriv=1), 1, 10, col = 2, lwd = 1.5)
curve(f(x, deriv=2), 1, 10, col = 2, lwd = 1.5, n = 401)
curve(f(x, deriv=3), 1, 10, col = 2, lwd = 1.5, n = 401)

par(op)











