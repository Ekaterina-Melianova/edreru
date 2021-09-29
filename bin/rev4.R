# rev4.R

library(dplyr)
library(foreign)
library(ggplot2)

# Also from Rosstat


rst_19 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_19i.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_18 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_18.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_17 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_17.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_16 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_16.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_15 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_15.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_14 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_14.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)


df_19 <- rst_19 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2019)
df_18 <- rst_18 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2018)
df_17 <- rst_17 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2017)
df_16 <- rst_16 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2016)
df_15 <- rst_15 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2015)
df_14 <- rst_14 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2014)


#  Adjust for the prices in 2016
df_14$R_DEN <- df_14$R_DEN * 1.237
df_15$R_DEN <- df_15$R_DEN * 1.071
df_16$R_DEN <- df_16$R_DEN * 1
df_17$R_DEN <- df_17$R_DEN * 0.96
df_18$R_DEN <- df_18$R_DEN * 0.94
df_19$R_DEN <- df_19$R_DEN * 0.90


df_ <- rbind(df_14, df_15, df_16, df_17, df_18, df_19)



# No Filtering age
# df <- df_[df_$H01_02 >= 22 & df_$H01_02 < 65,]

# Filtering employed
df <- df_[!is.na(df_$VZR_RAB),]

# Education 

# 4 categories:
# 0 - lower than secondary
# 1 - secondary 
# 2 - specialized / vocational
# 3 - higher and above

df$edu_4 <- car::recode(df$I01_10, "9=0; 7:8=1; 5:6=2; 1:4=3")


# Filtering 3 education levels
df <- df[df$edu_4>0,]

# Education as factor
df$edu_4 <- factor(df$edu_4, levels=c(1,2,3),
                   labels=c("Secondary",
                            "Vocational",
                            "Higher"))

# Wage
df$wage <- df$R_DEN/12

# Filtering wage > 0 
df <- df %>%
  filter(wage >0)

# Socio-demographics
# Gender
df$female[df$H01_01==2] <- 1
df$female[df$H01_01==1] <- 0


# Experience (naive)
df$edu_yrs <- car::recode(df$I01_10, "1=20; 2=17; 3=16; 4=14; 5=12;
                            6=11; 7=11; 8=9")
df$exper <- df$H01_02 - df$edu_yrs - 6
df$exper <- ifelse(df$exper < 0, 0, df$exper)

df_rosstat <- df


###
# Construct age earnings profiles by age 
z19 <- df_rosstat %>% filter(YEAR==2019 & H01_02 >= 22 & H01_02 <=65 &edu_4=="Higher") 

ggplot(data=z19,aes(x=H01_02,y=R_DEN)) + geom_smooth(method = "loess")


res_ <- coef(lm(R_DEN ~ poly(H01_02, 2,raw=TRUE), data = z19)) %>% as.vector()


m <- summary(lm(R_DEN ~ poly(H01_02, 2,raw=TRUE), data = z19))



res_ <- as.vector(m$coefficients[,1])
sres_ <- as.vector(m$coefficients[,2])

xname <- "x"
yname <- "y"
y2a <- "y2a"

(x <- seq(0:65))
(y <- res_[1]+ x*res_[2] + (x^2)*res_[3])
(y2a <- y+1.96*sres_[2])


x <- x[22:65]
y <- y[22:65]
y2a <- y2a[22:65]
(dat <- data.frame(x,y,y2a))

plot(dat$x,dat$y,type="l",col="blue")  
lines(dat$x,dat$y2a,col="red",lty=3)

res_ <- coef(lm(R_DEN ~ poly(H01_02, 3,raw=TRUE), data = z19)) %>% as.vector()
(x <- seq(0:64))
(y <- res_[1]+ x*res_[2] + (x^2)*res_[3] + (x^3)*res_[4])
x <- x[22:65]
y <- y[22:65]

(dat <- data.frame(x,y))
plot(dat$x,dat$y,type="p",col="blue")  

############################################
a <- loess(R_DEN ~ H01_02,data = z19,span=20)
a2 <- predict(a,se=T)


# fake data
myData <- data.frame("x"=1:100, "y"=rnorm(100))
# loess object
my.loess <- loess(y~x, data=temp,span=5)
# get SE
myPred <- predict(my.loess, se=T)
my.output <- data.frame("fitted"=myPred$fit, "SE"=myPred$se.fit)



pp <- ggplot(data=z19,aes(x=H01_02,y=R_DEN)) + geom_smooth(method = "loess")
a <- ggplot_build(pp)
b <- a$data[[1]]
b %>% tibble::rownames_to_column() %>% mutate(ismax = (y==max(y))) %>% filter(ismax)


length(a$data[[1]][,1])

x <- a$data[[1]][1]
y <- a$data[[1]][2]


(dat <- data.frame(x,y))
plot(dat$x,dat$y,type="l",col="blue")  



# Create dataframe with aggregated salary by year, edu level, region and age
df_year_edu_region_age_salary <- df_rosstat %>% group_by(YEAR, edu_4, H00_02, H01_02) %>% summarise(average_salary=weighted.mean(wage, w=KVZV)) %>% filter(edu_4 != "Secondary") %>% ungroup()

df_year_edu_region_age_salary <- df_year_edu_region_age_salary[!(df_year_edu_region_age_salary$H01_02 == 22 & df_year_edu_region_age_salary$edu_4 == "Higher"),]
# Calculate annual wage
df_year_edu_region_age_salary$annual_wage <- df_year_edu_region_age_salary$average_salary*12


# Create dataframe with colleges and universities
df_year_edu_region_age_salary_colleges <- df_year_edu_region_age_salary %>% filter(edu_4 == "Vocational")
df_year_edu_region_age_salary_universities <- df_year_edu_region_age_salary %>% filter(edu_4 == "Higher")














# Create dataframe with aggregated salary by year, edu level, region and age
df_year_edu_region_age_salary <- df_rosstat %>% group_by(YEAR, edu_4, H00_02, H01_02) %>% summarise(average_salary=weighted.mean(wage, w=KVZV)) %>% filter(edu_4 != "Secondary") %>% ungroup()

df_year_edu_region_age_salary <- df_year_edu_region_age_salary[!(df_year_edu_region_age_salary$H01_02 == 22 & df_year_edu_region_age_salary$edu_4 == "Higher"),]
# Calculate annual wage
df_year_edu_region_age_salary$annual_wage <- df_year_edu_region_age_salary$average_salary*12


# Create dataframe with colleges and universities
df_year_edu_region_age_salary_colleges <- df_year_edu_region_age_salary %>% filter(edu_4 == "Vocational")
df_year_edu_region_age_salary_universities <- df_year_edu_region_age_salary %>% filter(edu_4 == "Higher")

# Remove regions which are not present in graduate.edu
#df_year_edu_region_age_salary_colleges <- df_year_edu_region_age_salary_colleges %>% filter(H00_02 %in% df_colleges_mean_cost$OKATO)
#df_year_edu_region_age_salary_universities <- df_year_edu_region_age_salary_universities %>% filter(H00_02 %in% df_universities_mean_cost$OKATO)


save(df_year_edu_region_age_salary_colleges, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_colleges.rda")
save(df_year_edu_region_age_salary_universities,file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_universities.rda")

load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_universities.rda")

###############################################################################################################################

####################################################################################################################
## Look at First university in the list, from OKATO 01

## Generate 

df1 <- df_universities %>% transmute(SPUNID=SPUNID,s14_15=salary_2015-salary_2014, s15_16=salary_2016-salary_2015,s14_16=salary_2016-salary_2014,
                                     d2S=s15_16-s14_15,
                                     age=round(graduate_years),salary_2014=salary_2014,salary_2015=salary_2015,salary_2016=salary_2016) %>%
  arrange(SPUNID)



for (i in seq_along(1:dim(df1)[1])){
  df2 <- df1 %>% mutate(x1=age+1,x2=age+2,x3=age+3,y1=salary_2014*12,y2=salary_2015*12,y3=salary_2016*12) 
  df2[i,]
  (x <- c(df2[i,]$x1,df2[i,]$x2,df2[i,]$x3))
  (y <- c(df2[i,]$y1,df2[i,]$y2,df2[i,]$y3))
  xname <- "x"
  yname <- "y"
  dat <- data.frame(x,y)
  names(dat) <- c(xname,yname)
  dat
  (res_ <- coef(lm(y ~ poly(x, 2,raw=TRUE), data = dat)) %>% as.vector())
  # Form age earnings profile
  (x <- seq(df2[i,]$x1,length=66-df2[i,]$x1))
  (y <- res_[1]+ x*res_[2] + (x^2)*res_[3])
  dat <- data.frame(x,y)
  names(dat) <- c(xname,yname)
  SPUNID <- rep(df2[i,]$SPUNID,66-df2[i,]$x1)
  (dat <- cbind(SPUNID,dat))
  assign(paste0("UNI",SPUNID[1]),dat)
}


my.list <- lapply(ls(pattern="UNI*"), get)

a <- as.data.frame(do.call(rbind, my.list[-1])) 

a_ <- a %>% group_by(SPUNID) %>% summarise(miny=min(y))

a2 <- a_ %>% mutate(can=ifelse(miny>=0,1,0)) 

sum(a2$can)  # only 128 without negative values



#########*&(*^&(*^(* )))
#########*
#########*
#########*
df2 <- df1 %>% mutate(x1=age+1,x2=age+2,x3=age+3,y1=salary_2014*12,y2=salary_2015*12,y3=salary_2016*12) 
df2[1,]
(x <- c(df2[1,]$x1,df2[1,]$x2,df2[1,]$x3))
(y <- c(df2[1,]$y1,df2[1,]$y2,df2[1,]$y3))
xname <- "x"
yname <- "y"
dat <- data.frame(x,y)
names(dat) <- c(xname,yname)
dat
(res_ <- coef(lm(y ~ poly(x, 2,raw=TRUE), data = dat)) %>% as.vector())
# Form age earnings profile
(x <- seq(df2[1,]$x1,length=66-df2[1,]$x1))
(y <- res_[1]+ x*res_[2] + (x^2)*res_[3])
dat <- data.frame(x,y)
names(dat) <- c(xname,yname)
SPUNID <- rep(df2[1,]$SPUNID,66-df2[1,]$x1)
(dat <- cbind(SPUNID,dat))

dat %>% tibble::rownames_to_column() %>% mutate(ismax = (y==max(y))) %>% filter(ismax)



# https://stackoverflow.com/questions/29273012/find-first-occurence-of-value-in-group-using-dplyr-mutate 



z1 <- df_year_edu_region_age_salary_universities 
(z14 <- z1 %>% filter(H00_02=="01" & YEAR==2014) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))
(z15 <- z1 %>% filter(H00_02=="01" & YEAR==2015) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))
(z16 <- z1 %>% filter(H00_02=="01" & YEAR==2016) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))
(z17 <- z1 %>% filter(H00_02=="01" & YEAR==2017) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))
(z18 <- z1 %>% filter(H00_02=="01" & YEAR==2018) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))
(z19 <- z1 %>% filter(H00_02=="01" & YEAR==2019) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))


seq_along(1:5)


z2 <- z1 %>% group_by(H00_02,H01_02) %>% summarize(aws=mean(annual_wage))

(z2_01 <- z2 %>% filter(H00_02=="01") %>% mutate(ismax=(aws==max(aws))) %>% filter(ismax))

# vector of regions in data
(regs <- z2 %>% dplyr::select(H00_02) %>% unique() %>% arrange())

regs[1]

z2 %>% dplyr::select(H00_02) %>% unique()

result <- vector("list",85)

for (i in regs[1]){
  blix <<- z2  %>% group_by(H00_02) %>% mutate(blix=(aws==max(aws))) %>% filter(blix)
}


z3 <- z2 %>% group_by(H01_02) %>% summarise(aaws=mean(aws))
plot(z3$H01_02,z3$aaws)

plot(z1[z1$H00_02=="01",]$H01_02, z1[z1$H00_02=="01",]$annual_wage)


ggplot(data=df[df$H00_02=="01",],aes(x=H01_02,y=wage)) +
  geom_smooth()



var <- enquo(var)
col=!!var

zf1 <- df_year_edu_region_age_salary_universities %>% dplyr::rename(OKATO=H00_02)

zf2 <- zf1 %>% filter(OKATO=="01")

glimpse(zf1)






x <- 1:10
y <- rnorm(10)
par(mfrow = c(2,1))
plot(x, y, main = "approx(.) and approxfun(.)")
points(approx(x, y), col = 2, pch = "*")
points(approx(x, y, method = "constant"), col = 4, pch = "*")



