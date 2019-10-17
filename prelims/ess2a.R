# ess2a.R

# Exploring Earnings regression - in alphabetical order of countries

library(dplyr) # for data mungeing
library(stargazer) # for tables
library(Hmisc) # deprecated - perhaps not in CRAN, for summary function of descriptives


# To use the data; 
load("C:/Country/Russia/Data/SEABYTE/ESS/downloads/ess_all_rounds.rdata")

# Start with round 8

round8 <- as.data.frame(all_rounds[8])
glimpse(round8)

# AUSTRIA

round8_AT <- round8 %>% filter(cntry=="AT") %>% filter(hincsrca==1) %>% filter(pdwrk==1) %>%  
  filter(!is.na(hinctnta)) %>%  filter(hhmmb>=1) %>% filter(edulvlb!=5555) %>% 
  filter(agea %in% (25:64)) %>% 
  select(brncntr,hhmmb,gndr,agea,edulvlb,eduyrs,hinctnta,dweight,pspwght,pweight)


# Define variables of interest

# Mid-point income # from EESA Income definitions Appendix A2 page 3 for Austria
#atry_h atrributed labor income household
# income is nominal euros per year
round8_AT$atry_h[round8_AT$hinctnta==1] <- 14800/2
round8_AT$atry_h[round8_AT$hinctnta==2] <- (14800+19800)/2
round8_AT$atry_h[round8_AT$hinctnta==3] <- (19800+24200)/2
round8_AT$atry_h[round8_AT$hinctnta==4] <- (24200+29400)/2
round8_AT$atry_h[round8_AT$hinctnta==5] <- (29400+35000)/2

round8_AT$atry_h[round8_AT$hinctnta==6] <- (35000+41000)/2
round8_AT$atry_h[round8_AT$hinctnta==7] <- (41000+48000)/2
round8_AT$atry_h[round8_AT$hinctnta==8] <- (48000+56500)/2
round8_AT$atry_h[round8_AT$hinctnta==9] <- (56500+70700)/2
round8_AT$atry_h[round8_AT$hinctnta==10] <- 70700*2

# atry_l attributed labor income individual 
round8_AT$atry_i <- round8_AT$atry_h/round8_AT$hhmmb
round8_AT$atry_lni <- log(round8_AT$atry_i)

# Gender
round8_AT$FEMALE[round8_AT$gndr==1] <-0
round8_AT$FEMALE[round8_AT$gndr==2] <-1


# Migrant
round8_AT$MIGRANT[round8_AT$brncntr==1] <-0
round8_AT$MIGRANT[round8_AT$brncntr==2] <-1


# Education 
# Define some functions and vectors for later use

`%notin%` <- Negate(`%in%`)
voc <- c(421,422,423,520)
ter <- c(412,413,510,610,620,710,720,800)

round8_AT$edu_SEC[round8_AT$edulvlb < 412] <- 1
round8_AT$edu_SEC[round8_AT$edulvlb >= 412] <- 0

round8_AT$edu_VOC[round8_AT$edulvlb %in% voc] <- 1
round8_AT$edu_VOC[round8_AT$edulvlb %notin% voc] <- 0

round8_AT$edu_TER[round8_AT$edulvlb %in% ter] <- 1
round8_AT$edu_TER[round8_AT$edulvlb %notin% ter] <- 0

table(round8_AT$edu_SEC)
table(round8_AT$edu_VOC)
table(round8_AT$edu_TER)

# Attributed experience
round8_AT$atr_exp=round8_AT$agea -(6+round8_AT$eduyrs)


# Now run the Mincerian function

model1a <- lm(atry_lni~edu_VOC+edu_TER+ atr_exp + I(atr_exp^2)+FEMALE+MIGRANT,weights=pspwght,data=round8_AT)
summary(model1a)

model1b <- lm(atry_lni~eduyrs+ atr_exp + I(atr_exp^2)+FEMALE+MIGRANT,weights=pspwght,data=round8_AT)
summary(model1b)

###
# Russian Federation


round8_RU <- round8 %>% filter(cntry=="RU") %>% filter(hincsrca==1) %>% filter(pdwrk==1) %>%  
  filter(!is.na(hinctnta)) %>%  filter(hhmmb>=1) %>% filter(edulvlb!=5555) %>% 
  filter(agea %in% (25:64)) %>% 
  select(brncntr,hhmmb,gndr,agea,edulvlb,eduyrs,hinctnta,dweight,pspwght,pweight)

# Define variables of interest

# Mid-point income # from EESA Income definitions Appendix A2 page 24 for Russia
#atry_h atrributed labor income household
# Income is nominal rubles per month
round8_RU$atry_h[round8_RU$hinctnta==1] <- 12000/2
round8_RU$atry_h[round8_RU$hinctnta==2] <- (12000+15000)/2
round8_RU$atry_h[round8_RU$hinctnta==3] <- (15000+18000)/2
round8_RU$atry_h[round8_RU$hinctnta==4] <- (18000+21000)/2
round8_RU$atry_h[round8_RU$hinctnta==5] <- (21000+25000)/2

round8_RU$atry_h[round8_RU$hinctnta==6] <- (25000+30000)/2
round8_RU$atry_h[round8_RU$hinctnta==7] <- (30000+40000)/2
round8_RU$atry_h[round8_RU$hinctnta==8] <- (40000+60000)/2
round8_RU$atry_h[round8_RU$hinctnta==9] <- (60000+80000)/2 # typo in text
round8_RU$atry_h[round8_RU$hinctnta==10] <- 80000*2

# atry_l attributed labor income individual 
round8_RU$atry_i <- round8_RU$atry_h/round8_RU$hhmmb
round8_RU$atry_lni <- log(round8_RU$atry_i)

# Gender
round8_RU$FEMALE[round8_RU$gndr==1] <-0
round8_RU$FEMALE[round8_RU$gndr==2] <-1


# Migrant
round8_RU$MIGRANT[round8_RU$brncntr==1] <-0
round8_RU$MIGRANT[round8_RU$brncntr==2] <-1


# Education 
# Define some functions and vectors for later use

`%notin%` <- Negate(`%in%`)
voc <- c(421,422,423,520)
ter <- c(412,413,510,610,620,710,720,800)

round8_RU$edu_SEC[round8_RU$edulvlb < 412] <- 1
round8_RU$edu_SEC[round8_RU$edulvlb >= 412] <- 0

round8_RU$edu_VOC[round8_RU$edulvlb %in% voc] <- 1
round8_RU$edu_VOC[round8_RU$edulvlb %notin% voc] <- 0

round8_RU$edu_TER[round8_RU$edulvlb %in% ter] <- 1
round8_RU$edu_TER[round8_RU$edulvlb %notin% ter] <- 0

table(round8_RU$edu_SEC)
table(round8_RU$edu_VOC)
table(round8_RU$edu_TER)

# Attributed experience
round8_RU$atr_exp=round8_RU$agea -(6+round8_RU$eduyrs)



# Now run the Mincerian function

model2a <- lm(atry_lni~edu_VOC+edu_TER+ atr_exp + I(atr_exp^2)+FEMALE+MIGRANT,weights=pspwght,data=round8_RU)
summary(model2a)

model2b <- lm(atry_lni~eduyrs+ atr_exp + I(atr_exp^2)+FEMALE+MIGRANT,weights=pspwght,data=round8_RU)
summary(model2b)


