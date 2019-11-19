# extension1a.R
# WP1 extension along lines of Neuman-Weiss 1995.

library(dplyr)
library(sqldf)
library(XLConnectJars)
library(questionr)
library(labelled)
library(tidyr)
library(magrittr)
library(ggplot2)
library(data.table)
library(pbapply)
library(gridExtra)
library(psych)
library(stringi)
library(sjPlot)
library(sjmisc)

# wd
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1"
setwd(wd)

# Data
df_mincer <- readRDS("df_mincer.rds")

########## Aggregating occupations by 2 digits

# First, aggregating military men
df_mincer$occup <- ifelse(df_mincer$occup == 0, 110, df_mincer$occup) 

# For simplicity those with one digit go to the first category with 2 digits (e.g., 1 go to 11)
df_mincer$occup <- as.numeric(ifelse(df_mincer$occup<10,
                                     paste0(df_mincer$occup, "1"),
                                     df_mincer$occup))

# Leaving only 2 digits
df_mincer$occup2d <- as.numeric(substr(df_mincer$occup, 1, 2)) # leaving 2 digits

# If N obs is < 30 we technically cannot run a regression
# Let's aggregate such categories with the respective closest category
# I detected them manually for the easiness of computations base on this table:
table(df_mincer$occup2d, df_mincer$YEAR)

# Aggregating
df_mincer$occup2d[df_mincer$occup2d == 11|df_mincer$occup2d == 12] <- 112
df_mincer$occup2d[df_mincer$occup2d == 24|df_mincer$occup2d == 25] <- 245
df_mincer$occup2d[df_mincer$occup2d == 34|df_mincer$occup2d == 35] <- 345
df_mincer$occup2d[df_mincer$occup2d == 41|df_mincer$occup2d == 42] <- 412
df_mincer$occup2d[df_mincer$occup2d == 43|df_mincer$occup2d == 44] <- 434
df_mincer$occup2d[df_mincer$occup2d == 53|df_mincer$occup2d == 54] <- 534
df_mincer$occup2d[df_mincer$occup2d == 73|df_mincer$occup2d == 74|
                  df_mincer$occup2d == 75] <- 7345
df_mincer$occup2d[df_mincer$occup2d == 81|df_mincer$occup2d == 82] <- 812
df_mincer$occup2d[df_mincer$occup2d == 92|df_mincer$occup2d == 93|
                  df_mincer$occup2d == 94|df_mincer$occup2d == 95|
                  df_mincer$occup2d == 96] <- 923456
df_mincer$occup2d[df_mincer$occup2d == 61|df_mincer$occup2d == 62] <- 71

table(df_mincer$occup2d, df_mincer$YEAR)
# 345 category is too small even within its digit so 
# we need to merge it with another digit
df_mincer$occup2d[df_mincer$occup2d == 345] <- 412

# Checking
tbl <- as.data.frame(table(df_mincer$occup2d, df_mincer$YEAR))
tiny <- as.numeric(as.character(
  unique(tbl$Var1[tbl$Freq<30]))); tiny # no categaries with < 30 obs

# Creating a dummy set for occupations
dummy_set <- dummy.code(df_mincer$occup2d)
colnames(dummy_set) <- paste0("occup", colnames(dummy_set), sep = "")
df <- cbind(df_mincer, dummy_set)

# Probit regression: developing a female - non-female typology of occupations

# Empty list where the regression output will be written
probit <- vector("list", length(unique(df$YEAR)))
for (i in seq(length(probit))){
  probit[[i]] <- vector("list", length(unique(colnames(dummy_set))))
}
seq_year <- unique(df$YEAR)

# Looping over each year and occupation
# takes ~15 sec
for(i in seq(length(seq_year))){
  for(j in seq(length(colnames(dummy_set)))){
    probit[[i]][[j]] <- glm(as.formula(paste0(colnames(dummy_set)[j], "~", "female")),
                          family = binomial(link = "probit"),
                          data = df[df$YEAR == seq_year[i],])
  }
}

# Naming
names(probit) <- seq_year

# Computng summary
smry <-  lapply(probit, function(x) {lapply(x, summary)})

# A table with coefficients for the female variable
tbl_fem_ <- c()
for (y in seq_year){
  for (n in seq(length(colnames(dummy_set)))){
   tbl_fem_ <- rbind.data.frame(tbl_fem_, cbind.data.frame(
    "female" = round(smry[[paste0(y)]][[n]]$coefficients[2,1], 2),
    "p-value" = round(smry[[paste0(y)]][[n]]$coefficients[2,4], 3))
    )   
  }
}

YEAR <- rep(seq_year, each = ncol(dummy_set))
occup <- rep(colnames(dummy_set), length(seq_year))
tbl_fem <- cbind.data.frame(YEAR, occup, tbl_fem_) # final table

# Female-dominated occupations
fem_occup_vec <- as.character(unique(
  tbl_fem[tbl_fem$female > 0 &
            tbl_fem$'p-value' < 0.05, "occup"]))

# Non-female occupations
nonfem_occup_vec <- as.character(unique(
  tbl_fem[!(tbl_fem$female > 0 &
            tbl_fem$'p-value' < 0.05), "occup"]))

# Occupations which are in both categories depending on a wave
fem_occup_vec[fem_occup_vec %in% nonfem_occup_vec]

# Let us examine those cases
both <- tbl_fem[tbl_fem$occup %in% 
                  fem_occup_vec[fem_occup_vec %in% nonfem_occup_vec],]
# occup14 is insignificant in the majoriy of waves -> let's put it in nonfem_occup
# occup245 is significant and positive almost each time -> let's put it in fem_occup
# occup51 is mostly significant and positive -> let's put it in fem_occup
# occup91 is mostly significant and positive -> let's put it in fem_occup

# Defining a variable with nonfem_occup
df$occup2d <- as.character(df$occup2d)
df$fem_occup <- ifelse(df$occup2d %in% 
                            substr(fem_occup_vec[!fem_occup_vec=="occup14"],
                                   6, nchar(fem_occup_vec[!fem_occup_vec=="occup14"])), 1, 0)

# Looking at the distribution (looks logical)
table(df$fem_occup)
table(df$fem_occup, df$female)

# Filtering the missings left
df <- df %>%
  filter(!is.na(wage) & !is.na(edu_4) & wage > 0)

df$fem_occup <- factor(df$fem_occup, 
                       levels = c(1,0),
                       labels = c("Female Occupations",
                                  "Non-female Occupations"))

# Adjusting to prices in 2018
cpi <- rio::import("cpi.xlsx")[,c(1,4)]
df <- df %>%
  left_join(cpi, by = "YEAR")

df$wage_adjusted_to_2018 <- df$wage*df$norm
# wages in 2018 are alomst 3 times as high as wages in 2000:
aggregate(wage_adjusted_to_2018 ~ YEAR, df, mean)
aggregate(wage ~ YEAR, df, mean)
#################### Regressions with depreciation of education ###################

# for all 2018 data

# Empty list where the regression output will be written
lm_dep <- vector("list", length(unique(df$YEAR)))
seq_year <- unique(df$YEAR)

# Looping over each year
for(i in seq(length(seq_year))){
  lm_dep[[i]] <- lm(log(wage_adjusted_to_2018) ~ edu_4 +
                                exper + 
                                I(exper^2) + 
                                exper*edu_4 +
                                I(exper^2)*edu_4,
                           data = df[df$YEAR == seq_year[i],])
}
names(lm_dep) <- seq_year
smry_lm_dep <- lapply(lm_dep, summary)

###################################### Model prediction: 2018 
df_2018 <- as.data.frame(df[df$YEAR == 2018,])
pred_y_2018 <- exp(predict(lm_dep[['2018']], df_2018, interval="conf"))
df_2018 <- cbind(df_2018, pred_y_2018)

# Finding maximums for 2018 jointly
grid <- expand.grid(edu_4 = factor(1:3,
                                   labels = c("Higher",
                                               "Secondary",
                                               "Vocational"))) 

ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2018[
      df_2018$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

xmax <- unique(df_2018[df_2018$fit %in% ymax, "exper"])

# Plot
ggplot(df_2018, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                    linetype = edu_4)) +
  geom_line(size = 0.6) +
  geom_vline(aes(xintercept = xmax[1])) +
  geom_vline(aes(xintercept = xmax[2])) +
  geom_vline(aes(xintercept = xmax[3])) +
  geom_point(shape = 4, aes(x = xmax[1], y = ymax[1]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[2], y = ymax[3]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[3], y = ymax[2]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(size = 12))  +
  scale_color_manual(values = c("blue", "red", "darkgreen")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
  ylab("Monthly wage, RUB") +
  xlab("Experience") 

ggsave("2018.png", width = 7.5, height = 4,
            units = "in")

###################################### Model prediction: 2009 
df_2009 <- as.data.frame(df[df$YEAR == 2009,])
pred_y_2009 <- exp(predict(lm_dep[['2009']], df_2009, interval="conf"))
df_2009 <- cbind(df_2009, pred_y_2009)

# Finding maximums for 2018 jointly
grid <- expand.grid(edu_4 = factor(1:3,
                                   labels = c("Higher",
                                              "Secondary",
                                              "Vocational"))) 

ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2009[
    df_2009$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

xmax <- unique(df_2009[df_2009$fit %in% ymax, "exper"])

# Plot
ggplot(df_2009, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                    linetype = edu_4)) +
  geom_line(size = 0.6) +
  geom_vline(aes(xintercept = xmax[1])) +
  geom_vline(aes(xintercept = xmax[2])) +
  geom_vline(aes(xintercept = xmax[3])) +
  geom_point(shape = 4, aes(x = xmax[1], y = ymax[1]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[2], y = ymax[3]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[3], y = ymax[2]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(size = 12))  +
  scale_color_manual(values = c("blue", "red", "darkgreen")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
  ylab("Monthly wage, RUB") +
  xlab("Experience") 

ggsave("2009.png", width = 7.5, height = 4,
       units = "in")

###################################### Model prediction: 2000 
df_2000 <- as.data.frame(df[df$YEAR == 2000,])
pred_y_2000 <- exp(predict(lm_dep[['2000']], df_2000, interval="conf"))
df_2000 <- cbind(df_2000, pred_y_2000)

# Finding maximums for 2018 jointly
grid <- expand.grid(edu_4 = factor(1:3,
                                   labels = c("Higher",
                                              "Secondary",
                                              "Vocational"))) 

ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2000[
    df_2000$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

xmax <- unique(df_2000[df_2000$fit %in% ymax, "exper"])

# Plot
ggplot(df_2000, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                    linetype = edu_4)) +
  geom_line(size = 0.6) +
  geom_vline(aes(xintercept = xmax[1])) +
  geom_vline(aes(xintercept = xmax[2])) +
  geom_vline(aes(xintercept = xmax[3])) +
  geom_point(shape = 4, aes(x = xmax[1], y = ymax[1]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[2], y = ymax[3]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[3], y = ymax[2]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(size = 12))  +
  scale_color_manual(values = c("blue", "red", "darkgreen")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
  ylab("Monthly wage, RUB") +
  xlab("Experience") 

ggsave("2000.png", width = 7.5, height = 4,
       units = "in")


################################## for occupations separately

# Empty list where the regression output will be written
lm_dep <- vector("list", length(unique(df$YEAR)))
seq_year <- unique(df$YEAR)

# Looping over each year
for(i in seq(length(seq_year))){
  lm_dep[[i]] <- lm(log(wage_adjusted_to_2018) ~ edu_4 +
                      exper + 
                      I(exper^2) + 
                      fem_occup +
                      fem_occup*exper +
                      fem_occup*I(exper^2) +
                      fem_occup*edu_4 +
                      exper*edu_4 +
                      I(exper^2)*edu_4 +
                      fem_occup*edu_4*exper +
                      fem_occup*edu_4*I(exper^2),
                    data = df[df$YEAR == seq_year[i],])
}
names(lm_dep) <- seq_year
smry_lm_dep <- lapply(lm_dep, summary)

###################################### Model prediction: 2018
df_2018 <- as.data.frame(df[df$YEAR == 2018,])
pred_y_2018 <- exp(predict(lm_dep[['2018']], df_2018, interval="conf"))
df_2018 <- cbind(df_2018, pred_y_2018)

# Finding maximums separately for occupational facets
grid <- expand.grid(fem_occup = 
                    factor(0:1, labels = c("Female Occupations",
                                           "Non-female Occupations")),
                    edu_4 = factor(1:3, labels = c("Higher",
                                                   "Secondary",
                                                   "Vocational"))) %>%
  arrange(fem_occup)
  
ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2018[
     df_2018$fem_occup == as.character(grid[i, "fem_occup"]) &
     df_2018$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

max <- cbind(grid, ymax)
xmax <- unique(df_2018[df_2018$fit %in% ymax, c("exper", "fit")])
names(xmax)[2] <- "ymax"

max_f <- max %>%
  left_join(xmax, by = "ymax")

# Plot
ggplot(df_2018, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                               linetype = edu_4)) +
   geom_line(aes(y = fit), size = 0.6) +
   geom_vline(data = filter(df_2018, fem_occup == "Female Occupations"),
               aes(xintercept = max_f$exper[1])) +
   geom_vline(data = filter(df_2018, fem_occup == "Female Occupations"),
              aes(xintercept = max_f$exper[2])) +
   geom_vline(data = filter(df_2018, fem_occup == "Female Occupations"),
             aes(xintercept = max_f$exper[3])) +
   geom_vline(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(xintercept = max_f$exper[4])) +
   geom_vline(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(xintercept = max_f$exper[5])) +
   geom_vline(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(xintercept = max_f$exper[6])) +
   geom_point(data = filter(df_2018, fem_occup == "Female Occupations"),
              aes(x = max_f$exper[1], y = max_f$ymax[1]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   geom_point(data = filter(df_2018, fem_occup == "Female Occupations"),
              aes(x = max_f$exper[2], y = max_f$ymax[2]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   geom_point(data = filter(df_2018, fem_occup == "Female Occupations"),
              aes(x = max_f$exper[3], y = max_f$ymax[3]), size = 1,
              show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   geom_point(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(x = max_f$exper[4], y = max_f$ymax[4]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   geom_point(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(x = max_f$exper[5], y = max_f$ymax[5]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   geom_point(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(x = max_f$exper[6], y = max_f$ymax[6]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   facet_grid(~ as.factor(fem_occup)) +
   theme(legend.title = element_blank(),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.key = element_rect(size = 12))  +
   scale_color_manual(values = c("blue", "red", "darkgreen")) + 
   scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
   scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
   ylab("Monthly wage, RUB") +
   xlab("Experience") 

ggsave("2018_int.png", width = 7.5, height = 4,
             units = "in")

###################################### Model prediction: 2009
df_2009 <- as.data.frame(df[df$YEAR == 2009,])
pred_y_2009 <- exp(predict(lm_dep[['2009']], df_2009, interval="conf"))
df_2009 <- cbind(df_2009, pred_y_2009)

# Finding maximums separately for occupational facets
grid <- expand.grid(fem_occup = 
                      factor(0:1, labels = c("Female Occupations",
                                             "Non-female Occupations")),
                    edu_4 = factor(1:3, labels = c("Higher",
                                                   "Secondary",
                                                   "Vocational"))) %>%
  arrange(fem_occup)

ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2009[
    df_2009$fem_occup == as.character(grid[i, "fem_occup"]) &
      df_2009$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

max <- cbind(grid, ymax)
xmax <- unique(df_2009[df_2009$fit %in% ymax, c("exper", "fit")])
names(xmax)[2] <- "ymax"

max_f <- max %>%
  left_join(xmax, by = "ymax")

# Plot
ggplot(df_2009, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                    linetype = edu_4)) +
  geom_line(aes(y = fit), size = 0.6) +
  geom_vline(data = filter(df_2009, fem_occup == "Female Occupations"),
             aes(xintercept = max_f$exper[1])) +
  geom_vline(data = filter(df_2009, fem_occup == "Female Occupations"),
             aes(xintercept = max_f$exper[2])) +
  geom_vline(data = filter(df_2009, fem_occup == "Female Occupations"),
           aes(xintercept = max_f$exper[3])) +
  geom_vline(data = filter(df_2009, fem_occup == "Non-female Occupations"),
             aes(xintercept = max_f$exper[4])) +
  geom_vline(data = filter(df_2009, fem_occup == "Non-female Occupations"),
             aes(xintercept = max_f$exper[5])) +
  geom_vline(data = filter(df_2009, fem_occup == "Non-female Occupations"),
            aes(xintercept = max_f$exper[6])) +
  geom_point(data = filter(df_2009, fem_occup == "Female Occupations"),
             aes(x = max_f$exper[1], y = max_f$ymax[1]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2009, fem_occup == "Female Occupations"),
             aes(x = max_f$exper[2], y = max_f$ymax[2]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2009, fem_occup == "Female Occupations"),
             aes(x = max_f$exper[3], y = max_f$ymax[3]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2009, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[4], y = max_f$ymax[4]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2009, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[5], y = max_f$ymax[5]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2009, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[6], y = max_f$ymax[6]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  facet_grid(~ as.factor(fem_occup)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(size = 12))  +
  scale_color_manual(values = c("blue", "red", "darkgreen")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
  ylab("Monthly wage, RUB") +
  xlab("Experience") 

ggsave("2009_int.png", width = 7.5, height = 4,
       units = "in")

###################################### Model prediction: 2000
df_2000 <- as.data.frame(df[df$YEAR == 2000,])
pred_y_2000 <- exp(predict(lm_dep[['2000']], df_2000, interval="conf"))
df_2000 <- cbind(df_2000, pred_y_2000)

# Finding maximums separately for occupational facets
grid <- expand.grid(fem_occup = 
                      factor(0:1, labels = c("Female Occupations",
                                             "Non-female Occupations")),
                    edu_4 = factor(1:3, labels = c("Higher",
                                                   "Secondary",
                                                   "Vocational"))) %>%
  arrange(fem_occup)

ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2000[
    df_2000$fem_occup == as.character(grid[i, "fem_occup"]) &
      df_2000$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

max <- cbind(grid, ymax)
xmax <- unique(df_2000[df_2000$fit %in% ymax, c("exper", "fit")])
names(xmax)[2] <- "ymax"

max_f <- max %>%
  left_join(xmax, by = "ymax")

# Plot
ggplot(df_2000, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                    linetype = edu_4)) +
  geom_line(aes(y = fit), size = 0.6) +
  geom_vline(data = filter(df_2000, fem_occup == "Female Occupations"),
             aes(xintercept = max_f$exper[1])) +
  geom_vline(data = filter(df_2000, fem_occup == "Female Occupations"),
             aes(xintercept = max_f$exper[2])) +
  geom_vline(data = filter(df_2000, fem_occup == "Female Occupations"),
           aes(xintercept = max_f$exper[3])) +
  geom_vline(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(xintercept = max_f$exper[4])) +
  geom_vline(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(xintercept = max_f$exper[5])) +
  geom_vline(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(xintercept = max_f$exper[6])) +
  geom_point(data = filter(df_2000, fem_occup == "Female Occupations"),
             aes(x = max_f$exper[1], y = max_f$ymax[1]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2000, fem_occup == "Female Occupations"),
             aes(x = max_f$exper[2], y = max_f$ymax[2]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2000, fem_occup == "Female Occupations"),
            aes(x = max_f$exper[3], y = max_f$ymax[3]), size = 1,
          show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[4], y = max_f$ymax[4]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[5], y = max_f$ymax[5]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[6], y = max_f$ymax[6]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  facet_grid(~ as.factor(fem_occup)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(size = 12))  +
  scale_color_manual(values = c("blue", "red", "darkgreen")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
  ylab("Monthly wage, RUB") +
  xlab("Experience") 

ggsave("2000_int.png", width = 7.5, height = 4,
       units = "in")

################# Model prediction (for each year)
#for (i in 1:length(seq_year)){
#  df_year <- as.data.frame(df[df$YEAR == seq_year[i],])
#  pred_y <- exp(predict(lm_dep[[i]], df_year, interval="conf"))
#  df_year <- cbind(df_year, pred_y)
  
# Plot
# p_int <- ggplot(df_year, aes(x = exper, y = fit, group = edu_4, color = edu_4,
#                  linetype = edu_4)) +
#    geom_line(aes(y = fit), size = 1.2) +
#    geom_ribbon(aes(ymin=lwr, ymax=upr, fill = edu_4), alpha = 0.1, colour = NA) +
#    facet_grid(~ as.factor(fem_occup)) +
#    theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         panel.grid.minor = element_blank(),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         legend.key = element_rect(size = 12))  +
#   scale_color_manual(values = c("blue", "red", "darkgreen")) + 
#    scale_fill_manual(values=c("blue", "red", "darkgreen")) +
#    scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
#    scale_y_continuous(limits = c(0, 3)) +
#    ylab("Monthly wage normed by median") +
#    xlab("Experience") 
  
# ggsave(paste0("p_", seq_year[i], "_int.png"),  width = 7.5, height = 4,
#       units = "in")
# print(i)
#}

