#SP_1b.R

# Post rmd manipulations. 
library(ggplot2)
df_universities <- df_universities

setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp5")

(median(df_universities$salary_2014))/12
# 30521

(median(df_colleges$graduate_salary_2014))/12
# 23273

plot1 <- ggplot(df_universities, aes(reorder(specialization_type, salary_2014/12), salary_2014/12)) +
  geom_boxplot(outlier.shape = NA, fill="deepskyblue2") + 
  geom_hline(yintercept = 30521,linetype="dashed",color="blue",size=1)+
  coord_flip() +
  ggtitle("University Graduates") + 
  labs(y="Median monthly salary", x = "Specialization of university") + 
  theme_light(base_size = 18) + 
  theme(plot.title = element_text(size = 18), 
        axis.text = element_text(size=14)) + 
  scale_y_continuous(limits=c(0, 50000),breaks = c(10000,30521,40000))
plot1
ggsave(filename = "sal_spnu.png")

median(df_colleges$graduate_salary_2014)

# 2. Colleges
plot2 <- ggplot(df_colleges, aes(reorder(speciality_type, graduate_salary_2014/12), graduate_salary_2014/12)) +
  geom_boxplot(outlier.shape = NA, fill="lightcoral") + 
  geom_hline(yintercept = 23273,linetype="dashed",color="blue",size=1)+
  coord_flip() +
  ggtitle("College Graduates") +
  labs(y="Median monthly salary", x = "Specialization of college") + 
  theme_light(base_size = 18) +
  theme(plot.title = element_text(size = 18), 
        axis.text = element_text(size=14)) + 
  scale_y_continuous(limits=c(0, 50000),breaks = c(10000,23723,40000)) 

ggsave(filename = "sal_spnc.png")

# Arrange in one plot
grid.arrange(plot2, plot1, ncol=2)

glimpse(df_universities)

#########################################
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp5")

# Load dataset
df_russia_economic <- openxlsx::read.xlsx("ICSID Russia Regions economic database v121.xlsx")
df_russia_economic <- df_russia_economic %>%
  filter(reg_year == 2013) %>% dplyr::select(reg_minckfd, reg_name)

# Calculate rerurns difference
df_region_returns_university$diff_returns <- df_region_returns_university$private_returns - df_region_returns_university$social_returns

# Add reg_minckfd
df_region_returns_university <- df_region_returns_university %>% left_join(df_russia_economic, by = c("region_name"="reg_name"))

# Plot the scatterplot
ggplot(df_region_returns_university, aes(x=reg_minckfd, y=diff_returns)) +
  geom_point(size=3, aes(color=as.factor(Dep_reg)),alpha=0.7) + 
  geom_smooth(method=lm,color="black",level=0.90)+
  theme_minimal() + 
  
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 18),
        legend.title=element_blank(),
        legend.position=c(0.70,0.90),
        legend.box.background = element_rect(colour = "black")) + 
  scale_color_manual(values=c("blue", "red"),
                     labels=c("Other","Priority Region"))+
  scale_x_continuous(limits=c(10,20))+
  scale_y_continuous(limits=c(-0.01,0.20, 
                              breaks=seq(0.00,0.20,by=.05)))+
  labs(x="Income gap", y = "Social-Private gap") 

ggsave("igap_u.png")

##################



glimpse(df_region_returns_university)

setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp5")
df_region_returns_college$diff_returns <- df_region_returns_college$private_returns - df_region_returns_college$social_returns

# Add reg_minckfd
df_region_returns_college <- df_region_returns_college %>% left_join(df_russia_economic, by = c("region_name"="reg_name"))


# Plot the scatterplot
lm_eqn <- function(df_region_returns_college){
  m <- lm(diff_returns ~ reg_minckfd, data=df_region_returns_college);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

p <- ggplot(df_region_returns_college, aes(x=reg_minckfd, y=diff_returns)) +
  geom_point(size=3, aes(color=as.factor(Dep_reg)),alpha=0.7) + 
  geom_smooth(method=lm,color="black",level=0.90)+
  theme_minimal() + 
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 18),
        legend.title=element_blank(),
        legend.position=c(0.70,0.90),
        legend.box.background = element_rect(colour = "black")) + 
  scale_color_manual(values=c("black", "red"),
                     labels=c("Other","Priority Region"))+
  scale_x_continuous(limits=c(10,20))+
  scale_y_continuous(limits=c(-0.01,0.20, 
                              breaks=seq(0.00,0.20,by=.05)))+
    labs(x="Income gap", y = "Social-Private gap") 


p1 <- p + geom_text(x=15,y=0.3,label=lm_eqn(df_region_returns_college),
                    parse=TRUE)


ggsave("igap_c.png")


#########

mean(df_region_returns_college$psgapc)

