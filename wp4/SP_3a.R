
# SP_3a.R

#########################################################################################

# Graph 3.3 WP5 in English instead of Russian



### 1. Create dataframes with russian-english names

# University names
university_specialization_names <- data.frame(russian_name=c("аграрный", "классический", "педагогический", "медицинский",
                                                             "специализированный", "социально-экономический", "технический",
                                                             "художественный"),
                                              english_name=c("Agricultural", "Classical", "Pedagogical", "Medical",
                                                             "Specialized", "Socio-economic", "Technical", "Art"))

# College names
college_specialization_names <- data.frame(russian_name=c("аграрный", "педагогический", "медицинский",
                                                          "общий", "социально-экономический", "технический",
                                                          "художественный"),
                                           english_name=c("Agricultural", "Pedagogical", "Medical",
                                                          "General", "Socio-economic", "Technical", "Art"))

# Study area names
area_names <- rio::import("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp5/area_names.xlsx", "xlsx")

# Region names
region_names <- rio::import("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp3/rgnames2.xlsx", "xlsx")
region_names <- region_names %>% rename(region_name = en_rgnames)

df_universities <- df_universities[nchar(df_universities$OKATO) == 2,]
df_colleges <- df_colleges[nchar(df_colleges$OKATO) == 2,]

df_colleges$region_name <- (df_colleges %>% dplyr::select(OKATO) %>% left_join(region_names))$region_name
df_universities$region_name <- (df_universities %>% dplyr::select(OKATO) %>% left_join(region_names))$region_name


### 2. Rename 

# Universities
df_temp <- df_universities %>% dplyr::select(specialization_type) %>% left_join(university_specialization_names,
                                                                                by=c("specialization_type"="russian_name"))
df_universities$specialization_type <- df_temp$english_name

# Colleges
df_temp <- df_colleges %>% dplyr::select(speciality_type) %>% left_join(college_specialization_names,
                                                                        by=c("speciality_type"="russian_name"))
df_colleges$speciality_type <- df_temp$english_name

# Study areas
df_temp <- df_universities_areas %>% dplyr::select(area_name) %>% left_join(area_names,
                                                                            by=c("area_name"="russian_name"))
df_universities_areas$area_name <- df_temp$english_name






# 1. Universities
plot1 <- ggplot(df_universities, aes(reorder(specialization_type, social_returns), social_returns)) +
  geom_boxplot(outlier.shape = NA, fill="deepskyblue2") + 
  coord_flip() +
  ggtitle("Fiscal IRR after 3 years by Specialization of University") + 
  labs(y="Average Fiscal IRR", x = "Specialization of university") + 
  theme_light(base_size = 18) + theme(plot.title = element_text(size = 24), axis.text = element_text(size=22))
# scale_y_continuous(limits=c(20000, 80000))

# 2. Colleges
plot2 <- ggplot(df_colleges, aes(reorder(speciality_type, social_returns), social_returns)) +
  geom_boxplot(outlier.shape = NA, fill="lightcoral") + 
  coord_flip() +
  ggtitle("Fiscal IRR after 3 years by Specialization of College") +
  labs(y="Average Fiscal IRR", x = "Specialization of college") + 
  theme_light(base_size = 18) + theme(plot.title = element_text(size = 24), axis.text = element_text(size=22)) 
# scale_y_continuous(limits=c(20000, 80000)) 

# Arrange in one plot
plot_grid <- grid.arrange(plot2, plot1, ncol=2)

plot(plot_grid)
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4")
ggsave(filename = "returns_by_areasEN.png", plot_grid, width=25, height=12)



table(df_colleges$region_name)

