#barter.R
# http://www.rebeccabarter.com/blog/2019-01-23_scoped-verbs/ scoped verbs
# http://www.rebeccabarter.com/blog/2020-07-09-across/
#remotes::inrstall_github("allisonhorst/palmerpenguins")

# load in the only library you ever really need
library(tidyverse)
library(lubridate)
# load in survey data
av_survey <- read_csv("bikepghpublic.csv")

set.seed(45679)
av_survey_sample <- av_survey %>% 
  # select jsut a few columns and give some more intuitive column names
  dplyr::select(id = `Response ID`,
         start_date = `Start Date`, 
         end_date = `End Date`,
         interacted_with_av_as_pedestrian = InteractPedestrian,
         interacted_with_av_as_cyclist = InteractBicycle,
         circumstanses_of_interaction = CircumstancesCoded, # lol @ typo in data
         approve_av_testing_pgh = FeelingsProvingGround) %>%
  # take a random sample of 10 rows
  sample_n(10) %>%
  # make data frame so that we view the whole thing
  as.data.frame()
av_survey_sample

# Count # of missing values in each column
# using apply and the normal temporary function syntax
sapply(av_survey_sample, function(x) sum(is.na(x)))

# using purrr::map_dbl and the normal temporary function syntax
av_survey_sample %>% map_dbl(function(x) sum(is.na(x)))

# using purrr::map_dbl and the `~fun(.x)` temporary function syntax
av_survey_sample %>% map_dbl(~sum(is.na(.x)))


# Select if
av_survey_sample %>% select_if(is.numeric)

av_survey_sample %>% 
  # select columns with at least one NA
  # the expression evaluates to TRUE if there is one or more missing values
  select_if(~sum(is.na(.x)) > 0) 


av_survey_sample %>%
  # only rename numeric columns by adding a "num_" prefix
  rename_if(is.numeric, ~paste0("num_", .x))



av_survey_sample %>% 
  # only mutate columns with at least one NA
  # replace each NA value with the character "missing"
  mutate_if(~sum(is.na(.x)) > 0,
            ~if_else(is.na(.x), "missing", as.character(.x)))

# function to calculate the mode (most common) observation
mode <- function(x) {
  names(sort(table(x)))[1]
}
# summarise character
av_survey_sample %>% 
  summarise_if(is.character, mode)


###################################################
#http://www.rebeccabarter.com/blog/2020-07-09-across/

library(palmerpenguins)
library(tidyverse)

penguins %>%
  summarise(distinct_species = n_distinct(species))

penguins %>%
  summarise(distinct_species = n_distinct(species),
            distinct_island = n_distinct(island),
            distinct_sex = n_distinct(sex))

penguins %>%
  summarise(across(c(species, island, sex), 
                   n_distinct))

# replace

function(x) {
  x + 10
}

# with

~{.x + 10}





