# necessary libraries
library(tidyverse)
library(seplyr)

# read in data_frame of tests
main_frame <- read_rds("main_frame.rds")

# create a function that summarizes 1 data frame by id which is the ELL status variable we choose
summarize_one <- function(df, id){
  df %>% 
    filter(!is.na(Scale.Score)) %>% 
    mutate(Scale.Score = scale(Scale.Score)) %>% 
    group_by_se(id) %>% 
    summarize(count = n(), mean_scale_score = mean(Scale.Score) %>% round(2), sd_scale_score = sd(Scale.Score) %>% round(2)) %>%
    mutate(test_grade_year = paste0(df$Content.Area[1], "_", df$Grade[1], "_", df$Year[1])) %>% 
    select(test_grade_year, everything()) %>% 
    mutate_all(as.character)
}

# apply that function to all data frames and merge into a single data frame
summarize_all <- main_frame$data %>% 
  map(~ summarize_one(., id = "Language.Proficiency")) %>% 
  bind_rows()

# take a look then save to csv
summarize_all

summarize_all %>%  write_csv("summarize_language_proficiency.csv")
