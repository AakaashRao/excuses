library(tidyverse)
library("haven")

pew <- read.dta13("data/working/pew_cleaned.dta") %>%
  filter(!is.na(political_party)) %>%
  mutate(age2 = age^2,
         democrat = ifelse(political_party == 2, 1, 0), 
         other_race = ifelse(black != 1 & white != 1 & asian != 1 & hisp != 1 , 1, 0))
  
sender <- read.dta13("data/working/data.dta") %>%
  mutate(black = ifelse(race == "African American/Black", 1, 0),
         asian = ifelse(race == "Asian/Asian American", 1, 0),
         white = ifelse(hisp == 1, 0, white), 
         other_race = ifelse(black != 1 & white != 1 & asian != 1 & hisp != 1 , 1, 0),
         lt_high_school = ifelse(education == "Less than high school degree", 1, 0),
         high_school = ifelse(education == "High school graduate (high school diploma or equivalent including GED)", 1, 0),
         college_no_degree = ifelse(education == "Some college but no degree", 1, 0),
         associate = ifelse(education == "Associate degree in college (2-year)", 1, 0),
         bachelor = ifelse(education == "Bachelor's degree in college (4-year)", 1, 0),
         post_bachelor = ifelse(education == "Professional degree (JD, MD)" | education == "Doctoral degree" | education == "Master's degree", 1, 0),
         bachelor_more = ifelse(bachelor == 1 | post_bachelor == 1, 1, 0),
         type = "sender", 
         political_party = ifelse(partisan >= 2, 1, 3) # 1=rep, 2= dem, 3 = ind
         )

sender_nomis = sender %>% dplyr::filter(attrit!=1)
  
# list of covariates in both pew and sender datasets
# age, age2, lt_high_school, high_school, college_no_degree, associate, bachelor, post_bachelor, bachelor_more, male, hisp, asian, black,white,other_race


