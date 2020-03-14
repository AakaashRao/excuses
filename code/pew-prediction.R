library(tidyverse)
library(readstata13)

setwd('~/Dropbox/Excuses - Social Image/replication')

# CLEAN PEW DATA (NAs come from 'don't know/refuse to answer' in the raw data)
pew_all <- read.dta13("data/working/pew.dta") %>%
  mutate(education = case_when(
    lt_high_school == 1 ~ 'Less than high school degree',
    high_school == 1 ~ "High school graduate (high school diploma or equivalent including GED)", 
    college_no_degree == 1 ~ "Some college but no degree", 
    associate == 1 ~ "Associate degree in college (2-year)",
    bachelor == 1 ~ "Bachelor's degree in college (4-year)",
    post_bachelor == 1 ~ "Post-bachelor degree"
  ),
  education2 = case_when(
    lt_high_school == 1 ~ 'Less than high school degree',
    high_school == 1 ~ "High school graduate (high school diploma or equivalent including GED)", 
    college_no_degree == 1 ~ "Some college but no degree", 
    associate == 1 ~ "Associate degree in college (2-year)",
    bachelor_more == 1 ~ "Bachelor's degree or higher"
  ),
  education2 = factor(education2, levels = c('Less than high school degree',
                                           "High school graduate (high school diploma or equivalent including GED)",
                                           "Some college but no degree",
                                           "Associate degree in college (2-year)",
                                           "Bachelor's degree or higher")),
  race = case_when(
    asian != 1 & black != 1 & white != 1 ~ 'Other', 
    black == 1 ~ 'African American/Black',
    asian == 1 ~ 'Asian/Asian American',
    white == 1 ~ 'Caucasian/White'
  ),
  race = factor(race, levels = c('Other',
                                 'African American/Black',
                                 'Asian/Asian American',
                                 'Caucasian/White')),
  party = case_when(
    political_party == 1 ~ 'Republican',
    political_party == 3 ~ 'Independent',
    political_party == 2 ~ 'Democrat'
  ),
  party = factor(party, levels = c('Republican',
                                   'Independent',
                                   'Democrat')),
  age2 = age^2)
  
# DROP MISSING PARTY DATA
pew = pew_all %>%
  filter(!is.na(political_party))
  
# CLEAN SENDER DATA (from exp2-analysis.R)
data_all = read_dta('data/working/exp2.dta') %>% 
  mutate(condition = case_when(
    excuse==1 ~ 'Excuse',
    noexcuse==1 ~ 'No excuse',
    control==1 ~ 'Control'
  ),
  condition = factor(condition, levels = c('No excuse','Control','Excuse')),
  partisan = case_when(
    partisan== -1 ~ 'Dem-leaning Ind',
    partisan== 1 ~ 'Rep-leaning Ind',
    partisan== 2 ~ 'Weak Rep',
    partisan== 3 ~ 'Strong Rep'
  ),
  partisan = factor(partisan, levels=c('Dem-leaning Ind', 'Rep-leaning Ind', 'Weak Rep','Strong Rep')),
  education = ifelse(education %in% c('Professional degree (JD, MD)', 'Doctoral degree', "Master's degree"), 'Post-bachelor degree', education),
  education = factor(education, levels = c('Less than high school degree', 
                                           'High school graduate (high school diploma or equivalent including GED)',
                                           'Some college but no degree',
                                           'Associate degree in college (2-year)',
                                           "Bachelor's degree in college (4-year)",
                                           'Post-bachelor degree')),
  education2 = case_when(
    education == 'Less than high school degree' ~ 'Less than high school degree',
    education == "High school graduate (high school diploma or equivalent including GED)" ~ "High school graduate (high school diploma or equivalent including GED)", 
    education == "Some college but no degree" ~ "Some college but no degree", 
    education == "Associate degree in college (2-year)" ~ "Associate degree in college (2-year)",
    education == "Bachelor's degree in college (4-year)" | education == 'Post-bachelor degree'  ~ "Bachelor's degree or higher"
  ),
  education2 = factor(education2, levels = c('Less than high school degree',
                                             "High school graduate (high school diploma or equivalent including GED)",
                                             "Some college but no degree",
                                             "Associate degree in college (2-year)",
                                             "Bachelor's degree or higher")),
  race = ifelse(race == 'Native American, Inuit or Aleut' | race == 'Native Hawaiian/Pacific Islander' | hisp == 1, 'Other', race),
  race = factor(race, levels = c('Other', 'African American/Black', 'Asian/Asian American', 'Caucasian/White')),
  agesq = age^2, 
  party = case_when(
    partisan== 'Dem-leaning Ind' | partisan== 'Rep-leaning Ind' ~ 'Independent',
    partisan== 'Weak Rep' | partisan== 'Strong Rep' ~ 'Republican',
    partisan== 'Weak Dem' | partisan== 'Strong Dem' ~ 'Democrat'
  ),
  party = factor(party, levels = c('Republican',
                                   'Independent',
                                   'Democrat'))) %>%
  mutate(white = ifelse(hisp == 1, 0, white))

data = data_all %>% dplyr::filter(attrit!=1)

shares = read_csv('data/raw/exp2-rep-shares.csv') 
data = data %>% inner_join(shares, by='responseid') %>%
  mutate(repshare_bin = as.factor(ntile(share_rep, 2)),
         share_rep = scale(share_rep))


# LIST OF COVARIATES 
# education (education2), race, hisp, white (non-hisp), male, age, age2, party 

# Pew doesn't have explicit variables for bachelor, master or above so i had to 
# infer the values for bachelor and post_bachelor for the education variable
# To avoid inconsistency, I created the variable education2 which combines 
# bachelor and post_bachelor degree as bachelor_higher. 



