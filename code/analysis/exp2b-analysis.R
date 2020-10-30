source('code/analysis/load.R')
library(extrafont)
loadfonts()
library(multcomp)

data_all = read_dta('data/working/exp2b.dta') %>% 
  mutate(condition = case_when(
    excuse==1 ~ 'Excuse',
    noexcuse==1 ~ 'No Excuse',
  ),
  condition = factor(condition, levels = c('No Excuse','Excuse')),
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
  race = ifelse(race == 'Native American, Inuit or Aleut' | race == 'Native Hawaiian/Pacific Islander', 'Other', race),
  race = factor(race, levels = c('Other', 'African American/Black', 'Asian/Asian American', 'Caucasian/White')),
  agesq = age^2)

data = data_all %>% dplyr::filter(attrit!=1)

shares = read_csv('data/raw/exp2b-rep-shares.csv') 
data = data %>% left_join(shares, by='responseid') %>%
  mutate(repshare_bin = as.factor(ntile(share_rep, 2)),
         share_rep = scale(share_rep))

getp = function(model) {
  p = summary(glht(model, linfct = c("excuse - control = 0")))$test$pvalues[1]
  if (p<0.001) {
    return ('$<0.001$')
  }
  return(format.pval(p, digits=2))
}

attrition = function(data) {
  data = data %>% 
    mutate(condition = factor(condition, levels = c('No Excuse','Excuse')))
  model = lm(attrit ~ age+I(age^2)+race+hisp+male+education+as.factor(partisan)+
               excuse:(age+I(age^2)+race+hisp+male+education+as.factor(partisan)), 
             data=data)
  vars = c('Age', 'Age squared', 'Black', 'Asian', 'White', 'Hispanic',
           'Male', 'High school', 'Some college, no degree', 'Associate degree',
           'Bachelor degree', 'Post-bachelor degree', 'Rep-leaning Ind', 'Weak Rep', 'Strong Rep')
  vars_int = paste0('Excuse $\\times$ ', vars)
  out = stargazer(list(model), 
                  covariate.labels = c(vars, vars_int),
                  omit = 'Constant',
                  keep.stat = c('rsq','adj.rsq', 'n'), column.labels = c('Respondent attrited post-randomization'),
                  title = 'Experiment 2b: Attrition', label = 't:2b-attrition')
  out = star_notes_tex(out, note.type = 'threeparttable', note = tablenotes[["t2b-attrition"]])
  template = readLines('code/templates/longtable-template.txt')
  noexcuse_mean = formatC(mean(data$attrit[data$noexcuse==1], na.rm=T), digits=3, format='f')
  excuse_mean = formatC(mean(data$attrit[data$excuse==1], na.rm=T), digits=3, format='f')
  out = c(template[1:21], 
          out[14:104], 
          str_interp('DV mean (no excuse) & ${noexcuse_mean} \\\\'), str_interp('DV mean (excuse) & ${excuse_mean} \\\\'),
          out[104:108], 
          c('\\end{longtable}', '\\end{ThreePartTable}', '\\end{center}'),
          template[22:23],
          str_interp('\\textit{Notes: }${tablenotes[["t2b-attrition"]]}'))
  
  writeLines(out, con = str_interp('output/tables/t2b-attrition.tex'))
}

make_balance = function(data) {
  make_row = function(outcome) {
    dict = c('age'='Age', 'agesq'='Age squared', 'black'='Black', 
             'white'='White', 'asian'='Asian', 'hisp'='Hispanic', 'male'='Male',
             'hs' = 'High school diploma', 'bachelors' = 'Bachelors degree',
             'ind' = 'Independent', 'rep' = 'Republican')
    data$outcome = data[[outcome]]
    model = lm(outcome~excuse, data=data)
    p1 = summary(model)$coefficients['excuse','Pr(>|t|)']
    values = c(mean(data$outcome, na.rm=T), sd(data$outcome, na.rm=T), mean(data$outcome[data$excuse==1], na.rm=T), 
               mean(data$outcome[data$noexcuse==1], na.rm=T), mean(data$outcome[data$control==1], na.rm = T), p1)
    values = c(dict[outcome], map_chr(values, function(x) formatC(x, digits=3, format='f')))
    values = paste(values, collapse = ' & ')
    values = paste(values, '\\\\')
    return(values)
  }
  
  data$white = data$race=='Caucasian/White'
  data$black = data$race=='African American/Black'
  data$asian = data$race=='Asian/Asian American'
  data$hs = data$education!='Less than high school degree'
  data$bachelors = data$education %in% c("Bachelor's degree in college (4-year)", 'Post-bachelor degree')
  data$rep = data$partisan=='Weak Rep' | data$partisan == 'Strong Rep'
  rows = map(c('age','black','asian','white','hisp','male','hs','bachelors','rep'), make_row)
  rows = c(rows[1], '\\addlinespace', rows[2:5], '\\addlinespace', rows[6], '\\addlinespace', rows[7:8], '\\addlinespace', rows[9])
  
  template = readLines('code/templates/exp1-balance-template.tex')
  out = c(template[1:2], '\\caption{Experiment 2b: Balance of covariates}', 
          '\\label{t:2b-balance}', template[5:13], unlist(rows), template[14:19])
  writeLines(out, 'output/tables/t2b-balance.tex')
  out = c(out[1:2], out[4:29], '\\end{threeparttable} \\end{table}')
  writeLines(out, 'output/tables/t2b-balance-slides.tex')
}


attrition(data_all)
make_balance(data)


