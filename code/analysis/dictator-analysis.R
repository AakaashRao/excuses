source('code/analysis/load.R')
library(extrafont)
loadfonts()
library(multcomp)

format_p = function(p) {
  if (p < 0.001) {
    return('p<0.001')
  }
  p = format.pval(p, digits=3)
  return(str_interp('p=${p}'))
}

data_all = read_dta('data/working/dictator.dta') %>% 
  mutate(condition = case_when(
    intolerant==1 ~ 'Intolerant partner',
    gullible==1 ~ 'Gullible partner',
  ),
  condition = factor(condition, levels = c('Intolerant partner','Gullible partner')),
  partisan = case_when(
    partisan== -3 ~ 'Strong Dem',
    partisan== -2 ~ 'Weak Dem',
  ),
  partisan = factor(partisan, levels=c('Strong Dem', 'Weak Dem')),
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

make_figure = function(data) {
  summary = data %>% group_by(condition) %>% 
    summarise(donate = mean(donated, na.rm=T),
              se = sd(donated, na.rm=T)/sqrt(n()),
              value = str_pad(round(donate, 3), 3))
  
  pval = format_p(summary(lm(donated~condition,data=data))$coefficients['conditionGullible partner','Pr(>|t|)'])
  textsize=5
  tl = 0.1
  
  plot = ggplot(summary, aes(x = condition, y = donate, fill = condition)) + 
    geom_bar(stat='identity', alpha=0.65, width=0.5) +
    geom_errorbar(aes(ymin = donate-1.96*se, ymax = donate+1.96*se, col = condition), position = 'dodge', alpha=1, width=0.5) +
    geom_signif(comparisons = list(c('Intolerant partner','Gullible partner')), 
                annotations=pval, 
                textsize = textsize, y_position = 0.66, tip_length = tl) +
    ylab('Bonus authorization rate') +
    coord_cartesian(ylim = c(0, 0.68)) +
    geom_text(aes(x=condition, y = 0.02, label=value, vjust='bottom'), family = 'LM Roman 10', size = 6) +
    theme_excuses +
    two_palette
  
  ggsave('output/figures/fd-main.png', width=8, height=6, plot )
}

make_table = function(data) {
  
  model1 = lm(as.formula(str_interp('donated~condition')), data=data)
  model2 = lm(as.formula(str_interp('donated~condition+age+I(age^2)+race+hisp+male+education+as.factor(partisan)')), data=data)
  
  out = stargazer(list(model1, model2), 
                  omit='age|race|hisp|male|partisan|education',
                  covariate.labels = c('Gullible partner', 'Constant'),
                  keep.stat = c('rsq','adj.rsq', 'n'), 
                  dep.var.labels = '',
                  title = 'Punishment of intolerant vs. gullible types', label = str_interp('t:dictator'),
                  add.lines = list(c('Demographic controls', 'No', 'Yes')))
  
  out = star_notes_tex(out, note.type = 'threeparttable', note = tablenotes[["td-results"]])
  out[12] = '& \\multicolumn{2}{c}{Authorized \\$1 bonus to partner} \\\\'
  write_lines(out, str_interp('output/tables/td-main.tex'))
}

make_figure(data)
make_table(data)

