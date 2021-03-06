
source('code/analysis/load.R')
library(extrafont)
loadfonts()

motives = read_csv('data/working/exp1-motives.csv')
scores = read_dta('data/working/exp1-svr-scores.dta')

data_all = read_dta('data/working/exp1.dta') %>%  
  filter(previous_lott==0) %>%
  mutate(condition = case_when(
    excuse==1 ~ 'Excuse',
    noexcuse==1 ~ 'No Excuse'
  ),
  condition = factor(condition, levels = c('No Excuse','Excuse')),
  partisan = case_when(
    partisan==-3 ~ 'Strong Dem',
    partisan==-2 ~ 'Weak Dem'
  ),
  partisan = factor(partisan, levels=c('Independent','Weak Dem','Strong Dem')),
  education = ifelse(education %in% c('Professional degree (JD, MD)', 'Doctoral degree', "Master's degree"), 'Post-bachelor degree', education),
  education = factor(education, levels = c('Less than high school degree', 
                                           'High school graduate (high school diploma or equivalent including GED)',
                                           'Some college but no degree',
                                           'Associate degree in college (2-year)',
                                           "Bachelor's degree in college (4-year)",
                                           'Post-bachelor degree')),
  race = ifelse(race == 'Native American, Inuit or Aleut' | race == 'Native Hawaiian/Pacific Islander', 'Other', race),
  race = factor(race, levels = c('Other', 'African American/Black', 'Asian/Asian American', 'Caucasian/White'))) 

data = data_all %>% filter(attrit==0) %>%
  left_join(motives, by='responseid') %>% 
  left_join(scores, by='responseid')

format_p = function(p) {
  if (p < 0.001) {
    return('p<0.001')
  }
  p = format.pval(p, digits=3)
  return(str_interp('p=${p}'))
}

make_panel_part = function(data, drop_previous, outcome) {
  if (drop_previous) {
    data = data %>% filter(previous_lott==0)
    previous_tag = ''
  } else {
    previous_tag = '-previous'
  }
  indvar = 'excuse'
  label1 = 'Excuse'
  if (outcome == 'culture') {
    data$outcome = -1*scale(data$cultural_score)
    tag = 't1-score'
  } else if (outcome == 'gullibility') {
    data$outcome = scale(data$gullibility_score)
    tag = 't1-score'
  } else if (outcome == 'bias-words') {
    data$outcome = data$bias_word
    data$outcome2 = -1*data$cultural_score_predicted
    tag = 't1-word'
  } else if (outcome == 'gullibility-words') {
    data$outcome = data$gullibility_word
    data$outcome2 = data$gullibility_score_predicted
    tag = 't1-word'
  } else if (outcome == 'bias-validation') {
    data$outcome = -1*scale(data$cultural_score)
    tag = 't1-validation'
    indvar = 'bias_word'
    label1 = 'Used bias word'
  } else if (outcome == 'gullibility-validation') {
    data$outcome = scale(data$gullibility_score)
    tag = 't1-validation'
    indvar = 'gullibility_word'
    label1 = 'Used gullibility word'
  }
  
    model1 = lm(as.formula(str_interp('outcome~${indvar}')), data=data %>% filter(main==1))
    model2 = lm(as.formula(str_interp('outcome~${indvar}+age+I(age^2)+race+hisp+male+education+as.factor(partisan)')), data=data  %>% filter(main==1))
    model3 = lm(as.formula(str_interp('outcome~${indvar}+age+I(age^2)+race+hisp+male+education+as.factor(partisan)')), data=data)
    models = list(model1, model2, model3)
    out = stargazer(models, 
                    omit='age|race|hisp|male|partisan|education',
                    covariate.labels = c(label1, 'Constant'),
                    keep.stat = c('rsq','adj.rsq', 'n'), 
                    dep.var.labels = '')
    
    if (outcome == 'bias-words' | outcome == 'gullibility-words') {
      model4 = lm(as.formula(str_interp('outcome2~${indvar}')), data=data %>% filter(main==1))
      model5 = lm(as.formula(str_interp('outcome2~${indvar}+age+I(age^2)+race+hisp+male+education+as.factor(partisan)')), data=data  %>% filter(main==1))
      model6 = lm(as.formula(str_interp('outcome2~${indvar}+age+I(age^2)+race+hisp+male+education+as.factor(partisan)')), data=data)
      models = list(model1, model2, model3, model4, model5, model6)
      out = stargazer(models, 
                      omit='age|race|hisp|male|partisan|education',
                      covariate.labels = c(label1, 'Constant'),
                      keep.stat = c('rsq','adj.rsq', 'n'), 
                      column.labels = c('Used keyword', 'Predicted inference about score'),
                      column.separate = c(3,3))
    }
    
  

  out[11] = ifelse(str_detect(outcome, 'words'), 
                   " & \\multicolumn{6}{c}{Inference about partner's donation motive} \\\\ ",
                   " & \\multicolumn{3}{c}{Inference about partner's score} \\\\ ")

  out = star_notes_tex(out, note.type = 'threeparttable', 
                       note = tablenotes[[tag]]) 
  return (out)
  
}

consolidate_panels = function(tabletype) {
  if (tabletype == 'score') {
    tabletitle = 'Inferred bias and gullibility scores'
    outcome1 = 'Bias ($z$-score)'
    outcome2 = 'Gullibility ($z$-score)'
    panel1 = make_panel_part(data, drop_previous = T, outcome='culture')
    panel2 = make_panel_part(data, drop_previous = T, outcome='gullibility')
  } else if (tabletype == 'word') {
    tabletitle = "Inferred donation motives"
    outcome1 = 'Inference about intolerance'
    outcome2 = 'Inference about gullibility'
    panel1 = make_panel_part(data, drop_previous = T, outcome='bias-words')
    panel2 = make_panel_part(data, drop_previous = T, outcome='gullibility-words')
  } else if (tabletype == 'validation') {
    tabletitle = 'Relationship between perceived motive and scores'
    outcome1 = 'Bias ($z$-score)'
    outcome2 = 'Gullibility ($z$-score)'
    panel1 = make_panel_part(data, drop_previous = T, outcome='bias-validation')
    panel2 = make_panel_part(data, drop_previous = T, outcome='gullibility-validation')
  }
  extra_lines_1 = c('Demographic controls & No & Yes & Yes \\\\ ',
                    'Include pilot data & No & No & Yes\\\\')
  extra_lines_2 = c('Demographic controls & No & Yes & Yes & No & Yes & Yes \\\\ ',
                  'Include pilot data & No & No & Yes & No & No & Yes \\\\')
  
  if (tabletype != 'word') {
    panel = c(
      panel1[4],
      str_interp("  \\caption{Experiment 1: ${tabletitle}}"),
      str_interp("  \\label{t:1-${tabletype}}"),
      panel1[7:12],
      " \\cmidrule(lr){2-4}",
      panel1[14],
      '\\midrule',
      str_interp('\\multicolumn{4}{l}{\\textbf{Panel A}: \\textit{${outcome1}}} \\\\'),
      '\\midrule',
      panel1[16:25],
      '\\midrule',
      str_interp('\\multicolumn{4}{l}{\\textbf{Panel B}: \\textit{${outcome2}}} \\\\'),
      '\\midrule',
      panel2[16:25],
      '\\midrule',
      extra_lines_1,
      panel2[26:30]
    )
    panelshort = c(panel[1], panel[3:23], panel[25:36], panel[38:42], panel[44:45])
  } else {
    panel = c(
      panel1[4],
      str_interp("  \\caption{Experiment 1: ${tabletitle}}"),
      str_interp("  \\label{t:1-${tabletype}}"),
      panel1[7:12],
      '\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}',
      panel1[14:16],
      '\\midrule',
      str_interp('\\multicolumn{7}{l}{\\textbf{Panel A}: \\textit{${outcome1}}} \\\\'),
      '\\midrule',
      panel1[17:26],
      '\\midrule',
      str_interp('\\multicolumn{7}{l}{\\textbf{Panel B}: \\textit{${outcome2}}} \\\\'),
      '\\midrule',
      panel2[17:26],
      '\\midrule',
      extra_lines_2,
      panel2[27:31]
    )
    panelshort = c(panel[1], panel[3:25], panel[27:38], panel[40:44], panel[46:47])
  }
  
  writeLines(panel, con = str_interp('output/tables/t1-${tabletype}.tex'))
  writeLines(panelshort, con = str_interp('output/tables/t1-${tabletype}-slides.tex'))
  
  
}

make_figure = function(data) {
  summary = data %>% 
    dplyr::select(condition, gullibility_score, gullibility_word, cultural_score, bias_word, gullibility_score_predicted, cultural_score_predicted) %>%
    mutate(cultural_score = 100-cultural_score,
           cultural_score_predicted = -1*cultural_score_predicted) %>% # Reverse scale so higher is more bias
    gather(outcome, value, -condition)  %>%
    filter(!is.na(value)) %>%
    group_by(condition, outcome) %>%
    summarise(mean = mean(value), 
              se = sd(value)/sqrt(n())) %>%
    mutate(measure = case_when(
      str_detect(outcome, 'word') ~ 'word',
      str_detect(outcome, 'score$') ~ 'score',
      str_detect(outcome, 'score_predicted') ~ 'scorepred'),
           outcome = ifelse(str_detect(outcome, 'gullibility'), 'Gullibility', 'Bias'),
           value = case_when(
             measure=='score' ~ str_pad(round(mean, 1), 2),
             measure!='score' ~ str_pad(round(mean, 3), 3)))
  
  textsize = 5
  pvals = list()
  
  pvals[['Gullibilityscore']] = format_p(summary(lm(gullibility_score~condition,data=data))$coefficients['conditionExcuse','Pr(>|t|)'])
  pvals[['Biasscore']] = format_p(summary(lm(cultural_score~condition,data=data))$coefficients['conditionExcuse','Pr(>|t|)'])
  pvals[['Gullibilityword']] = format_p(summary(lm(gullibility_word~condition,data=data))$coefficients['conditionExcuse','Pr(>|t|)'])
  pvals[['Biasword']] = format_p(summary(lm(bias_word~condition,data=data))$coefficients['conditionExcuse','Pr(>|t|)'])
  pvals[['Gullibilityscorepred']] = format_p(summary(lm(gullibility_score_predicted~condition,data=data))$coefficients['conditionExcuse','Pr(>|t|)'])
  pvals[['Biasscorepred']] = format_p(summary(lm(cultural_score_predicted~condition,data=data))$coefficients['conditionExcuse','Pr(>|t|)'])
  
  individual_figure = function(word_target, outcome_target, ypos, ylim, tl) {
    if (word_target=='word') {
      lab = 'Fraction using relevant word'
    } else if (word_target=='score') {
      lab = 'Mean guess'
    } else {
      lab = 'Mean predicted guess'
    }
    print(pvals[[str_interp('${outcome_target}${word_target}')]])
    plot = ggplot(summary %>% filter(measure==word_target, outcome==outcome_target), aes(x = condition, y = mean, fill = condition)) + 
      geom_bar(stat='identity', alpha=0.65, width=0.5) +
      geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se,col = condition), position = 'dodge', alpha=1, width=0.5) +
      geom_signif(comparisons = list(c('No Excuse','Excuse')), 
                  annotations=pvals[[str_interp('${outcome_target}${word_target}')]], 
                  textsize = textsize, y_position = ypos, tip_length = tl) +
      geom_text(aes(x=condition, y = ylim/40, label=value, vjust='bottom', family = 'LM Roman 10')) +
      ylab(lab) +
      coord_cartesian(ylim = c(0, ylim)) +
      theme_excuses +
      two_palette
    
    ggsave(str_interp('output/figures/f1-${outcome_target}${word_target}.pdf'), width=4, height=4, plot)
  }
  
  individual_figure('word', 'Gullibility', 0.115, 0.125, 0.05)
  individual_figure('word', 'Bias', 0.195, 0.21, 0.05)
  individual_figure('score', 'Bias', 73.3, 77, 0.05)
  individual_figure('score', 'Gullibility', 74, 78, 0.05)
  individual_figure('scorepred', 'Bias', 0.2, 0.215, 0.05)
  individual_figure('scorepred', 'Gullibility', 0.22, 0.23, 0.05)
}

attrition = function(data) {
  data = data %>% 
    mutate(condition = factor(condition, levels = c('No Excuse','Excuse'))) %>% 
    filter(main==1)
  model = lm(attrit ~ age+I(age^2)+race+hisp+male+education+as.factor(partisan)+
               excuse:(age+I(age^2)+race+hisp+male+education+as.factor(partisan)), 
             data=data)
  vars = c('Age', 'Age squared', 'Black', 'Asian', 'White', 'Hispanic',
           'Male', 'High school', 'Some college, no degree', 'Associate degree',
           'Bachelor degree', 'Post-bachelor degree', 'Strong Democrat')
  vars_int = paste0('Excuse $\\times$ ', vars)
  out = stargazer(list(model), 
                  covariate.labels = c(vars, vars_int),
                  omit = 'Constant',
                  keep.stat = c('rsq','adj.rsq', 'n'), column.labels = c('Respondent attrited post-randomization'),
                  title = 'Experiment 1: Attrition', label = 't:1-attrition')
  out = star_notes_tex(out, note.type = 'threeparttable', note = tablenotes[["t1-attrition"]])
  template = readLines('code/templates/longtable-template.txt')
  noexcuse_mean = formatC(mean(data$attrit[data$noexcuse==1], na.rm=T), digits=3, format='f')
  excuse_mean = formatC(mean(data$attrit[data$excuse==1], na.rm=T), digits=3, format='f')
  out = c(template[1:21], 
          out[15:93],
          str_interp('DV mean (no excuse) & ${noexcuse_mean} \\\\'), str_interp('DV mean (excuse) & ${excuse_mean} \\\\'),
          out[94:97], 
          c('\\end{longtable}', '\\end{ThreePartTable}', '\\end{center}'),
          template[22:23],
          str_interp('\\textit{Notes: }${tablenotes[["t1-attrition"]]}'))
  
  writeLines(out, con = str_interp('output/tables/t1-attrition.tex'))
  
}

make_balance = function(data) {
  data = data %>% filter(main==1)
  make_row = function(outcome) {
    dict = c('age'='Age', 'agesq'='Age squared', 'black'='Black', 
             'white'='White', 'asian'='Asian', 'hisp'='Hispanic', 'male'='Male',
             'hs' = 'High school diploma', 'bachelors' = 'Bachelors degree',
             'ind' = 'Independent', 'rep' = 'Republican')
    data$outcome = data[[outcome]]
    model = lm(outcome~excuse, data=data)
    p1 = summary(model)$coefficients['excuse','Pr(>|t|)']
    values = c(mean(data$outcome, na.rm=T), sd(data$outcome, na.rm=T), mean(data$outcome[data$excuse==1], na.rm=T), 
               mean(data$outcome[data$noexcuse==1], na.rm=T), p1)
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
  rows = map(c('age','black','asian','white','hisp','male','hs','bachelors'), make_row)
  template = readLines('code/templates/exp1-balance-template.tex')
  rows = c(rows[1], '\\addlinespace', rows[2:5], '\\addlinespace', rows[6], '\\addlinespace', rows[7:8])
  out = c(template[1:2], '\\caption{Experiment 1: Balance of covariates}', 
          '\\label{t:1-balance}',
          template[5:13], unlist(rows), template[14:19])
  writeLines(out, 'output/tables/t1-balance.tex')
  out = c(out[1:2], out[4:27], '\\end{threeparttable} \\end{table}')
  writeLines(out, 'output/tables/t1-balance-slides.tex')
}


consolidate_panels(tabletype = 'score')
consolidate_panels(tabletype = 'word')
consolidate_panels(tabletype = 'validation')
attrition(data_all)
make_balance(data)
make_figure(data)
