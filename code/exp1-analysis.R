library(tidyverse)
library(haven)
library(stargazer)
library(starpolishr)
library(ggsignif)

tablenotes = rjson::fromJSON(file='code/templates/table-notes.json')

motives = read_csv('data/working/exp1-motives.csv')

data_all = read_dta('data/working/exp1.dta') %>%  
  filter(previous_lott==0) %>%
  mutate(condition = case_when(
    excuse==1 ~ 'Excuse',
    noexcuse==1 ~ 'No excuse'
  ),
  condition = factor(condition, levels = c('No excuse','Excuse')),
  partisan = case_when(
    partisan==-2 ~ 'Strong Dem',
    partisan==-1 ~ 'Weak Dem'
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
  inner_join(motives, by='responseid')

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
    tag = 't1-word'
  } else if (outcome == 'gullibility-words') {
    data$outcome = data$gullibility_word
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
    model2 = lm(as.formula(str_interp('outcome~${indvar}+age+I(age^2)+race+hisp+male+education')), data=data %>% filter(main==1))
    model3 = lm(as.formula(str_interp('outcome~${indvar}+age+I(age^2)+race+hisp+male+education+as.factor(partisan)')), data=data  %>% filter(main==1))
    model4 = lm(as.formula(str_interp('outcome~${indvar}')), data=data)
    model5 = lm(as.formula(str_interp('outcome~${indvar}+age+I(age^2)+race+hisp+male+education')), data=data)
    model6 = lm(as.formula(str_interp('outcome~${indvar}+age+I(age^2)+race+hisp+male+education+as.factor(partisan)')), data=data)
  
  out = stargazer(list(model1, model2, model3, model4, model5, model6), 
                  omit='age|race|hisp|male|partisan|education',
                  covariate.labels = c(label1, 'Constant'),
                  keep.stat = c('rsq','adj.rsq', 'n'), 
                  dep.var.labels = '',
                  title = str_interp("Experiment 1: "), label = str_interp('t:1-'),
                  add.lines = list(c('Demographic controls', rep(c('No', 'Yes', 'Yes'), 2)),
                                   c('Partisan affiliation controls', rep(c('No', 'No', 'Yes'), 2))))

  out[11] = ifelse(str_detect(outcome, 'words'), 
                   " & \\multicolumn{6}{c}{Inference about partner's donation motive} \\\\ ",
                   " & \\multicolumn{6}{c}{Inference about partner's score} \\\\ ")

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
    outcome1 = 'Probability of using word relating to bias'
    outcome2 = 'Probability of using word relating to gullibility'
    panel1 = make_panel_part(data, drop_previous = T, outcome='bias-words')
    panel2 = make_panel_part(data, drop_previous = T, outcome='gullibility-words')
  } else if (tabletype == 'validation') {
    tabletitle = 'Relationship between perceived motive and scores'
    outcome1 = 'Bias ($z$-score)'
    outcome2 = 'Gullibility ($z$-score)'
    panel1 = make_panel_part(data, drop_previous = T, outcome='bias-validation')
    panel2 = make_panel_part(data, drop_previous = T, outcome='gullibility-validation')
  }
  panel = c(
    panel1[4],
    str_interp("  \\caption{Experiment 1: ${tabletitle}}"),
    str_interp("  \\label{t:1-${tabletype}}"),
    panel1[7:14],
    '\\midrule',
    str_interp('\\multicolumn{7}{l}{\\textbf{Panel A}: \\textit{${outcome1}}} \\\\'),
    '\\midrule',
    panel1[15:21],
    panel1[24:26],
    '\\midrule',
    '\\midrule',
    str_interp('\\multicolumn{7}{l}{\\textbf{Panel B}: \\textit{${outcome2}}} \\\\'),
    '\\midrule',
    panel2[15:21],
    panel2[24:26],
    '\\midrule',
    '\\midrule',
    'Demographic controls & No & Yes & Yes & No & Yes & Yes \\\\ ',
    'Partisan affiliation controls & No & No & Yes & No & No & Yes \\\\ ',
    'Include pilot data & No & No & No & Yes & Yes & Yes \\\\',
    panel2[27:31]
  )
  writeLines(panel, con = str_interp('output/tables/t1-${tabletype}.tex'))
  panel = c(panel[1], panel[3:23], panel[25:37], panel[39:45], panel[47:48])
  writeLines(panel, con = str_interp('output/tables/t1-${tabletype}-slides.tex'))
  
  
}

make_figure = function(data) {
  summary = data %>% 
    dplyr::select(condition, gullibility_score, gullibility_word, cultural_score, bias_word) %>%
    mutate(cultural_score = 100-cultural_score) %>% # Reverse scale so higher is more bias
    gather(outcome, value, -condition)  %>%
    filter(!is.na(value)) %>%
    group_by(condition, outcome) %>%
    summarise(mean = mean(value), 
              se = sd(value)/sqrt(n())) %>%
    mutate(word = str_detect(outcome, 'word'),
           outcome = ifelse(str_detect(outcome, 'gullibility'), 'Gullibility', 'Bias'))
  
  textsize = 12
  titlesize = 13
  pvals = list()
  pvals[['Gullibilityscore']] = format_p(summary(lm(gullibility_score~condition,data=data))$coefficients['conditionExcuse','Pr(>|t|)'])
  pvals[['Biasscore']] = format_p(summary(lm(cultural_score~condition,data=data))$coefficients['conditionExcuse','Pr(>|t|)'])
  pvals[['Gullibilityword']] = format_p(summary(lm(bias_word~condition,data=data))$coefficients['conditionExcuse','Pr(>|t|)'])
  pvals[['Biasword']] = format_p(summary(lm(gullibility_word~condition,data=data))$coefficients['conditionExcuse','Pr(>|t|)'])
  
  outcome_target = 'Gullibility'
  
  individual_figure = function(word_target, outcome_target, ypos, ylim, tl) {
    if (word_target=='word') {
      lab = 'Fraction using relevant word'
    } else {
      lab = 'Mean guess'
    }
    plot = ggplot(summary %>% filter(word==(word_target=='word'), outcome==outcome_target), aes(x = condition, y = mean, fill = condition)) + 
      geom_bar(stat='identity', alpha=0.65, width=0.5) +
      geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se,col = condition), position = 'dodge', alpha=1, width=0.5) +
      geom_signif(comparisons = list(c('No excuse','Excuse')), 
                  annotations=pvals[[str_interp('${outcome_target}${word_target}')]], 
                  textsize = 6, y_position = ypos, tip_length = tl) +
      xlab('Treatment condition') + ylab(lab) + labs(fill = 'Condition') + 
      guides(fill=F, col=F) +
      theme_bw() +
      scale_fill_manual(values=c('#F8766D','#628DFF')) + scale_color_manual(values=c('#F8766D','#628DFF')) +
      theme(axis.text.x = element_text(size=textsize), axis.text.y = element_text(size=textsize),
            axis.title.x = element_text(size=titlesize), axis.title.y = element_text(size=titlesize)) +
      coord_cartesian(ylim = c(0, ylim)) +
    ggsave(str_interp('output/figures/f1-${outcome_target}${word_target}.pdf'), width=4, height=6, plot)
  }
  
  individual_figure('word', 'Gullibility', 0.115, 0.125, 0.07)
  individual_figure('word', 'Bias', 0.195, 0.21, 0.07)
  individual_figure('score', 'Bias', 73.3, 77, 0.38)
  individual_figure('score', 'Gullibility', 74, 78, 0.25)
  
  plot = ggplot(summary %>% filter(word, outcome==outcome_target), aes(x = condition, y = mean, fill = condition)) + 
    geom_bar(stat='identity', alpha=0.65, width=0.5) +
    geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se,col = condition), position = 'dodge', alpha=1, width=0.5) +
    geom_signif(comparisons = list(c('No excuse','Excuse')), annotations=pvals[[outcome_target]], 
                textsize = 6, y_position = 0.2, tip_length = 0.08) +
    xlab('Treatment condition') + ylab('Fraction using relevant word') + labs(fill = 'Condition') + 
    guides(fill=F, col=F) + theme_bw() +
    scale_fill_manual(values=c('#F8766D','#628DFF')) + scale_color_manual(values=c('#F8766D','#628DFF')) +
    theme(axis.text.x = element_text(size=textsize), axis.text.y = element_text(size=textsize),
          axis.title.x = element_text(size=titlesize), axis.title.y = element_text(size=titlesize)) +
    coord_cartesian(ylim = c(0, 0.22)) +
    
  ggsave(str_interp('output/figures/f1-${outcome_target}.pdf'), width=4, height=6, plot)
  
}

attrition = function(data) {
  data = data %>% 
    mutate(condition = factor(condition, levels = c('No excuse','Excuse')))
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
  out = c(template[1:4], str_interp('\\item \\textit{Notes: }${tablenotes[["t1-attrition"]]}'), template[6:7], "\\caption{Experiment 1: Attrition}  \\label{t:1-attrition} \\\\",
          template[9:27], out[15:93], str_interp('DV mean (no excuse) & ${noexcuse_mean} \\\\'), str_interp('DV mean (excuse) & ${excuse_mean} \\\\'),
          out[94:97], c('\\end{longtable}', '\\end{ThreePartTable}', '\\end{center}'))
  
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
make_figure(data)
attrition(data_all)
make_balance(data)
