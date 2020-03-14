library(tidyverse)
library(haven)
library(stargazer)
library(starpolishr)
library(ggsignif)
library(multcomp)

tablenotes = rjson::fromJSON(file='code/templates/table-notes.json')

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
  race = ifelse(race == 'Native American, Inuit or Aleut' | race == 'Native Hawaiian/Pacific Islander', 'Other', race),
  race = factor(race, levels = c('Other', 'African American/Black', 'Asian/Asian American', 'Caucasian/White')),
  agesq = age^2)

data = data_all %>% dplyr::filter(attrit!=1)

shares = read_csv('data/raw/exp2-rep-shares.csv') 
data = data %>% inner_join(shares, by='responseid') %>%
  mutate(repshare_bin = as.factor(ntile(share_rep, 2)),
         share_rep = scale(share_rep))

getp = function(model) {
  p = summary(glht(model, linfct = c("excuse - control = 0")))$test$pvalues[1]
  if (p<0.001) {
    return ('$<0.001$')
  }
  return(format.pval(p, digits=2))
}


main_results = function(data, drop_previous) {
  if (drop_previous) {
    data = data %>% dplyr::filter(previous_lott==0)
    previous_tag = ''
  } else {
    previous_tag = '-previous'
  }
  model1 = lm(donated~excuse+control, data=data %>% dplyr::filter(main==1))
  model2 = lm(donated~excuse+control+age+I(age^2)+race+hisp+male+education, data=data %>% dplyr::filter(main==1))
  model3 = lm(donated~excuse+control+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data  %>% dplyr::filter(main==1))
  model4 = lm(donated~excuse+control, data=data)
  model5 = lm(donated~excuse+control+age+I(age^2)+race+hisp+male+education, data=data)
  model6 = lm(donated~excuse+control+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data)
  pvalues = summary(model1)$coefficients[,'Pr(>|t|)']
  p_excuse_noexcuse = format.pval(pvalues['excuse'], digits=3)
  p_control_noexcuse = format.pval(pvalues['control'], digits=3)
  p_excuse_control = getp(model1)
  mean_main = formatC(mean(data$donated[data$main==1]), digits=3, format='f')
  mean_all = format(mean(data$donated), digits=3, format='f')
  sd_main = formatC(sd(data$donated[data$main==1]), format='f', digits=3)
  sd_all = formatC(sd(data$donated), digits=3, format='f')
  
  out = stargazer(list(model1, model2, model3, model4, model5, model6), 
                  omit='age|race|hisp|male|partisan|education|Constant',
                  covariate.labels = c('Excuse', 'Control'),
                  keep.stat = c('rsq','adj.rsq', 'n'), dep.var.labels = 'Donated to Fund the Wall',
                  title = 'Experiment 2: Main results', label = 't:2-main',
                  add.lines = list(c('Demographic controls', rep(c('No', 'Yes', 'Yes'), 2)),
                                   c('Partisan affiliation controls', rep(c('No', 'No', 'Yes'), 2)),
                                   c('p-value (Excuse = Control)', getp(model1), getp(model2), getp(model3), 
                                     getp(model4), getp(model5), getp(model5), getp(model6))))
  
  means = str_interp('DV mean & ${mean_main} & ${mean_main} & ${mean_main} & ${mean_all} & ${mean_all} & ${mean_all} \\\\')
  sds = str_interp('DV std. dev. & ${sd_main} & ${sd_main} & ${sd_main} & ${sd_all} & ${sd_all} & ${sd_all} \\\\')
  pilot_note = 'Include pilot data & No & No & No & Yes & Yes & Yes \\\\'
  
  out = c(out[1:20], out[24], out[21:23], '\\midrule', pilot_note, '\\addlinespace', means, sds, out[25:31])
  out = star_notes_tex(out, note.type = 'threeparttable', note = tablenotes[["t2-main"]])

  writeLines(out, con = str_interp('output/tables/t2-main${previous_tag}.tex'))
  
  out = c(out[1:4], out[6:32], out[34:35], out[37:38])
  
  writeLines(out, con = str_interp('output/tables/t2-main${previous_tag}-slides.tex'))
  
  summary = data %>% 
    mutate(condition = factor(condition, levels = c('Control','No excuse', 'Excuse'))) %>%
    group_by(condition) %>% 
    summarise(mean = mean(donated), se = sd(donated)/sqrt(n()))
  
  make_figure = function(summary) {
    textsize = 12
    titlesize = 13
    plot = ggplot(summary, aes(x = condition, y = mean, fill = condition)) + 
      geom_bar(stat='identity', alpha=0.65, width=0.5) +
      geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se,col = condition), position = 'dodge', alpha=1, width=0.5) +
      geom_signif(comparisons = list(c('No excuse','Excuse')), annotations=str_interp('p=${p_excuse_noexcuse}'), textsize = 6, y_position = 0.62, tip_length = 0.2) +
      geom_signif(comparisons = list(c('Control','Excuse')), annotations=str_interp('p=${p_excuse_control}'), textsize = 6, y_position=0.69, tip_length=0.2) +
      geom_signif(comparisons = list(c('Control','No excuse')), annotations=str_interp('p=${p_control_noexcuse}'), textsize = 6, y_position=0.55, tip_length = 0.2) +
      xlab('Treatment condition') + ylab('Donation rate') + labs(fill = 'Condition') + 
      guides(fill=F, col=F) +
      theme_bw() + 
      theme(axis.text.x = element_text(size=textsize), axis.text.y = element_text(size=textsize),
            axis.title.x = element_text(size=titlesize), axis.title.y = element_text(size=titlesize)) +
      coord_cartesian(ylim=c(0, 0.8))
    
    ggsave(str_interp('output/figures/f2-main${previous_tag}.pdf'), width=8, height=6, plot)
  }
  make_figure(summary)
  
  
}

party_heterogeneity = function(data, drop_previous) {
  if (drop_previous) {
    data = data %>% dplyr::filter(previous_lott==0)
    previous_tag = ''
  } else {
    previous_tag = '-previous'
  }
  model1 = lm(donated~excuse+control+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data %>% dplyr::filter(rep==1, main==1))
  model2 = lm(donated~excuse+control+age+I(age^2)+race+hisp+male+education, data=data %>% dplyr::filter(rep==1))
  model3 = lm(donated~excuse+control+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data %>% dplyr::filter(rep==0, main==1))
  model4 = lm(donated~excuse+control+age+I(age^2)+race+hisp+male+education, data=data %>% dplyr::filter(rep==0))
  
  pvalues_rep = summary(model1)$coefficients[,'Pr(>|t|)']
  p_excuse_noexcuse_rep = format.pval(pvalues_rep['excuse'], digits=3)
  p_control_noexcuse_rep = format.pval(pvalues_rep['control'], digits=3)
  p_excuse_control_rep = getp(model1)
  mean_main_rep = formatC(mean(data$donated[data$main==1 & data$rep==1]), digits=3, format='f')
  mean_all_rep = format(mean(data$donated[data$rep==1]), digits=3, format='f')
  sd_main_rep = formatC(sd(data$donated[data$main==1 & data$rep==1]), format='f', digits=3)
  sd_all_rep = formatC(sd(data$donated[data$rep==1]), digits=3, format='f')
  
  pvalues_ind = summary(model3)$coefficients[,'Pr(>|t|)']
  p_excuse_noexcuse_ind = format.pval(pvalues_ind['excuse'], digits=3)
  p_control_noexcuse_ind = format.pval(pvalues_ind['control'], digits=3)
  p_excuse_control_ind = getp(model3)
  mean_main_ind = formatC(mean(data$donated[data$main==1 & data$rep==0]), digits=3, format='f')
  mean_all_ind = format(mean(data$donated[data$rep==0]), digits=3, format='f')
  sd_main_ind = formatC(sd(data$donated[data$main==1 & data$rep==0]), format='f', digits=3)
  sd_all_ind = formatC(sd(data$donated[data$rep==0]), digits=3, format='f')
  
  out = stargazer(list(model1, model2, model3, model4), 
                  omit='age|race|hisp|male|partisan|education|Constant',
                  covariate.labels = c('Excuse', 'Control'),
                  column.separate = c(2, 2), column.labels = c('Republicans', 'Independents'),
                  keep.stat = c('rsq','adj.rsq', 'n'), dep.var.labels = 'Donated to Fund the Wall',
                  title = 'Experiment 2: Party heterogeneity', label = 't:2-partyheterogeneity',
                  add.lines = list(c('Demographic controls', rep('Yes', 4)),
                                   c('Partisan affiliation controls', rep('Yes', 4)),
                                   c('p-value (Excuse = Control)', getp(model1), getp(model2), getp(model3), getp(model4))))
  
  means = str_interp('DV mean & ${mean_main_rep} & ${mean_all_rep} & ${mean_main_ind} & ${mean_all_ind} \\\\')
  sds = str_interp('DV std. dev. & ${sd_main_rep} & ${sd_all_rep} & ${sd_main_ind} & ${sd_all_rep} \\\\')
  pilot_note = 'Include pilot data & No & Yes & No & Yes \\\\'
  
  out = c(out[1:21], out[25], out[22:24], '\\midrule', pilot_note, '\\addlinespace', means, sds, out[26:32])
  
  writeLines(star_notes_tex(out, note.type = 'threeparttable', note = tablenotes[["t2-partyheterogeneity"]]),
             con = str_interp('output/tables/t2-partyheterogeneity${previous_tag}.tex'))
  
  summary = data %>% 
    group_by(condition, rep) %>% 
    summarise(mean = mean(donated), se = sd(donated)/sqrt(n())) %>%
    mutate(party= ifelse(rep==1, 'Republican', 'Independent'))

  plot = ggplot(summary %>% dplyr::filter(party=='Republican'), aes(x = condition, y = mean, fill = condition)) + 
    geom_bar(stat='identity', alpha=0.65, width=0.5) +
    geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se,col = condition), position = 'dodge', alpha=1, width=0.5) +
    geom_signif(comparisons = list(c('No excuse','Excuse')), annotations=str_interp('p=${p_excuse_noexcuse_rep}'), textsize = 8, y_position = 0.82, tip_length = 0.2) +
    geom_signif(comparisons = list(c('Control','Excuse')), annotations=str_interp('p=${p_excuse_control_rep}'), textsize = 8, y_position=0.89, tip_length=0.2) +
    geom_signif(comparisons = list(c('Control','No excuse')), annotations=str_interp('p=${p_control_noexcuse_rep}'), textsize = 8, y_position=0.75, tip_length = 0.2) +
    xlab('Treatment condition') + ylab('Donation rate') + labs(fill = 'Condition') + guides(fill=F, col=F) + coord_cartesian(ylim=c(0, 0.95))
  ggsave(str_interp('output/figures/f2-partyheterogeneity-rep${previous_tag}.pdf'), width=4.96, height=6.33, units='in', plot)
  
  plot = ggplot(summary %>% dplyr::filter(party=='Independent'), aes(x = condition, y = mean, fill = condition)) + 
    geom_bar(stat='identity', alpha=0.65, width=0.5) +
    geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se,col = condition), position = 'dodge', alpha=1, width=0.5) +
    geom_signif(comparisons = list(c('No excuse','Excuse')), annotations=str_interp('p=${p_excuse_noexcuse_ind}'), textsize = 8, y_position = 0.49, tip_length = 0.2) +
    geom_signif(comparisons = list(c('Control','Excuse')), annotations=str_interp('p=${p_excuse_control_ind}'), textsize = 8, y_position=0.56, tip_length=0.2) +
    geom_signif(comparisons = list(c('Control','No excuse')), annotations=str_interp('p=${p_control_noexcuse_ind}'), textsize = 8, y_position=0.42, tip_length = 0.2) +
    xlab('Treatment condition') + ylab('Donation rate') + labs(fill = 'Condition') + guides(fill=F, col=F) + coord_cartesian(ylim=c(0, 0.95))
  ggsave(str_interp('output/figures/f2-partyheterogeneity-ind${previous_tag}.pdf'), width=4.96, height=6.33, units='in', plot)
}

city_heterogeneity = function(data, drop_previous) {
  if (drop_previous) {
    data = data %>% dplyr::filter(previous_lott==0)
    previous_tag = ''
  } else {
    previous_tag = '-previous'
  }
  
  model1 = lm(donated~excuse*share_rep+control*share_rep+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data  %>% dplyr::filter(main==1))
  model2 = lm(donated~excuse*share_rep+control*share_rep+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data  %>% dplyr::filter())
  model3 = lm(donated~excuse*share_rep+control*share_rep+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data  %>% dplyr::filter(main==1, rep==1))
  model4 = lm(donated~excuse*share_rep+control*share_rep+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data  %>% dplyr::filter(rep==1))
  model5 = lm(donated~excuse*share_rep+control*share_rep+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data  %>% dplyr::filter(main==1, rep==0))
  model6 = lm(donated~excuse*share_rep+control*share_rep+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data  %>% dplyr::filter(rep==0))
  
  vars.order = c('excuse','excuse:share_rep', 'control','control:share_rep', 'share_rep')
  out = stargazer(list(model1, model2, model3, model4, model5, model6), 
                  omit='age|race|hisp|male|partisan|education|Constant', 
                  order = paste0("^", vars.order , "$"),
                  covariate.labels = c('Excuse', 'Excuse $\\times$ County Republican vote share', 'Control', 'Control $\\times$ County Republican vote share', 'County Republican vote share'),
                  column.separate = c(2, 2, 2), column.labels = c('All', 'Republicans', 'Independents'),
                  keep.stat = c('rsq','adj.rsq', 'n'), dep.var.labels = 'Donated to Fund the Wall',
                  title = 'Experiment 2: County heterogeneity', label = 't:2-cityheterogeneity',
                  add.lines = list(c('Demographic controls', rep('Yes', 6)),
                                   c('Partisan affiliation controls', rep('Yes', 6))))
  mean_main = formatC(mean(data$donated[data$main==1]), digits=3, format='f')
  mean_all = format(mean(data$donated), digits=3, format='f')
  sd_main = formatC(sd(data$donated[data$main==1]), format='f', digits=3)
  sd_all = formatC(sd(data$donated), digits=3, format='f')
  means = str_interp('DV mean & ${mean_main} & ${mean_all} & ${mean_main} & ${mean_all} & ${mean_main} & ${mean_all} \\\\')
  sds = str_interp('DV std. dev. & ${sd_main} & ${sd_all} & ${sd_main} & ${sd_all} & ${sd_main} & ${sd_all} \\\\')
  pilot_note = 'Include pilot data & No & Yes & No & Yes & No & Yes \\\\'  
  out = c(out[1:33], '\\midrule', pilot_note, '\\addlinespace', means, sds, out[34:40])
  out = star_notes_tex(out, note.type = 'threeparttable', note = tablenotes[["t2-cityheterogeneity"]])
  writeLines(out,
             con = str_interp('output/tables/t2-cityheterogeneity-continuous${previous_tag}.tex'))
  out_slides_full = c(out[1:4], out[6:37], out[40:41], out[43:44], out[46:47])
  writeLines(out_slides_full,
             con = str_interp('output/tables/t2-cityheterogeneity-continuous${previous_tag}-fullslides.tex'))

  vars.order = c('excuse','excuse:share_rep', 'share_rep')
  out = stargazer(list(model1, model2), 
                  omit='age|race|hisp|male|partisan|education|Constant|control', 
                  order = paste0("^", vars.order , "$"),
                  covariate.labels = c('Excuse', 'Excuse $\\times$ Rep share', 'Rep share'),
                  keep.stat = c('rsq', 'n'), dep.var.labels = 'Donated to Fund the Wall',
                  title = 'Experiment 2: County heterogeneity', label = 't:2-cityheterogeneity-slides')
  pilot_note = 'Include pilot data & No & Yes \\\\'  
  means = str_interp('DV mean & ${mean_main} & ${mean_all} \\\\')
  sds = str_interp('DV std. dev. & ${sd_main} & ${sd_all} \\\\')
  out_slides = c(out[1:4], out[6:24], '\\midrule', pilot_note, means, sds, out[25:30])
  writeLines(out_slides,
             con = str_interp('output/tables/t2-cityheterogeneity-continuous${previous_tag}-slides.tex'))
}

purpose = function(data) {
  purpose = read_csv('data/raw/exp2-coded-purpose.csv') 
  data = data %>% inner_join(purpose, by = 'responseid')
  excuse_model = lm(purpose_excuse~excuse+control+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data)
  immigration_attitude_model = lm(purpose_immigration_attitude~excuse+control+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data)
  public_image_model = lm(purpose_public_image~excuse+control+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data)
  information_model = lm(purpose_information~excuse+control+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data)
  persuasion_model = lm(purpose_persuasion~excuse+control+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data)
  bias_model = lm(purpose_biased~excuse+control+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data)
  
  summarystats=list()
  summarystats[['means']]=c()
  summarystats[['sds']]=c()
  for (outcome in c('purpose_excuse','purpose_immigration_attitude','purpose_public_image','purpose_information',
                    'purpose_persuasion', 'purpose_biased')) {
    summarystats[['means']] = c(summarystats[['means']], formatC(mean(data[[outcome]], na.rm=T), digits=3, format='f'))
    summarystats[['sds']] = c(summarystats[['sds']], formatC(sd(data[[outcome]], na.rm=T), digits=3, format='f'))
  }
  out = stargazer(list(excuse_model, immigration_attitude_model, public_image_model, information_model, persuasion_model, bias_model), 
                  omit='age|race|hisp|male|partisan|education|Constant',
                  covariate.labels = c('Excuse', 'Control'),
                  keep.stat = c('rsq','adj.rsq', 'n'), dep.var.labels = c('Excuse','Immigration attitudes','Public image','Information','Persuasion','Biased'),
                  title = 'Experiment 2: Perceived purpose of study', label = 't:2-purpose',
                  add.lines = list(c('Demographic controls', rep('Yes', 6)),
                                   c('Partisan affiliation controls', rep('Yes',6 )),
                                   c('p-value (Excuse = Control)', getp(excuse_model), getp(immigration_attitude_model), 
                                     getp(public_image_model), getp(information_model), getp(persuasion_model), getp(bias_model))))
  
  means = str_interp("DV mean & ${summarystats[['means']][1]} & ${summarystats[['means']][2]} & ${summarystats[['means']][3]} & ${summarystats[['means']][4]} & ${summarystats[['means']][5]} & ${summarystats[['means']][6]} \\\\")
  sds = str_interp("DV std. dev. & ${summarystats[['sds']][1]} & ${summarystats[['sds']][2]} & ${summarystats[['sds']][3]} & ${summarystats[['sds']][4]} & ${summarystats[['sds']][5]} & ${summarystats[['sds']][6]} \\\\")
  pilot_note = 'Include pilot data & Yes & Yes & Yes & Yes & Yes & Yes \\\\'
  
  out = c(out[1:19], out[23], out[20:22], '\\midrule', pilot_note, '\\addlinespace', means, sds, out[24:30])
  out = star_notes_tex(out, note.type = 'threeparttable', note = tablenotes[["t2-purpose"]])
  
  writeLines(out, con = str_interp('output/tables/t2-purpose.tex'))
  
  out = c(out[1:4], out[6:31], out[33:34], out[36:37])
  out[9] = " & \\multicolumn{6}{c}{\\textit{Dependent variable: Perceived purpose of survey}} \\\\ " 
  out[11] = " & Excuse & Imm. attitudes & Publicity & Info. & Persuasion & Biased \\\\ "
  out[20] = str_replace(out[20], fixed('p-value (Excuse = Control)'), 'p-value (Ex. = Ctrl.)')
  out[22] = "Demo controls & Yes & Yes & Yes & Yes & Yes & Yes \\\\ " 
  out[23] = "Partisan controls & Yes & Yes & Yes & Yes & Yes & Yes \\\\ " 
  writeLines(out, con = str_interp('output/tables/t2-purpose-slides.tex'))
  
}

attrition = function(data) {
  data = data %>% 
    mutate(condition = factor(condition, levels = c('No excuse','Control','Excuse')))
  model = lm(attrit ~ age+I(age^2)+race+hisp+male+education+as.factor(partisan)+
               excuse:(age+I(age^2)+race+hisp+male+education+as.factor(partisan)), 
             data=data %>% filter(!control))
  vars = c('Age', 'Age squared', 'Black', 'Asian', 'White', 'Hispanic',
           'Male', 'High school', 'Some college, no degree', 'Associate degree',
           'Bachelor degree', 'Post-bachelor degree', 'Rep-leaning Ind', 'Weak Rep', 'Strong Rep')
  vars_int = paste0('Excuse $\\times$ ', vars)
  out = stargazer(list(model), 
                  covariate.labels = c(vars, vars_int),
                  omit = 'Constant',
                  keep.stat = c('rsq','adj.rsq', 'n'), column.labels = c('Respondent attrited post-randomization'),
                  title = 'Experiment 2: Attrition', label = 't:2-attrition')
  out = star_notes_tex(out, note.type = 'threeparttable', note = tablenotes[["t2-attrition"]])
  template = readLines('code/templates/longtable-template.txt')
  noexcuse_mean = formatC(mean(data$attrit[data$noexcuse==1], na.rm=T), digits=3, format='f')
  excuse_mean = formatC(mean(data$attrit[data$excuse==1], na.rm=T), digits=3, format='f')
  out = c(template[1:4], str_interp('\\item \\textit{Notes: }${tablenotes[["t2-attrition"]]}'),
          template[6:27], out[14:104], str_interp('DV mean (no excuse) & ${noexcuse_mean} \\\\'), str_interp('DV mean (excuse) & ${excuse_mean} \\\\'),
          out[104:108], c('\\end{longtable}', '\\end{ThreePartTable}', '\\end{center}'))
  
  writeLines(out, con = str_interp('output/tables/t2-attrition.tex'))
  
}

make_balance = function(data) {
  data = data %>% filter(main==1)
  make_row = function(outcome) {
    dict = c('age'='Age', 'agesq'='Age squared', 'black'='Black', 
             'white'='White', 'asian'='Asian', 'hisp'='Hispanic', 'male'='Male',
             'hs' = 'High school diploma', 'bachelors' = 'Bachelors degree',
             'ind' = 'Independent', 'rep' = 'Republican')
    data$outcome = data[[outcome]]
    model = lm(outcome~excuse, data=data%>% filter(!control))
    p1 = summary(model)$coefficients['excuse','Pr(>|t|)']
    model = lm(outcome~excuse, data=data%>% filter(!noexcuse))
    p2 = summary(model)$coefficients['excuse','Pr(>|t|)']
    model = lm(outcome~noexcuse, data=data%>% filter(!excuse))
    p3 = summary(model)$coefficients['noexcuse','Pr(>|t|)']
    values = c(mean(data$outcome, na.rm=T), sd(data$outcome, na.rm=T), mean(data$outcome[data$excuse==1], na.rm=T), 
               mean(data$outcome[data$noexcuse==1], na.rm=T), mean(data$outcome[data$control==1], na.rm = T), p1, p2, p3)
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
  
  template = readLines('code/templates/exp2-balance-template.tex')
  out = c(template[1:2], '\\caption{Experiment 2: Balance of covariates}', 
          '\\label{t:2-balance}', template[5:13], unlist(rows), template[14:19])
  writeLines(out, 'output/tables/t2-balance.tex')
  out = c(out[1:2], out[4:29], '\\end{threeparttable} \\end{table}')
  writeLines(out, 'output/tables/t2-balance-slides.tex')
}

for (dp in c(T, F)) {
  main_results(data, drop_previous = dp)
  party_heterogeneity(data, drop_previous = dp)
  city_heterogeneity(data, drop_previous=dp)
  purpose(data)
  attrition(data_all)
  make_balance(data)
}



