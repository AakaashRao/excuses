source('code/analysis/load.R')
library(extrafont)
loadfonts()

data = read_dta('data/raw/covid.dta') %>%
  filter(!is.na(excuse), !is.na(support_ban)) %>%
  mutate(age = 2020-as.numeric(as.character(as_factor(year))),
         male = sex==1,
         hisp = hispanic==1,
         education = as_factor(education),
         race = as_factor(race)) %>%
  filter(party==1) %>%
  mutate(condition = recode(excuse, `1`='Excuse', `0` = 'No Excuse'),
         condition = factor(condition, levels=c('Control', 'No Excuse','Excuse')),
         partisan = case_when(
           strong_rep == 1 ~ 2,
           strong_rep == 2 ~ 1
         ))

make_figure = function() {
  summary = data %>% 
    group_by(condition) %>% 
    summarise(mean = mean(support_ban),
              n = n(),
              se = sd(support_ban)/sqrt(n),
              value = str_pad(round(mean, 2), 2))
  print(summary)
  
  tl = 0.05
  
  plot = ggplot(summary, aes(x = condition, y = mean, fill = condition)) + 
    geom_bar(stat='identity', alpha=0.65, width=0.5) +
    geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se,col = condition), position = 'dodge', alpha=1, width=0.5) +
    geom_signif(comparisons = list(c('No Excuse','Excuse')), 
                annotations= 'p<0.001', 
                textsize = 4, y_position = 0.6, tip_length = tl) +
    coord_cartesian(ylim=c(0, 0.65)) +
    geom_text(aes(x=condition, y = 0.02, label=value, vjust='bottom'), family = 'LM Roman 10', size = 4.5) +
    ylab('Mean support') + 
    theme_excuses +
    two_palette
  
  ggsave('output/figures/m-support.pdf', height=4, width=6)
}

make_table = function() {
  mean = formatC(mean(data$support_ban), digits=3, format='f')
  sd = formatC(sd(data$support_ban), digits=3, format='f')
  
  model1 = lm(support_ban~condition, data=data)
  model2 = lm(support_ban~condition+age+I(age^2)+race+hisp+male+education, data=data)
  model3 = lm(support_ban~condition+age+I(age^2)+race+hisp+male+education+as.factor(partisan), data=data)
  
  out = stargazer(list(model1, model2, model3), 
                  omit='age|race|hisp|male|partisan|education|Constant',
                  covariate.labels = c('Excuse'),
                  keep.stat = c('rsq','adj.rsq', 'n'), 
                  dep.var.labels = '',
                  title = str_interp("Motivating survey: results"), label = str_interp('t:m-results'),
                  add.lines = list(c('Demographic controls', rep(c('No', 'Yes', 'Yes'), 2)),
                                   c('Partisan affiliation controls', rep(c('No', 'No', 'Yes'), 2)),
                                   c('DV mean', rep(mean, 3)),
                                   c('DV std. dev.', rep(sd, 3))))
  
  out[11] = " & \\multicolumn{3}{c}{Publicly supports permanent ban} \\\\ "
  out = c(out[1:20], '\\midrule', out[21:29])
  
  out = star_notes_tex(out, note.type = 'threeparttable', note = tablenotes[['tm-results']])
  
  writeLines(out, 'output/tables/m-results.tex')
}

make_balance = function() {
  data$age2 = data$age^2
  make_row = function(outcome) {
    dict = c('age'='Age', 'age2'='Age squared', 'black'='Black', 
             'white'='White', 'asian'='Asian', 'hisp'='Hispanic', 'male'='Male',
             'hs' = 'High school diploma', 'bachelors' = 'Bachelors degree')
    data$outcome = data[[outcome]]
    model = lm(outcome~excuse, data=data)
    p1 = summary(model)$coefficients['excuse','Pr(>|t|)']
    values = c(mean(data$outcome, na.rm=T), sd(data$outcome, na.rm=T), mean(data$outcome[data$excuse==1], na.rm=T), 
               mean(data$outcome[data$excuse==0], na.rm=T), p1)
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
  out = c(template[1:2], '\\caption{Motivating survey: Balance of covariates}', 
          '\\label{t:m-balance}',
          template[5:13], unlist(rows), template[14:19])
  writeLines(out, 'output/tables/m-balance.tex')
  out = c(out[1:2], out[4:27], '\\end{threeparttable} \\end{table}')
  writeLines(out, 'output/tables/m-balance-slides.tex')
}

make_figure()
make_table()
make_balance()

