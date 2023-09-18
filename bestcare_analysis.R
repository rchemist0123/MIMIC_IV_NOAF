
fwrite(bestDfs, 'bestcare/bestDfs.csv')
fwrite(bestDf, 'bestcare/bestDf.csv')
# Check missing data ------------------------------------------------------
library(naniar)
naniar::gg_miss_var(bestDf, show_pct = T)

# table 1. baseline table ----------------------------------------------------------
require(gtsummary)
require(gt)
bestMatch %>% select(age, sex, bmi, 
                  unit, cci, apache_score, saps2,
                  vis, ventilator, crrt, heart_surgery,
                  map,RR,PR,BT,SpO2,dex_usage) %>% 
  tbl_summary(
    by='dex_usage',
    statistic = list(all_continuous() ~ "{mean} ± {sd}",
                     all_categorical() ~ "{n} ({p})"),
                     # icu_los ~ "{median} [{p25},{p75}]"),
    digits = list(all_continuous() ~ 2,
                  all_categorical() ~ c(0,2)),
    type = list(c(heart_surgery,sex,ventilator) ~ 'dichotomous'),
    value = list(sex ~ 'M'),
    label = c(
      sex ~ 'Sex, %',
      age ~ 'Age, year',
      unit ~ 'ICU unit, %',
      apache_score ~ 'APACHE',
      map ~ 'Mean artery pressure',
      cci ~ 'Charlson comorbidity index',
      vis ~ 'Vasoactive inotropic score',
      crrt ~ 'CRRT, %',
      heart_surgery ~ 'Heart surgery, %',
      bmi ~ 'BMI'
    ),
    
    missing = "no") %>% 
    add_p(test = list(all_continuous() ~ "t.test"),
          pvalue_fun = ~ style_pvalue(., digits=3)) %>% 
    add_overall() %>% 
    # show_header_names()
    # modify_header(label='**Variable**',
    #               stat_0 = '**Overall**\n (n=3,654)',
    #               stat_1 = '**Non-DEX**\n (n=2,764)',
    #               stat_2 = '**DEX**\n (n=890)',
    #               p.value = '***P* value**') %>% 
  as_gt() %>% 
  tab_options(table.font.names = 'Times New Roman')
require(gt)
mytable(dex_usage ~ afib_within_7days, bestCohort)

# table 2. outcome --------------------------------------------------------

tbl_summary(
  data=bestMatch,
  include=c(dex_usage, afib_within_7days, icu_los, inhos_mortality),
  label = list(
    afib_within_7days ~ 'New onset Atrial fibrillation, %',
    inhos_mortality ~ 'In-hospital mortality, %',
    icu_los ~ 'ICU Length of stay, day'
  ),
  by=dex_usage,
  type = list(c(afib_within_7days, inhos_mortality) ~ "dichotomous"),
  statistic = list(all_continuous() ~ "{mean} ± {sd}",
                   all_categorical() ~ "{n} ({p})",
                   icu_los ~ "{median} [{p25},{p75}]"),
  digits = list(all_continuous() ~ 1,
                all_categorical() ~ c(0,1)),
  missing='no') %>% 
  add_overall() %>% 
  add_p(pvalue_fun = ~style_pvalue(., digits=3)) %>% 
  # show_header_names()
  # modify_header(label='**Variable**',
  #               stat_0 = '**Overall**\n(n=3,654)',
  #               stat_1 = '**Non-DEX**\n(n=2,764)',
  #               stat_2 = '**DEX**\n(n=890)'
  # ) %>% 
  as_gt() %>% 
  tab_options(table.font.names = "Times New Roman")
temp <- bestDf[pid %in% bestMatch$pid]
bestDf[afib_within_7days==1, summary(t2e)]
mytable(dex_usage~ t2e, temp[afib_within_7days==1], method=2)


bestDf[afib_within_7days==1, .(mean(t2e), sd(t2e))]
bestDf[afib_within_7days==1, .(mean(t2e), sd(t2e)), by=dex_usage]
bestDf[,.(t2e,afib_within_7days)] %>% View()
with(bestDfs[afib_within_7days==1], t.test(t2e ~ dex_usage))

summary(bestDf[afib_within_7days==1 & dex_usage==0, t2e])

with(bestDf[afib_within_7days==1],
     wilcox.test(t2e ~ dex_usage))

# table 3. Regression : AF --------------------------------------------------------------

# cox

uv <- tbl_uvregression(
  data = bestMatch,
  method = coxph,
  
  # method.args = list(binomial()),
  y = Surv(obs_duration, afib_within_7days==1),
  
  exponentiate = T,
  # show_single_row = c(sex, dex_usage, heart_surgery, surgery),
  include = c(sex, age, bmi,unit, dex_usage, heart_surgery, crrt, cci, vis,apache_score),
  label = list(age ~ 'Age',
               sex ~ 'Sex',
               dex_usage ~ 'Dexmedetomidine',
               unit ~ 'ICU Unit',
               heart_surgery ~ 'Heart surgery',
               surgery ~ 'Surgery',
               cci ~ 'Charlson comorbidity index',
               vis ~ 'Vasoactive inotropic score',
               crrt ~ 'CRRT',
               apache_score ~ 'APACHE',
               bmi ~ 'BMI'
               # apache ~ 'APACHE-II'
  ),
  pvalue_fun = ~style_pvalue(., digits=3),
  hide_n = T
) %>% 
  modify_cols_merge(pattern = "{estimate} [{ci}]",
                    rows = !is.na(estimate)) %>% 
  modify_header(label = '**Variable**',
                estimate = '**HR (95% CI)**',
                p.value = '**P value**')

x <- uv$table_body %>% dplyr::filter(p.value<0.2)

form <- paste(x$variable, collapse=' + ')
forms <- paste('Surv(obs_duration, afib_within_7days==1) ~ ', 
               form)
fit <- coxph(as.formula(forms), data=bestDf)
fit %>% summary()
mlt <- tbl_regression(fit,
                      exponentiate = T,
                      pvalue_fun = ~style_pvalue(., digits=3),
                      # show_single_row = c(dex_usage, heart_surgery, surgery),
                      label = list(age ~ 'Age',
                                   # sex ~ 'Sex',
                                   crrt ~ 'CRRT',
                                   dex_usage ~ 'Dexmedetomidine',
                                   unit ~ 'ICU Unit',
                                   heart_surgery ~ 'Heart surgery',
                                   bmi ~ 'BMI',
                                   # surgery ~ 'Surgery',
                                   cci ~ 'Charlson comorbidity index',
                                   vis ~ 'Vasoactive inotropic score',
                                   apache_score ~ 'APACHE'
                      )) %>% 
  modify_cols_merge(pattern = "{estimate} [{ci}]",
                    rows = !is.na(estimate)) %>% 
  modify_header(label = '**Variable**',
                estimate = '**HR (95% CI)**',
                p.value = '**P value**')

tbl_merge(list(uv,  mlt), tab_spanner = c('**Univariate**','**Multivariate**'))


# table 4. Mortality ---------------------------------------------------

# cox

uv <- tbl_uvregression(
  data = bestDf,
  method = coxph,
  
  # method.args = list(binomial()),
  y = Surv(inhos_duration, inhos_mortality==1),
  exponentiate = T,
  # show_single_row = c(sex, dex_usage, heart_surgery, surgery),
  include = c(sex, age, bmi, unit, dex_usage, heart_surgery, crrt, cci, vis, afib_within_7days,apache_score),
  label = list(age ~ 'Age',
               sex ~ 'Sex',
               dex_usage ~ 'Dexmedetomidine',
               unit ~ 'ICU Unit',
               heart_surgery ~ 'Heart surgery',
               surgery ~ 'Surgery',
               cci ~ 'Charlson comorbidity index',
               vis ~ 'Vasoactive inotropic score',
               afib_within_7days ~ 'New onset Atrial fibrillation',
               crrt ~ 'CRRT',
               apache_score ~ 'APACHE'
  ),
  pvalue_fun = ~style_pvalue(., digits = 3),
  hide_n = T
) %>% 
  modify_cols_merge(pattern = "{estimate} [{ci}]",
                    rows = !is.na(estimate)) %>% 
  modify_header(label = '**Variable**',
                estimate = '**HR (95% CI)**',
                p.value = '**P value**')

x <- uv$table_body %>% dplyr::filter(p.value<0.2)
form <- paste(x$variable, collapse=' + ')
forms <- paste('Surv(inhos_duration, inhos_mortality==1) ~ ', 
               form)
fit <- coxph(as.formula(forms), data=bestDf)
fit2 <- step(fit,trace=F)
fit %>% summary()
mlt <- tbl_regression(fit,
                      exponentiate = T,
                      pvalue_fun = ~style_pvalue(., digits = 3),
                      # show_single_row = c(dex_usage, heart_surgery, surgery),
                      label = list(
                        # age ~ 'Age',
                         sex ~ 'Sex',
                         crrt ~ 'CRRT',
                         # dex_usage ~ 'Dexmedetomidine',
                         unit ~ 'ICU Unit',
                         # heart_surgery ~ 'Heart surgery',
                         # cci ~ 'Charlson comorbidity index',
                         vis ~ 'Vasoactive inotropic score',
                         heart_surgery ~ 'Heart  surgery',
                         afib_within_7days ~ 'New onset Atrial fibrillation'
                         # apache ~ 'APACHE-II'
                      )) %>% 
  modify_cols_merge(pattern = "{estimate} [{ci}]",
                    rows = !is.na(estimate)) %>% 
  modify_header(label = '**Variable**',
                estimate = '**HR (95% CI)**',
                p.value = '**P value**')

car::vif(fit)
tbl_merge(list(uv, mlt), tab_spanner = c('**Univariate**','**Multivariate**'))


coxfit <- coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage+sex+age+
                  afib_within_7days + unit + crrt + vis + apache_score, data=bestDf)
summary(coxfit)
multiRegression(coxfit)
rms::vif(coxfit)

# After matching ----------------------------------------------------

# table 5. baseline ----------------------------------------------------------------
bestDf %>% 
  # dplyr::filter() %>% 
  select(age, sex,bmi, 
         unit,apache_score, cci,vis,
         ventilator,crrt, heart_surgery,
         map,RR,PR,BT,SpO2) %>% summary()
  

require(gtsummary)
bestMatch %>% 
  select(age, sex,bmi, 
         unit,apache_score, cci,vis,
         ventilator,crrt, heart_surgery,
         map,RR,PR,BT,SpO2,dex_usage) %>% 
  tbl_summary(
    by='dex_usage',
    statistic = list(all_continuous() ~ "{mean} ± {sd}",
                     all_categorical() ~ "{n} ({p})"),
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0,1)),
    label = c(
      sex ~ 'Sex, %',
      age ~ 'Age, year',
      # icuunit ~ 'ICU unit, %',
      # icu_los ~ 'ICU LOS, day',
      apache_score ~ 'APACHE',
      crrt ~ 'CRRT, %',
      heart_surgery ~ 'Heart surgery, %',
      # surgery ~ 'Surgery, %',
      bmi ~ 'BMI',
      cci ~ 'Charlson comorbidity score',
      vis ~ 'Vasoactive inotropic score',
      unit_MICU='ICU Unit: MICU',
      unit_SICU = 'ICU Unit: SICU',
      unit_NCU = 'ICU Unit: NCU',
      unit_CICU = 'ICU Unit: CICU',
      unit_OTHERS = 'ICU Unit: Others'
    ),
    type=list(c(sex, heart_surgery, crrt) ~ 'dichotomous'),
    value = list(sex='M'),
    missing = "no") %>% 
  # add_difference() %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits=3),
        test = list(all_continuous() ~ 't.test',
                    all_categorical() ~ 'chisq.test',
                    all_dichotomous() ~ 'chisq.test')) %>% 
  add_overall() %>% 
  # show_header_names()
  modify_header(label='**Variable**',
                stat_0 = '**Overall**\n (n=1,676)',
                stat_1 = '**Non-DEX**\n (n=838)',
                stat_2 = '**DEX**\n (n=838)',
                p.value = '***P* value**')

require(tableone)
tab <- CreateTableOne(vars=c('age','sex', 'bmi','unit','cci',
                             'apache_score',
                             'vis','ventilator', 'crrt','heart_surgery',
                             'map','RR','PR','BT','SpO2'),
                      factorVars=c('sex','unit','heart_surgery',
                                   'crrt','ventilator'),
                      strata='dex_usage',
                      data=bestMatch,
                      addOverall = T,
                      smd = T,
                      includeNA = F)

print(tab,smd=T, catDigits = 1, contDigits = 1,pDigits = 3 ) %>% kableone() %>% 
  kableExtra::kable_classic()

# table 6. outcome table -----------------------------------------------------------

tbl_summary(
  data=bestMatch,
  include=c(dex_usage, afib_within_7days, t2e,icu_los, inhos_mortality),
  label = list(
    afib_within_7days ~ 'New onset Atrial fibrillation, %',
    inhos_mortality ~ 'In-hospital mortality, %',
    icu_los ~ 'ICU Length of stay, day',
    t2e ~ 'Time to development of atrial fibrillation, day'
  ),
  by=dex_usage,
  type = list(c(afib_within_7days, inhos_mortality) ~ "dichotomous"),
  statistic = list(all_continuous() ~ "{mean} ± {sd}",
                   all_categorical() ~ "{n} ({p})",
                   icu_los ~ "{median} [{p25}, {p75}]"),
  digits = list(all_continuous() ~ 1,
                all_categorical() ~ c(0,1)),
  missing='no') %>% 
  add_overall() %>% 
  add_p(pvalue_fun = ~style_pvalue(., digits=3),
        test = list(all_continuous()~'wilcox.test')) %>% 
  # show_header_names()
  # modify_header(label='**Variable**',
  #               stat_0 = '**Overall**\n(n=1,676)',
  #               stat_1 = '**Non-DEX**\n(n=838)',
  #               stat_2 = '**DEX**\n(n=838)'
  # ) %>% 
  as_gt() %>% 
  tab_options(table.font.names = "Times New Roman")
bestMatch[afib_within_7days==1, .(mean(t2e), sd(t2e))]
bestMatch[afib_within_7days==1, .(mean(t2e), sd(t2e)), by=dex_usage]

summary(bestMatch[afib_within_7days==1 &
                    dex_usage==1, t2e])

with(bestMatch[afib_within_7days==1], wilcox.test(t2e ~ dex_usage))




# table 7. ----------------------------------------------------------------
fit <- coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage, bestMatch)
fit <- coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage + age + sex + apache, bestMatch)
bestMatch$dex_usage <- factor(bestMatch$dex_usage, levels = c(0,1))
tbl_regression(
  fit,
  exponentiate = T,
  pvalue_fun = ~style_pvalue(., digits=3)
)


uv <- tbl_uvregression(
  data = bestMatch,
  method = coxph,
  
  # method.args = list(binomial()),
  y = Surv(obs_duration, afib_within_7days==1),
  
  exponentiate = T,
  # show_single_row = c(sex, dex_usage, heart_surgery, surgery),
  include = c(sex, age, bmi,unit, dex_usage, heart_surgery, crrt, cci, vis,apache_score),
  label = list(age ~ 'Age',
               sex ~ 'Sex',
               dex_usage ~ 'Dexmedetomidine',
               unit ~ 'ICU Unit',
               heart_surgery ~ 'Heart surgery',
               surgery ~ 'Surgery',
               cci ~ 'Charlson comorbidity index',
               vis ~ 'Vasoactive inotropic score',
               crrt ~ 'CRRT',
               apache_score ~ 'APACHE',
               bmi ~ 'BMI'
               # apache ~ 'APACHE-II'
  ),
  pvalue_fun = ~style_pvalue(., digits=3),
  hide_n = T
) %>% 
  modify_cols_merge(pattern = "{estimate} [{ci}]",
                    rows = !is.na(estimate)) %>% 
  modify_header(label = '**Variable**',
                estimate = '**HR (95% CI)**',
                p.value = '**P value**')

x <- uv$table_body %>% dplyr::filter(p.value<0.2)

form <- paste(x$variable, collapse=' + ')
forms <- paste('Surv(obs_duration, afib_within_7days==1) ~ ', 
               form)
fit <- coxph(as.formula(forms), data=bestDf)
fit %>% summary()
mlt <- tbl_regression(fit,
                      exponentiate = T,
                      pvalue_fun = ~style_pvalue(., digits=3),
                      # show_single_row = c(dex_usage, heart_surgery, surgery),
                      label = list(age ~ 'Age',
                                   sex ~ 'Sex',
                                   crrt ~ 'CRRT',
                                   dex_usage ~ 'Dexmedetomidine',
                                   unit ~ 'ICU Unit',
                                   heart_surgery ~ 'Heart surgery',
                                   # bmi ~ 'BMI',
                                   # surgery ~ 'Surgery',
                                   cci ~ 'Charlson comorbidity index',
                                   vis ~ 'Vasoactive inotropic score',
                                   apache_score ~ 'APACHE'
                      )) %>% 
  modify_cols_merge(pattern = "{estimate} [{ci}]",
                    rows = !is.na(estimate)) %>% 
  modify_header(label = '**Variable**',
                estimate = '**HR (95% CI)**',
                p.value = '**P value**')

tbl_merge(list(uv,  mlt), tab_spanner = c('**Univariate**','**Multivariate**')) %>% 
  as_gt() %>% 
  tab_options(table.font.names = 'Times New Roman')


# table 8.Mortality ---------------------------------------------------

fit <- coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage, bestMatch)
tbl_regression(
  fit,
  exponentiate = T,
  pvalue_fun = ~style_pvalue(., digits=3)
)

uv <- tbl_uvregression(
  data = bestDf,
  method = coxph,
  
  # method.args = list(binomial()),
  y = Surv(inhos_duration, inhos_mortality==1),
  exponentiate = T,
  # show_single_row = c(sex, dex_usage, heart_surgery, surgery),
  include = c(sex, age, bmi, unit, dex_usage, heart_surgery, crrt, cci, vis, afib_within_7days,apache_score),
  label = list(age ~ 'Age',
               sex ~ 'Sex',
               dex_usage ~ 'Dexmedetomidine',
               unit ~ 'ICU Unit',
               heart_surgery ~ 'Heart surgery',
               surgery ~ 'Surgery',
               cci ~ 'Charlson comorbidity index',
               vis ~ 'Vasoactive inotropic score',
               afib_within_7days ~ 'New onset Atrial fibrillation',
               crrt ~ 'CRRT',
               apache_score ~ 'APACHE'
  ),
  pvalue_fun = ~style_pvalue(., digits = 3),
  hide_n = T
) %>% 
  modify_cols_merge(pattern = "{estimate} [{ci}]",
                    rows = !is.na(estimate)) %>% 
  modify_header(label = '**Variable**',
                estimate = '**HR (95% CI)**',
                p.value = '**P value**')

x <- uv$table_body %>% dplyr::filter(p.value<0.2)
form <- paste(x$variable, collapse=' + ')
forms <- paste('Surv(inhos_duration, inhos_mortality==1) ~ ', 
               form)
forms
fit <- coxph(as.formula(forms), data=bestDf)
fit %>% summary()
mlt <- tbl_regression(fit,
                      exponentiate = T,
                      pvalue_fun = ~style_pvalue(., digits = 3),
                      # show_single_row = c(dex_usage, heart_surgery, surgery),
                      label = list(
                        # age ~ 'Age',
                        sex ~ 'Sex',
                        crrt ~ 'CRRT',
                        # dex_usage ~ 'Dexmedetomidine',
                        unit ~ 'ICU Unit',
                        # heart_surgery ~ 'Heart surgery',
                        # cci ~ 'Charlson comorbidity index',
                        vis ~ 'Vasoactive inotropic score',
                        heart_surgery ~ 'Heart  surgery',
                        afib_within_7days ~ 'New onset Atrial fibrillation'
                        # apache ~ 'APACHE-II'
                      )) %>% 
  modify_cols_merge(pattern = "{estimate} [{ci}]",
                    rows = !is.na(estimate)) %>% 
  modify_header(label = '**Variable**',
                estimate = '**HR (95% CI)**',
                p.value = '**P value**')

tbl_merge(list(uv, mlt), tab_spanner = c('**Univariate**','**Multivariate**'))
bestMatch$obs_duration
bestMatch[,dex_usage:=factor(dex_usage, levels = c(0,1))]
coxph(Surv(obs_duration,afib_within_7days==1)~dex_usage, bestMatch) %>% summary()
coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage, bestMatch) %>% summary()
# figure E5----------------------------------------------------------------
best.name <- c(sex_M = 'Sex: Male',
               age = 'Age',
               bmi = 'Body mass index',
               cci = 'Charlson comorbidity index',
               vis = 'Vasoactive Inotropic Score',
               crrt_Yes = 'Continuous kidney replacement therapy',
               unit_CICU = 'ICU Unit: CICU',
               unit_NCU = 'ICU Unit: NCU',
               unit_SICU = 'ICU Unit: SICU',
               unit_MICU = 'ICU Unit: MICU',
               unit_OTHERS = 'ICU Unit: Other',
               ventilator = 'Mechanical ventilation',
               saps2 = 'SAPS II',
               map = 'Mean arterial pressure',
               RR = 'Respiratory rate',
               PR = 'Pulse rate',
               BT = 'Temperature',
               heart_surgery  = 'Cardiac surgery',
               SpO2 = 'Spo₂',
               apache_score = 'APACHE II')
require(cobalt)
require(ggthemes)
smd_plot <- love.plot(best_m, 
                      abs=T, 
                      thresholds = 0.1,
                      drop.distance = T,
                      # binary='std',
                      stars='none',
                      position='bottom',
                      var.order = 'unadjusted',
                      var.names = best.name,
                      size=2,
                      shapes=c('circle filled','circle'),
                      # line=T,
                      # grid = T
)
smd_plot <- smd_plot + 
  theme_classic(base_family = 'sans')+ 
  labs(x='Absolute Standardized Mean Difference')+
  theme(axis.text.y = element_text(angle = 0,size=6),
        axis.text.x = element_text(size=6),
        axis.title = element_text(size=6),
        axis.line = element_line(size=0.3),
        axis.ticks = element_line(size=0.3),
        title=element_blank(),
        legend.text = element_text(size=6),
        legend.title = element_text(size=6),
        panel.grid.major.y = element_line(color='#F1F1F1'),
        legend.position='bottom')
smd_plot
ggsave(plot=smd_plot,
       filename='result/JAMA network/revised_figures/eFigure4.wmf',
       device = 'wmf',
       width=150,
       height=120,
       units='mm',
       dpi=300)
bestMatch %>% glimpse()
# figureE6_A: kaplan meier ------------------------------------------------------------
bestMatch[,obs_duration_day := obs_duration/24]
bestMatch[,dex_usage := factor(dex_usage, levels=c(1,0))]
require(ggsci)
require(ggthemes)
require(ggpubr)
bestMatch[,.N,]
fit <- survfit(Surv(obs_duration_day, afib_within_7days==1) ~ dex_usage, bestMatch)
km1 <- ggsurvplot(fit, data=bestMatch,
           fun='event',
           censor=F,
           break.time.by=1,
           size = 0.3,
           xlim = c(0,7),
           fontsize=2,
           linetype='strata',
           legend.title="DEX",
           # legend.labs=c('DEX','Non-DEX'),
           font.family='sans')

km1$plot <- km1$plot+
  theme_classic(base_family = 'sans') +
  labs(x="Time, d",
       y="Incidence") +
  # ggtitle("Cumulative incidence of new-onset atrial fibrillation for dexmedetomidine use")+
  scale_y_continuous(breaks = seq(0,0.3,0.05))+
  theme(text=element_text(family="sans"),
        # axis.title = element_text(size=8),
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position = 'none',
        legend.text = element_text(size=7),
        legend.title = element_text(size=7),
        axis.line=element_line(size = 0.4),
        axis.ticks=element_line(size=0.4)) +
  # scale_color_hue(direction=-1) +
  scale_color_d3() +
  annotate('text',x=6.5, y=0.03, label="italic(P)<.001",
           family='sans', size=4, parse=T) +
  annotate('text', x=2, y=0.26,  size=3,label='No dexmedetomidine')+
  annotate('text', x=4, y=0.1, size=3,label='Dexmedetomidine')
km1
km1$table <- km1$table + 
  theme_void(base_family = 'sans') +
  # theme_stata(base_family = 'serif') +
  labs(x="", y="") + 
  theme(
    # axis.title.y = element_text(size=10,angle = 0),
    axis.text.y = element_text(angle = 0, size=7,hjust=1),
    text= element_text(size=) #number at risk
  )

km1
require(patchwork)
figureE6_A <- km1$plot/km1$table +
  plot_layout(heights = c(7.5,2.5))

figureE6_A
ggsave(filename = "result/JAMA network/revised_figures/eFigure5A.wmf",
       plot = km1$plot,
       device = 'wmf',
       width = 150,
       units = 'mm',
       dpi = 300)

# figure E6_B -------------------------------
fit2 <- survfit(Surv(inhos_duration, inhos_mortality==1) ~ dex_usage, data=bestMatch[inhos_duration>0])
km2 <- ggsurvplot(fit2, data=bestMatch[inhos_duration>0],
           censor=F,
           break.time.by=5,
           # pval = 0.699,
           size = 0.3,
           linetype = 'strata',
           pval.coord=c(1,0.25),
           pval.digits=3,
           xlim = c(0,30),
           fontsize=2,
           legend.title="DEX",
           legend.labs=c('DEX','non-DEX'),
           font.family='sans')

km2$plot <- km2$plot+
  labs(x="Time, d",
       y="Survival probability") +
  # ggtitle("Cumulative incidence of new-onset atrial fibrillation for dexmedetomidine use")+
  scale_y_continuous(breaks = seq(0,1,0.2))+
  theme(text=element_text(family="sans"),
        # axis.title = element_text(size=8),
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position = 'none',
        legend.text = element_text(size=7),
        legend.title = element_text(size=7),
        axis.line=element_line(size = 0.4),
        axis.ticks=element_line(size=0.4)) +
  # scale_color_hue(direction=-1) +
  scale_color_d3() +
  annotate('text',x=3, y=0.15, label="italic(P)==0.699",
           family='sans', size=4, parse=T) +
  annotate('text', x=22.5, y=0.63,  size=3,label='No dexmedetomidine')+
  annotate('text', x=18, y=0.4, size=3,label='Dexmedetomidine')

km2
km2$table <- km2$table + 
  theme_void(base_family = 'sans') +
  # theme_stata(base_family = 'serif') +
  labs(x="", y="") + 
  theme(
    # axis.title.y = element_text(size=10,angle = 0),
    axis.text.y = element_text(angle = 0, size=5,hjust=1),
    text= element_text(size=6) #number at risk
  )
figureE6_B
figureE6_B <- km2$plot/km2$table +
  plot_layout(heights = c(7.5,2.5))
ggsave(filename = "result/JAMA network/revised_figures/eFigure5B.wmf",
       plot = km2$plot,
       device = 'wmf',
       width = 150,
       units = 'mm',
       dpi = 300)

bestMatch[,.N, by=.(dex_usage,inhos_mortality)]
bestDf[obs_duration==0,.(pid, intime, outtime, afib_onset_time)]

 # 층화분석 --------------------------------------------------------------------
bestMice_comp[,group:=as.factor(ifelse(surgery=='Yes' & age65==1,1,
                             ifelse(surgery=='Yes' & age65==0,2,
                                    ifelse(surgery=='No' & age65==1,3,4))))]
model <- dlply(bestMice_comp, 'group', function(df)
  coxph(
    Surv(inhos_duration, inhos_mortality == 1) ~ dex_usage +
      sex + age + bmi + unit + apache + crrt + icu_los + afib_within_7days +
      map + PR + RR + BT + SpO2 + cci + vis,
    data = df
  ))

table(bestMice_comp$group)
bestMice_comp[,coef(coxph(Surv(inhos_duration, inhos_mortality==1) ~ dex_usage)), by=group]

ldply(model,coef)
l_ply(model, summary, .print=T)


levels(bestMice_comp$group)

L <- list()
for(i in levels(bestMice_comp$group)){
  L[[i]] <- coxph(Surv(inhos_duration, inhos_mortality==1) ~ dex_usage +
          sex+age+bmi+unit+apache+crrt+icu_los+afib_within_7days +
          map+PR+RR+BT+SpO2+cci+vis,
        data = bestMice_comp,
        subset = group==i)
}

summary(L$`2`)[[8]]

# 1. 수술여부

bestDf$inhos_mortality
coxph(Surv(inhos_duration, inhos_mortality==1) ~ dex_usage + sex + age + unit +
        cci + crrt,
      data=bestDf[heart_surgery=='Yes']) %>% summary()

bestMice_comp$inhos_mortality
coxph(Surv(inhos_duration, inhos_mortality)~ dex_usage + 
        sex+age+bmi+unit+apache+crrt+icu_los+afib_within_7days +
        map+PR+RR+BT+SpO2+cci+vis, data=bestMice_comp[surgery=='Yes'])

temp <- coxph(Surv(inhos_duration, inhos_mortality==1) ~ dex_usage +
                sex+bmi+unit+apache+crrt+icu_los+afib_within_7days +
                dex_usage + 
                map+PR+RR+BT+SpO2+cci+
                heart_surgery + 
                surgery+cci+vis,bestMice_comp[heart_surgery=='No'])


require(Publish)
bestMice_comp[,inhos_mortality:=as.numeric(ifelse(inhos_mortality==1,1,0))]
bestMice_comp[,age65:=as.factor(ifelse(age>=65,1,0))]

bestMice_comp %>% ggplot(aes(x=apache))+geom_histogram()
bestMice_comp[,apache_group:=as.factor(ntile(apache,4))]



rst <- subgroupAnalysis(coxph(Surv(inhos_duration, inhos_mortality==1) ~ dex_usage +
                         sex+bmi+unit+crrt+icu_los+afib_within_7days +
                         map+PR+RR+BT+SpO2+cci+vis,data = bestMice_comp),
                 treatment = 'dex_usage',
                 subgroups = c('surgery','heart_surgery','age65','apache_group'),
                 data = bestMice_comp)

rst
setDT(rst)[,`:=`(ratio1= paste0(event_0,'/',sample_0),
                 ratio2=paste0(event_1,'/',sample_1))][,.(subgroups,level,ratio1,ratio2,
                                                          HR = format(round(HazardRatio,2),nsmall=2),
                                                          lower=format(round(Lower,2),nsmall=2),
                                                          upper=format(round(Upper,2),nsmall=2),
                                                          pinteraction=ifelse(pinteraction<0.001,'<0.001',
                                                                              round(pinteraction,4)))]
bestMice_comp[,mean(apache),by=apache_group]


# forestplot --------------------------------------------------------------

forestdf <- data.table::data.table(
        check.names = FALSE,
           Subgroup = c("Age","< 65","≥ 65",
                        "Any Surgery","No","Yes","Heart surgery","No","Yes",
                        "APACHE","1 (mean:13.3)","2 (mean:21.7)","3 (mean:27.2)",
                        "4 (mean:35.7)"),
          `Non-DEX` = c(NA,"161/1722","395/2282",
                        NA,"523/3423","33/581",NA,"552/3980","4/24",NA,
                        "71/1100","94/1035","145/964","246/905"),
                DEX = c(NA,"78/478","158/543",NA,
                        "213/836","23/185",NA,"226/973","10/48",NA,
                        "14/157","42.221","72/292","108/351"),
                 HR = c(NA,1.05,1.07,NA,1.09,
                        1.13,NA,1.12,0.09,NA,1.29,1.02,0.91,1.13),
              lower = c(NA,0.79,0.88,NA,0.92,
                        0.65,NA,0.95,0.03,NA,0.72,0.7,0.68,0.89),
              upper = c(NA,1.39,1.29,NA,1.29,
                        1.97,NA,1.32,0.29,NA,2.32,1.49,1.22,1.43),
  p = c("0.99",NA,NA,"0.907",NA,
                        NA,"< 0.001",NA,NA,"0.6233",NA,NA,NA,NA)
)
subgroups <- c(2,3,5,6,8,9,11,12,13,14)
forestdf$Subgroup[subgroups] <- paste("     ",forestdf$Subgroup[subgroups])

forestdf <- forestdf %>% mutate(lower= ifelse(!is.na(lower),format(round(as.numeric(lower),2),nsmall=2), NA))
forestdf <- forestdf %>% mutate(upper= ifelse(!is.na(upper),format(round(as.numeric(upper),2),nsmall=2), NA))
forestdf <- forestdf %>% mutate(p= ifelse(p=="< 0.001", '<0.001', 
                                              ifelse(!is.na(p),format(round(as.numeric(p),3),nsmall=3), NA)))

forestdf <- forestdf %>% mutate(HR= ifelse(HR=="1 (ref)", '1 (reference)',
                                               ifelse(!is.na(HR),format(round(as.numeric(HR),2),nsmall=2),NA)))
forestdf
forestdf <- forestdf %>% mutate(`HR (95% CI)` = ifelse(!is.na(HR), paste0(HR, ' (', lower, '-', upper, ')'),""))
setnames(forestdf,'p','P for interaction')

shape <- rep(16, times=14)
sizes <- rep(3.25, times=14)

require(forester)
forester(left_side_data = forestdf[,1:3],
         right_side_data= forestdf[,c(8,7)],
         estimate = as.numeric(forestdf[['HR']]),
         estimate_col_name = "HR (95% CI)",
         estimate_precision =2,
         ci_low = as.numeric(forestdf$lower),
         ci_high= as.numeric(forestdf$upper),
         # stripe_colour = "#ffffff",
         # ci_sep="  ", 
         display = T,
         null_line_at = 1,
         xlim=c(0,2),
         dpi=2000,
         justify=c(0,0.5,0.5,0.5,0.5), # 정렬
         point_shapes= shape,
         point_sizes = sizes,
         nudge_x = 0.8,
         # ggplot_width = 30,
         font_family = "serif",
         arrows = T,
         arrow_labels=c('DEX Better', 'Non-DEX Better'),
         render_as = 'jpg'
)


# moderation, mediation ---------------------------------------------------

temp <- coxph(Surv(inhos_duration, inhos_mortality==1) ~ 
                sex + age + dex_usage*afib_within_7days, data=bestDf)

temp <- glm(inhos_mortality ~ sex + age + dex_usage*afib_within_7days,
            family=binomial(link = 'logit'),
            data=bestDf)
summary(temp)
exp(coef(temp))

pmacroModel(7)

require(processR)
labels <- list(X='heart_surgery',
               M='afib_within_7days',
               Y='inhos_mortality',
               C1='sex',C2='age')
covar <- list(name=c('sex','age'),site=list(c('Y'),c('Y')))
moderator <- list(name='dex_usage',site=list(c('b')),pos=2)
drawConcept(labels = labels, 
            covar = covar,
            moderator = moderator,
            nodemode = 2)

modelsSummaryTable(labels=labels, 
                   moderator = moderator,
                   covar=covar,
                   data=bestDf)

temp <- glm(inhos_mortality~ 
        sex + age + dex_usage*afib_within_7days, data=bestDf,family=binomial())

drawModel(labels=labels, data=bestDf,
          covar = covar,
          moderator = moderator,
          nodemode = 2,
          mode = 1,
          whatLabel = 'est')

medSummaryTable()

require(lavaan)
model <- tripleEquation(labels = labels, moderator = moderator,
                        covar = covar)
model
cat(model)
set.seed(7795)
semfit <- sem(model=model, data=bestDf, se='boot',
              bootstrap=1000,
              link='probit')

summary(semfit, ci=T)
medSummaryTable(semfit, vanilla = T)

lav <- '
afib_within_7days ~ heart_surgery + dex_usage + heart_surgery:dex_usage
inhos_mortality ~ heart_surgery + afib_within_7days + sex + age
'
semfit2 <- sem(lav, data=bestDf, se='boot',bootstrap=100,
               estimator='ml')
summary(semfit2, ci=T)

# RERI --------------------------------------------------------------------

require(epiR)
table(interaction_df$inhos_mortality)
interaction_df <- bestMatch[,.(dex_usage, afib_within_7days, inhos_mortality)]
epi_fit <- glm(inhos_mortality ~ dex_usage*afib_within_7days,
               data=interaction_df,
               family='binomial')
summary(epi_fit)
interaction_df$dex_usage
interaction_df[,group:=as.factor(fifelse(dex_usage==1 & afib_within_7days==0,0,
                                         ifelse(dex_usage==1 & afib_within_7days==1,1,
                                                ifelse(dex_usage==0 & afib_within_7days==0,2,3))))]


epi_fit2 <- glm(inhos_mortality ~ group, data=interaction_df, family='binomial')

summary(epi_fit2)
epiR::epi.interaction(epi_fit2, coef=c(2,3,4), param='dummy')


x <- summary(epi_fit2)
exp(epi_fit2$coefficients)
rr1 <- exp(epi_fit2$coefficients[2])
rr2 <- exp(epi_fit2$coefficients[3])
rr3 <- exp(epi_fit2$coefficients[4])


reri = rr3-rr1-rr2+1
reri
ORplot(epi_fit2, show.CI = T)

match_data[, .(event=sum(inhos_mortality), .N),by=.(dex_usage,afib_within_7days)]


# subgroup anaylsis -------------------------------------------------------

bestMatch$inhos_mortality
require(Publish)
bestMatch[,afib_within_7days:=ifelse(afib_within_7days==1,1,0)]
bestMatch[,inhos_mortality:=ifelse(inhos_mortality==1,1,0)]
bestMatch[,apache_group := as.factor(ifelse(apache_score<median(apache_score),0,1))]
bestMatch[,apache_quantile := as.factor(ntile(apache_score,4))]
bestMatch[,vis_quantile := as.factor(ntile(vis,4))]
fit <- coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage,bestMatch)
fit <- coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage,bestMatch)
fit %>% summary()
bestMatch[,severity_group:=as.factor(ifelse(apache_quantile==4 &
                                    vis_quantile==4,1,
                                  ifelse(apache_quantile==1 &
                                           vis_quantile==1,0,NA)))]


require(Publish)
subgroupAnalTable(fit = fit,
                  trt = 'dex_usage',
                  subgroup = c('sex','age65','surgery',
                               'heart_surgery','crrt',
                               'apache_group','vis_quantile'
                               ),
                  data = bestMatch)

subgroupAnalTable(fit = fit,
                  trt = 'dex_usage',
                  subgroup = c('apache_quantile', 'vis_quantile','severity_group'),
                  data = bestMatch)


summary(bestMatch$apache_score)
bestMatch %>% ggplot(aes(x=apache_score))+
  geom_density()
require(survival)

coxph(Surv(inhos_duration, inhos_mortality)~dex_usage + apache_quantile,
      bestMatch)
coxph(Surv(inhos_duration, inhos_mortality)~dex_usage + vis_quantile,
      bestMatch) %>% summary()




# brady hypo --------------------------------------------------------------

bestBp[,`:=`(
  brady_count = ifelse(category=='PR' & value<60,1,0),
  hypo_count = ifelse(category =='SBP' & value <90,1,0)
)]
bestBp <- inner_join(bestBp, bestDf[,.(pid,intime, outtime)],by='pid')
bestBp <- bestBp[storetime>=intime & storetime<outtime]

bestBradyTime <- bestBp[brady_count>0,.(brady_occur_time=first(storetime)),by=pid]
bestHypoTime <- bestBp[hypo_count>0, .(hypo_occur_time = first(storetime)),by=pid]

bestBradyTime <- toDate(bestBradyTime)
bestHypoTime <- toDate(bestHypoTime)
bestBradyHypo <- bestBp[,.(brady_sum = sum(brady_count),
                           hypo_sum = sum(hypo_count)),by=pid]

bestBradyHypo <- left_join(bestBradyHypo, bestBradyTime, by='pid')
bestBradyHypo <- left_join(bestBradyHypo, bestHypoTime, by='pid')
bestBradyHypo
bestDf <- inner_join(bestDf, bestBradyHypo, by='pid')
bestDf <- inner_join(bestDf, bestBradyHypo[,.(pid,brady_occur_time, hypo_occur_time)], by='pid')


bestDf[,`:=`(
  brady_yn = as.factor(ifelse(brady_sum>0,1,0)),
  hypo_yn = as.factor(ifelse(hypo_sum>0,1,0)),
  icu_duration = as.numeric(difftime(outtime,intime, units='hours'))
)]

bestDf[,`:=`(
  brady_duration = as.numeric(ifelse(brady_yn==0,difftime(outtime,intime, units='hours'), difftime(brady_occur_time,intime, units='hours'))),
  hypo_duration = as.numeric(ifelse(hypo_yn==0,difftime(outtime,intime, units='hours'), difftime(hypo_occur_time,intime, units='hours')))
)]

mytable(dex_usage ~ brady_yn+ hypo_yn, bestDf) 


# Sensitivity analysis ----------------------------------------------------


library(ipw)
bestComp$afib_within_7days
bestComp[,afib_within_7days:=as.numeric(ifelse(afib_within_7days==1,1,0))]
ipw_fit <- ipwpoint(exposure = dex_usage, family='binomial',
                    link = 'logit', 
                    numerator = ~ sex + age + cci +ventilator, 
                    denominator = ~sex+ age+ bmi+ cci+ heart_surgery +
                      icuunit + vis+ ventilator+ crrt+ apache_score+ 
                      saps2+ map+ PR+ RR+ BT+ SpO2, 
                    data=bestComp)
names(bestComp)
ipw_fit
summary(ipw_fit$ipw.weights)
ipwplot(ipw_fit$ipw.weights, logscale = F)



names(bestDf)
bestDf[,saps2 := rowSums(.SD),.SDcol=grep('saps_',names(bestDf))]




# sensitivity -------------------------------------------------------------

best_m$X
target <- names(best_m$X);target
target <- c('dex_usage',target)
noaf_f <- paste('Surv(obs_duration, afib_within_7days==1)','~',paste(target,collapse='+'))
inhos_f <- paste('Surv(inhos_duration, inhos_mortality==1)','~',paste(target,collapse='+'))
afib_dex[dex_usage==1, .(icu_intime,dex_starttime)]
summary(afib_dex[,as.numeric(difftime(dex_starttime,icu_intime, units='hours'))])


coxph(as.formula(noaf_f), bestDf) %>% summary()
coxph(as.formula(noaf_f), bestComp) %>% summary()
coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage, bestMatch) %>% summary()
coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage, 
      weights = ipw_fit$ipw.weights, data=bestComp) %>% summary()

coxph(as.formula(inhos_f), bestDf) %>% summary()
coxph(as.formula(inhos_f), bestComp) %>% summary()
coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage, bestMatch) %>% summary()
coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage, 
      weights = ipw_fit$ipw.weights, data=bestComp) %>% summary()


# 약물 사용 분석 ----------------------------------------------------------------

sedatives <- fread('csv/af_sedatives.csv')
sedatives[,amount := ifelse(amount_unit=='mcg',amount/1000,amount)]
seda_df <- sedatives[stay_id %in% df_match$stay_id] %>% 
  dcast(., stay_id ~ drugname, value.var = 'amount',
        fill = 0)
df_match_temp <- merge.data.table(df_match,
                                  seda_df,by='stay_id',all.x=T)
target <- c('fentanyl','ketamin','midazolam','morphine','propofol')
df_match_temp[,(target):=lapply(.SD, \(x) ifelse(is.na(x),0,x)),.SDcol=target]
df_match_temp[,`:=`(
  propofol_yn = ifelse(propofol>0,1,0),
  midazolam_yn = ifelse(midazolam>0,1,0),
  fentanyl_yn = ifelse(fentanyl>0,1,0),
  ketamin_yn = ifelse(ketamin>0,1,0),
  morphine_yn = ifelse(morphine>0,1,0)
)]
mytable(dex_usage ~ propofol_yn + 
          midazolam_yn + fentanyl_yn + ketamin_yn + morphine_yn, df_match_temp) %>% ztable()

df_match_temp[,(target):=lapply(.SD, \(x) x/24),.SDcol=target]
mytable(dex_usage ~ propofol + 
          midazolam + fentanyl + ketamin + morphine, df_match_temp,method=1,digit=2) %>% ztable()

df_match_temp
