
df_match$dex_usage <- factor(df_match$dex_usage, levels = c(0,1))

temp <- coxph(Surv(inhos_duration, inhos_mortality=='Yes')~dex_usage + sex + age + sapsii + sofa_score, df_match)
mytable(dex_usage ~ inhos_mortality, df_match)
summary(temp)
car::vif(temp)


# missing -----------------------------------------------------------------
target <- c('sex', 'age', 'bmi','ethnicity','cci', 'heart_surgery', 'icuunit', 'vis', 'ventilator', 'crrt_before_afib', 
            'sepsis', 'sofa_score','sapsii', 'map', 'heart_rate', 'resp_rate', 'temperature', 'spo2', 
            'wbc', 'hemoglobin', 'platelet', 'creatinine', 
            'bun', 'lactate', 'sodium', 'chloride', 'potassium', 'calcium', 'bicarbonate', 'magnesium')
df[stay_id %in% df_match$stay_id,..target] %>% summary()
df[,..target] %>% summary()


# table 1. baseline ----------------------------------------------------------

require(gtsummary)
require(tableone)
# tab <- CreateTableOne(vars=names(df_match),
#                factorVars=c('sex','ethnicity','heart_surgery','icuunit','sepsis',
#                             'ventilation','crrt'),
#                strata='dex_usage',
#                data=afib_dex48,
#                addOverall = T,
#                smd = T,
#                includeNA = F)
# print(tab,smd=T, catDigits = 1, contDigits = 2,pDigits = 3 ) %>% kableone() %>% 
#   kableExtra::kable_classic()

require(R6)
require(gtsummary)
tbl_summary (
  include=c(age,sex, bmi, ethnicity,heart_surgery,
            icuunit, cci, vis,ventilation, crrt, 
            sepsis, sofa_score,sapsii,
            map_min,heart_rate_max, resp_rate_max, temperature_max, spo2_min,
            ph_min, po2_min, pco2_max,
            wbc_max, hemoglobin_min, platelets_min, creatinine_max, bun_max, lactate_max,
            sodium_max,chloride_max, potassium_max, calcium_min, bicarbonate_min, magnesium_min),
  by='dex_usage',
  statistic = list(all_categorical() ~ "{n} ({p})",
                   all_continuous() ~ "{mean} ± {sd}"),
  type = list(c('sex') ~ "dichotomous"),
  value= list('sex'~'M'),
  digits = list(all_categorical() ~c(0,2),
                all_continuous()~ 2),
  data=afib_dex48,
  missing='no'
) %>% 
  add_p(test = list(all_categorical()~"chisq.test",
                    all_continuous() ~ "t.test"),
        
        pvalue_fun = ~style_pvalue(., digits=3)) %>%
  add_overall() 


# table 2. outcome ----------------------------------

require(gtsummary)
tbl_summary(
  data=df_match,
  by='dex_usage',
  include=c(afib_within_7days, 
            los_icu,los_hospital,
            inhos_mortality,
            brady, hypo),
  # brady_per_duration,
  # hypo_per_duration),
  statistic = list(all_continuous() ~ "{mean} ± {sd}",
                   all_categorical() ~ "{n} ({p})",
                   c(los_icu, los_hospital)~ "{median} [{p25}–{p75}]"),
  digits = list(all_continuous() ~ 2,
                all_categorical() ~ c(0,2)),
  type = list(all_categorical() ~ "dichotomous"),
  label = list(afib_within_7days ~ "Atrial Fibrillation, %",
               los_icu ~ "ICU LOS, days",
               los_hospital ~ "Hospital LOS, days",
               inhos_mortality ~ "In-hospital mortality, %"
  ),
  missing='no',
) %>% 
  add_overall() %>% 
  add_p(test = list(all_categorical()~"chisq.test",
                    all_continuous() ~ "wilcox.test"),
        pvalue_fun = ~style_pvalue(., digits = 3))


df_match[afib_within_7days==1] %>% 
  tbl_summary(include = c(t2e, rate_control, rhythm_control,
                          cardioversion),
              statistic = list(all_categorical()~"{n} ({p})",
                               t2e ~ "{median} [{p25}-{p75}]"),
              type= all_categorical() ~ "dichotomous",
              digits = list(all_categorical()~ c(0,2),
                            all_continuous() ~ 2),
              by=dex_usage,
              missing='no') %>% 
  add_overall() %>%
  add_p(test = list(all_categorical()~"chisq.test",
                    all_continuous() ~ "wilcox.test"),
        pvalue_fun = ~style_pvalue(., digits=3))
  

# table 3. treatment ------------------------------------------------------
table(df_match$brady_per_duration==0)
require(gtsummary)

dfPills[,`:=`(
  rate_control_yn = as.factor(ifelse(diltiazem ==1 | 
                                        esmolol ==1 |
                                        metoprolol == 1 |
                                        digoxin == 1, 1,0)),
  rhythm_control_yn=as.factor(ifelse(amiodarone ==1 | 
                                        magnesium==1 |
                                        sotalol==1 |
                                       dronedarone==1 |
                                       propafenone==1 |
                                       flecainide == 1,1,0))
)]
mytable(dex_usage ~ rate_control_yn + rhythm_control_yn,
        dfPills[afib_within_7days==1])
tbl_summary(
  data= dfPills[afib_within_7days==1],
  statistic = list(all_categorical() ~ "{n} ({p})"),
  include = c( rate_control_yn, rhythm_control_yn),
  by=dex_usage,
  type = list(all_categorical() ~ "dichotomous"),
  digits = list(all_categorical()~c(0,1)),
  missing='no',
  
) %>% 
  add_overall() %>% 
  add_p(test = list(all_categorical()~"chisq.test",
                    all_continuous() ~ "t.test"),
        pvalue_fun = ~style_pvalue(., digits=3)) 
  # show_header_names()
  # modify_header(label='**Variable**',
  #               stat_0 = '**Overall**\n(n=1,563)',
  #               stat_1 = '**Non-DEX**\n(n=1,136)',
  #               stat_2 = '**DEX**\n(n=427)')

quantile(df_match[afib_within_7days==1, t2e])
mean(df_match[afib_within_7days==1, t2e])
df_match[afib_within_7days==1, paste(mean(t2e), quantile(t2e,.25), quantile(t2e, .75)),by=dex_usage]
summary(df_match[afib_within_7days==1,t2e])

mytable(dex_usage ~ cardioversion, df_match[afib_within_7days==1])

#t2e
df_match %>% dplyr::filter(afib_within_7days==1) %>% 
  group_by(dex_usage) %>%
  summarise(t2e_mean=mean(t2e), t2e_sd= sd(t2e))
mytable(dex_usage ~ quantile(t2e,probs=.25) + quantile(t2e,probs=.75), data=df_match[afib_within_7days==1])
require(tableone)
require(kableExtra)
tableone::CreateTableOne(vars=c(
  'brady','hypo',
  'los_icu','los_hospital', 't2e',
  'inhos_mortality','mortality_90','ventil_duration'),
  factorVars = c('inhos_mortality','brady','hypo'),
  strata='dex_usage',
  addOverall = T,
  data=df_match) %>% kableone() %>% kable_paper()

with(temp[afib_within_7days==1],
     t.test(t2e_days ~ dex_usage))

# table 4. univariate and multivariate -------------------

cox_fit <- coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage, df_match)
cox_fit <- coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage, df_match)
exp(cbind(coef(cox_fit),confint(cox_fit)))
cox.zph(cox_fit)



# table 5. subgroup analysis ----------------------------------------------

df_match[,age65 := ifelse(age>=65,1,0)]
df_match[,race:= as.factor(ifelse(ethnicity %like% 'African-American', 'African American','etc'))]
df_match[,cvicu := as.factor(ifelse(icuunit %like% 'CVICU','CVICU','Non-CVICU'))]

sub_df <- df_match[,.( obs_duration, afib_within_7days, age65, race, cvicu , dex_usage ,  
                         sex,vis,  sepsis  ,  cci , crrt , heart_surgery,
                       ventilation, inhos_mortality,
                       inhos_duration)]
sub_df[,dex_usage := factor(dex_usage, levels = c(0,1))]
sub_fit <- coxph(Surv(obs_duration, afib_within_7days==1) ~ 
                 dex_usage, data=sub_df)
summary(sub_fit)
sub_df[,afib_within_7days:=as.numeric(ifelse(afib_within_7days==1,1,0))]
# sub_fit <- coxph(Surv(inhos_duration, inhos_mortality==1) ~ dex_usage,
#                  sub_df)

# sub_df[,inhos_mortality:= ifelse(inhos_mortality=='Yes',1,0)]
sub_df[,vis_quantile := as.factor(ntile(vis,4))]
target <- c('sex','age65', 
            'race','cvicu',
            'sepsis','heart_surgery', 
            'ventilation','crrt')
sub_df[,(target):=lapply(.SD, as.factor),.SDcols=target]

require(Publish)
subgroupAnalTable(fit=sub_fit, 
                  trt='dex_usage', 
                  subgroup = target, data=sub_df)
df_new <- as.data.table(match.data(m))
df_new[,dex_usage:=as.factor(dex_usage)]
df_new[,afib_within_7days := as.numeric(ifelse(afib_within_7days==1,1,0))]
fit <- coxph(Surv(obs_duration, afib_within_7days==1) ~ dex_usage, df_new)
summary(sub_fit)

# addition: sofa median group
df_match2[,sofa_group := as.factor(fifelse(sofa_score<median(sofa_score),0,1))]
df_match_id[,saps_group := as.factor(fifelse(sapsii<median(sapsii),0,1))]



# table E1. ---------------------------------------------------------
require(kableExtra)
require(tableone)
tab <- CreateTableOne(vars=c('age', 'sex','bmi', 'ethnicity','heart_surgery',
                             'icuunit','cci','vis','ventilation', 'crrt', 
                             'sepsis', 'sofa_score','sapsii',
                             'map_min','heart_rate_max', 'resp_rate_max', 'temperature_max', 'spo2_min',
                             'ph_min', 'po2_min', 'pco2_max',
                             'wbc_max', 'hemoglobin_min', 'platelets_min', 'creatinine_max', 'bun_max', 'lactate_max',
                             'sodium_max','chloride_max', 'potassium_max', 'calcium_min', 'bicarbonate_min', 'magnesium_min'),
                      factorVars=c('sex','ethnicity','heart_surgery','icuunit','sepsis',
                                   'ventilation','crrt'),
                      strata='dex_usage',
                      data=afib_dex48,
                      addOverall = T,
                      smd = T,
                      includeNA = F)
print(tab,smd=T, catDigits = 1, contDigits = 1,pDigits = 3 ) %>% kableone() %>% 
  kableExtra::kable_classic()

tbl_summary (
  include=c(age,sex,bmi, ethnicity,heart_surgery,
            icuunit, cci, vis,ventilation, crrt, 
            sepsis, sofa_score,sapsii,
            map_min,heart_rate_max, resp_rate_max, temperature_max, spo2_min,
            ph_min, po2_min, pco2_max,
            wbc_max, hemoglobin_min, platelets_min, creatinine_max, bun_max, lactate_max,
            sodium_max,chloride_max, potassium_max, calcium_min, bicarbonate_min, magnesium_min),
  by='dex_usage',
  missing = 'no',
  statistic = list(all_categorical() ~ "{n} ({p})",
                   all_continuous() ~ "{mean} ± {sd}"),
  type = list(c('sex') ~ "dichotomous"),
  value= list('sex'~'M'),
  digits = list(all_categorical() ~c(0,1),
                all_continuous()~ 3),
  
  # label = tab1Label,
  data=afib_dex48,
) %>% 
  add_p(test = list(all_categorical()~"chisq.test",
                    all_continuous() ~ "t.test"),
        
        pvalue_fun = ~style_pvalue(., digits=3)) %>%
  add_overall() 

# Table E2. -------------------------------------------------------------------

interaction_df <- df_match[,.(dex_usage, afib_within_7days, inhos_mortality)]
interaction_df[,c('dex_usage','afib_within_7days'):=lapply(.SD, function(x) as.numeric(ifelse(x==0,0,1))),
               .SDcol=c('dex_usage','afib_within_7days')]
# epiR
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

extractOR(epi_fit2)
ORplot(epi_fit2, show.CI = T)


# sensitivity analysis: age65  ---------------------------------



# supple table 3 ----------------------------------------------------------

uv <- tbl_uvregression(
  data = df_comp,
  method = coxph,
  y= Surv(obs_duration, afib_within_7days==1),
  exponentiate = T,
  include = c(sex,age,bmi,race,heart_surgery, cvicu, dex_usage, vis,sepsis,sofa_score,cci,ventilator,crrt_before_afib, map,heart_rate, resp_rate, temperature, hemoglobin, wbc, creatinine, bun,spo2, lactate, platelet, sodium,chloride, potassium, calcium, bicarbonate, magnesium),
  show_single_row = c(sex, race, heart_surgery, sepsis, ventilator, crrt_before_afib, cvicu),
  hide_n = T,
  # label = regLabel,
) %>% modify_cols_merge(pattern = "{estimate} [{ci}]",
                        row = !is.na(estimate)) %>% 
  modify_header(label="**Variable**", estimate="**HR (95% CI)**")

x <- uv$table_body %>% dplyr::filter(p.value<0.2)
form <- paste(x$variable, collapse=' + ')
forms <- paste('Surv(obs_duration, afib_within_7days==1)', 
               form, sep=' ~ ')
fit <- coxph(as.formula(forms) , data=df_comp)
fit
fit2 <- step(fit,trace=F)

mult <- tbl_regression(
  fit2,
  exponentiate = T,
  show_single_row = all_dichotomous()
)%>% modify_cols_merge(pattern = "{estimate} [{ci}]",
                       row = !is.na(estimate)) %>% 
  modify_header(label="**Variable**", estimate="**HR (95% CI)**")

tbl_merge(list(uv,mult),tab_spanner = c('univariate','multivariate'))

# supple: sensitivity -------------------------------------------------------------


target <- names(m$X)
target <- c('dex_usage', target[1:18],'po2_min', 'lactate_max')
noaf_f <- paste('Surv(obs_duration, afib_within_7days==1)','~',paste(target,collapse='+'))
inhos_f <- paste('Surv(inhos_duration, inhos_mortality==1)','~',paste(target,collapse='+'))
summary(afib_dex[,as.numeric(difftime(dex_starttime,icu_intime, units='hours'))])


coxph(as.formula(noaf_f), afib_dex48) %>% summary()
coxph(as.formula(noaf_f), df_comp) %>% summary()
coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage, df_match) %>% summary()

library(ipw)
df_comp[,afib_within_7days:=as.numeric(ifelse(afib_within_7days==1,1,0))]
ipw_fit <- ipwpoint(exposure = dex_usage, family='binomial',
                    link = 'logit', 
                    numerator = ~ sex + age + ethnicity + cci + sepsis + sofa_score+
                      ventilation, 
                    denominator = ~sex+ age+ bmi+ ethnicity+ cci+ heart_surgery +
                      icuunit + vis+ ventilation+ crrt+ sepsis+ sofa_score+
                      sapsii+ map_min+ heart_rate_max+ resp_rate_max+ temperature_max+
                      spo2_min+ ph_min+ po2_min+ pco2_max+ wbc_max+ hemoglobin_min+ platelets_min+
                      creatinine_max+ bun_max+ lactate_max+ sodium_max+ chloride_max+ potassium_max+
                      calcium_min+ bicarbonate_min+ magnesium_min, data=df_comp)

coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage, 
      weights = ipw_fit$ipw.weights, data=df_comp) %>% summary()

coxph(as.formula(inhos_f), afib_dex48) %>% summary()
coxph(as.formula(inhos_f), df_comp) %>% summary()
coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage, df_match) %>% summary()

coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage, 
      weights = ipw_fit$ipw.weights, data=df_comp) %>% summary()



# hypo, brady -------------------------------------------------------------
dfCount[,duration := as.numeric(difftime(icu_outtime,icu_intime, units='hours'))]
dfCount <- dfCount[stay_id %in% df_match$stay_id]
# Calculating Incidence rate
# df_temp2[, sum(hypo_count,na.rm=T) / (.N * sum(duration)) * 1000, by=dex_usage]

dfCount[,`:=`(
  brady_rate = brady_count / duration* 24,
  hypo_rate = hypo_count / duration * 24
)]

dfCount %>% ggplot(aes(brady_rate)) + geom_histogram()
dfCount %>% ggplot(aes(hypo_rate)) + geom_histogram()



# dfCount[,hypo_count := ifelse(is.na(hypo_count),0,hypo_count)]
dfCount[,hypo_yn := as.factor(ifelse(hypo_count>0,1,0))]
dfCount[,brady_yn := as.factor(ifelse(brady_count>0,1,0))]
dfCount[,obs_duration_brady := ifelse(brady_yn==1, 
                                       as.numeric(difftime(brady_occur_time, icu_intime,units='hours')),
                                       as.numeric(difftime(icu_outtime,icu_intime, units='hours')))]

dfCount[,obs_duration_hypo := ifelse(hypo_yn==1, 
                                      as.numeric(difftime(hypo_occur_time, icu_intime,units='hours')),
                                      as.numeric(difftime(icu_outtime,icu_intime, units='hours')))]

  geom_boxplot()+
  scale_x_discrete(label=c('Non-DEX','DEX'),name='DEX')+
  scale_y_discrete(limits=c(0,10))+
  theme_stata(base_family = 'serif') 


mytable(dex_usage ~ hypo_yn + brady_yn, dfCount)
mytable( ~ hypo_yn + brady_yn, dfCount)

a <- dfCount[hypo_count==0 , .(hypo_count=.N), by=.dex_usage]
b <- dfCount[brady_count==0 , .(brady_count=.N), by=dex_usage]

merge(a,b) %>% kableExtra::kable() %>% kableExtra::kable_classic()
with(dfCount,
     kruskal.test(brady_count*24 ~ dex_usage))
with(dfCount,
     wilcox.test(brady_count ~ dex_usage))


dfCount %>% ggplot(aes(hypo_count)) + geom_histogram()
dfCount %>% ggplot(aes(brady_count)) + geom_histogram()

mytable(dex_usage~brady_count + hypo_count, dfCount)

# IR
dfCount[, .(IR_hypo = sum(hypo_count,na.rm=T) / (.N * sum(duration)) * 1000), by=dex_usage]
dfCount[, .(IR_hypo = sum(brady_count,na.rm=T) / (.N * sum(duration)) * 1000), by=dex_usage]
dfCount[,.(hypo_count, duration)]
dfCount[,.N]
dfCount[, .(IR_hypo = sum(hypo_count,na.rm=T) / (.N * sum(duration)) * 1000)]
countA <- dfCount[, .(IR_hypo = sum(hypo_count,na.rm=T) / (.N * sum(duration)) * 1000), by=dex_usage]
countB <- dfCount[, .(IR_brady = sum(brady_count,na.rm=T) / (.N * sum(duration)) * 1000), by=dex_usage]

countBradyHypo <- merge(countA, countB)
countBradyHypo

# HR

coxph(Surv(obs_duration_brady, brady_yn ==1) ~ dex_usage, dfCount) %>% summary()
coxph(Surv(obs_duration_hypo, hypo_yn ==1) ~ dex_usage, dfCount) %>% summary()

#merge
countBradyHypo$HR_brady <- c(1,1.18)
countBradyHypo$HR_hypo <- c(1,1.09)

countBradyHypo$lower_brady <- c(NA, 1.11)
countBradyHypo$upper_brady <- c(NA, 1.26)

countBradyHypo$lower_hypo <- c(NA, 1.04)
countBradyHypo$upper_hypo <- c(NA, 1.15)



# HRV ---------------------------------------------------------------------
hrv <- fread('csv/Afib_hr_chart.csv')
hrv <- hrv[stay_id %in% df_match$stay_id]

hrv_afib <- hrv[stay_id %in% df_match[afib_within_7days==1]$stay_id]
hrv_afib

# dex 사용하지 않은 군: 입실 후 24시간 평균
# dex 사용군: dex 사용 후 24시간 평균
hrv[,hour:=rowid(stay_id)]
hrv_afib[,hour:=rowid(stay_id)]
hrv
  

hrv2 <- inner_join(cohort2, hrv, by='stay_id')
temp_hr <- hrv2[,.(afib_within_7days, gender , age , BMI , race , heart_surgery,
                      inhos_mortality, hr_mean, brady, hypo,ventil_duration,
                      los_icu, los_hospital, mortality_90, value, hour,
                      icuunit , t2e, obs_duration, vis , sepsis , sofa_score , cci , ventil , crrt ,dex_usage, spo2,
                      sbp , dbp , heart_rate , resp_rate , temperature , hemoglobin , wbc, map,phosphate,
                      creatinine,bun,lactate,platelet , sodium , chloride , potassium , calcium , bicarbonate, magnesium,
                      diltiazem , metoprolol , esmolol , nicardipine , amiodarone , milrinone,digoxin)]

set.seed(7795)
cohort2_imp <- mice(temp_hr)
cohort2_comp <- complete(cohort2_imp, 3)

psm_hr_df <- as.data.table(cohort2_comp)
psm_hr_df[,sex:=as.factor(fifelse(gender=='F',0,1))]
psm_hr_df[,cci:= as.numeric(cci)]
psm_hr_df <- psm_hr_df %>% mutate_at(vars(race,icuunit),as.factor)
set.seed(7795)
m_hr <- matchit(dex_usage ~ 
                  sex+ age + BMI+ race+ cci+ icuunit + 
                  sepsis + sofa_score +
                  ventil + crrt +
                  sbp + dbp + heart_rate + resp_rate +
                  temperature + spo2 +
                  hemoglobin + wbc + platelet +
                  sodium + chloride + potassium + 
                  calcium + bicarbonate + magnesium,
                # method = 'optimal',
                psm_hr_df)

# dex 사용군
hrv_dex <- hrv[dex_use==1]
hrv_dex[,rowid:=.I]
hrv_dex
mark <- hrv_dex[charttime>=dex_starttime, .I[1L], by=stay_id]$V1

hrv_dex[,mark := ifelse(rowid %in% mark,1,0)]
inds <- which(hrv_dex$mark==1)
rows <- lapply(inds, function(x) (x-10):(x+12))

hrv_dex <- hrv_dex[unlist(rows)]
hrv_dex %>% View()
hrv_dex[,hour:=rowid(stay_id)]
hrv_dex[,mark:=NULL]
hrv_dex[,rowid:=NULL]
hrv_dex
# hrv_dex<- hrv_[stay_id !='39993425']

# dex 미사용군

hrv_no_dex <- hrv[dex_use==0, head(.SD,50),by=stay_id]
hrv_no_dex[,hour := rowid(stay_id)]
hrv_no_dex %>% View()

names(hrv_no_dex)
names(hrv_dex)
hrv2 <- bind_rows(hrv_dex, hrv_no_dex)



#delirium ------------------------------------------------------------
delirium <- fread('csv/Afib_delirium.csv')
cols <- c('positive_length','uta_length','all_length')

for(k in cols){
  set(delirium, i=which(is.na(delirium[[k]])), j=k, value=0)
}



# subgroup Analysis -------------------------------------------------------
require(Publish)
require(survival)

sub_df <- as.data.table(df_match)
sub_df[,htn := ifelse(sbp>140 | dbp>=90,1,0)]
cox_fit <- coxph(Surv(obs_duration, afib_within_7days==1) ~ 
                   dex_usage + sex+ age60_+race+cvicu+sepsis+crrt_before_afib +
                   ventil_usage_before_afib,
                 data=df_comp)

summary(cox_fit)
sub_df[,afib_within_7days:=as.numeric(ifelse(afib_within_7days==1,1,0))]
sub_df[,dex_usage:=as.factor(dex_usage)]
subgroups=c('sex','age60', 'ethnicity','cvicu','sepsis','ventil','crrt','htn')
sub_df[,(subgroups):=lapply(.SD, as.factor), .SDcols=subgroups]

sub_df[,.(sex,cvicu,sepsis, dex_usage,ventil, afib_within_7days)]
sub_result <- subgroupAnalysis(cox_fit, data=sub_df,
                 treatment="dex_usage",
                 subgroups=subgroups)
sub_result %>% as.data.table()%>% 
  mutate(`HR (95% CI)` = paste0(format(round(HazardRatio,2),nsmall=2), 
                                ' (', format(round(Lower,2),nsmall=2), '-', 
                                format(round(Upper,2),nsmall=2),
                                ')'),
         control = paste0(event_0,'/',sample_0),
         case = paste0(event_1,'/',sample_1)) %>% 
  DT::datatable(options=list(dom='Bfrtip',buttons='copy'), extensions='Buttons',
                ) %>% 
  DT::formatRound(digit=2, columns = c('HazardRatio','Lower','Upper'))

# mortality
sub_df[,ethnicity:=as.factor(ifelse(race %like% 'African',1,0))]
subgroups=c('sex','age60', 'ethnicity','cvicu','sepsis','ventil','crrt','htn')
sub_df[,(subgroups):=lapply(.SD, as.factor), .SDcols=subgroups]

cox_fit <- coxph(Surv(duration_inhos_mortality, inhos_mortality==1) ~ 
                   dex_usage + sex + age60+
                   afib_within_7days+
                   sofa_score+ cvicu+ ethnicity + 
                   cci + crrt + ventil + htn+
                   heart_rate + resp_rate +
                   temperature + spo2 + lactate+
                   hemoglobin + wbc + platelet + sodium +
                   chloride + potassium + calcium +
                   bicarbonate + magnesium +
                   sepsis, sub_df)
sub_df[,inhos_mortality:=as.numeric(ifelse(inhos_mortality==1,1,0))]
sub_result <- subgroupAnalysis(cox_fit, data=sub_df,
                               treatment="dex_usage",
                               subgroups=subgroups)
sub_result
ggforest(cox_fit)

summary(cox_fit)

# delirium 없던 환자들 ---------------------------------------------------------

delirium_pos
cohort2[,delirium_yn := fifelse(delirium_pos==0,0,1)]
lm(delirium_yn ~  + dex_usage, cohort2) %>% summary()
fit <- glm(delirium_yn ~ dex_usage + age + gender + cci, cohort2, family='binomial')

require(moonBook)
ORplot(fit)

mytable(delirium_yn ~ cci + sofa_score + sepsis + ventil + crrt, cohort2)

mytable(dex_usage ~ cci + sofa_score, cohort2)

mytable(dex_usage ~ delirium_yn,cohort2)


fit <- glm(delirium_yn ~ dex_usage, cohort2, family='binomial')
exp(coef(fit))
require(MatchIt)
psm_hr_df <- as.data.table(cohort2_comp)
psm_hr_df[,sex:=as.factor(fifelse(gender=='F',0,1))]
psm_hr_df[,cci:= as.numeric(cci)]
df_match[,delirium_yn:=fifelse(delirium_pos==0,0,1)]
set.seed(7795)
require(MatchIt)
m_hr <- matchit(dex_usage ~ sofa_score, psm_hr_df)



## interactionR -----------------------------
require(interactionR)
interaction_df <- df_match[,.(inhos_duration,dex_usage, afib_within_7days, inhos_mortality)]
interaction_df[,`:=`(
  dex_usage = as.integer(ifelse(dex_usage==0, 0,1)),
  afib_within_7days= as.integer(ifelse(afib_within_7days==1,1,0))
)]
str(interaction_df)

epi_fit <- glm(inhos_mortality ~ dex_usage * afib_within_7days, interaction_df, family=binomial('logit'))
epi_fit <- coxph(Surv(inhos_duration, inhos_mortality=='Yes') ~ dex_usage*afib_within_7days,
                 data=interaction_df)
epi_fit %>% summary()
obj <- interactionR(epi_fit,
             ci.type='mover',
             recode=T,
             em = F,
             exposure_names = c('afib_within_7days','dex_usage'))

interactionR_table(obj)

glm(inhos_mortality ~ dex_usage * afib_within_7days, df_match2, family=binomial) %>% ORplot()

coxph(Surv(inhos_duration, inhos_mortality=='Yes')~ dex_usage,
      df_match2[afib_within_7days==1])

glm(inhos_mortality ~ dex_usage, family=binomial, df_match2[afib_within_7days==1]) %>% ORplot()
glm(inhos_mortality ~ dex_usage, family=binomial, df_match2[afib_within_7days==1]) %>% ORplot()

glm(inhos_mortality~ group, family=binomial, interaction_df) %>% ORplot()

# Dex 사용량에 따른 afib 발생 ---------------------------------------------------------------------

df_trend <- left_join(df_match2, dexRateSummary[,.(stay_id,dex_amount=sum_total_amount)],by='stay_id')
df_trend
df_trend[,dex_amount:=ifelse(is.na(dex_amount),0,dex_amount)]
df_trend[,dex_quantile := ntile(dex_amount,4)]

dexQuantileDf <- df_trend[dex_usage==1,.(stay_id, afib_within_7days, dex_amount, quantile=ntile(dex_amount,4))]
mytable(quantile~dex_amount, dexQuantileDf)
mytable(quantile~afib_within_7days, dexQuantileDf)

tbl <- xtabs(~quantile + afib_within_7days, data = dexQuantileDf)
margin.table(tbl,margin = 1)
class(tbl)
plot(tbl)
tbl

  
  

# supple: inhos mortality table -------------------------------------------
names(df_match)
df_match$cvi
df_match[,hypertension:= as.factor(ifelse(sbp>=140 | dbp>=90,1,0))]
inhos_mortal_fit <- coxph(Surv(inhos_duration, inhos_mortality==1) ~ 
        sex+age60+ BMI + ethnicity + cvicu + dex_usage + cci + sepsis + sofa_score+
          crrt +
          sbp + dbp + heart_rate + resp_rate +
          temperature + spo2 +
          hemoglobin + wbc + platelet + sodium +
          chloride + potassium + calcium + 
          bicarbonate + magnesium,data=df_match)

rst2 <- step(inhos_mortal_fit, trace=F)
multiRegression(rst2)
gtsummary::tbl_regression(inhos_mortal_fit, exponentiate=T)


# VIS 추가분석 ----------------------------------------------------------------
vis_df <- vis_summary[df_match[,.(stay_id, sepsis, dex_usage)], on='stay_id']
mytable(dex_usage ~ vis_sum + vis_mean + vis_max,
        data = vis_df[sepsis==0])

mytable(dex_usage ~ vis_sum + vis_mean + vis_max,
        data = vis_df[sepsis==1])

tab1 <- tbl_summary(
  data=df_match[!is.na(vis) & 
                     sepsis=='Yes' &
                     max_vis<1.5*quantile(max_vis,probs=0.75,
                                          na.rm=T)],
  by = dex_usage,
  include = max_vis,
  statistic = all_continuous() ~ "{mean} ± {sd}",
  digits = all_continuous() ~ 1,
  label= list(
    vis7 = "Cumulative VIS (7 days)"
  )
) %>% 
  add_p(test= all_continuous() ~ "t.test",
        pvalue_fun = ~style_pvalue(.,digits = 3)) %>% 
  modify_header(label="Variable",
                stat_1 = 'Non-DEX',
                stat_2 = 'DEX')

tab2 <- tbl_summary(
  data=df_match_id[!is.na(vis7) & 
                     sepsis=='No' &
                     max_vis<1.5*quantile(max_vis,probs=0.75,
                                          na.rm=T)],
  by = dex_usage,
  include = max_vis,
  label= list(
    vis7 = "Cumulative VIS (7 days)"
  ),
  statistic = all_continuous() ~ "{mean} ± {sd}",
  digits = all_continuous() ~ 1,
) %>% 
  add_p(test= all_continuous() ~ "t.test",
        pvalue_fun = ~style_pvalue(., digits = 3)) %>% 
  modify_header(label="Variable",
                stat_1 = 'Non-DEX',
                stat_2 = 'DEX')

tbl_merge(list(tab1, tab2),
          tab_spanner = c('Sepsis','Non-Sepsis'))



# DEX Duration ------------------------------------------------------------

# dex 사용량 -----------------------------------------------------------------
dexRate[stay_id %in% df_match$stay_id]
sid <- dexRate[stay_id %in% df_match$stay_id &
          duration_min>=1440,stay_id]
sid2 <- dexRate[stay_id %in% df_match$stay_id &
          duration_min>=720,stay_id]
dexRate[df[stay_id %in% df_match$stay_id, .(stay_id, icu_intime, 
                                            icu_outtime, los_icu)], on=.(stay_id), `:=`(icu_intime=i.icu_intime,
                                                                                        icu_outtime=i.icu_outtime,
                                                                                        los_icu = i.los_icu)]
a <- dexRate[stay_id %in% df_match$stay_id &
          los_icu<=7] %>% 
  ggplot(aes(x=duration_min))+
  labs(x='Infusion Duration (Minute)')+
  ggtitle('DEX Infusion Duration (~7 days)')+
  geom_histogram()
a
b <- dexRate[stay_id %in% df_match$stay_id] %>% 
  ggplot(aes(x=duration_min))+
  labs(x='Infusion Duration (Minute)')+
  ggtitle('DEX Infusion Duration (ICU stay)')+
  geom_histogram()

a/b
dexRateSummary <- dexRate[,.(sum_rate=sum(rate),
                             sum_total_amount = sum(total_amount)),by=stay_id]

dexRateSummary
