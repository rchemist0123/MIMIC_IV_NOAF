require(MatchIt)

colnames(psm_df)
table(psm_df$race)
class(cohort2_comp$icutype)

psm_df <- as.data.table(cohort2_comp)
psm_df[, group := fifelse(dex_usage==1,T,F)]
psm_df[,sex:=fifelse(gender=='F',0,1)]
psm_df[,race:=fifelse(race=="Black",1,0)]
psm_df[,icutype:=factor(icutype ,levels=c('MICU','Neuro ICU','SICU','Trauma ICU','CCU'))]

psm_df2 <- as.data.table(cohort2_comp2)
psm_df2[,sex:=as.factor(fifelse(gender=='F',0,1))]
psm_df2[,race:= as.factor(fifelse(race=="Black",1,0))]
psm_df2[,cci:= as.numeric(cci)]
psm_df2[,icutype:=as.numeric(factor(icutype ,levels=c('MICU','Neuro ICU','SICU','Trauma ICU','CCU')))]

psm_hr_df <- as.data.table(cohort_hr_comp)
psm_hr_df[,sex:=as.factor(fifelse(gender=='F',0,1))]
psm_hr_df[,race:= as.factor(fifelse(race=="Black",1,0))]
psm_hr_df[,cci:= as.numeric(cci)]
psm_hr_df[,ccu:=as.factor(fifelse(icutype=='CCU',1,0))]
psm_hr_df[,icutype:=as.numeric(factor(icutype ,levels=c('MICU','Neuro ICU','SICU','Trauma ICU','CCU')))]
setnames(psm_hr_df,'V19','event_duration')
require(MatchIt)

set.seed(7795)
m_hr <- matchit(dex_usage ~
                  sex + age + BMI+ race+
                  ccu +
                  cci+
                  sepsis + sofa_score +
                  ventil + crrt + 
                  sbp_mean + dbp_mean + heart_rate_mean + resp_rate_mean +
                  temperature_mean + spo2_mean + 
                  hemoglobin_mean + wbc_mean + platelets_mean + 
                  sodium_mean +
                  chloride_mean + potassium_mean + calcium_mean + bun_mean +
                  bicarbonate_mean + creatinine_mean + magnesium_mean, psm_hr_df)

install.packages('cobalt')
require(cobalt)
bal.tab(m_hr, m.threshold=0.2) %>% love.plot(stat="mean.diffs")
install.packages('tableone')
require(tableone)

matching_vars <- c('sex' , 'age' , 'BMI', 'race',
                     'ccu' ,
                     'cci',
                     'sepsis' , 'sofa_score' ,
                     'ventil' , 'crrt' , 
                     'sbp_mean' , 'dbp_mean' , 'heart_rate_mean' , 'resp_rate_mean' ,
                     'temperature_mean' , 'spo2_mean' , 
                     'hemoglobin_mean' , 'wbc_mean' , 'platelets_mean' , 
                     'sodium_mean' ,
                     'chloride_mean' , 'potassium_mean' , 'calcium_mean' , 'bun_mean' ,
                     'bicarbonate_mean' , 'creatinine_mean' , 'magnesium_mean')

tab_before <- CreateTableOne(vars=matching_vars,data=psm_hr_df, strata="dex_usage", smd=TRUE)
tab_after <- CreateTableOne(vars=matching_vars,data=match_data, strata="dex_usage", smd=TRUE)
tab_after <- print(tab_after, smd = T)
tab_after
match_data <- match.data(m_hr)
mytable(dex_usage ~ 
          afib_within_7days + sex + age + BMI+ race+
          ccu + cci+
          propofol+ midazolam + dexmedetomidine + 
          event_duration+ vis +
          sepsis + sofa_score + 
          ventil + crrt +
          sbp_mean + dbp_mean + heart_rate_mean + resp_rate_mean +
          temperature_mean + spo2_mean + 
          hemoglobin_mean + wbc_mean + platelets_mean + 
          sodium_mean +
          chloride_mean + potassium_mean + calcium_mean + bun_mean +
          bicarbonate_mean + creatinine_mean + magnesium_mean, match_data) %>% myhtml()
psm_df[,icutype:=as.numeric(factor(icutype ,levels=c('MICU','Neuro ICU','SICU','Trauma ICU','CCU')))]

match_data[,icutype:=as.factor(ifelse(
  icutype==1,'MICU', ifelse(
    icutype==2,'SICU',ifelse(
      icutype==3,'Trauma ICU',ifelse(
        icutype==4,'Neuro ICU','CCU'
      )
    )
  )
))]
fit1 <- glm(afib_within_7days ~ 
               dex_usage+
              sex + age + BMI+ race+
              icutype +
              # cci+
              chronic_pulmonary_disease+
              cerebrovascular_disease +
              congestive_heart_failure +
              malignant_cancer +
              valvular_disease +
              chronic_liver_disease +
              diabetes +
              hypertension +
              renal_disease +
              obstructive_sleep_apnea+
              propofol+
              sepsis + sofa_score +
              ventil + crrt +
              sbp_mean + dbp_mean + heart_rate_mean + resp_rate_mean +
              temperature_mean + spo2_mean + 
              hemoglobin_mean + wbc_mean + platelets_mean + 
              sodium_mean +
              chloride_mean + potassium_mean + calcium_mean + bun_mean +
              bicarbonate_mean + creatinine_mean + magnesium_mean, 
              family=binomial, data=match_data)



summary(fit1)

target <- c(
  'chronic_pulmonary_disease',
  'cerebrovascular_disease' ,
  'congestive_heart_failure' ,
  'malignant_cancer' ,
  'valvular_disease' ,
  'chronic_liver_disease' ,
  'diabetes' ,
  'hypertension' , 
  'renal_disease' ,
  'obstructive_sleep_apnea',
  'crrt' , 'ventil' , 'sepsis'
)

match_data[,(target):=lapply(.SD, as.factor),.SDcols=target]
match_data[,sex:=as.factor(sex)]
xvars <- c('sex' , 'age' , 'dex_usage',
            'cci',
           # 'chronic_pulmonary_disease',
           # 'cerebrovascular_disease' ,
           # 'congestive_heart_failure' ,
           # 'malignant_cancer' ,
           # 'valvular_disease' ,
           # 'chronic_liver_disease' ,
           # 'diabetes' ,
           # 'hypertension' , 
           # 'renal_disease' ,
           # 'obstructive_sleep_apnea',
           'sofa_score',
           'crrt' , 'ventil' , 'sepsis',
           'sbp_mean' , 'dbp_mean' , 'heart_rate_mean' , 'resp_rate_mean' ,
           'temperature_mean' , 'spo2_mean' , 'lactate_mean',
           'hemoglobin_mean' , 'wbc_mean' , 'platelets_mean' , 'sodium_mean' ,
           'chloride_mean' , 'potassium_mean' , 'calcium_mean' , 'bun_mean' ,
           'bicarbonate_mean' ,'creatinine_mean','magnesium_mean')

ORplot(fit1)
require(finalfit)
match_data %>% or_plot(explanatory = xvars, dependent = 'afib_within_7days',
                       table_text_size=4, title_text_size=13)


devtools::install_github("NightingaleHealth/ggforestplot")


forestplot(
  df=match_data,
  estimate = beta,
  logodds = T
)

require(survival)
