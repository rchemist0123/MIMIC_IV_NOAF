
# 1. Afib 발생군 중 Dex 사용에 따른 분류 ---------------------------------------------

newonset_id <- cohort2[newonset==1]$stay_id

drugs_sum %>% colnames()
#icu 입실 후 7일 이내 afib 발생 여부
length(unique(drugs_sum$stay_id))
drugs_sum[,afib_within_7days:= as.factor(fifelse(
  stay_id %in% newonset_id & 
    difftime(afib_onset_time,icu_intime, units='days')<=7 &
    difftime(afib_onset_time,icu_intime, units='days')>=0,1,0
))]
length(unique(drugs_sum[afib_within_7days==1]$stay_id))

#afib 발생 이전 dex 복용 여부
drugs_sum[,dex_before_afib:= as.factor(
  fifelse(
    drugname=='dexmedetomidine' & 
      drug_duration>=24 &
      afib_onset_time>= drug_starttime,1,0
  )
)]
table(drugs_sum$dex_before_afib)
drugs_sum[,dex_before_afib_12:=as.factor(
  fifelse(drugname=='dexmedetomidine' &
          difftime(afib_onset_time,drug_starttime, units='hours')>=12,1,0
        )
)]


drugs_sum[dex_before_afib==1] %>% View()

afib_within_7days_id <- unique(drugs_sum[afib_within_7days==1]$stay_id)
cohort2[,afib_within_7days:=as.factor(fifelse(stay_id %in% afib_within_7days_id,1,0))]

dex_before_afib_id <- unique(drugs_sum[dex_before_afib==1]$stay_id)
dex_before_afib_12_id <- unique(drugs_sum[dex_before_afib_12==1]$stay_id)

cohort2[,dex_before_afib:=as.factor(fifelse(stay_id %in% dex_before_afib_id,1,0))]
cohort2[,dex_before_afib_12:=as.factor(fifelse(stay_id %in% dex_before_afib_12_id,1,0))]

cohort2

rst <- cohort2[,.N, by=c('afib_within_7days','dex_before_afib')][order(-afib_within_7days)][,pct:= round(N/sum(N)*100,2), by=afib_within_7days]
rst <- rst[order(-dex_before_afib)]
rst
rst[,y_label:=cumsum(pct)-0.5 * pct, by=afib_within_7days]
rst %>% ggplot(aes(x=afib_within_7days, y=pct, fill=dex_before_afib))+
  geom_col(width=.5)+
  geom_text(aes(label=paste0(pct,'%'), y=y_label))+
  theme(legend.position = 'bottom')
require(ggplot2)
require(survival)

fit <- glm(dex_before_afib ~ afib_within_7days , data=cohort2,family=binomial)
ORplot(fit)
summary(fit)


require(MASS)
exp(cbind(coef(fit), confint(fit)))
mytable(dex_before_afib~afib_within_7days,cohort2)

mytable(dex_before_afib~afib_within_7days,cohort2)


# 선후관계 파악 -----------------------------------------------------------------

no_dex_yes_afib_id <- cohort2[afib_within_7days==1 & dex_usage==0]$stay_id
drugs2[drugname=='dexmedetomidine',.(stay_id,afib_onset_time, drug_starttime)]
drugs2[drugname=='dexmedetomidine']
drugs2[dex_use==1]

# regression -------------------------------------------------------------
fit <- glm(afib_within_7days ~ dex_usage + gender + age +
             icutype +
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
             sofa_score+
            cci + rrt + ventil + 
             sbp_mean + dbp_mean + heart_rate_mean + resp_rate_mean +
             temperature_mean + spo2_mean + lactate_mean+
             hemoglobin_mean + wbc_mean + platelets_mean + sodium_mean +
             chloride_mean + potassium_mean + calcium_mean + bun_mean +
             bicarbonate_mean + creatinine_mean + magnesium_mean +
             ph_mean + po2_mean + pco2_mean+
            sepsis, cohort2, family=binomial)

summary(fit)
ORplot(fit, type=2)

require(sjPlot)
require(stargazer)
stargazer(fit, type='html', ci=T, sigle.row=T)
tab_model(fit, ci_method='wald')

mytable( ~ newonset + gender + age + weight + height + BMI + race +
          icutype +
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
          cci + sepsis + sofa_score +
          ventil + rrt +  
          sbp_mean + dbp_mean + heart_rate_mean + resp_rate_mean +
          temperature_mean + spo2_mean + 
          hemoglobin_mean + wbc_mean + platelets_mean + 
           bnp_mean + troponin_mean+sodium_mean +
          chloride_mean + potassium_mean + calcium_mean + bun_mean +
          bicarbonate_mean + creatinine_mean + magnesium_mean +
          ph_mean + po2_mean + fio2_mean +pco2_mean+ lactate_mean + crp_mean
          , cohort2)

mytable(dex_usage ~ bnp_mean + troponin_mean, cohort2)
cohort2[,.(med = median(event_duration,na.rm=T))]



# subgroup: ICU type ------------------------------------------------------

fit <- glm(afib_within_7days ~ dex_usage + gender + age +
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
             sofa_score+
             cci + crrt + ventil + 
             sbp_mean + dbp_mean + heart_rate_mean + resp_rate_mean +
             temperature_mean + spo2_mean + lactate_mean+
             hemoglobin_mean + wbc_mean + platelets_mean + sodium_mean +
             chloride_mean + potassium_mean + calcium_mean + bun_mean +
             bicarbonate_mean + creatinine_mean + magnesium_mean +
             ph_mean + po2_mean + pco2_mean+
             sepsis, family=binomial, data=cohort2[icutype=='SICU'])
summary(fit)
require(finalfit)
ORplot(fit)
setnames(cohort2, 'rrt','crrt')
Xs <- c( 'dex_usage',
         'gender' , 'age' ,
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
           'sofa_score',
           'cci' , 'crrt' , 'ventil' , 
           'sbp_mean' , 'dbp_mean' , 'heart_rate_mean' , 'resp_rate_mean' ,
           'temperature_mean' , 'spo2_mean' , 'lactate_mean',
           'hemoglobin_mean' , 'wbc_mean' , 'platelets_mean' , 'sodium_mean' ,
           'chloride_mean' , 'potassium_mean' , 'calcium_mean' , 'bun_mean' ,
           'bicarbonate_mean' , 'creatinine_mean' , 'magnesium_mean' ,
           'ph_mean' , 'po2_mean' , 'pco2_mean',
           'sepsis')

cohort2[icutype=='CCU'] %>% 
  or_plot(dependent = 'afib_within_7days',
           explanatory = Xs,
          table_text_size = 4,
          plot_opts = list(xlab('OR, 95% CI')))
