require(survival)
require(moonBook)
require(survminer)

cohort_hr$midazolam
cohort_hr <- drugs2[drugname=='dexmedetomidine',.(stay_id,drug_starttime)][cohort2,on='stay_id']
cohort_hr[,t2e:= ifelse(afib_within_7days==1,
                     difftime(afib_onset_time, intime, units="hours"),
                     difftime(outtime, intime, units="hours"))]

summary(cohort_hr$t2e, na.rm=T)

fit <- survfit(Surv(t2e, afib_within_7days==1) ~ dex_usage, cohort_hr)
ggsurvplot(fit, data=cohort_hr, fun='event', pval=T, risk.table = T,
           conf.int = T,
           xlim=c(0,170),
           break.time.by=30,
           legend.labs=c('Dex X', 'Dex O'))


HRplot(fit)
HRplot(fit, type=3)


# 변수 factor로 만들어주기
target <- c('chronic_pulmonary_disease',
            'cerebrovascular_disease' ,
            'congestive_heart_failure' ,
            'malignant_cancer' ,
            'valvular_disease' ,
            'chronic_liver_disease' ,
            'diabetes' ,
            'hypertension' , 
            'renal_disease' ,
            'obstructive_sleep_apnea',
            'crrt' , 'ventil',
            'sepsis')
cohort_hr[, (target):= lapply(.SD, as.factor), .SDcols=target]
cohort_hr[,ccu:=as.factor(fifelse(icutype=='CCU',1,0))]
cohort_hr$icutype <- as.factor(cohort_hr$icutype)
class(cohort_hr$icutype)
cohort_hr$icutype <- relevel(cohort_hr$icutype, ref='Trauma ICU')

require(survival)
cox_fit <- coxph(Surv(t2e, afib_within_7days==1) ~ 
                   dex_usage+ gender + age+
                   sofa_score+
                   cci + crrt + ventil + 
                   sbp_mean + dbp_mean + heart_rate_mean + resp_rate_mean +
                   temperature_mean + spo2_mean + lactate_mean+
                   hemoglobin_mean + wbc_mean + platelets_mean + sodium_mean +
                   chloride_mean + potassium_mean + calcium_mean + bun_mean +
                   bicarbonate_mean + creatinine_mean + magnesium_mean +
                   sepsis, cohort_hr[ccu==1])


ggforest(cox_fit, data=cohort_hr[ccu==1])
ggcoxfunctional(formula = cox_fit, data=cohort_hr[ccu==1])
HRplot(cox_fit, type=2)



# HR by icutype -----------------------------------------------------------
cox_fit <- coxph(Surv(t2e, afib_within_7days==1) ~ dex_usage+ gender + age+
                   # chronic_pulmonary_disease+
                   # cerebrovascular_disease +
                   # congestive_heart_failure +
                   # malignant_cancer +
                   # valvular_disease +
                   # chronic_liver_disease +
                   # diabetes +
                   # hypertension + 
                   # renal_disease +
                   # obstructive_sleep_apnea+
                   sofa_score+
                   cci + crrt + ventil + sepsis +
                   sbp_mean + dbp_mean + heart_rate_mean + resp_rate_mean +
                   temperature_mean + spo2_mean + lactate_mean+
                   hemoglobin_mean + wbc_mean + platelets_mean + sodium_mean +
                   chloride_mean + potassium_mean + calcium_mean + bun_mean +
                   bicarbonate_mean + creatinine_mean + magnesium_mean, cohort_hr[ccu==1])
summary(cox_fit)
survminer::ggforest(cox_fit, data=cohort_hr[ccu==1])
aggr(cohort_hr[icutype=='CCU'], cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

colnames(cohort_hr[icutype=='CCU'])
# Log Rank Test -----------------------------------------------------------

survdiff(TS ~ dexmedetomidine, survival_cohort2)





matchit(~, distance='logit')

# HR with PSM -------------------------------------------------------------

fit <- survfit(Surv(t2e, afib_within_7days==1) ~ dex_usage, match_data)
ggsurvplot(fit, pval=T, fun = 'event', 
           risk.table = T,
           conf.int = T,
           conf.int.style="step",
           data = match_data,
           xlim=c(0,170),
           break.time.by=30,
           legend.labs=c('Dex X', 'Dex O'))

cox_fit <- coxph(Surv(t2e, afib_within_7days==1) ~ dex_usage + sex+ age +
                   cci+sofa_score + crrt + ventil + sepsis + 
                   ccu
                   sbp_mean + dbp_mean + heart_rate_mean + resp_rate_mean +
                   temperature_mean + spo2_mean + lactate_mean+
                   hemoglobin_mean + wbc_mean + platelets_mean + sodium_mean +
                   chloride_mean + potassium_mean + calcium_mean + bun_mean, match_data)

survminer::ggforest(cox_fit)


# HR PSM by CCU -----------------------------------------------------------

cox_fit <- coxph(Surv(t2e, afib_within_7days==1) ~ dex_usage + ccu, data=match_data)
ggforest(cox_fit, fontsize = 0.9)

cox_fit <- coxph(Surv(t2e, afib_within_7days==1) ~ dex_usage + sex+ age +
                   cci+sofa_score + crrt + ventil + sepsis + 
                   sbp_mean + dbp_mean + heart_rate_mean + resp_rate_mean +
                   temperature_mean + spo2_mean + lactate_mean+
                   hemoglobin_mean + wbc_mean + platelets_mean + sodium_mean +
                   chloride_mean + potassium_mean + calcium_mean + bun_mean, match_data[ccu==0])

survminer::ggforest(cox_fit, fontsize = 0.8, main = "Cox (Not CCU)", data=match_data[ccu==0])

