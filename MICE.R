
# MICE --------------------------------------------------------------------
install.packages('mice')
install.packages('VIM')
install.packages('heatmaply')
require(mice)
require(VIM)
require(missForest)
require(heatmaply)

cohort2$rrt
setnames(cohort2, 'rrt','crrt')
colnames(cohort2)
View(cohort2)
temp
cohort2
temp <- cohort2[,.(afib_within_7days, dex_usage, gender,age, weight, height, BMI, race, 
         chronic_pulmonary_disease,
         cerebrovascular_disease,
         congestive_heart_failure,
         malignant_cancer ,
         valvular_disease ,
         chronic_liver_disease ,
         diabetes ,
         hypertension ,
         renal_disease ,
         obstructive_sleep_apnea,
         icutype ,
         sepsis,
         propofol,crrt,ventil, sofa_score,
         sbp_mean , dbp_mean , heart_rate_mean , resp_rate_mean ,
        temperature_mean , spo2_mean , lactate_mean,
        hemoglobin_mean , wbc_mean , platelets_mean , sodium_mean,
        chloride_mean , potassium_mean , calcium_mean , bun_mean ,
        bicarbonate_mean , creatinine_mean , magnesium_mean)]

cohort_hr
temp2 <- cohort2[,.(afib_within_7days, dex_usage, gender,age, weight, height, BMI, race, 
                    icutype ,
                    sepsis,
                    cci,
                    propofol,crrt,ventil, sofa_score,
                    sbp_mean , dbp_mean , heart_rate_mean , resp_rate_mean ,
                    temperature_mean , spo2_mean , lactate_mean,
                    hemoglobin_mean , wbc_mean , platelets_mean , sodium_mean,
                    chloride_mean , potassium_mean , calcium_mean , bun_mean ,
                    bicarbonate_mean , creatinine_mean , magnesium_mean)]


setnames(cohort_hr,'rrt','crrt')
cohort_hr[,vis:=ifelse(is.na(vis),0,vis)]
temp_hr <- cohort_hr[,.(t2e, afib_within_7days, dex_usage, gender,age, weight, height, BMI, race, 
                        icutype ,sepsis,cci,propofol, midazolam, dexmedetomidine,
                        crrt,ventil, sofa_score, as.numeric(event_duration), vis,
                        sbp_mean , dbp_mean , heart_rate_mean , resp_rate_mean ,
                        temperature_mean , spo2_mean , lactate_mean,
                        hemoglobin_mean , wbc_mean , platelets_mean , sodium_mean,
                        chloride_mean , potassium_mean , calcium_mean , bun_mean ,
                        bicarbonate_mean , creatinine_mean , magnesium_mean)]


aggr(temp, cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

require(dplyr)
require(tidyverse)
require(ggplot2)

set.seed(7795)
cohort_hr_imp <- mice(temp_hr)
cohort_hr_comp <- complete(cohort_hr_imp, 3)


fwrite(cohort2_comp, "csv/Afib_newonset_ML_imp.csv")
fwrite(temp, 'csv/Afib_newonset_ML_raw.csv')
fit <- glm(newonset~., family='binomial', data=cohort2_comp)

require(sjPlot)
