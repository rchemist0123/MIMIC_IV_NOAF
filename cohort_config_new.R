

target2 <- grep('_min',names(afib_dex),value = T)
target3 <- grep('_max',names(afib_dex),value = T)


b <- afib_dex[,lapply(.SD, \(x) sum(is.na(x))/nrow(afib_dex)*100),.SDcol=target2] %>% t()
c <- afib_dex[,lapply(.SD, \(x) sum(is.na(x))/nrow(afib_dex)*100),.SDcol=target3] %>% t()
rst <- cbind(b,c)
rst <- as.data.table(rst, keep.rownames = T)
rst
colnames(rst) <- c('variable', 'min','max')
rst[,c('min','max'):=lapply(.SD, \(x) round(x,2)), .SDcol=2:3]
rst %>% gt()
rsetdiff(rownames(b),rownames(c))
target2
target3
target
names(afib_dex)


# exclusion criteria ------------------------------------------------------

# 1. ICU LOS 2 to 28
cohortConfig <- function(dex_hour, afib_day, dex_dur){
  df <- fread('csv/af_dex_df2.csv')
  id <- df[los_icu <2, stay_id]; print(length(id))
  df <- df[!stay_id %in% id]
  id <- df[los_icu >28, stay_id];print(length(id))
  df <- df[!stay_id %in% id]
  
  # NOAF before ICU Admission
  id <- df[difftime(afib_onset_time, icu_intime,units = 'mins')<0, stay_id]; print(length(id))
  df <- df[!stay_id %in% id]
  
  # Start DEX After NOAF
  id <- df[dex_starttime>=afib_onset_time, stay_id]; print(length(id))
  df <- df[!stay_id %in% id]
  
  # Start DEX Too late (7 days)
  id <- df[difftime(afib_onset_time, dex_starttime, units = 'days')>7, stay_id]; print(length(id))
  df <- df[!stay_id %in% id]
  
  # DEX before ICU admission
  id <- df[dex_starttime<icu_intime, stay_id]; print(length(id))
  df <- df[!stay_id %in% id]
  
  # DEX after 48hours after ICU admission
  id <- df[difftime(dex_starttime,icu_intime, units = 'hours') >= as.numeric(dex_hour), stay_id];print(length(id))
  # id <- df[difftime(dex_starttime,icu_intime, units = 'hours')>=72, stay_id];length(id)
  # id <- df[difftime(dex_starttime,icu_intime, units = 'days')>=7, stay_id]; length(id)
  df <- df[!stay_id %in% id]
  
  # DEX after AF
  id <- df[dex_starttime>afib_onset_time,stay_id];print(length(id))
  df <- df[!stay_id %in% id]
  
  
  # dex duration criteria
  dexRate[,duration_h := duration_min/60][]
  df[dexRate, on=.(stay_id), dex_duration :=i.duration_h]
  # 
  id <- df[dex_duration < dex_dur, stay_id]
  print(sprintf('dex dur: %s', length(id)))
  df <- df[!stay_id %in% id]
  
  df[,`:=`(
    rate_control = as.factor(ifelse(diltiazem ==1 | 
                                         esmolol == 1 |
                                         metoprolol == 1 |
                                         digoxin == 1,1,0)),
    rhythm_control =as.factor(ifelse(amiodarone ==1 | 
                                       # amiodarone2==1|
                                        magnesium==1 | 
                                       magnesium2==1|
                                        sotalol==1 |
                                        dronedarone==1 |
                                        propafenone==1 |
                                        flecainide == 1,1,0)),
    rhythm_control2 =as.factor(ifelse(amiodarone ==1 | 
                                        # amiodarone2==1|
                                         # magnesium==1 |
                                         sotalol==1 |
                                         dronedarone==1 |
                                         propafenone==1 |
                                         flecainide == 1,1,0)
  ))]
  
  df[,afib_within_7days := as.factor(ifelse(is.na(afib_onset_time),0,
                                                  ifelse(difftime(afib_onset_time,icu_intime,units='days')<=afib_day,1,0)))]
  
  df[,bmi := weight/(height*0.01)^2]
  df[,obs_duration := round(as.numeric(ifelse(is.na(afib_onset_time),
                                   difftime(icu_outtime,icu_intime,units='days'),
                                   difftime(afib_onset_time, icu_intime, units='days'))),1)]
  df[,inhos_mortality:= as.factor(ifelse(is.na(dod),0,
                                         ifelse(dod <= dischtime, 1,0)))]
  df[,inhos_duration := round(as.numeric(ifelse(inhos_mortality==0,
                                                difftime(dischtime, icu_intime, units='days'),
                                                difftime(dod, icu_intime, units='days'))),1)]
  
  
  id <- df[inhos_duration<0, unique(stay_id)]
  print(length(id))
  df <- df[!stay_id %in% id]
  # df[obs_duration > inhos_duration,.(obs_duration, inhos_duration, inhos_mortality)]
  df[,obs_duration:=ifelse(inhos_mortality==1 & inhos_duration < obs_duration, 
                   inhos_duration, obs_duration)]
  df[,ethnicity := ifelse(ethnicity == 'ASIAN','Asian',
                                    ifelse(ethnicity %like% 'BLACK','African-American',
                                           ifelse(ethnicity =='WHITE','Caucasian',
                                                  ifelse(ethnicity %like% 'HISPANIC','Hispanic','Other'))))]
  
  df[,icuunit:=
       fifelse(first_careunit %like% 'SICU', 'SICU',
                         fifelse(first_careunit %like% 'CVICU','CVICU',
                                 fifelse(first_careunit %like% 'CCU', 'CCU',
                                         fifelse(first_careunit %like% 'MICU', 'MICU','ETC'))))]
  setnames(df, c('dexmedetomidine','gender'),
           c('dex_usage','sex'))
  df[,t2e := as.numeric(ifelse(is.na(afib_onset_time),0, ifelse(afib_within_7days==1,difftime(afib_onset_time, icu_intime, units='days'),0)))]
  
  target <- c('ph_min','ph_max','po2_min','po2_max','lactate_min','lactate_max',
              'pco2_min','pco2_max')
  
  df[,(target):=lapply(.SD, \(x) ifelse(x>1000,NA,x)), .SDcol=target]
  
  return(df)
}
afib_dex48 <- cohortConfig(dex_hour = 48, afib_day = 7, dex_dur = 48)
afib_dex96 <- cohortConfig(dex_hour = 48, afib_day = 7, dex_dur = 96)

afib_dex48[,.N, by=dex_usage]
afib_dex96[,.N, by=dex_usage]
# temp <- df[stay_id %in% keep_id]

afib_dex96
# matching ----------------------------------------------------------------

temp <- afib_dex96 %>% select(
  sex, age, bmi, ethnicity, cci, heart_surgery,
  icuunit,
  vis, ventilation, crrt, sepsis, sofa_score,
  sapsii, dex_usage,
  map_min, heart_rate_max, resp_rate_max, temperature_max,
  spo2_min,
  ph_min, po2_min, pco2_max, wbc_max, hemoglobin_min, platelets_min,
  creatinine_max, bun_max, lactate_max, sodium_max, chloride_max, potassium_max,
  calcium_min, bicarbonate_min, magnesium_min,
  obs_duration, afib_within_7days, 
  inhos_duration, inhos_mortality, t2e,
  rate_control, rhythm_control,
  los_icu, los_hospital, cardioversion, brady, hypo
  )

temp[,lapply(.SD, \(x) sum(is.na(x))/.N*100)]
# target <- temp[,.SD, .SDcol=is.numeric] %>% names()
# df_mice2 <- temp[,(target):=lapply(.SD, \(x) ifelse(is.na(x), median(x,na.rm=T),x)),.SDcol=target]
require(mice)
temp[,vis:=ifelse(vis>200,NA,vis)]
df_mice48 <- mice(temp, seed=2022)
df_mice96 <- mice(temp, seed=2022)
df_comp <- setDT(complete(df_mice48,1))
df_comp <- setDT(complete(df_mice96,1))
fwrite(df_comp,'csv/df_comp_final.csv')
# df_comp$stay_id <- afib_dex48$stay_id
df_comp
require(MatchIt)
set.seed(2022)
mm <- matchit(dex_usage ~ sex+ age+ bmi+ ethnicity+ cci+ heart_surgery +
               icuunit + vis+ ventilation+ crrt+ sepsis+ sofa_score+
             sapsii+ map_min+ heart_rate_max+ resp_rate_max+ temperature_max+
             spo2_min+ ph_min+ po2_min+ pco2_max+ wbc_max+ hemoglobin_min+ platelets_min+
             creatinine_max+ bun_max+ lactate_max+ sodium_max+ chloride_max+ potassium_max+
             calcium_min+ bicarbonate_min+ magnesium_min, 
             df_comp,
             ratio = 3, caliper = 0.1)

df_match2 <- setDT(match.data(mm))
df_match2

df_match2[stay_id %in% df_match$stay_id]
df_match2[,`:=`(
  distance=NULL,
  weights=NULL,
  subclass=NULL
)]

df_match2[,.N,by=dex_usage]
df_match2 %>% dim()
plot(summary(m))
require(tableone)
CreateTableOne(data=df_match2,
               vars = names(df_match),
               addOverall = T,
               factorVars = c('ventilation'),
               strata = 'dex_usage')


ipwplot(ipw_fit$ipw.weights, logscale = F)




# sensitivity analysis-------------------------------------------------------------

target <- afib_dex48[,.N, by=anchor_year_group]$anchor_year_group
target

## anchor_year
dt <- afib_dex48[anchor_year_group %in% c(target[1],target[3])]

## age 65
dt <- afib_dex48[age>=65]
temp <- dt %>% select(
  sex, age, bmi, ethnicity, cci, heart_surgery,
  icuunit,
  vis, ventilation, crrt, sepsis, sofa_score,
  sapsii, dex_usage,
  map_min, heart_rate_max, resp_rate_max, temperature_max,
  spo2_min,
  ph_min, po2_min, pco2_max, wbc_max, hemoglobin_min, platelets_min,
  creatinine_max, bun_max, lactate_max, sodium_max, chloride_max, potassium_max,
  calcium_min, bicarbonate_min, magnesium_min,
  obs_duration, afib_within_7days, 
  inhos_duration, inhos_mortality, t2e,
  rate_control, rhythm_control,
  los_icu, los_hospital, cardioversion, brady, hypo
)

temp[,lapply(.SD, \(x) sum(is.na(x))/.N*100)]
# target <- temp[,.SD, .SDcol=is.numeric] %>% names()
# df_mice2 <- temp[,(target):=lapply(.SD, \(x) ifelse(is.na(x), median(x,na.rm=T),x)),.SDcol=target]
require(mice)
temp[,vis:=ifelse(vis>200,NA,vis)]
temp_imp <- mice(temp, seed=2022)
temp_comp <- setDT(complete(temp_imp))
# df_comp$stay_id <- afib_dex48$stay_id

require(MatchIt)
set.seed(2022)
m <- matchit(dex_usage ~ sex+ age+ bmi+ ethnicity+ cci+ heart_surgery +
               icuunit + vis+ ventilation+ crrt+ sepsis+ sofa_score+
               sapsii+ map_min+ heart_rate_max+ resp_rate_max+ temperature_max+
               spo2_min+ ph_min+ po2_min+ pco2_max+ wbc_max+ hemoglobin_min+ platelets_min+
               creatinine_max+ bun_max+ lactate_max+ sodium_max+ chloride_max+ potassium_max+
               calcium_min+ bicarbonate_min+ magnesium_min, 
             temp_comp,
             ratio = 1, caliper = 0.2)
temp_match <- setDT(match.data(m))
temp_match[,`:=`(
  distance=NULL,
  weights=NULL,
  subclass=NULL
)]
temp_match[,.N,by=dex_usage]
df_match2 %>% dim()
plot(summary(m))
require(tableone)
temp_match
# 추가 분석 -------------------------------------------------------------------

rbind(coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage, temp_match) %>% 
        extractHR(),
      coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage, temp_match) %>% 
        extractHR())
df_match2 %>% dim()
df_match2[,.N, dex_usage]

afib_dex48 %>% select(stay_id,sex,age,dex_usage, icuunit, heart_surgery, sepsis, ventilation) %>% 
  fwrite('4주차_과제데이터1.csv')
afib_dex48[,.(stay_id, afib_onset_time, icu_intime = as.Date(icu_intime), icu_outtime = as.Date(icu_outtime), death_date = dod, hosp_outtime = dischtime)] %>% 
  fwrite('4주차_과제데이터2.csv')

getwd()
