require(data.table)

df
# import data -------------------------------------------------------------

df0 <- fread('csv/Afib_cohort.csv')
df0[exclusion==1]
df0 <- df0[exclusion==0]
comorbidity <- fread('csv/Afib_cci.csv')
ventilation <- fread('csv/Afib_ventilation.csv')
ventilationDuration <- fread('csv/Afib_ventilation_duration.csv')
drugs <- fread('csv/Afib_drugs.csv')
sofa <- fread('csv/Afib_sofa.csv')
sepsis <- fread('csv/Afib_sepsis.csv')
crrt <- fread('csv/Afib_crrt.csv')
lab <- fread('csv/Afib_lab.csv')
vitalsign <- fread('csv/Afib_vitalsign.csv')
map <- fread('csv/Afib_map.csv')
vis <- fread('csv/Afib_VIS.csv')
cardioVersion <- fread('csv/Afib_cardioversion.csv')
hypotension <- fread('csv/Afib_hypotension.csv')
brady <- fread('csv/Afib_Bradycardia.csv')
heartSurgery <- fread('csv/Afib_heart_surgery.csv')
drugRate <- fread('csv/Afib_drug_rate.csv')
dexRate <- fread('csv/Afib_dex_rate.csv')
mimic_apache <- fread('csv/mimic-apacheii.csv')
mimic_sapsii <- fread('csv/MIMIC_SAPSII.csv')
mimic_apsiii <- fread('csv/MIMIC_APSIII.csv')
mimic_oasis <- fread('csv/MIMIC_OASIS.csv')
# drugs -------------------------------------------------------------------
drugs[stay_id %in% df_match$stay_id & drugname=='dexmedetomidine']
# drugs_sum <- drugs[,.(icu_intime =min(icu_intime),
#                        drug_starttime = first(drug_starttime),
#                        drug_endtime = last(drug_endtime),
#                       afib_onset_time = first(afib_onset_time)),
#                        # newonset = min(afib_newonset)),
#                     # drug_rate = round(sum(rate, na.rm=T),2),
#                     # uom = first(rateuom),
#                     # afib_onset_time = min(afib_onset_time),
#                     # newonset = min(newonset),
#                     # drug_duration=sum(drug_duration)), 
#                     by=c('stay_id','drugname')]
# 
# drugs_use_yn_df <- drugs_sum[,.(
#   stay_id,
#   norepinephrine = fifelse(drugname == 'norepinephrine' ,1,0),
#   epinephrine = fifelse(drugname == 'epinephrine',1,0),
#   dobutamine = fifelse(drugname == 'dobutamine',1,0),
#   dopamine = fifelse(drugname == 'dopamine',1,0),
#   milrinone = fifelse(drugname == 'milrinone',1,0),
#   vasopressin = fifelse(drugname == 'vasopressin',1,0),
#   midazolam = fifelse(drugname == 'midazolam' ,1,0),
#   propofol = fifelse(drugname == 'propofol',1,0),
#   dexmedetomidine = fifelse(drugname == 'dexmedetomidine',1,0),
#   morphine = fifelse(drugname == 'morphine',1,0),
#   fentanyl = fifelse(drugname == 'fentanyl',1,0),
#   metoprolol = fifelse(drugname == 'metoprolol' ,1,0),
#   esmolol = fifelse(drugname == 'esmolol',1,0),
#   diltiazem = fifelse(drugname == 'diltiazem' ,1,0),
#   nicardipine = fifelse(drugname == 'nicardipine',1,0),
#   verapamil = fifelse(drugname == 'verapamil',1,0),
#   amiodarone = fifelse(drugname == 'amiodarone',1,0),
#   digoxin = fifelse(drugname =='digoxin',1,0)
# )]
# 
# drugs_use_yn_df <- drugs_use_yn_df[,lapply(.SD, sum), by=stay_id]




# dex id ------------------------------------------------------------------

# dex 사용여부:
# 1. afib: icu 입실 이후 afib 발생 전까지 dex 사용
# 2. afib no: icu 입실 이후 dex 사용


drugs[drugname=='dexmedetomidine'][,.N, by=.(dex_usage,afib_newonset)]
drugs2[drugname=='dexmedetomidine'][,.N, by=.(dex_use,newonset)]

# dex before icu : n=3
dex_before_icu_id <- drugs[drugname=='dexmedetomidine' & 
                           (drug_starttime<icu_intime),stay_id];length(dex_before_icu_id)

# dex_after_afib_id : n=523
dex_after_afib_id <- drugs[drugname=='dexmedetomidine' & 
                             afib_newonset==1 &
                              drug_starttime>=afib_onset_time,stay_id];length(dex_after_afib_id)

# dex after 7days since icu n=261
dex_after_7days_id <- drugs[drugname=='dexmedetomidine' & 
        as.numeric(difftime(drug_starttime, icu_intime, units = 'days'))>=7, stay_id]; length(dex_after_7days_id)



drugs <- drugs[!stay_id %in% dex_before_icu_id & 
                 !stay_id %in% dex_after_afib_id &
                 !stay_id %in% dex_after_7days_id]
drugs[,dex_usage:= as.factor(ifelse(drugname == 'dexmedetomidine' & 
                                      # afib 발생한 경우
                                      stay_id %in% afib_newonset==1 &
                                      drug_starttime >= icu_intime &
                                      drug_starttime < afib_onset_time, 'Yes',
                                    ifelse(drugname=='dexmedetomidine' &
                                      drug_starttime>=icu_intime, 'Yes','No')))]

drugs[drugname=='dexmedetomidine',.(afib_onset_time, drug_starttime)]
dex_id <- drugs[dex_usage=='Yes',unique(stay_id)]
dex_id

# 코호트  조인 -------------------------------------------------------------

df0 <- df0[stay_id %in% drugs$stay_id]
# df <- inner_join(df0, drugs_use_yn_df, by='stay_id')
df <- left_join(df0, comorbidity, by='hadm_id')
df <- left_join(df, lab, by='stay_id')
df <- left_join(df, vitalsign, by= 'stay_id')
df <- left_join(df, map, by='stay_id')
df <- left_join(df, sofa, by='stay_id')
df <- left_join(df, sepsis, by='stay_id')

# VIS: 입실 후 7일  -----------------------------------------------------------

vis_by_days <- fread('csv/Afib_vis_by_days.csv')
vis_by_days <- dcast(vis_by_days, stay_id+date ~ drugname, value.var = 'rate',fun.aggregate = \(x)x[1], fill=0)
vis_by_days[,.N, by=stay_id][order(-N)]
vis_by_days[,vis:= (dopamine +
                  dobutamine + #mcg/kg/min
                  100 * epinephrine + #mcg/kg/min
                  10 * milrinone + #mcg/kg/min
                  10000 * vasopressin +  #units/kg/min
                  100 * norepinephrine)]

vis_by_days[stay_id %in% df_match$stay_id &
              vis> 1.5*quantile(vis, 0.75)]
vis_by_days[,vis2:=fifelse(vis>100,100 ,vis)]
vis_days_summary <- vis_by_days[,.(vis_mean = mean(vis)/.N,
                                   vis_sum = sum(vis)/.N,
                                   vis_max = max(vis)/.N,
                                   nod = .N),by=stay_id]
vis_days_summary2 <- vis_by_days[,.(vis_mean = mean(vis2)/.N,
                                   vis_sum = sum(vis2)/.N,
                                   vis_max = max(vis2)/.N,
                                   nod = .N),by=stay_id]

df_match_vis <- left_join(df_match_id[,.(stay_id,sepsis,dex_usage)],
                          vis_days_summary, 
                          fill=0,
                          by='stay_id')
mytable(dex_usage ~ vis_sum + vis_mean + vis_max, 
        df_match_vis[sepsis=='Yes'])

summary(df_match_vis$vis_max)
df_match_vis2 <- left_join(df_match_id[,.(stay_id,sepsis,dex_usage)],vis_days_summary2, by='stay_id')
mytable(dex_usage ~ vis_sum + vis_mean + vis_max, 
        df_match_vis2[sepsis=='No'])


# VIS RAW -----------------------------------------------------------------
vis_raw <- fread('csv/Afib_vis_raw.csv')
vis_raw[,c('date','time'):= tstrsplit(starttime," ")]
vis_raw[,norepinephrine := ifelse(norepinephrine>=1,NA,norepinephrine)]
vis_raw[,dopamine := ifelse(dopamine >=200,NA,dopamine)]
vis_raw[,vis_h := (dopamine +
                     dobutamine + #mcg/kg/min
                     100 * epinephrine + #mcg/kg/min
                     10 * milrinone + #mcg/kg/min
                     10000 * vasopressin +  #units/kg/min
                     100 * norepinephrine)]
vis_raw
# 환자의 일별 시간별 vis 계산

# 일자별 VIS 계산
vis_by_date <- vis_raw[,vis:=max(vis_h),.(stay_id, date)]
vis_by_date[,.(stay_id,date,vis)] %>% View()

# ICU 입실 후 1일 vis: 1일동안 최대 VIS
vis_first <- vis_raw[,.( vis = first(vis)),by=stay_id]

summary(vis_first$vis)
vis_first[vis>100]
df <- vis_first[df, on='stay_id']
# 일자별 vis 계산하기
vis_by_days <- vis_raw[stay_id %in% df_match$stay_id,.(vis_per_date = max(vis_h)),by=.(stay_id,date)]
vis_summary <- vis_by_days[,.(vis_sum = sum(vis_per_date)/.N,
                             vis_mean = mean(vis_per_date, na.rm = T) / .N,
                             vis_max = max(vis_per_date) / .N), by = stay_id]

summary(vis_summary)
# pills -------------------------------------------------------------------
pills
pills <- fread('csv/Afib_treatment_pills.csv')
pills <- pills[,lapply(.SD,sum), by=stay_id]
dfPills <- left_join(afib_dex48[stay_id %in% df_match$stay_id,.(stay_id, dex_usage,afib_within_7days,
                                  diltiazem,
                                  metoprolol,
                                  esmolol,
                                  digoxin,
                                  magnesium)],pills,by='stay_id')
pill.names <- c('sotalol','dronedarone','flecainide','propafenone','quinidine','amiodarone')
pill.names
dfPills[,(pill.names) := lapply(.SD, \(x) ifelse(is.na(x),0,x)),.SDcol=pill.names]

mytable(dex_usage ~., dfPills[afib_within_7days==1])

with(dfPills, ks.test(dex_usage,sotalol))

# 변수 추가 or 조정 -------------------------------------------------------------------

df[,sex := as.factor(ifelse(gender=='F','Female','Male'))]
df[,ethnicity := as.factor(ifelse(ethnicity == 'ASIAN','Asian',
                        ifelse(ethnicity %like% 'BLACK','African-American',
                               ifelse(ethnicity =='WHITE','Caucasian',
                                      ifelse(ethnicity %like% 'HISPANIC','Hispanic','Other')))))]

df[,icuunit:=
          as.factor(fifelse(first_careunit %like% 'SICU', 'SICU',
                  fifelse(first_careunit %like% 'CVICU','CVICU',
                          fifelse(first_careunit %like% 'CCU', 'CCU',
                                  fifelse(first_careunit %like% 'MICU', 'MICU','ETC')))))]
df[,cvicu := as.factor(ifelse(first_careunit %like% 'CVICU','CVICU','Non-CVICU'))]
df[,dex_usage := ifelse(stay_id %in% dex_id,1,0)]
df[,afib_within_7days := as.factor(
  ifelse(afib_newonset==1 & 
           afib_onset_time >=icu_intime &
           as.numeric(difftime(afib_onset_time, icu_intime, units='days'))<7,1,0))]

# crrt
df[,crrt_before_afib := as.factor(ifelse(stay_id %in% crrt[crrt_type==1,stay_id],'Yes','No'))]
df[,crrt_after_afib := as.factor(ifelse(stay_id %in% crrt[crrt_type==0,stay_id],'Yes','No'))]
df[,crrt_usage := ifelse(stay_id %in% crrt[crrt_type==2,stay_id],'Yes','No')]

# ventilator
df[,ventil_usage_before_afib := ifelse(stay_id %in% ventilation[ventil_usage_before_afib==1 ,stay_id],'Yes','No')]
df[,ventil_usage_after_afib := ifelse(stay_id %in% ventilation[ventil_usage_after_afib==1, stay_id],'Yes','No')]
df[,ventil_usage := ifelse(stay_id %in% ventilation[ventil_usage==1, stay_id],'Yes','No')]
df <- left_join(df, ventilationDuration, by='stay_id')
df[,ventilator := as.factor(ifelse(afib_within_7days==1, ventil_usage_before_afib,
                                         ifelse(afib_within_7days==0, ventil_usage, 'No')))]
setnames(df, 'duration_hour','ventil_duration_hour')
df[,ventil_duration_hour := ifelse(is.na(ventil_duration_hour),0,ventil_duration_hour)]

# afib 발생 이후의 치료약물 사용 treatment 
drugsAfterAfib <- drugs[stay_id %in% df[afib_within_7days==1,stay_id] & drug_starttime>=afib_onset_time]

drugsAfterAfibList <- dcast(drugsAfterAfib, stay_id ~ drugname)
drugsAfterAfibList
names(drugsAfterAfibList) <- 
  paste0(names(drugsAfterAfibList),'_after_afib')
setnames(drugsAfterAfibList,'stay_id_after_afib','stay_id')

target <- grep('_after_afib',names(drugsAfterAfibList),value=T)

drugsAfterAfibList[,(target):=lapply(.SD, function(x) ifelse(x>=1,'Yes','No')),.SDcols=target]
df <- left_join(df, drugsAfterAfibList, by='stay_id')

df[,(target) := lapply(.SD, function(x) as.factor(ifelse(is.na(x),'No',x))),.SDcols=target]

df[,bmi := (weight / (height *0.01)^2)]

df[,sepsis:=as.factor(ifelse(is.na(sepsis),'No','Yes'))]
df[,race:= as.factor(ifelse(ethnicity %like% 'African-American', 'African American','etc'))]
df[,heart_surgery := as.factor(ifelse(hadm_id %in% heartSurgery$hadm_id,'Yes','No'))]
df[,obs_duration := as.numeric(ifelse(afib_within_7days==1, 
                                      difftime(afib_onset_time,icu_intime, units='hours'),
                                      difftime(icu_outtime, icu_intime, units = 'hours')))]

df[,t2e := as.numeric(ifelse(is.na(afib_onset_time),0, ifelse(afib_within_7days==1,difftime(afib_onset_time, icu_intime, units='days'),0)))]
df[,cardioversion := as.factor(ifelse(stay_id %in% cardioVersion$id, 'Yes','No'))]

dates <- c('dod',grep('time',names(df), value=T))
df[,(dates) := lapply(.SD, function(x) as.character(x)), .SDcols=dates]
df[,death:=as.factor(ifelse(!is.na(dod),'Yes','No'))]
df[,dod:=ifelse(inhos_mortality=='Yes' & dod > icu_outtime, icu_outtime ,dod)]
df[,inhos_mortality:= as.factor(ifelse(is.na(dod),'No',
                                            ifelse(dod <= dischtime, 'Yes','No')))]
df[,mortality_90 := as.factor(ifelse(is.na(dod),'No',
                                     ifelse(dod<=as.Date(icu_intime)+lubridate::days(90),'Yes','No')))]
# df[,inhos_mortality:=ifelse(inhos_mortality=='Yes' & dod > icu_outtime, 'No' ,dod)]
df[,inhos_duration := as.numeric(ifelse(inhos_mortality=='Yes', difftime(dod, icu_intime, units='days'), difftime(dischtime,icu_intime, units='days')))]
df[,mot90_duration := as.numeric(ifelse(mortality_90=='Yes', difftime(dod,icu_intime, units='days'), difftime(dischtime,icu_intime, units='days')))]

target_id <- df[inhos_duration>100,.(stay_id,dod, icu_intime, icu_outtime)]
mytable(dex_usage ~ afib_within_7days, df)
mytable(~dex_usage, df)
summary(df$vis)
# bradycardia & hypotension -----------------------------------------------


hypoCount <- fread('csv/Afib_hypotension_count.csv')
bradyCount <- fread('csv/Afib_bradycardia_count.csv')
hypoRate <- fread('csv/Afib_hypotension_rate.csv')
bradyRate <- fread('csv/Afib_bradycardia_rate.csv')
bradyHypoCount <- fread('csv/Afib_bradyhypo_count.csv')
bradyHypoCount

df <- bradyHypoCount[,.(stay_id,hypo_count,brady_count)][df,on='stay_id']

# 1. count
dfCount <- left_join(df[,.(dex_usage, stay_id,icu_intime,icu_outtime)],bradyHypoCount, by='stay_id')
dfCount
# setnames(dfCount,'first_occur_time','hypo_time')
# dfCount <- left_join(dfCount, bradyCount, by='stay_id')


# 중증도 변수 추가 ---------------------------------------------------------------

df <- mimic_apsiii[df, on='stay_id']
df <- mimic_sapsii[df,on='stay_id']
df <- mimic_oasis[df, on='stay_id']

df[stay_id %in% df_match$stay_id,
   .(mean(sapsii),
     sd(sapsii)),by=dex_usage]
with(df[stay_id %in% df_match$stay_id],
     t.test(sapsii ~ dex_usage))

df_match <- mimic_sapsii[df_match, on='stay_id']
require(tableone)
CreateTableOne(data=df_match, strata = 'dex_usage',
               addOverall = T, smd = T) %>% print(smd=T)
# Matching ----------------------------------------------------------------

require(mice)
require(MatchIt)
require(data.table)
temp <- df[,.(afib_within_7days, sex, age, race, ethnicity,bmi, heart_surgery,
              icuunit, vis, sepsis, sofa_score, cci, cvicu,
              ventil_usage_before_afib,
              ventil_usage,
              sapsii,
              ventilator, ventil_duration_hour,
              crrt_after_afib,
              crrt_before_afib,
              map, heart_rate, resp_rate, temperature, hemoglobin, wbc, creatinine,
              bun, spo2, lactate, platelet, sodium, chloride, potassium, calcium, bicarbonate, magnesium ,
              inhos_mortality, mortality_90,
              brady_count, hypo_count,
              obs_duration, dex_usage, inhos_duration, t2e, cardioversion,
              amiodarone_after_afib, diltiazem_after_afib, metoprolol_after_afib, esmolol_after_afib,
              nicardipine_after_afib, digoxin_after_afib, magnesium_after_afib,
              los_icu, los_hospital)]

# mytable(dex_usage ~ amiodarone_after_afib,df[afib_within_7days==1])
set.seed(7795)
df[,vis:=ifelse(vis>500,NA,vis)]
df_mice <- mice(temp, seed = 7795)
df_comp <- complete(df_mice,2)
stay_id <- df$stay_id
df_comp <- cbind(stay_id, df_comp)
set.seed(7795)
# matching: factor or numeric 변수만 사용할 것.
m2 <- matchit(dex_usage ~
                 sapsii + 
               sex + age + ethnicity + bmi + heart_surgery +
               icuunit + vis + sepsis + sofa_score + cci + 
               ventilator+ crrt_before_afib +
               map + heart_rate + resp_rate + temperature +
               hemoglobin + wbc + creatinine + bun + spo2 + 
               lactate + platelet + sodium + chloride + potassium + calcium+
               bicarbonate + magnesium,
             data=df_comp,
             ratio = 2,
             caliper = 0.1,
             method = 'nearest') # logistic regression algorithm.
# fwrite(df_match,'csv/mimic_df_final.csv')
df_match <- as.data.table(match.data(m2, data=df_comp))
df_match_id <- as.data.table(match.data(m2, data=df_comp))
df_match2 <- as.data.table(match.data(m2, data=df_comp))
df_match <- as.data.table(match.data(m1, data=df_comp,
                                     distance='prop.score'))


plot(summary(m2))
mytable(dex_usage ~ inhos_mortality, df_match)

bal.tab(m, abs = T, method)
bal.plot(m, var.name='sex', which='both')
bal.plot(m, var.name='age', which='both')
bal.plot(m, var.name='distance', which='both', type='histogram',
         mirror=T)
mytable(dex_usage ~ bmi, df_match)


df[,icu_duration_hour := as.numeric(difftime(icu_outtime, icu_intime,units='hours'))]
df_match2 <- inner_join(df_match2, df[,.(stay_id,icu_duration_hour)],by='stay_id')

df_match2[,`:=`(
  hypo_per_duration = hypo_count / icu_duration_hour,
  brady_per_duration = brady_count/icu_duration_hour
)]

df_match2[,`:=`(
  brady_yn = as.factor(ifelse(brady_count>1,1,0)),
  hypo_yn = as.factor(ifelse(hypo_count>1,1,0))
)]
df_match[,age65:=as.factor(ifelse(age>=65,1,0))]
df_match_id[,age65:=as.factor(ifelse(age>=65,1,0))]
mytable(dex_usage ~ hypo_per_duration + brady_per_duration + los_icu +
          icu_duration_hour,  
        df_match2,
        digits=2)

# additional: SAPS-II -----------------------------------------------------
con <- DBI::dbConnect(
  dbDriver('PostgreSQL'),
  dbname = 'r3417',
  host = 'localhost',
  port = 5432,
  user = 'r3417',
  password = 'snubh19940123'
)

sapsii <- setDT(dbGetQuery(conn = con, "select ac.stay_id, sapsii from sapsii
inner join afib_cohort ac on sapsii.stay_id = ac.stay_id"))
sapsii[stay_id %in% df_match_id$stay_id]
df_match_id <- inner_join(df_match_id, sapsii, by='stay_id')
df_match2 %>% head()
df_match_id %>% head()


# mortality ---------------------------------------------------------------
require(dplyr)
df[,`:=`(
  mortality_14 = as.factor(ifelse(is.na(dod),0,
                                   ifelse(dod<=as.Date(icu_intime) + lubridate::days(14),1,0))),
  mortality_28 = as.factor(ifelse(is.na(dod),0,
                                  ifelse(dod<=as.Date(icu_intime) + lubridate::days(28),1,0))),
  mortality_60 = as.factor(ifelse(is.na(dod),0,
                                   ifelse(dod<=as.Date(icu_intime) + lubridate::days(60),1,0)))
)]
df[,mot14_duration := as.numeric(ifelse(mortality_14=='Yes', difftime(dod,icu_intime, units='days'), difftime(dischtime,icu_intime, units='days')))]
df[,mot28_duration := as.numeric(ifelse(mortality_28=='Yes', difftime(dod,icu_intime, units='days'), difftime(dischtime,icu_intime, units='days')))]
df[,mot60_duration := as.numeric(ifelse(mortality_60=='Yes', difftime(dod,icu_intime, units='days'), difftime(dischtime,icu_intime, units='days')))]
df[,dod_duration := as.numeric(ifelse(!is.na(dod),difftime(dod, icu_intime, units='days'), 
                                      difftime(dischtime, icu_intime, units='days')))]
temp <- df %>% filter(stay_id %in% df_match$stay_id)
mytable(dex_usage~ inhos_mortality + mortality_14 + mortality_28 + mortality_60,
  temp)
require(survival)

coxph(Surv(inhos_duration, inhos_mortality=='Yes')~dex_usage, temp) %>% summary()
coxph(Surv(mot14_duration, mortality_14==1)~dex_usage, temp) %>% summary()
coxph(Surv(mot28_duration, mortality_28==1)~dex_usage, temp) %>% summary()
coxph(Surv(mot60_duration, mortality_60==1)~dex_usage, temp) %>% summary()
coxph(Surv(mot90_duration, mortality_90=='Yes')~dex_usage, temp) %>% summary()


tbl_summary(
  data=temp,
  include = c( inhos_mortality, 
               mortality_14, mortality_28, mortality_60,
               mortality_90),
  type = list(c(mortality_14, mortality_28, mortality_60)~'dichotomous'),
  value = list(c(mortality_14, mortality_28, mortality_60)~1),
  by=dex_usage,
  digits = list(all_dichotomous() ~ c(0,1))
)


temp[!is.na(dod),.(admittime, icu_intime, icu_outtime, dischtime,
                   dod, dod_duration)]
# save --------------------------------------------------------------------

fwrite(df_match,'csv/df_match.csv')
fwrite(df,'csv/df.csv')
fwrite(df_comp,'csv/df_comp.csv')

mimic_apache[stay_id %in% df_match$stay_id, mean(apacheii)]

with(mimic_sapsii[df_match, on='stay_id'],
     t.test(sapsii ~ dex_usage))
with(mimic_apsiii[df_match, on='stay_id'],
     t.test(apsiii ~ dex_usage))

with(mimic_oasis[df_match, on='stay_id'],
     t.test(oasis ~ dex_usage))
