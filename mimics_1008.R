afib %>% head()
demography %>% head()
race %>% head()

afib <- fread('csv/Afib_newonset_id.csv')
demography <- fread('csv/Afib_demography.csv')
comorbidity <- fread('csv/Afib_comorbidity.csv')
sepsis <- fread('csv/Afib_sepsis.csv')
ventil <- fread('csv/Afib_ventil.csv')
sofa <- fread('csv/Afib_sofa.csv')
fVitalSign <- fread('csv/Afib_vitalsign.csv')
flab <- fread('csv/Afib_lab.csv')

# mortality
require(lubridate)
mortality <- fread('csv/Afib_mortality.csv')
mortality
require(dplyr)
require(tidyr)

dplyr::inner_join(cohort2,mortality, by='stay_id')
cohort <- afib[demography, on=c('subject_id','stay_id')]

cohort <- comorbidity[cohort,on='subject_id']
cohort[,sepsis:= as.factor(fifelse(stay_id %in% sepsis$stay_id,1,0))]
cohort <- sofa[,.(stay_id,sofa_score)][cohort, on='stay_id']
ventil
cohort <- ventil[,.(stay_id,ventil)][cohort, on=c('stay_id')]
cohort <- cohort[fVitalSign, on=c('stay_id')]
cohort <- cohort[flab, on=c('stay_id')]
cohort <- cohort[rrt, on=c('subject_id','stay_id')]
cohort[,BMI:= (weight / (height *0.01)^2)]
cohort[,icutype:=
            fifelse(unit %like% 'MICU', 'MICU',
                     fifelse(unit %like% 'Neuro','Neuro ICU',
                             fifelse(unit %like% 'Trauma', 'Trauma ICU',
                                     fifelse(unit %like% 'SICU', 'SICU','CCU'))))]
cohort[,t2e:=difftime(afib_onset_time,intime, units='hours')]
# newonset 아닌 사람들 중 icu입실 전 dex 사용한 사람=0명

# 약물 데이터 추가 ---------------------------------------------------
drugs <- fread('csv/Afib_drugs.csv')
drugs %>% View()
length(unique(drugs$stay_id))

cohort[!stay_id %in% drugs$stay_id] %>%  View()
drugs_sum <- drugs2[,.(icu_intime =min(intime),
         drug_starttime = first(drug_starttime),
         drug_endtime = last(drug_endtime),
         drug_rate = round(sum(rate, na.rm=T),2),
         uom = first(rateuom),
         afib_onset_time = min(afib_onset_time),
         newonset = min(newonset),
     drug_duration=sum(drug_duration)), by=c('stay_id','drugname')]

drugs_sum <- drugs2[,.(
  icu_intime = min(intime),
  drug_starttime = first(drug_starttime),
  drug_endtime = last(drug_endtime)
), by=c('stay_id','drugname')]


length(unique(drugs2$stay_id))
# temp <- drugs_sum[drugname %in% c('propofol','dexmedetomidine','midazolam','morphine','fentanyl'),
#           .(usage_rate=mean(drug_rate),
#             uom = first(uom)),by=c('stay_id','drugname')]
# sedative_use_rate
# sedative_use_rate <- dcast(temp, stay_id ~ drugname, value.var = 'usage_rate')
# setnames(sedative_use_rate, 'dexmedetomidine','dex_rate')
# setnames(sedative_use_rate, 'fentanyl','fentanyl_rate')
# setnames(sedative_use_rate, 'midazolam','midazolam_rate')
# setnames(sedative_use_rate, 'morphine','morphine_rate')
# setnames(sedative_use_rate, 'propofol','propofol_rate')
drugs_sum
dex_before_afib_id <- drugs2[drugname=='dexmedetomidine' & 
                                  drug_starttime < afib_onset_time]$stay_id
drugs_use_yn_df <- drugs_sum[,.(
  stay_id,
  norepinephrine = fifelse(drugname == 'norepinephrine' ,1,0),
  epinephrine = fifelse(drugname == 'epinephrine',1,0),
  dobutamine = fifelse(drugname == 'dobutamine',1,0),
  dopamine = fifelse(drugname == 'dopamine',1,0),
  milrinone = fifelse(drugname == 'milrinone',1,0),
  vasopressin = fifelse(drugname == 'vasopressin',1,0),
  midazolam = fifelse(drugname == 'midazolam' ,1,0),
  propofol = fifelse(drugname == 'propofol',1,0),
  dexmedetomidine = fifelse(drugname == 'dexmedetomidine',1,0),
  morphine = fifelse(drugname == 'morphine',1,0),
  fentanyl = fifelse(drugname == 'fentanyl',1,0),
  metoprolol = fifelse(drugname == 'metoprolol' ,1,0),
  esmolol = fifelse(drugname == 'esmolol',1,0),
  diltiazem = fifelse(drugname == 'diltiazem' ,1,0),
  nicardipine = fifelse(drugname == 'nicardipine',1,0),
  verapamil = fifelse(drugname == 'verapamil',1,0),
  amiodarone = fifelse(drugname == 'amiodarone',1,0)
)]

table(cohort$stay_id %in% drugs_use_yn_df$stay_id)

# stay_id별로 column 합치기(약물 사용 여부 합치기 위해)
drugs_use_yn_df <- drugs_use_yn_df[,lapply(.SD, sum), by=stay_id]
cohort[drugs_use_yn_df, on=c('stay_id')] %>% View()
cohort2 <- cohort[drugs_use_yn_df, on=c('stay_id')]

# cohort2[,dex_before_afib:= as.factor(fifelse(stay_id %in% dex_before_afib_id, 1,0))]

# icu 입실 12시간 전 dex 약물 사용한 환자 제외


#mortality
cohort2 <- dplyr::left_join(cohort2,mortality[,.(stay_id,los_hospital,los_icu,dod)], by='stay_id')
# 7일 이내 Afib 발생
#icu 입실 후 7일 이내 afib 발생 여부
newonset_id <- cohort2[newonset==1]$stay_id
length(unique(drugs_sum$stay_id))
drugs_sum[,afib_within_7days:= as.factor(fifelse(
  stay_id %in% newonset_id & 
    difftime(afib_onset_time,icu_intime, units='days')<=7 &
    difftime(afib_onset_time,icu_intime, units='days')>=0,1,0
))]

afib_within_7days_id <- unique(drugs2[afib_within_7days==1]$stay_id)
cohort2[,afib_within_7days:=as.factor(fifelse(stay_id %in% afib_within_7days_id,1,0))]
cohort2[,dex_usage:=as.factor(fifelse(stay_id %in% dex_df$stay_id,1,0))]

mytable(afib_within_7days ~ dex_usage, cohort2)

