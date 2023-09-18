

# csv ---------------------------------------------------------------------
cohort <- fread('csv/Afib_cohort.csv')
comorbidity <- fread('csv/Afib_comorbidity.csv')
sepsis <- fread('csv/Afib_sepsis.csv')
ventil <- fread('csv/Afib_ventil.csv')
sofa <- fread('csv/Afib_sofa.csv')
fVitalSign <- fread('csv/Afib_vitalsign.csv')
flab <- fread('csv/Afib_lab.csv')
map <- fread('csv/Afib_map.csv')
flab <- map[flab, on='stay_id']
magnesium <- fread('csv/Afib_magnesium.csv')

# dex_after_delirium <- fread('csv/Afib_dex_after_delirium.csv')
mytable(~dex_usage,)

# cohort configuration ------------------------------------------------------------------


cohort <- left_join(cohort, comorbidity, by='subject_id')
cohort <- left_join(cohort, sepsis, by='stay_id')
cohort <- left_join(cohort, sofa[,.(stay_id,sofa_score)], by='stay_id')
cohort <- left_join(cohort,ventil[,.(stay_id,ventil)], on=c('stay_id'))
cohort <- left_join(cohort,fVitalSign, on=c('stay_id'))
cohort <- left_join(cohort,flab, on=c('stay_id'))
cohort <- left_join(cohort, rrt, on='stay_id')
cohort[,BMI:= (weight / (height *0.01)^2)]

cohort[,t2e_days:= round(as.numeric(difftime(afib_onset_time,icu_intime, units='days')),1)]
cohort[,sepsis:= as.factor(ifelse(is.na(sepsis),0, ifelse(sepsis,1,0)))]

# drugs -------------------------------------------------------------------
drugs2
drugs2 <- fread('csv/Afib_drugs.csv')
drugs_sum <- drugs2[,.(icu_intime =min(intime),
                       drug_starttime = first(drug_starttime),
                       drug_endtime = last(drug_endtime),
                        newonset = min(newonset)),
                       # drug_rate = round(sum(rate, na.rm=T),2),
                       # uom = first(rateuom),
                       # afib_onset_time = min(afib_onset_time),
                       # newonset = min(newonset),
                       # drug_duration=sum(drug_duration)), 
                    by=c('stay_id','drugname')]


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
  amiodarone = fifelse(drugname == 'amiodarone',1,0),
  digoxin = fifelse(drugname =='digoxin',1,0)
)]

drugs_use_yn_df <- drugs_use_yn_df[,lapply(.SD, sum), by=stay_id]


cohort2 <- inner_join(cohort,  drugs_use_yn_df, by='stay_id')
cohort2[,afib_within_7days:= ifelse(is.na(afib_onset_time),0, ifelse(
    as.numeric(difftime(afib_onset_time, icu_intime, units='days'))<=7,1,0
  )
)]
drugs2[,dex_use:=as.factor(fifelse(
  (newonset==1 &
     drugname=='dexmedetomidine' &  
     drug_starttime <= afib_onset_time &
     drug_starttime >= intime ) |
    (newonset==0 & drugname=='dexmedetomidine' &
       drug_starttime>=intime),1,0
))]
dex_id <- drugs2[dex_use==1]$stay_id
dex_df <- drugs2[dex_use==1]
cohort2[,dex_usage:=as.factor(fifelse(stay_id %in% dex_id,1,0))]
cohort2[,obs_duration:= as.numeric(ifelse(afib_within_7days==1,
                                 difftime(afib_onset_time, icu_intime, units="hours"),
                                 difftime(icu_outtime, icu_intime, units="hours")))]

setnames(cohort2, 'rrt','crrt')
target <- c('chronic_pulmonary_disease',
            'cerebrovascular_disease' ,
            'congestive_heart_failure' ,
            'valvular_disease' ,
            'chronic_liver_disease' ,
            'diabetes' ,
            'hypertension' , 
            'renal_disease' ,
            'obstructive_sleep_apnea',
            'crrt' , 'ventil',
            'sepsis')
cohort2[, (target):= lapply(.SD, as.factor), .SDcols=target]
cohort2[,ccu:=as.factor(fifelse(unit=='CCU',1,0))]

# 28481 -> 22251(No med records 제외) -> 21422(dex after delirium 제외)

# dex 사용용량 --------------------------
dex_rate <- fread('csv/Afib_dex_use_rate.csv')
dex_rate

cohort2 <- left_join(cohort2, dex_rate, by='stay_id')
cohort2 <- cohort2 %>% mutate(dex_dose_4tile = ntile(dex_dose,4))
cohort2[,dex_dose_4tile := ifelse(dex_dose_4tile==1,'1st',
                                  ifelse(dex_dose_4tile==2,'2nd',
                                         ifelse(dex_dose_4tile==3,'3rd',
                                                ifelse(dex_dose_4tile==4,'4th',NA))))]

# VIS ---------------------------------------------------------------------
vis <- fread('csv/Afib_vis.csv')
vis[,.N,by=c('drugname','rateuom')]
vis[rateuom=='mg/kg/min']
vis[drugname=='vasopressin']
vis[,rate:=fifelse(rateuom=='mg/kg/min',rate/1000, rate)]
vis[,rate:= ifelse(drugname=='vasopressin',rate/patientweight/60,rate)]

vis

vis_summary <- vis[rateuom!='mg/kg/min',.(dose=sum(rate)),by=c('stay_id','drugname')]
vis_dose <- dcast(vis_summary, stay_id ~ drugname, value.var='dose',fill=0)
vis_dose[,vis := 
           dopamine +
           dobutamine + #mcg/kg/min
           100 * epinephrine + #mcg/kg/min
           10 * milrinone + #mcg/kg/min
           10000 * vasopressin +  #units/kg/min
           100 * norepinephrine]
vis_dose %>% View()
fwrite(vis_dose,'csv/vis_dose_result.csv')
cohort2 <- vis_dose[,.(stay_id,vis)][cohort2, on='stay_id']


cohort2[stay_id %in% match_data$id,.(mean(vis,na.rm=T),
                                     sd(vis,na.rm=T)),by=dex_usage]

# mortality ---------------------------------------------------------------

mortality <- fread('csv/Afib_mortality.csv')
vals <- grep('time',names(cohort2), value=T)

cohort2[,(vals):=lapply(.SD, function(x) as.Date(as.character(x))), .SDcols=vals]
# in hospital mortality: 퇴원 전에 죽었는지 아닌지
cohort2[,inhos_mortality:= as.factor(ifelse(is.na(dod),0,
                                            ifelse(dod <= dischtime, 1,0)))]

cohort2[, duration_inhos_mortality:= ifelse(!is.na(dod),
                                            as.numeric(difftime(dod,as.Date(icu_intime), units='hours')),
                                            as.numeric(difftime(icu_outtime,icu_intime), units='hours'))]

cohort2[,mean(as.numeric(duration_inhos_mortality), na.rm=T),by=.(inhos_mortality,dex_usage)]

# 90 mortality: ICU 입실 이후 90일 이내 죽었는지 살았는지
cohort2[,mortality_90 := as.factor(ifelse(dod<=icu_intime+lubridate::days(90),1,0))]


# 심장 수술 여부? ---------------------------------------------------------------
heart_surgery
heart_surgery <- fread('csv/Afib_heart_surgery_before_icu.csv')
heart_surgery[cohort2,on='subject_id']
cohort2[,heart_surgery := ifelse(subject_id %in% heart_surgery$subject_id,1,0)]

# HR mean -----------------------------------------------------------------
hr_mean <- fread('csv/Afib_hr_mean.csv')
cohort2 <- inner_join(hr_mean,cohort2, by='stay_id')
cohort2

# 기타 변수 조정 ----------------------------------------------------------------

cohort2[,race := ifelse(race == 'ASIAN','Asian',
                        ifelse(race %like% 'BLACK','African-American',
                               ifelse(race =='WHITE','Caucasian',
                                      ifelse(race %like% 'HISPANIC','Hispanic','Other'))))]

cohort2[,icuunit:=
         fifelse(unit %like% 'SICU', 'SICU',
               fifelse(unit %like% 'CVICU','CVICU',
                       fifelse(unit %like% 'CCU', 'CCU',
                               fifelse(unit %like% 'MICU', 'MICU','ETC'))))]



# hypotension -------------------------------------------------------------


hypotension <- fread('csv/Afib_Hypotension_.csv')
hypotension
hypo_id <- hypotension[dex_use==1 & charttime>=dex_starttime]$stay_id
cohort2[,hypo:= ifelse(dex_usage==1 & stay_id %in% hypo_id,1,
                       ifelse(dex_usage==0 & stay_id %in% hypotension$stay_id,1,0))]


# bradycardia
brady <- fread('csv/Afib_Bradycardia.csv')
brady_id <- brady[dex_use==1 & charttime>=dex_starttime]$stay_id
cohort2[,brady:= ifelse(dex_usage==1 & stay_id %in% brady_id,1,
                       ifelse(dex_usage==0 & stay_id %in% brady$stay_id,1,0))]

mytable(dex_usage ~ hypo, cohort2)
mytable(dex_usage ~ brady, cohort2)


# 발생빈도
hypo_incidence <- fread('csv/Afib_hypo_incidence.csv')
brady_incidence <- fread('csv/Afib_brady_incidence.csv')
setnames(hypo_incidence,'stay_id','id')
setnames(brady_incidence,'stay_id','id')

hypo_incidence[id %in% match_data$id][order(-count)]
brady_incidence[stay_id %in% match_data$id][order(-count)]
hypo_incidence[stay_id %in% match_data$id][,.(sum(count)/sum(icu_duration)*1000)]
brady_incidence[stay_id %in% match_data$id][,.(sum(count)/sum(icu_duration)*1000)]

temp <- left_join(match_data2, hypo_incidence, by='id')
temp <- left_join(temp, brady_incidence, by='id')
setnames(temp,'count.x','hypo_count')
setnames(temp,'icu_duration.x','hypo_duration')
setnames(temp,'count.y','brady_count')
setnames(temp,'icu_duration.y','brady_duration')


temp[,.(mean(brady_count,na.rm=T), sd(brady_count,na.rm=T),
        mean(hypo_count, na.rm=T), sd(hypo_count,na.rm=T))]
with(temp,
     t.test(hypo_count ~ dex_usage))
# ventil duration -------------------------------------------------------------

# ventil duration: hour
ventil_duration <- fread('csv/Afib_ventil_hour.csv')
cohort2 <- left_join(cohort2, ventil_duration, by='stay_id')
cohort2[,ventil_duration:= ventil_duration / 24]
cohort2[,.(stay_id, ventil,ventil_duration)]
cohort2[,ventil:=ifelse(is.na(ventil),0, ifelse(ventil==1,1,0))]
cohort2[,ventil_duration:=ifelse(is.na(ventil_duration),0, ventil_duration)]


cohort2[stay_id %in% match_data$id, .(mean(ventil_duration)/24, sd(ventil_duration)/24)]

cohort2[,.N, by=stay_id][order(-N)]

cohort2[stay_id %in% match_data$id] %>% 
  gtsummary::tbl_summary(
  include=c(ventil_duration),
  statistic = list(all_continuous() ~ "{mean} ± {sd}"),
  by = dex_usage
) %>% add_p()




# magnesium ---------------------------------------------------------------

match_data2 <- left_join(match_data,magnesium, by='id')
mytable(dex_usage ~ magnesium_in_24 + magnesium_after_24, match_data2)

require(tableone)
CreateTableOne(vars = c('magnesium_in_24','magnesium_after_24'),
               factorVars = c('magnesium_in_24','magnesium_after_24'),
               data=match_data2,
               addOverall = T,
               includeNA = T,
               strata = 'dex_usage')


# cardiogram --------------------------------------------------------------

cardio <- fread('csv/Afib_cardioversion.csv')

match_data2 <- left_join(match_data2, cardio, by='id')
match_data2[,cardioversion:=ifelse(is.na(cardioversion),0,1)]

mytable( ~ cardioversion, match_data2)


# supple: 심박수 -------------------------------------------------------------

# fread('csv/Afib_hr_chart.csv')
heart_rate <- fread('csv/Afib_heartrate.csv')
heart_rate <- heart_rate[stay_id %in% match_data$id]

setnames(heart_rate,'value','hr')

# heart_rate[,dex_use := as.factor(ifelse(stay_id %in% cohort2[dex_usage==1]$stay_id,'Yes','No'))]

heart_rate[, hours:=format(charttime, format='%H')]
heart_rate[,dates:=format(charttime, format='%Y%m%d')]
heart_rate[,row_num:=rowid(stay_id)]
heart_rate[,dex_use:=as.factor(ifelse(dex_use==0,'No','Yes'))]

heart_rate[row_num<170 & hr<10000, .(m=mean(hr),sd=sd(hr)), by='dex_use']
heart_rate_afib <- heart_rate[stay_id %in% match_data[afib_within_7days==1]$id]
fwrite(match_data,'result/Afib_match.csv')
