
drugs_table
cohort_firstdays
colnames(cohort_firstdays)
cohort_firstdays <- cohort_firstdays %>% 
  select(subject_id,hadm_id, stay_id, newonset, contains('mean'))

cohort_firstdays
cohort_1
drugs_table

colnames(drugs_table)

temp1 <- cohort_firstdays[drugs_table, on=c('stay_id','newonset')]
Afib_cohort[,i.ventil:=NULL]
Afib_cohort <- temp1[cohort_1, on=c('subject_id','hadm_id','stay_id','newonset')]

Afib_cohort <- fread('csv/Afib_df.csv')
Afib_cohort[,dexuse:=fifelse(dexmedetomidine==1,'1','0')]
Afib_cohort[,dex_before_afib:=fifelse(stay_id %in% drugs[dex_before_afib==1,stay_id],'1','0')]
Afib_cohort_wo_sepsis <- Afib_cohort[sepsis==1]
fwrite(Afib_cohort,'csv/Afib_df.csv')
colnames(Afib_cohort)

mytable(~dex_before_afib,Afib_cohort)

# Table 1 -----------------------------------------------------------------

mytable(newonset ~  gender + age + weight + height + BMI + race + 
          chronic_pulmonary_disease+ cerebrovascular_disease+
        congestive_heart_failure+malignant_cancer+
        valvular_disease+chronic_liver_disease+
        diabetes+hypertension+renal_disease+
        obstructive_sleep_apnea+cci+
          icutype+
          sepsis+
          sofa_score+
          sbp_mean + dbp_mean + heart_rate_mean + 
          resp_rate_mean + temperature_mean + spo2_mean +
          hemoglobin_mean + wbc_mean + platelets_mean + 
          bnp_mean+ troponin_mean + sodium_mean + chloride_mean +
          potassium_mean + calcium_mean + magnesium_mean + bicarbonate_mean + 
          creatinine_mean + bun_mean + ph_mean + po2_mean +   +
          pco2_mean + lactate_mean +  ventil+
          dexmedetomidine + propofol + midazolam + morphine + fentanyl
          , cohort2)
colnames(Afib_cohort)
mytable(newonset~isafib, afib)
mytable(~newonset,afib)

Afib_cohort %>% View()
setnames(Afib_cohort, 'icu_intme','icu_intime')
Afib_cohort[,`:=`(
  afib_onset_time = as.POSIXct(afib_onset_time, format="%Y-%m-%d %H:%M:%S"),
  icu_intime = as.POSIXct(icu_intime, format="%Y-%m-%d %H:%M:%S")
)]

# Dexmedetomidine 사용 시점에 따른 onset duration --------------------------------------------
Afib_cohort[,.(stay_id, icu_intime, afib_onset_time)]

Afib_cohort[newonset==1,.(event_duration =
                            mean(
                              difftime(afib_onset_time,icu_intime, units='hours')
                                       ,na.rm=T)),by=dex_before_afib]

summary(Afib_cohort$event_duration)
drugs[newonset==1,.(event_duration=mean(difftime(eventtime-intime, units=''))),by=dex_before_afib]
drugs %>% View()

with(Afib_cohort_wo_sepsis[newonset==1],
     t.test(difftime(afib_onset_time,icu_intime, units='hours') ~ dex_before_afib))

# Drug tables -------------------------------------------------------------

Afib_cohort_core <- Afib_cohort[,.(subject_id, hadm_id, stay_id, dex_before_afib)]
Afib_cohort_core$dex_before_afib %>% table()
str(drugs$dex_before_afib)
drugs <- fread('csv/Afib_drugs.csv')
drugs[drug_starttime <= afib_onset_time & drugname=='dexmedetomidine']
dex_after_icu_in_df <- drugs[difftime(drug_starttime,intime, units='hours')<=3 & drugname=='dexmedetomidine']
dex_after_icu_in
x <- drugs[difftime(drug_starttime,intime, units='hours')<=3 & drugname=='dexmedetomidine'][,.N, by=newonset][,pct:= paste0(round(N/sum(N)*100,1),'%')]
y <- drugs[difftime(drug_starttime,intime, units='hours')>3 & drugname=='dexmedetomidine'][,.N, by=newonset][,pct:= paste0(round(N/sum(N)*100,1),'%')]


dex_after_icu_in[,.(stay_id, newonset)]
temp[stay_id %in% dex_after_icu_in_df$stay_id]



x <- drugs[drugname=='dexmedetomidine' & drug_starttime>=intime & drug_starttime<afib_onset_time][,.N, by=newonset][,pct:= round(N/sum(N)*100,1)]


temp[,dex_after_icu_in := as.factor(fifelse(stay_id %in% dex_after_icu_in$stay_id,1,0))]
dex_after_icu_in
mytable(dex_after_icu_in ~ newonset, dex_after_icu_in)









drugs[,dex_before_afib:=as.character(dex_before_afib)]
cohort_drugs <- drugs[Afib_cohort_core, on=c('stay_id','dex_before_afib')]
cohort_drugs[,`:=`(
  norepinephrine = fifelse(drugname == 'norepinephrine' & drug_duration>=24,1,0),
  epinephrine = fifelse(drugname == 'epinephrine'& drug_duration>=24,1,0),
  dobutamine = fifelse(drugname == 'dobutamine'& drug_duration>=24,1,0),
  dopamine = fifelse(drugname == 'dopamine'& drug_duration>=24,1,0),
  mirinone = fifelse(drugname == 'mirinone'& drug_duration>=24,1,0),
  vasopressin = fifelse(drugname == 'vasopressin'& drug_duration>=24,1,0),
  midazolam = fifelse(drugname == 'midazolam' & drug_duration>=24,1,0),
  propofol = fifelse(drugname == 'propofol'& drug_duration>=24,1,0),
  dexmedetomidine = fifelse(drugname == 'dexmedetomidine'& drug_duration>=24,1,0),
  morphine = fifelse(drugname == 'morphine'& drug_duration>=24,1,0),
  fentanyl = fifelse(drugname == 'fentanyl'& drug_duration>=24,1,0),
  metoprolol = fifelse(drugname == 'metoprolol' & drug_duration>=24,1,0),
  esmolol = fifelse(drugname == 'esmolol'& drug_duration>=24,1,0),
  diltiazem = fifelse(drugname == 'diltiazem' & drug_duration>=24,1,0),
  nicardipine = fifelse(drugname == 'nicardipine'& drug_duration>=24,1,0),
  verapamil = fifelse(drugname == 'verapamil'& drug_duration>=24,1,0),
  amiodarone = fifelse(drugname == 'amiodarone'& drug_duration>=24,1,0)
)]
colnames(drugs)
drugnames <- c('norepinephrine', 'epinephrine','dopamine','dobutamine','mirinone',
           'vasopressin',
           'midazolam','propofol','dexmedetomidine','morphine','fentanyl',
           'metoprolol',  'esmolol',  'diltiazem', 'nicardipine','verapamil',
           'amiodarone','amiodarone')

for(k in drugnames){
  set(cohort_drugs, i=which(is.na(cohort_drugs[[k]])), j=k, value=0)
}

drugs_table <- dcast(cohort_drugs, stay_id + dex_before_afib ~ drugname, fun.aggregate = length) 
drugs_table[,'NA':=NULL]

drugs_table[,`:=`(
  dexmedetomidine = fifelse(dexmedetomidine ==2,1,dexmedetomidine),
  fentanyl = fifelse(fentanyl==2,1,fentanyl),
  amiodarone = fifelse(amiodarone>=2,1,amiodarone),
  nicardipine = fifelse(nicardipine>=2,1,nicardipine)
)]
drugs_table %>% View()
mytable(dex_before_afib ~ ., drugs_table) %>% myhtml()


# tablets ----------------------------------------------------------------


tablets <- fread('csv/Afib_tablets.csv')
tablets %>% colnames()
cohorts_tablets <- tablets[Afib_cohort_core, on=c('subject_id','hadm_id')]
View(cohorts_tablets)
cohorts_tablets[,`:=`(
  Acebutolol = fifelse(drug == 'Acebutolol' | drug =='Acebutolol HCl','1','0'),
  Atenolol = fifelse(drug == 'Atenolol','1','0'),
  Bisoprolol = fifelse(drug %like% 'Bisoprolol','1','0'),
  Nadalol = fifelse(drug == 'Nadalol','1','0'),
  Carvedilol = fifelse(drug == 'Carvedilol','1','0'),
  Amlodipine = fifelse(drug == 'Amlodipine','1','0'),
  Felodipine = fifelse(drug == 'Felodipine','1','0'),
  Nisoldipine = fifelse(drug == 'Nisoldipine','1','0'))]


tablets <- c('Acebutolol', 'Acebutolol HCl','Atenolol','Bisoprolol Fumarate','Nadalol','Carvedilol',
             'Amlodipine','Felodipine','Nisoldipine')

for(k in tablets){
  set(cohorts_tablets, i=which(is.na(cohorts_tablets[[k]])), j=k, value=0)
}

tablets_table <- dcast(cohorts_tablets, stay_id + newonset ~ drug, fun.aggregate = length) 
tablets_table[,`:=`(
  Acebutolol = fifelse(Acebutolol ==2,1,Acebutolol),
  Felodipine = fifelse(Felodipine>=2,1,Felodipine),
  Amlodipine = fifelse(Amlodipine>=2,1,Amlodipine),
  Atenolol = fifelse(Atenolol>=2,1,Atenolol),
  Carvedilol = fifelse(Carvedilol>=2,1,Carvedilol)
)]

