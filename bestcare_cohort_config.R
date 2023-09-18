require(haven)
require(readxl)
require(data.table)
require(readr)
# import data -------------------------------------------------------------
bestCohort <- fread('bestcare/bestcare_cohort.csv', encoding="UTF-8")
bestOutcome <- fread('bestcare/bestcare_outcome.csv')
best_diags <- fread('bestcare/bestcare_icu_diag.csv')
bestDex <- fread('bestcare/bestcare_dex_user.csv')
bestBp <- fread('bestcare/bestcare_sbp_dbp_pr_rr.csv')
bestSpo2 <- fread('bestcare/bestcare_bt_spo2.csv')
bestDiag <- fread('bestcare/bestcare_icu_diag.csv')
bestDex2 <- fread('bestcare/bestcare_drugs_new.csv')
bestHeight <- fread('bestcare/bestcare_height.csv')

bestICU <- fread('bestcare/bestcare_icu_diag.csv',encoding = "UTF-8")
bestCohort

# time varaibles ----------------------------------------------------------

require(stringr)
timeVars <- grep('time',names(bestCohort),value=T)
bestCohort[,lapply(.SD,function(x) class(x)),.SDcols=timeVars]

# 시간 단위 맞춰주기
toDate <- function(x){
  require(stringr)
  timeVars <- grep('time',names(x),value=T)
  for(v in timeVars){
    for(i in seq_len(length(x[[v]]))){
      date <- substr(x[[v]][i],1,11)
      time <- substr(x[[v]][i],12,16)
      if(str_count(str_trim(time))==4){
        x[[v]][i] <- paste0(date,'0',time)
      }
      else if(v=='afib_onset_time' & time == ""){
        x[[v]][i] <- NA
      }
      else if(time == ""){
        x[[v]][i] <- paste0(date,' 00:00')
      }
    }
  }
  return(x)
}
charToNum <- function(x){
  target <- names(x)[-1]
  for (i in target){
    for (j in seq_len(length(x[[i]]))){
      tryCatch(
        x[[i]][j] <- as.numeric(x[[i]][j]),
        error = function() {
          x[[i]][j] <- NA
        }
      )
    }
    x[[i]] <- as.numeric(x[[i]])
  }
  return(x)
}
bestOutcome <- toDate(bestOutcome)
bestCohort <- toDate(bestCohort)

# patients before 2012 exclusion: 943
# ids <- bestCohort[difftime(as.Date(intime),'2013-04-01')<0, unique(pid)];length(ids)
# bestCohort <- bestCohort[!pid %in% ids]
# bestCohort
# bestCohort <- bestCohort[difftime(intime,'2013-04-01')>=0] 


# 중복 제거 n= 86
over_two_pid <- bestCohort[,.N,by=pid][N>1,pid]; length(over_two_pid)
noaf_id <- bestCohort[!is.na(afib_onset_time),unique(pid)]; length(noaf_id)
bestCohort <- bestCohort[bestCohort[,ifelse(pid %in% noaf_id & pid %in% over_two_pid, 
                                            .I[!is.na(afib_onset_time)],.I[1L]),by=pid]$V1]
bestCohort
# AF within 7days ---------------------------------------------------------

# ICU 입실 28일 이상 제외: 140명

# dex 사용자 찾기 -------------------------------------------------------------
bestDex2 <- bestDex2[drugname %like% 'Dexmede']
bestDex2 <- toDate(bestDex2)
bestDex2
# dex 약물사용기록자 : 1646명
bestTemp <- inner_join(bestDex2[pid %in% bestCohort$pid],
                        bestCohort[,.(pid, afib_onset_time)],
                        by='pid')
# afib 환자: dex 사용 일자와 afib 발생일자가 같은 연도
# dexAfib <- bestTemp[!is.na(afib_onset_time),
#                     .(first_dex_time=first(drugtime),
#                       intime = first(intime),
#                       outtime= first(outtime),
#                       afib_onset_time = first(afib_onset_time)),by=pid]
# 
# # non AF 환자: 
# dexNonAfib <- bestTemp[is.na(afib_onset_time),.(first_dex_time = first(drugtime),
#                                                 intime = first(intime),
#                                                 outtime= first(outtime)),by=pid]

bestTemp
bestDexUsers <- bestTemp[,.(first_dex_time=first(drugtime),
                            intime = first(intime),
                            outtime= first(outtime),
                            afib_onset_time = first(afib_onset_time)),by=pid]
bestDexUsers
# bestDexUsers <- bind_rows(dexAfib,dexNonAfib)

# ICU 입실 전 dex 사용: 240명
best_dex_before_icu_id <- bestDexUsers[first_dex_time<intime,unique(pid)];length(best_dex_before_icu_id)
bestDexUsers <- bestDexUsers[!pid %in% best_dex_before_icu_id]
bestCohort <- bestCohort[!pid %in% best_dex_before_icu_id]

# AF 이후 첫 DEX 사용자: 279명 -> 353명
best_dex_after_afib_id <- bestDexUsers[!is.na(afib_onset_time) & first_dex_time >afib_onset_time, unique(pid)];length(best_dex_after_afib_id)
bestDexUsers <- bestDexUsers[!pid %in% best_dex_after_afib_id]
bestCohort <- bestCohort[!pid %in% best_dex_after_afib_id]

# ICU 입실 7일 이후 첫 DEX 사용: 354명 --> 391명
best_dex_after_7days <- bestDexUsers[difftime(first_dex_time,intime,units='days')>2,pid];length(best_dex_after_7days)
bestDexUsers <- bestDexUsers[!pid %in% best_dex_after_7days]
bestCohort <- bestCohort[!pid %in% best_dex_after_7days]

# CICU 제외: n=621
bestCohort <- bestCohort[icuunit!='CICU'] # n=621

# DEX 사용일자가 ICU 입실 일자와 같거나 (ICU 입실 이전에 사용했을 수도),
# AFIB 발생 일자와 같은 경우 (AFIB 발생 이후에 사용했을 수도 있음)
# 999명
# best_dex_same_date <- bestDexUsers[first_dex_time==as.Date(intime) |
#                             first_dex_time == as.Date(afib_onset_time),pid];length(best_dex_same_date)
# bestDexUsers <- bestDexUsers[!pid %in% best_dex_same_date]
# bestCohort <- bestCohort[!pid %in% best_dex_same_date];nrow(bestCohort)


# dex 사용 정의:
# 1. afib 발생자: icu 입실 ~ afib 발생
# 2. afib 미발생: icu 입실 ~ icu 퇴실
bestDexUsers[,dex_usage := ifelse(!is.na(afib_onset_time) &
                                 (first_dex_time > intime&
                                    (first_dex_time < (afib_onset_time))) |
                                 (is.na(afib_onset_time) &
                                    (first_dex_time> (intime) &
                                       (first_dex_time < (outtime)))),1,0)]

mytable(~ dex_usage, bestDexUsers)
# ICU 퇴실 이후에 dex 사용한 사람: 81명은 bestCohort에 포함(ICU 기간 중 사용한 것이 아니기 때문)
# n=2

# bestCohort[,dex_usage := as.factor(ifelse(pid %in% bestDexUsers[dex_usage==1,pid], 1,0))]
# bestCohort[,dex_usage := as.factor(ifelse(pid %in% bestDexUsers$pid, 1,0))]
bestCohort

# vitalsign ---------------------------------------------------------------

bestBp <- bestBp[pid %in% bestCohort$pid]
bestBpFirst <- bestBp[,.(value=first(value)), by=.(pid,category)]
bestBpWide <- dcast(bestBpFirst, pid ~ category, value.var = 'value')

bestBpWide <- charToNum(bestBpWide)
bestSpo2 <- bestSpo2[pid %in% bestCohort$pid]
bestSpo2First <- bestSpo2[,.(value=first(value)), by=.(pid,category)]
bestSpo2Wide <- dcast(bestSpo2First, pid ~ category, value.var = 'value')
bestSpo2Wide <- charToNum(bestSpo2Wide)

# Outcome 변수 조정 --------------------------------------------------------------------
outcome_id <- bestOutcome[,.N,by=pid][N>1,pid]

heart_surgery_keys <- c('valve','valvu','heart',
                        'valvotomy','valvular',
                        'anomalies',
                        'coronary artery',
                        'cardio','pericardium',
                        'periarterial',
                        'septal','atrial','cardiac',
                        'coronary','pump',
                        'aortic valve', 'mitral valve',
                        'vessel','varicose','pulmonary')
bestOutcome[,heart_surgery:= as.factor(ifelse(
  grepl(paste(heart_surgery_keys, collapse = '|'), tolower(bestOutcome$surgery2))|
    grepl(paste(heart_surgery_keys, collapse = '|'), tolower(bestOutcome$surgery2)),1,0))]
bestOutcome[,surgery_before_icu := as.factor(ifelse(surgery!="" | surgery2 !="",1,0))]
bestOutcome[,inhos_mortality:= as.factor(ifelse(inhos_mortality=='Y',1,0))]
bestOutcome[,inhos_duration:= as.numeric(difftime(lubridate::ymd_hm(dod),intime, units='days'))]
# CCI ---------------------------------------------------------------------

bestDiag[,`:=`(
  # Haematologic malignancy
  hematologic_malignancy = ifelse(icd %like% "C81|C82|C83|C84|C85|C86|C87|C88|C89|C90|C91|C92|C93|C94|C95|C96",1,0),
  
  # Myocardial Infarction
  cci_1 = ifelse(grepl(paste(c('I21','I22','I252'),collapse = '|'), icd),1,0),
  
  # Congestive Heart Failure
  cci_2 = ifelse(grepl(paste(c('I43','I50','I099','I110','I130','I132','I255','I420',
                               'I425','I426','I427','I428','I429','P290'),collapse = '|'), icd),1,0),
  
  # Periphral Vascular Disease
  cci_3 = ifelse(grepl(paste(c('I70','I71','I731','I738','I739','I771','I790','I792',
                               'K551','K558','K559','Z958','Z959'),collapse = '|'), icd),1,0),
  
  # Cerebrovascular Disease
  cci_4 = ifelse(grepl(paste(c('G45','G46','I60','I61','I62','I63','I64','I65',
                               'I66','I67','I68','I69'),collapse = '|'), icd),1,0),
  
  # Dementia
  cci_5 = ifelse(grepl(paste(c('F00','F01','F02','F03','G30','F051','G311'),collapse='|'),icd),1,0),
  
  # Chronic Pulmonary Disease
  cci_6 = ifelse(grepl(paste(c('J40','J41','J42','J43','J44','J45','J46','J47','J60','J61','J62','J63',
                               'J64','J65','J66','J67','J684','J701','J703','i278','i279'),collapse='|'),icd),1,0),
  
  # Connective Tissue Disease-Rheumatic Disease
  cci_7 = ifelse(grepl(paste(c('M05','M06','M08','M09','M30','M31','M32','M33','M34','M35','M36'),collapse='|'),icd),1,0),
  
  # Peptic Ulcer Disease
  cci_8 = ifelse(grepl(paste(c('K25','K26','K27','K28'),collapse='|'),icd),1,0),
  
  # Mild Liver Disease
  cci_9 = ifelse(grepl(paste(c('B18','K73','K74','K71','K700','K701','K702',
                               'K703','k713','k714','K715','K717', 'K709','K760',
                               'K760','K762','K763','K764','K768','K769','Z944'),collapse='|'),icd),1,0),
                 
  # Diabetes without complications
  cci_10 = ifelse(grepl(paste(c('E100','E101','E109','E110','E111', 'E116','E118', 
                                'E119','E120','E121','E126','E128','E129','E130',
                                'E131','E136','E138','E139','E140','E141','E146',
                                'E148','E149'),collapse = '|'),icd),1,0),
  
  # Diabetes with complications
  cci_11 = ifelse(grepl(paste(c('E102','E103','E104','E105','E107','E112','E113','E114','E115','E117',
                               'E122','E123','E124','E125','E127','E132','E133','E134','E135','E137',
                               'E142','E143','E144','E145','E147'),collapse = '|'),icd),1,0),
  
  # Paraplegia and Hemiplegia
  cci_12 = ifelse(grepl(paste(c('G81','G82','G041','G114','G801','G802','G830',
                                'G831','G833','G834','G839'),collapse = '|'),icd),1,0),
  
  # Renal Disease
  cci_13 = ifelse(grepl(paste(c('N11','I12','N07','N11','N14','N17','N18','N19',
                                'I13','N00','N01','N02','N03','N04','N05','Q61'),collapse = '|'),icd),1,0),
  
  # Cancer
  cci_14 = ifelse(grepl(paste(c('C00','C01','C02','C03','C04','C05','C06','C07','C08','C09','C10','C11',
                                'C12','C13','C14','C15','C16','C17','C18','C19','C20','C21','C22','C23',
                                'C24','C25','C26','C30','C31','C32','C33','C34','C37','C38','C39','C40',
                                'C41','C43','C45','C46','C47','C48','C49','C50','C51','C52','C53','C54',
                                'C55','C56','C57','C58','C60','C61','C62','C63','C64','C65','C66','C67',
                                'C68','C69','C70','C71','C72','C73','C74','C75','C81','C82','C83',
                                'C84','C85','C88','C90','C91','C92','C93','C94','C95','C96'),collapse = '|'),icd),1,0),
  
  # Moderate or Severe Liver Disease
  cci_15 = ifelse(grepl(paste(c('K72','I85','K704','k711','K721','K729','K765', 
                                'K766','K767', 'I850','I859','I864','I982'),collapse = '|'),icd),1,0),
  
  # Metastatic Carcinoma
  cci_16 = ifelse(grepl(paste(c('C77','C78','C79','C80'),collapse = '|'),icd),1,0),
  
  # AIDS/HIV
  cci_17 = ifelse(grepl(paste(c('B20', 'B21','B22','B24'),collapse = '|'),icd),1,0)
)]
bestDiag[,cci := cci_1 + cci_2 + cci_3 + 
           cci_4 + cci_5 + cci_6 +
           cci_7 + cci_8+
           pmax(cci_9, 3*cci_10)+
           pmax(2*cci_11, cci_12)+
           pmax(2*cci_13, 6*cci_14) +
           2*cci_15 + 2*cci_16+ 6*cci_17]

# bestDiag[,cci:= rowSums(.SD), .SDcols = names(bestDiag) %like% 'cci']
# bestDiag[,grep('cci',names(bestDiag),value=T), with=F]




# VIS:ICU 입실 후 24시간 이내 복용 ---------------------------------------------------------------------

bestDrugs <- fread('bestcare/bestcare_drugs.csv')
bestDrugs
# 날짜 보정
bestDrugs <- toDate(bestDrugs)

# 입실 후 24시간 이내 복용으로 한정
bestDrugs <- bestDrugs[difftime(inputtime, intime, units='hours')<24]

# remifentanil, midazolam 등 VIS에 필요하지 않은 약물 제외
bestDrugs <- bestDrugs[grepl(paste(c('epinephrine','norepinephrine','dopamine','dobutamin','vasopressin'),collapse = '|'),tolower(drugname))]

bestDrugs[,drugname_new :=ifelse(tolower(drugname) %like% 'norepi','norepinephrine',
                                 ifelse(tolower(drugname) %like% 'epinephrine','epinephrine',
                                        ifelse(tolower(drugname) %like% 'dobutamine','dobutamine',
                                               ifelse(tolower(drugname) %like% 'dopamine','dopamine',
                                                      ifelse(tolower(drugname) %like% 'vasopre', 'vasopressin','etc')))))]

pattern <- paste(c('([0-9]{1})(.[0-9]+)[a-zA-Z]*/[a-zA-Z]*/[a-z]*', #0.00mcg/kg/min
                   '([0-9]{1})(.[0-9]+)[a-zA-Z]*/[a-zA-Z]*', # 0.00mcg/kg
                   '[0-9]+[a-zA-Z]+/[a-zA-Z]*/[a-zA-Z]*', # 12mcg/kg/min
                   '[0-9]+[a-zA-Z]+/[a-zA-Z]*', # 12mcg/kg
                   '[0-9]+[a-zA-Z]+'),collapse = '|')
bestDrugs[,value_new:=str_extract(string = value, pattern = pattern)]
bestDrugs <- bestDrugs[str_count(value)>1]

# 수치와 단위 분리
number_pattern <- paste(c('([0-9]{1})(.[0-9]+)', #실수 0.00
                          '[0-9]+'), # 정수
                        collapse = "|")
unit_pattern <- paste(c('[a-zA-Z]*/[a-zA-Z]*/[a-zA-Z]*', #mcg/kg/min 
                        '[a-zA-Z]*/[a-zA-Z]*' # mcg/kg or mcg/min
                        ),collapse = "|")

bestDrugs[,rate:= as.numeric(str_extract(value_new, number_pattern))]
bestDrugs[,unit := str_extract(value_new, unit_pattern)]

bestDrugs <- bestDrugs[!is.na(rate) & !is.na(unit)]
table(bestDrugs$unit)
bestDrugs <- bestDrugs[unit !='amp' & unit != 'bag' & unit  !='cc' &
            unit !='kg' & unit !='cc/hr' &
            unit !='mcg/cc' & unit !='mg/cc' & 
            unit !='u/cc' & unit !='cc/hr' & unit!='cc/h' &
            unit !='a' & unit !='c/hr' & unit !='ccc']

table(bestDrugs$unit)
# target of unit change
target <- paste(c('cg/kg/min','cmg/kg/min','cm/min','mc/kg/min','mcag/kg/min',
                  'mccg/kg/min','mcg/gk/min','mcg/jg/min','mcg/k/gmin','mcg/k/min',
                  'mcg/kg/','mcg/k/gmin','mcg/k/min','mcg/kg/mim','mcg/kg/mine','mcg/kg/ming',
                  'mcg/kg/minj','mcg/kg/minmin','mcg/kg/minr','mcg/kg/mion','mcg/kg/mn',
                  'mcg/kg/mnin','mcg/kgm','mcg/kgm/in','mcg/kgmin','mcg/kmin','mcq/kg/min','mcgkg/min',
                  'mcv/kg/min','mncg/kg/min','ncg/kg/min','mgc/kg/min','mcvg/kg/min','/kg/min'),collapse = '|')
target2 <- paste(c('cmg/min','cg/min','cmq/min','mc/gmin','mc/min','mc/qmin',
                  'mcg//min','mch/min','mcgm/min','mcg/mmin','mcg/mibn','mcg/miin','mcg/mim',
                  'mcg/mimn','mcg/minm','mcg/miun','mcg/mn','mcg/ming','mcgc/min','mcgh/min',
                  'mcgm/min','mch/min','mcq/min','mncg/ming','mgc/min','mcgc/min','mcvg/min',
                  'mc/gm','mgc/min','mic/min','mcg/m','mcg/mi'), collapse = '|')

target3 <- paste(c('iu/min','u/min','ui/min','iu/minr','iu/mini','iu/in','ui/min',
                   'unt/min','Unit/min','U/min'),collapse='|')
target4 <- paste(c('iu/hr','u/hr','unit/hr'), collapse='|')


bestDrugs[,unit:=ifelse(
  grepl(target, unit), 'mcg/kg/min',ifelse(
    grepl(target2, unit),'mcg/min', ifelse(
      grepl(target3,unit),'unit/min',ifelse(
        grepl(target4,unit),'unit/hour',unit
      )
    )
  )
)]
bestDrugs[,`:=`(
  unit = ifelse((drugname_new=='dobutamine'|drugname_new=='dopamine') &
                  unit=='mcg/', 'mcg/kg/min',
                ifelse((drugname_new=='norepinephrine' | drugname_new=='epinephrine') & unit=='mcg/' & rate>1, 'mcg/min',
                        ifelse((drugname_new=='norepinephrine' | drugname_new=='epinephrine') & unit=='mcg/' & rate<1, 'mcg/kg/min',unit))))]

bestDrugs <- bestDrugs[unit!='mg/' & unit!='mg/cc' & 
                         unit !='mc/cc' & unit!='mcg/cc' &
                         unit !='iu/cc' & unit !='mcg/mcc']
fwrite(bestDrugs,'bestcare/drugtemp.csv')

bestDrugs2 <- fread('bestcare/drugtemp_revise.csv')
bestDrugs2[,pid:=as.numeric(pid)]
bestDf[,pid:=as.numeric(pid)]
bestDrugs2 <- inner_join(bestDrugs2, bestDf[,.(pid,weight)],by='pid')

bestDrugs2[,.N,.(drugname_new,unit)][order(drugname_new)]
bestDrugs2[,rate := ifelse(unit=='mcg/min' | unit=='unit/min', rate/weight,
                       ifelse(unit=='mg/min', rate/weight/1000,
                          ifelse(unit=='mg/hour' | unit=='mg/hr', rate/weight/1000/60,
                             ifelse(unit=='mcg/kg/hour' | unit=='unit/kg/hour', rate/60,
                                ifelse(unit=='unit/hour' | unit=='unit/hr' | unit=='mcg/hour',rate/60/weight,rate)))))]
bestDrugs2[,unit := ifelse(drugname_new=='vasopressin', 'unit/kg/min', 'mcg/kg/min')]
bestDrugs2[,rate:=ifelse((drugname_new=='norepinephrine' |
                           drugname_new == 'epinephrine' ) & rate>=1, 0.01*rate,rate)]

bestDrugs3 <- dcast(bestDrugs2, pid + inputtime ~ drugname_new, value.var='rate',
      function(x)x[1],fill=0)
bestDrugs3[,c('date','time'):=tstrsplit(inputtime," ")]
bestDrugs3[,time2:=substr(time,1,2)]
target <- c('vasopressin','dobutamine','dopamine',
            'epinephrine','norepinephrine')
bestDrugs3 <- bestDrugs3[,lapply(.SD, sum),.SDcol=target,by=.(pid,date,time2)]

bestDrugs3[,vis := dopamine + dobutamine + 100*epinephrine +
             100 * norepinephrine + 10000*vasopressin]
bestDrugs3 <- bestDrugs3[,.(vis = max(vis)),.(pid,date)]
bestDrugs3[,summary(vis)]
bestMatch[,summary(vis)]
bestDrugs3[vis>200]
bestDrugs3[,day := rowid(pid)]
bestDrugs3[,.(mean(vis)),by=.(pid)]
bestDrugs3[,dex_usage := ifelse(pid %in% bestMatch[dex_usage==1,pid],1,0)]
bestDrugs3[pid %in% bestMatch$pid, mean(vis),by=.(dex_usage, day)][order(dex_usage,day)]

with(bestDrugs3[pid %in% bestMatch$pid])

bestDrugs3[pid %in% bestMatch$pid, mean(vis) ,by=.(dex_usage,pid)]
# surgery_list ------------------------------------------------------------
fread('bestcare/bestcare_surgery_list.csv')
surgery <- fread('bestcare/bestcare_surgery_list.csv', encoding="UTF-8")
surgery <- surgery[surgery_date>=intime &
                     surgery_date <=outtime]
surgery <- surgery[difftime(intime,'2013-04-01')>=0]

# post-operative icu admission:  n= 1307
surgery_id <- surgery[!in_ward %like% '중환자실' & 
          surgery_after_unit %like% '중환자실',unique(pid)]

# post-operative icu admission & heart_surgery : n= 284
heart_surgery_id <- surgery[!in_ward %like% '중환자실' & 
          surgery_after_unit %like% '중환자실' &
          surgery_depart %like% '심장혈관', unique(pid)]

heart_surgery_id
# apache ------------------------------------------------------------------
apache <- fread('bestcare/bestcare_apache.csv')
apache <- apache[,.(apache = max(apache)),by=pid]
apache <- apache[pid %in% bestDf$pid]
length(unique(apache$pid))
apache
# SAPS2 계산 -------------------------------------------------
# bestSaps <- fread('bestcare/bestcare_saps.csv', encoding = 'UTF-8')
bestSaps <- setDT(readxl::read_xlsx('bestcare/snubh_saps2.xlsx'))
bestSaps <- bestSaps[pid %in% bestDf$pid]
bestSaps[,.SD, .SDcol=is.character] %>% names()
target <- c('age','BT_adm_max','BT_adm_min','UO_adm',
            'FIO2_PO2_max','FIO2_PO2_min','pid')
bestSaps[,(target):=lapply(.SD, as.numeric), .SDcol=target]

bestSaps[,saps_age := ifelse(age<40,0,
                             ifelse(age<60,7,
                                    ifelse(age<70,12,
                                           ifelse(age<75,15,
                                                  ifelse(age<80,16,18)))))]
bestSaps[,saps_hr := ifelse(PR_adm_min<40,11,
                            ifelse(PR_adm_min<70,2,
                                   ifelse(PR_adm_max>=120,4,
                                          ifelse(PR_adm_max>=160,7,0))))]

bestSaps[,saps_sbp := ifelse(SBP_adm_min<70 | SBP_adm_max<70,13,
                             ifelse(SBP_adm_min<100 | SBP_adm_max<100,5,
                                    ifelse(SBP_adm_max>=200,2,0)))]
bestSaps[,saps_pf := ifelse(is.na(FIO2_PO2_max),0,
                            ifelse(FIO2_PO2_max<13.3 | FIO2_PO2_min<13.3,11,
                                   ifelse(FIO2_PO2_max>=26.6 | FIO2_PO2_min>=26.6,6,9)))]
bestSaps[,saps_bt := ifelse(BT_adm_min>=39 | BT_adm_max>=39,3,0)]
bestSaps[,saps_urine := ifelse(UO_adm<0.5,11,
                               ifelse(UO_adm<0.999,4,0))]
bestSaps[,saps_serum_urea:= ifelse(BUN_MAX<10 | BUN_MIN<10,0,
                                   ifelse(BUN_MAX<30|BUN_MIN<30,6,10))]
bestSaps[,saps_WBC := ifelse(WBC_MIN<1,12,
                             ifelse(WBC_MAX<20 | WBC_MIN<20,1, 3))]
bestSaps[,saps_K := ifelse(K_MAX<3 | K_MIN<3,3,
                           ifelse(K_MAX<5 | K_MIN<5,0,3))]

bestSaps[,saps_na := ifelse(NA_MAX<125 | NA_MIN<125,5,
                            ifelse(NA_MAX>=145 | NA_MIN>=145,1,0))]
bestSaps[,saps_hco := ifelse(HCO3_MAX<15 | HCO3_MIN<15,6,
                             ifelse(HCO3_MAX<20 | HCO3_MIN<20,3,0))]

bestSaps[,saps_bilirubin := ifelse(BILIRUBIN_MAX<68.4 | BILIRUBIN_MIN<68.4,0,
                                   ifelse(BILIRUBIN_MAX>=102.6 | BILIRUBIN_MIN>=102.6,9,4))]
setnames(bestSaps, c('M(Motor)', 'V(verbal)','E(eye)'),c('M','V','E'))
bestSaps[,E := as.numeric(ifelse(E=='1(sed)',1,E))]
bestSaps[,M := as.numeric(M)]
bestSaps[,V := as.numeric(ifelse(V %in% c('VE','VT','E'),round(5 * (E+M)/10,0),V))]
bestSaps[,gcs := rowSums(.SD), .SDcol=c('E','V','M')]
bestSaps[,saps_gcs := ifelse(gcs<6,26,
                             ifelse(gcs<9,13,
                                    ifelse(gcs<11,7,
                                           ifelse(gcs<14,5,0))))]
bestSaps[,saps_adm := ifelse(is.na(OP_TYPE),6,
                             ifelse(OP_TYPE==2,8,0))]

bestSaps <- bestSaps[,.SD, .SDcol=c('pid',grep('saps_',names(bestSaps), value = T))]

## Chronic disease


# 변수 추가 및 merge -------------------------------------------------------------------
remotes::install_github("Rdatatable/data.table")
snubh_cohort <- function(afib_day){
  bestCohort <- fread('bestcare/bestcare_cohort.csv', encoding="UTF-8")
  bestOutcome <- fread('bestcare/bestcare_outcome.csv', encoding="UTF-8")
  # bestDex2 <- fread('bestcare/bestcare_drugs_new.csv')
  
  bestCohort[,name := NULL]
  bestCohort <- toDate(bestCohort)
  bestOutcome <- toDate(bestOutcome)
  bestCohort[, afib_within_7days := as.factor(ifelse(
    is.na(afib_onset_time),0, ifelse(
      as.numeric(difftime(afib_onset_time, intime,units='days')) < afib_day,1,0
    )
  ))]
  
  heart_surgery_keys <- c('valve','valvu','heart',
                          'valvotomy','valvular',
                          'anomalies',
                          'coronary artery',
                          'cardio','pericardium',
                          'periarterial',
                          'septal','atrial','cardiac',
                          'coronary','pump',
                          'aortic valve', 'mitral valve',
                          'vessel','varicose','pulmonary')
  bestOutcome[,heart_surgery:= as.factor(ifelse(
    grepl(paste(heart_surgery_keys, collapse = '|'), tolower(bestOutcome$surgery2))|
      grepl(paste(heart_surgery_keys, collapse = '|'), tolower(bestOutcome$surgery2)),1,0))]
  bestOutcome[,surgery_before_icu := as.factor(ifelse(surgery!="" | surgery2 !="",1,0))]
  bestOutcome[,inhos_mortality:= as.factor(ifelse(inhos_mortality=='Y',1,0))]
  bestOutcome[,inhos_duration:= as.numeric(difftime(lubridate::ymd_hm(dod),intime, units='days'))]
  
  
  # ICU stay
  icustay_except_id <- bestCohort[icu_los < 2 | icu_los > 28, unique(pid)];print(length(icustay_except_id))
  bestCohort <- bestCohort[!pid %in% icustay_except_id]
  # ICU 입실 전 afib
  bestICU <- bestICU[pid %in% bestCohort$pid];nrow(bestICU)
  target <- paste(c('atrial','fibrillation'),collapse='|')
  
  # 입실 전 afib 발생 환자: 97명
  afib_before_icu_id <- bestICU[grepl(target, diag_before_icu),pid]; print(length(afib_before_icu_id))
  bestCohort <- bestCohort[!pid %in% afib_before_icu_id]
  
  bestDf <- copy(bestCohort[bestBpWide, on='pid', `:=`(DBP=i.DBP,
                                                       SBP=i.SBP,
                                                       PR=i.PR,
                                                       RR=i.RR)])
  bestDf[bestSpo2Wide, on='pid', `:=`(BT=i.BT, 
                                      SpO2=i.SpO2)]
  bestDf[,year := as.factor(substr(intime,1,4))][]
  
  bestDf[,unit := as.factor(ifelse(icuunit=='MICU','MICU',
                                   ifelse(icuunit=='SICU','SICU',
                                          ifelse(icuunit=='NCU','NCU','OTHERS'))))]
  bestDf[,age65 := as.factor(ifelse(age>=65,1,0))]
  bestDf[,dex_usage := as.factor(ifelse(pid %in% bestDexUsers[dex_usage==1,pid],1,0))]
  bestDf[,crrt:= as.factor(fifelse(crrt=='Y','Yes','No'))]
  bestDf[,obs_duration := as.numeric(ifelse(
    afib_within_7days==1, as.numeric(difftime(afib_onset_time, intime, units='hour')), as.numeric(difftime(outtime, intime, units='hours'))
  ))]
  bestDf[,icu_los:= as.numeric(difftime(outtime, intime, units='days'))]
  
  bestDf[,heart_surgery := as.factor(ifelse(pid %in% heart_surgery_id, 1,0))]
  # bestDf[,surgery := as.factor(ifelse(pid %in% surgery_id,1,0))]
  
  bestHeight[,height := as.numeric(ifelse(grepl(height_na, height),NA,height))]
  bestDf[bestHeight, on=.(pid),height2:=i.height]
  bestDf[,height := pmax(height, height2, na.rm=T)]
  
  bestDf[,bmi := weight/(height/100)^2]
  bestDf[,t2e := ifelse(is.na(afib_onset_time),0, as.numeric(difftime(afib_onset_time, intime,unit='days')))]
  bestDf[,map := (2*DBP + SBP)/3]
  bestDf[,RR:=as.numeric(RR)]
  bestDf[,PR:=as.numeric(PR)]
  bestDf[,SpO2 := as.numeric(SpO2)]
  
  # outcome merge
  bestDf[bestOutcome, on=.(id,pid), `:=`(dod=i.dod,
                                         inhos_mortality=i.inhos_mortality,
                                         inhos_duration=i.inhos_duration)][]
  bestDf[bestDiag, on=.(id,pid), `:=`(cci=i.cci,
                                      meta_cancer=i.cci_16,
                                      AIDS = i.cci_17,
                                      haematologic_malignancy = i.hematologic_malignancy)]
  bestDf[,inhos_duration := ifelse(inhos_mortality==0,
                                   as.numeric(difftime(outtime,intime,units='days')),inhos_duration)]
  
  bestDf[apache, on=.(pid), apache2:=i.apache]
  bestDf[,apache_score := pmax(apache,apache2, na.rm=T),by=pid]
  bestDf[bestDrugs3, on=.(pid),vis:=i.vis]
  bestDf[,vis:= ifelse(is.na(vis),0,vis)]
  
  bestDf[,vis_quantile:=ntile(vis,4)]
  bestDf[vis_quantile==4,mean(vis,na.rm=T)]
  
  require(readxl)
  mv$pid <- as.numeric(mv$pid)
  mv <- mv[,.(first = first(order_date),
              last=last(order_date)), by=pid]
  temp <- mv[bestDf,on='pid'][first>=intime] # 3654명 중 1701명
  
  bestDf[,ventilator := ifelse(pid %in% temp$pid,1,0) %>% as.factor()]
  bestDf <- bestDf[bestSaps, on=.(pid)]
  bestDf[,saps_chronic_disease := ifelse(meta_cancer==1,9,
                                         ifelse(haematologic_malignancy==1,10,
                                                ifelse(AIDS==1,17,0)))]
  bestDf <- bestDf[as.Date(intime)>='2013-04-01']
  return(bestDf)
}
bestDf
require(data.table)
bestDf <- snubh_cohort(afib_day=7)
rbind(coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage, temp[pid %in% bestMatch$pid]) %>% 
        extractHR(),
      coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage, temp[pid %in% bestMatch$pid]) %>% 
        extractHR())


mytable(dex_usage ~ inhos_mortality, bestDf)
mytable(dex_usage ~ inhos_duration, bestDf)
mytable(dex_usage ~ afib_within_7days, bestDf)


#VIS Q4

# bestDf[inhos_mortality==1,.(pid,intime,dod,outtime)] %>% View()

# require(ggthemes)
# bestDf[inhos_mortality==1] %>% ggplot(aes(x=dex_usage,y=inhos_duration, color=dex_usage)) + 
#   geom_boxplot(width=.5)+
#   ylim(c(0,20)) +
#   theme_stata()


require(Hmisc)
require(stringr)
describe(bestHeight)

mytable(dex_usage ~ afib_within_7days, bestDf)

## ventilation check


# matching ----------------------------------------------------------------

bestDf[,bmi:=ifelse(bmi>1.5*quantile(bmi,na.rm=T,probs=0.75),NA,bmi)]
bestDf[,.N, dex_usage]
bestTemp <- bestDf %>% 
  select(-c(pid,id,name,weight, height,t2e,
                                 intime, outtime, afib_within_24hours,
                                 afib_onset_time,height2,dod, year,
                                 apache, apache2,SBP,DBP,
                                 meta_cancer, AIDS, haematologic_malignancy))

bestTemp[,lapply(.SD, \(x) sum(is.na(x))/nrow(afib_dex)*100)]
target <- names(bestTemp[,sapply(bestTemp,is.character),with=F])
bestTemp[,(target):=lapply(.SD, as.factor), .SDcols=target]
require(mice)
bestMice <- mice(bestTemp, seed=2022)
bestComp <- setDT(complete(bestMice))
bestComp
bestComp$pid <- bestDf$pid
bestComp[,saps_bilirubin:=0]
bestComp[,saps2 := rowSums(.SD),.SDcol=grepl('saps_',names(bestComp))]

require(MatchIt)
best_m <- matchit(dex_usage ~ sex + age + bmi  + unit + 
                    ventilator + 
                    cci + vis + crrt + heart_surgery + 
                    apache_score + saps2+
                    map + RR + PR + BT + SpO2,
                  data=bestComp,
                  ratio = 2,
                  caliper=.1)
mytable(~age, bestDf,digits=2)
plot(summary(best_m))
plot(summary(best_m2))
bestMatch <- setDT(match.data(best_m))
bestMatch
bestMatch2 <- setDT(match.data(best_m2))
require(tableone)
tab <- CreateTableOne(vars=c('age','sex','bmi','unit',
                             'cci','apache_score', 'saps2',
                             'vis','ventilator', 'crrt','heart_surgery',
                             'map','RR','PR','BT','SpO2'),
                      factorVars=c('sex','unit','heart_surgery',
                                   'crrt','ventilator'),
                      strata='dex_usage',
                      data=bestMatch,
                      addOverall = T,
                      smd = T,
                      includeNA = F)

print(tab,smd=T, catDigits = 1, contDigits = 1,
      pDigits = 3 ) %>% 
  kableone() %>% 
  kableExtra::kable_classic(lightable_options = 'striped')


rbind(coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage, bestMatch) %>% 
        extractHR(),
      coxph(Surv(inhos_duration, inhos_mortality==1)~dex_usage, bestMatch) %>% 
        extractHR()) %>% ztable()
bestMatch2 %>% dim()
bestMatch2[,.N,by=dex_usage]
