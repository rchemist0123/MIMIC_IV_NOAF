# Revision: baseline of DEX after 48 --------------------------------------
# 1103 + 6

temp <- df[stay_id %in% keep_id]
temp[,`:=`(
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

temp[,afib_within_7days := as.factor(ifelse(is.na(afib_onset_time),0,
                                            ifelse(difftime(afib_onset_time,icu_intime,units='days')<=afib_day,1,0)))]

temp[,bmi := weight/(height*0.01)^2]
temp[,obs_duration := round(as.numeric(ifelse(is.na(afib_onset_time),
                                              difftime(icu_outtime,icu_intime,units='days'),
                                              difftime(afib_onset_time, icu_intime, units='days'))),1)]
temp[,inhos_mortality:= as.factor(ifelse(is.na(dod),0,
                                         ifelse(dod <= dischtime, 1,0)))]
temp[,inhos_duration := round(as.numeric(ifelse(inhos_mortality==0,
                                                difftime(dischtime, icu_intime, units='days'),
                                                difftime(dod, icu_intime, units='days'))),1)]
temp[,obs_duration:=ifelse(inhos_mortality==1 & inhos_duration < obs_duration, 
                           inhos_duration, obs_duration)]
temp[,ethnicity := ifelse(ethnicity == 'ASIAN','Asian',
                          ifelse(ethnicity %like% 'BLACK','African-American',
                                 ifelse(ethnicity =='WHITE','Caucasian',
                                        ifelse(ethnicity %like% 'HISPANIC','Hispanic','Other'))))]

temp[,icuunit:=
       fifelse(first_careunit %like% 'SICU', 'SICU',
               fifelse(first_careunit %like% 'CVICU','CVICU',
                       fifelse(first_careunit %like% 'CCU', 'CCU',
                               fifelse(first_careunit %like% 'MICU', 'MICU','ETC'))))]
setnames(temp, c('dexmedetomidine','gender'),
         c('dex_usage','sex'))
temp[,t2e := as.numeric(ifelse(is.na(afib_onset_time),0, ifelse(afib_within_7days==1,difftime(afib_onset_time, icu_intime, units='days'),0)))]

target <- c('ph_min','ph_max','po2_min','po2_max','lactate_min','lactate_max',
            'pco2_min','pco2_max')

temp[,(target):=lapply(.SD, \(x) ifelse(x>1000,NA,x)), .SDcol=target]
temp |> names()

## table 1
tbl_summary (
  include=c(age,sex, bmi, ethnicity,heart_surgery,
            icuunit, cci, vis,ventilation, crrt, 
            sepsis, sofa_score,sapsii,
            map_min,heart_rate_max, resp_rate_max, temperature_max, spo2_min,
            ph_min, po2_min, pco2_max,
            wbc_max, hemoglobin_min, platelets_min, creatinine_max, bun_max, lactate_max,
            sodium_max,chloride_max, potassium_max, calcium_min, bicarbonate_min, magnesium_min),
  statistic = list(all_categorical() ~ "{n} ({p})",
                   all_continuous() ~ "{mean} ± {sd}"),
  type = list(c('sex') ~ "dichotomous"),
  value= list('sex'~'M'),
  digits = list(all_categorical() ~c(0,2),
                all_continuous()~ 2),
  data=temp,
  missing='no'
)


## table 2
tbl_summary(
  data=temp,
  include=c(afib_within_7days, 
            los_icu,los_hospital,
            inhos_mortality,
            brady, hypo),
  # brady_per_duration,
  # hypo_per_duration),
  statistic = list(all_continuous() ~ "{mean} ± {sd}",
                   all_categorical() ~ "{n} ({p})",
                   c(los_icu, los_hospital)~ "{median} [{p25}–{p75}]"),
  digits = list(all_continuous() ~ 2,
                all_categorical() ~ c(0,2)),
  type = list(all_categorical() ~ "dichotomous"),
  label = list(afib_within_7days ~ "Atrial Fibrillation, %",
               los_icu ~ "ICU LOS, days",
               los_hospital ~ "Hospital LOS, days",
               inhos_mortality ~ "In-hospital mortality, %"
  ),
  missing='no',
)


temp[afib_within_7days==1] %>% 
  tbl_summary(include = c(t2e, rate_control, rhythm_control,
                          cardioversion),
              statistic = list(all_categorical()~"{n} ({p})",
                               t2e ~ "{median} [{p25}-{p75}]"),
              type= all_categorical() ~ "dichotomous",
              digits = list(all_categorical()~ c(0,2),
                            all_continuous() ~ 2),
              missing='no')

# dex infusion histogram --------------------------------------------------
temp[dex_usage==1,.(icu_intime,dex_starttime)][,.(dex_infusion_gap = difftime(dex_starttime, icu_intime,
                                                                              units="hours") |> as.numeric()
)] |> summary()

temp[dex_usage==1,.(icu_intime,dex_starttime)][,.(dex_infusion_gap = difftime(dex_starttime, icu_intime,
                                   units="hours") |> as.numeric()
)] |> 
  ggplot(aes(x=dex_infusion_gap))+
  geom_histogram() +
  theme_classic() + 
  scale_x_continuous(limits = c(48, 469),
                     breaks=c(48,100,200,300,400)) +
  labs(x="Hours")

## additional univariable all cox regression -----------------------

target <- df_match |> select(dex_usage,age,sex, bmi, ethnicity,heart_surgery,
                             icuunit, cci, vis,ventilation, crrt, 
                             sepsis, sofa_score,sapsii,
                             map_min,heart_rate_max, resp_rate_max, temperature_max, spo2_min,
                             ph_min, po2_min, pco2_max,
                             wbc_max, hemoglobin_min, platelets_min, creatinine_max, bun_max, lactate_max,
                             sodium_max,chloride_max, potassium_max, calcium_min, bicarbonate_min, magnesium_min) |> names()

cox_univ_list <- list()
for(i in target){
  # form <- paste0('Surv(obs_duration, afib_within_7days==1)~',i)
  form <- paste0('Surv(inhos_duration, inhos_mortality==1)~',i)
  fit <- coxph(as.formula(form),data=df_comp)
  cox_univ_list[[i]] <- extractHR(fit)
}
tbl <- do.call(rbind,cox_univ_list) |> 
  data.table(keep.rownames = T) 

tbl[,`HR (95% CI)` := paste0(
  format(HR,2),' (', format(lcl,2),'-',format(ucl,2),')'
)][,p := ifelse(as.character(p)=="0.000",'<.001',p)][,c(1,6,5)] |> gt()


# multivariable
form <- paste0("Surv(obs_duration, afib_within_7days==1)~",
               paste0(target,collapse = "+"))
form <- paste0("Surv(inhos_duration, inhos_mortality==1)~",
               paste0(target,collapse = "+"))
fit <- coxph(as.formula(form), data=df_comp)
tbl <- data.table(extractHR(fit), keep.rownames = T)
tbl[,`HR (95% CI)` := paste0(
  format(HR,2),' (', format(lcl,2),'-',format(ucl,2),')'
)][,p := ifelse(as.character(p)=="0.000",'<.001',p)][,c(1,6,5)] |> gt()
# 2nd PSM -----------------------------------------------------------------

df_match[,age65 := ifelse(age>=65,1,0)]
df_match[,race:= as.factor(ifelse(ethnicity %like% 'African-American', 'African American','etc'))]
df_match[,cvicu := as.factor(ifelse(icuunit %like% 'CVICU','CVICU','Non-CVICU'))]

require(MatchIt)
set.seed(2022)


df_match[,dex_usage := as.fact]
rematching <- function(subgroup){
  setkeyv(df_match, subgroup)
  df_match[,eval(subgroup):= as.factor(get(subgroup))]
  lvl <- levels(df_match[[subgroup]])
  print(lvl)

  vars <- df_match[,.(sex, age, bmi, ethnicity, cci, heart_surgery ,
              vis, ventilation, crrt, sepsis, sofa_score, cvicu,
              sapsii ,
              map_min, heart_rate_max, resp_rate_max, temperature_max,
              spo2_min ,
              ph_min, po2_min, pco2_max, wbc_max, hemoglobin_min, platelets_min,
              creatinine_max, bun_max, lactate_max, sodium_max, chloride_max, potassium_max,
              calcium_min, bicarbonate_min, magnesium_min)] |> names()
  if(subgroup=='race'){
    forms <- paste0('dex_usage~', paste0(setdiff(vars,'ethnicity'),collapse = "+"))
  } else {
    forms <- paste0('dex_usage~', paste0(setdiff(vars,subgroup),collapse = "+"))
  }
  m1 <- matchit(as.formula(forms), 
                df_match[df_match[[subgroup]]==lvl[1]],
                ratio = 1, caliper = 0.2)
  m2 <-  matchit(as.formula(forms), 
                 df_match[df_match[[subgroup]]==lvl[2]],
                 ratio = 1, caliper = 0.2)
  
  second_match1 <- setDT(match.data(m1))
  second_match2 <- setDT(match.data(m2))
  second_match <- rbindlist(list(second_match1, second_match2))
  second_match[,dex_usage := factor(dex_usage, levels=c('0','1'))]
  sub_fit <- coxph(Surv(obs_duration, afib_within_7days==1) ~ 
                     dex_usage, data=second_match)
  print(summary(sub_fit))
  require(Publish)
  subgroupAnalTable(fit=sub_fit, 
                    trt='dex_usage', 
                    subgroup = subgroup, data=second_match)
}
sub_fit <- coxph(Surv(obs_duration, afib_within_7days==1) ~ 
                   dex_usage, data=second_match)
summary(sub_fit)
second_match$dex_usage <- factor(second_match$dex_usage,levels=c(0,1))
second_match[,.N,by=.(dex_usage, afib_within_7days)]
rematching('sex')
df_match$race
sub_result <- data.table::data.table(
        check.names = FALSE,
           Subgroup = c("Sex","Female","Male",
                        "Age","< 65","≥ 65","Ethnicity",
                        "African-American","Non-African-American","ICU unit","CVICU",
                        "Non-CVICU","Sepsis","No","Yes","Cardiac surgery","No",
                        "Yes","Mechanical ventilation support","No","Yes",
                        "CRRT","No","Yes","Total"),
                DEX = c(NA,"104/707","263/1392",NA,
                        "96/1175","274/927",NA,"10/142","360/1963",NA,
                        "261/828","110/1272",NA,"104/651","266/1453",NA,
                        "340/2025","29/73",NA,"24/300","347/1799",NA,
                        "352/2034","17/70","372/2112"),
          `Non-DEX` = c(NA,"137/707","335/1392",NA,
                        "124/1175","332/927",NA,"24/242","441/1963",NA,
                        "278/828","211/1272",NA,"128/651","322/1453",NA,
                        "450/2025","27/73",NA,"42/300","411/1799",NA,
                        "425/2034","17/70","1359/5907"),
                 HR = c(NA,0.66,0.7,NA,0.7,0.7,
                        NA,0.36,0.73,NA,0.87,0.44,NA,0.76,0.73,NA,0.67,
                        0.9,NA,0.5,0.76,NA,0.74,0.85, 0.70),
              lower = c(NA,0.51,0.6,NA,0.54,0.6,
                        NA,0.17,0.64,NA,0.74,0.35,NA,0.58,0.62,NA,
                        0.58,0.53,NA,0.3,0.66,NA,0.64,0.44, 0.63),
              upper = c(NA,0.86,0.83,NA,0.91,
                        0.82,NA,0.76,0.84,NA,1.03,0.56,NA,0.98,0.86,NA,
                        0.77,1.52,NA,0.83,0.88,NA,0.85,1.67, 0.79),
      `HR.(95%.CI)` = c(NA,"0.66 (0.51-0.86)",
                        "0.70 (0.60-0.83)",NA,"0.70 (0.54-0.91)","0.70 (0.60-0.82)",
                        NA,"0.36 (0.17-0.76)","0.73 (0.64-0.84)",NA,
                        "0.87 (0.74-1.03)","0.44 (0.35-0.56)",NA,"0.76 (0.58-0.98)",
                        "0.73 (0.62-0.86)",NA,"0.67 (0.58-0.77)",
                        "0.90 (0.53-1.52)",NA,"0.50 (0.30-0.83)","0.76 (0.66-0.88)",NA,
                        "0.74 (0.64-0.85)","0.85 (0.44-1.67)", "0.70 (0.63-0.79)"),
  P.for.interaction = c("0.70",NA,NA,"0.99",NA,
                        NA,"0.06",NA,NA,"<0.001",NA,NA,"0.83",NA,NA,
                        "0.28",NA,NA,"0.11",NA,NA,"0.68",NA,NA,NA)
)
subgroups <- c(2,3,5,6,8,9,11,12,14,15,17,18,20,21,23,24)
sub_result$Subgroup[subgroups] <- paste("     ",sub_result$Subgroup[subgroups])

sub_result <- sub_result %>% mutate(lower= ifelse(!is.na(lower),format(round(as.numeric(lower),2),nsmall=2), NA))
sub_result <- sub_result %>% mutate(upper= ifelse(!is.na(upper),format(round(as.numeric(upper),2),nsmall=2), NA))
sub_result <- sub_result %>% mutate(HR= ifelse(!is.na(HR),format(round(as.numeric(HR),2),nsmall=2), NA))
sub_result <- sub_result %>% mutate(P.for.interaction= ifelse(P.for.interaction=="<0.001", '<0.001', 
                                                              ifelse(!is.na(P.for.interaction),format(round(as.numeric(P.for.interaction),2),nsmall=2), NA)))

# sub_result <- sub_result %>% mutate(HR= ifelse(HR=="1 (ref)", '1 (Ref.)',
#                                                ifelse(!is.na(HR),format(round(as.numeric(HR),2),nsmall=2),NA)))
sub_result <- sub_result %>% rename(`HR (95% CI)` = 'HR.(95%.CI)')
sub_result <-sub_result %>% rename('P for interaction'='P.for.interaction')
setnames(sub_result, 'Non-DEX',' Non-DEX')
setnames(sub_result, 'DEX','    DEX')
sub_result
shape <- rep(16, times=25)
sizes <- rep(3.25, times=25)
total <- c(25)
for(i in total){
  shape[i] <- 18
  sizes[i] <- 7
}

require(forester)
forester(left_side_data = sub_result[,1:3],
         right_side_data= sub_result[,c(7,8)],
         estimate = as.numeric(sub_result[['HR']]),
         estimate_col_name = "HR (95% CI)",
         estimate_precision = 2,
         ci_low = as.numeric(sub_result$lower),
         ci_high= as.numeric(sub_result$upper),
         # stripe_colour = "#ffffff",
         # ci_sep="  ", 
         display = T,
         null_line_at = 1,
         xlim=c(0,2),
         dpi=300,
         justify=c(0,0.5,0.5,0.5,0.5), # 정렬
         point_shapes= shape,
         point_sizes = sizes,
         # nudge_x = 0.75,
         # ggplot_width = 30,
         font_family = "sans",
         arrows = T,
         arrow_labels=c('DEX favored', 'Non-DEX favored'),
         render_as = 'png'
)


# PH test -----------------------------------------------------------------
require(survival)
zph <- cox.zph(coxph(Surv(inhos_duration, inhos_mortality==1)~ dex_usage, data=df_match))
zph <- cox.zph(coxph(Surv(inhos_duration, inhos_mortality==1)~ dex_usage, data=bestMatch))
zph
zph <- cox.zph(coxph(Surv(obs_duration, afib_within_7days==1)~ dex_usage, data=df_match))
df_match$afib_within_7days <- factor(df_match$afib_within_7days, levels=c(0,1))
df_match$dex_usage <- factor(df_match$dex_usage, levels=c(0,1))
coxph(Surv(obs_duration, afib_within_7days==1)~ dex_usage, data=df_match)
zph <- cox.zph(coxph(Surv(obs_duration, afib_within_7days==1)~ dex_usage, data=df_match[obs_duration>0]))

df_match |> names()
df_match[,.(obs_duration, log(obs_duration))] |> View()
survminer::ggcoxzph(zph)

install.packages("flexsurv")
require(flexsurv)
fs1 <- flexsurvreg(Surv(obs_duration, afib_within_7days==1) ~ dex_usage, data = df_match[obs_duration>0],
                   dist = "weibull")
fs2 <- flexsurvreg(Surv(obs_duration, afib_within_7days==1) ~ dex_usage, data = bestMatch[obs_duration>0],
                   dist = "weibull")
df_match$dex_usage
sp1
sp2
plot(fs1, type="hazard")
plot(fs2, type="hazard")
plot(fs2, type="hazard")
df_match_temp <- df_match[obs_duration>0]
sp1 <- flexsurvspline(Surv(obs_duration, afib_within_7days==1) ~ dex_usage, data = df_match_temp,
               scale = "hazard")
sp2 <- flexsurvspline(Surv(obs_duration, afib_within_7days==1) ~ dex_usage, data = bestMatch[obs_duration>0],
                      scale = "hazard")
fs1 <- flexsurvreg(Surv(obs_duration, afib_within_7days==1) ~ dex_usage, 
                   data = df_match_temp,
                   dist = "weibull")
fs1$res.t
fs2$res.t
survminer::ggflexsurvplot(sp1,
                          data = df_match_temp,
                          )
bestMatch$afib_within_7days
cox.zph(fs1)
df_match[,.N,by=obs_duration][order(obs_duration)]

survreg(Surv(obs_duration, afib_within_7days==1)~dex_usage, df_match_temp)
flexsurvreg(Surv(obs_duration, afib_within_7days==1)~dex_usage, 
            data=df_match_temp,
            dist="weibull")
# validation cohort -------------------------------------------------------

df_match[,.N,by=race]
