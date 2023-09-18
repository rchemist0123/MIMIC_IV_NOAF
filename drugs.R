# drugs 다시 체크 -------------------------------------------------------------
drugs2 <- fread('csv/Afib_drugs2.csv')
drugs2

drugs2[,afib_within_7days:=as.factor(fifelse(
  difftime(afib_onset_time,intime, units='days')<=7 & 
    difftime(afib_onset_time,intime, units='days')>=0,1,0
))]


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
# drugs2[,drug_infusion_time := difftime(drug_endtime, drug_starttime,units='hours')]

dex_df[,.(stay_id, afib_within_7days, drug_starttime)]
dex_df[,afib_within_7days:=ifelse(is.na(afib_within_7days),0,
                                  ifelse(afib_within_7days==1,1,0))]
table(dex_df$afib_within_7days)
dex_df[,drug_infusion_time:=difftime(drug_endtime,drug_starttime,units='hours')]

quantile(dex_df$drug_infusion_time)[2]

dex_df[,quantile:=as.factor(fifelse(drug_infusion_time<5.5,'Q1',
                                    fifelse(drug_infusion_time<22.08333333,'Q2',
                                            fifelse(drug_infusion_time<64.17500,'Q3','Q4'))))]


dex_df[,.(.N, m=mean(drug_infusion_time), sd=sd(drug_infusion_time)),by=quantile][order(quantile)]

dex_df[,.(quantile, stay_id)][cohort2, on='stay_id']


drugs2[,.(stay_id,drugname, drug_infusion_time)]

drugs2[afib_within_7days==1]
colnames(drugs2)
drugs2_wide <- dcast(drugs2, stay_id + afib_onset_time + 
                       newonset + intime + afib_within_7days+
                       dex_before_afib ~ drugname)
drugs2_wide
drugs2_wide[,`:=`(
  afib_within_7days=as.numeric(afib_within_7days),
  dex_before_afib = as.numeric(dex_before_afib)
)]
drugs2_wide <- drugs2_wide[,lapply(.SD, sum), by=stay_id]
drugs2_wide %>% View()
drugs2_wide %>% colnames()
drugs2_wide[dex_before_afib>=1]
dex_before_afib_id <- unique(drugs2[dex_before_afib==1]$stay_id)
cohort2[,dex_usage:=as.factor(fifelse(stay_id %in% dex_id,1,0))]


fit <- glm(afib_within_7days ~ dex_usage, cohort2, family='binomial')
summary(fit)
exp(cbind(coef(fit), confint(fit)))
mytable(dex_usage  ~ afib_within_7days, cohort2)
mytable(afib_within_7days ~ dex_usage , cohort2)

rst <- cohort2[,.N, by=c('afib_within_7days','dex_usage')][,pct:= round(N/sum(N)*100,2), by=dex_usage][order(-afib_within_7days)]
rst <- rst[order(-dex_usage)]
rst[,y_label:=cumsum(pct)-0.5 * pct, by=dex_usage]
rst
require(ggplot2)

rst %>% ggplot(aes(x=dex_usage, y=pct, fill=afib_within_7days)) +
  geom_col(width=.5)+
  geom_text(aes(y=y_label, label=paste0(pct,'%','\n','(n=',N,')')))+
  scale_x_discrete(labels=c('No','Yes'))+
  scale_fill_discrete(labels=c('No','Yes'))+
  theme(legend.position = 'top')


# VIS ---------------------------------------------------------------------
vis <- fread('csv/Afib_vis.csv')
vis[,.N,by=c('drugname','rateuom')]
vis[,rate:=fifelse(rateuom=='mg/kg/min',rate*1000, rate)]


vis_summary <- vis[,.(dose=sum(rate)),by=c('stay_id','drugname')]
vis_dose <- dcast(vis_summary, stay_id ~ drugname, value.var='dose',fill=0)
vis_dose[,vis:=dopamine + dobutamine + 100 * epinephrine + 10 * milrinone + 10000*vasopressin/60 + 100*norepinephrine]
vis_dose

cohort_hr <- vis_dose[,.(stay_id,vis)][cohort_hr, on='stay_id']


cohort_hr