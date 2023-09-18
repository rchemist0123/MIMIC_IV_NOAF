require(ggplot2)
require(tidyverse)
require(tidyr)

# Flow chart --------------------------------------------------------------
library(DiagrammeR)

text_labels <- c('Patients within MIMIC-IV database\n(n=76,540)',
                 'Exclusion Criteria\nMultiple ICU admissions (n=23,304)\nICU study ≤ 48 hours (n=24,455)\nNo medication records (n=6,525)',
                 'Patients Included\n(n=21,422)',
                 'DEX\n(n=3,633)',
                 'Non-DEX\n(n=18,624)',
                 'AF\n(n=480)',
                 'Non-AF\n(n=3,153)',
                 'AF\n(n=6,203)',
                 'Non-AF\n(n=12,421)'
                 )

grViz(
  "digraph my_flowchart {
    graph[layout = dot]
    
    ## Main boxes
    node [fontname = Times, fontsize=15, width=4, shape=rectangle, height=1]
    B1 [label = '@@1']
    B2 [label = '@@2']
    B3 [label = '@@3']
    B4 [label = '@@4']
    B5 [label = '@@5']
    B6 [label = '@@6']
    B7 [label = '@@7']
    B8 [label = '@@8']
    B9 [label = '@@9']    
  
    ## Invisible node for joints
    node [shape = point, width=0]
    P1 P2 P3 P4
    
    subgraph {
      rank=same; rankdir = LR; B1;
    }
    
    subgraph {
      rank = same; rankdir = LR; P1; B2;
    }

    subgraph {
      rank = same; rankdir = LR; P2; P3; P4;
    }
    
    
    B1 -> P1 [arrowhead = none];
    P1 -> B2 [minlen= 5];
    P1 -> B3;
    B3 -> P3 [arrowhead = none];
    
    P2 -> P3 -> P4 [arrowhead = none, minlen = 10]; # minlen: for balanced bifurcation
    P2 -> B4;
    P4 -> B5;
    
    B4 -> B6;
    B4 -> B7;
    
    B5 -> B8;
    B5 -> B9;
    
  }
  
  [1]: text_labels[1]
  [2]: text_labels[2]
  [3]: text_labels[3]
  [4]: text_labels[4]
  [5]: text_labels[5]
  [6]: text_labels[6]
  [7]: text_labels[7]
  [8]: text_labels[8]
  [9]: text_labels[9]
")

# 결측치 시각화 --------------------------------------------
cohort_hr %>% 
  summarise_all(list(~is.na(.))) %>% 
  pivot_longer(everything(),
               names_to='variables',values_to="missing") %>% 
  count(variables, missing) %>% 
  group_by(variables) %>% 
  mutate(prop=n/sum(n))%>% 
  ggplot(aes(y=variables, x=prop, fill=missing))+
  geom_col()+
  scale_fill_manual(values=c("skyblue3","gold"))+
  labs(x='Missing Value Proportion')+
  theme_bw()+
  theme(axis.title.y=element_blank())

temp_hr %>% 
  summarise_all(list(~is.na(.))) %>% 
  pivot_longer(everything(),
               names_to='variables',values_to="missing") %>% 
  count(variables, missing) %>% 
  group_by(variables) %>% 
  mutate(prop=n/sum(n)) %>% filter(missing==T)

library(naniar)
gg_miss_var(df, show_pct = T)

table(is.na(drugs2[drugname=='dexmedetomidine']$afib_within_7days))
drugs2[,afib_within_7days:=as.factor(fifelse())]

dex_df <- drugs2[drugname=='dexmedetomidine']
table(is.na(dex_df$afib_within_7days))
dex_df[,afib_within_7days:=as.factor(ifelse(is.na(afib_within_7days), 'No',
                                        ifelse(afib_within_7days==1,'Yes','No')))]

# dex 사용여부에 따른 afib time 2 event --------------------- 
df_match[t2e<=7] %>% 
  ggplot(aes(x=t2e,
             fill=dex_usage))+
  geom_histogram(bins=50)+
  xlim(c(0,7))+
  theme_bw()+
  theme(legend.position = 'bottom')+
  labs(title="Afib duration by dexmedetomidine")

df_match[t2e<=7,.(mean=round(mean(t2e, na.rm=T),2)), by='dex_usage'] %>% 
  ggplot(aes(x=dex_usage,y=mean, fill=dex_usage)) +
  geom_col(width=.5) +
  geom_text(aes(label=paste0(mean,' hour')), vjust=1.5)+
  scale_x_discrete(label=c('No','Yes'))+
  scale_fill_discrete(label=c('No','Yes'))

df_match[t2e<=7] %>% ggplot(aes(x=dex_usage,y=t2e, fill=dex_usage))+
  geom_boxplot()



# dex 시작시간 히스토그램 ----------------------------------------------------------

drugs[stay_id %in% df_match$stay_id & drugname=='dexmedetomidine',
      .(dex_start = as.numeric(difftime(drug_starttime, icu_intime, units='hours')))][dex_start>=48]%>% 
  ggplot(aes(x=dex_start))+
  geom_histogram()


# dex 사용시간 히스토그램 -------------------
dex_df %>% 
  ggplot(aes(x=as.numeric(drug_infusion_time),
             fill=afib_within_7days)) +
  geom_histogram()+
  ylim(c(0,1000)) +
  labs(x='Dexmedetomidine Infusion Time', title="Dexmedetomidine 사용시간 히스토그램")+
  theme_bw() +
  facet_wrap(~afib_within_7days)+
  theme(legend.position = 'bottom')

dex_df[,.(stay_id, dex_use,drug_infusion_time)]
dex_df[,afib_within_7days:=as.factor(afib_within_7days)]
dex_df[,.(mean=round(mean(drug_infusion_time),2)),by='afib_within_7days'] %>% 
  ggplot(aes(x=afib_within_7days, y=mean, fill=fct_rev(afib_within_7days))) +
  geom_col(width=.5) +
  geom_text(aes(label=paste0(mean,' hour')), vjust=1.5)+
  scale_x_discrete(label=c('No','Yes'))+
  scale_fill_discrete(label=c('Yes','Yes'))+
  guides(fill=guide_legend(reverse = T, title="Afib within 7 days"))


# dex 사용량
dex_rate[dex_df,on='stay_id']
dex_rate[dex_df,on='stay_id'] %>% 
  ggplot(aes(x=dex_rate, fill=afib_within_7days)) +
  labs(x='Dexmedetomidine Input rate', title="Dexmedetomidine 사용용량")+
  geom_histogram(bins=50)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  facet_wrap(~afib_within_7days)

dex_df %>% colnames()
require(forcats)
?fct_rev

dex_df[,afib_within_7days:=as.factor(afib_within_7days)]
dex_rate
dex_rate[dex_df,on='stay_id'][,.(mean=round(mean(dex_rate),2)),by='afib_within_7days'] %>% 
  ggplot(aes(x=afib_within_7days, y=mean, fill=fct_rev(afib_within_7days))) +
  geom_col(width=.5) +
  geom_text(aes(label=paste0(mean,' mcg/kg/hour')), vjust=-.5)+
  scale_x_discrete(label=c('No','Yes'))+
  scale_fill_discrete(label=c('Yes','No'), name="Afib within 7 days")+
  guides(fill=guide_legend(reverse = T))+
  labs(y='dose')


#boxplot
dex_rate[dex_df, on='stay_id'][!is.na(afib_within_7days)] %>% 
  ggplot(aes(x=afib_within_7days, y=dex_rate, fill=afib_within_7days))+
  geom_boxplot()



# dex 사용과 delirium --------------------------------------------------------
require(ggthemes)
cohort2[,.(mean=mean(delirium_pos)), by='dex_usage'] %>% 
  ggplot(aes(x=dex_usage, y=mean, fill=dex_usage))+
  geom_col(width=.7)+
  geom_text(aes(label=round(mean,2)), vjust=-.5)+
  scale_x_discrete(label=c('No','Yes'))+
  scale_fill_discrete(label=c('No','Yes'), name='Dexmedetomidine')+
  labs(x='Dexmedetomidine Usage', y='No. of Delrium Incidence')+
  theme_stata()+
  theme(legend.position = 'top',
        axis.text.x=element_text(size=12),
        axis.title = element_text(size=15))

df_match[,.(mean=mean(delirium_pos)), by='dex_usage'] %>% 
  ggplot(aes(x=dex_usage, y=mean, fill=dex_usage))+
  geom_col(width=.7)+
  geom_text(aes(label=round(mean,2)), vjust=-.5)+
  scale_x_discrete(label=c('No','Yes'))+
  scale_fill_discrete(label=c('No','Yes'), name='Dexmedetomidine')+
  labs(x='Dexmedetomidine Usage', y='No. of Delrium Incidence')+
  theme_stata()+
  theme(legend.position = 'top',
        axis.text.x=element_text(size=12),
        axis.title = element_text(size=15))

# Heart Rate --------------------------------------------------------------
hrv_df <- hrv[,.(hr_mean = mean(value)),by=c('dex_use','stay_id')][order(stay_id)]
with(hrv_df, t.test(hr_mean ~ dex_use))
hrv_df$dex_use <- as.factor(hrv_df$dex_use)
hrv_df %>% ggplot(aes(x=dex_use,y=hr_mean)) +
  geom_violin(aes(fill=dex_use),
              color=NA,
              alpha=.3, width=.5 ,
              lwd=0.0)+
  stat_boxplot(geom='errorbar',width=.15, aes(color=dex_use))+
  geom_boxplot(aes(color=dex_use),width=.3) +
  scale_fill_discrete(name='DEX usage', labels=c('No','Yes'))+
  scale_color_discrete(name='DEX usage', labels=c('No','Yes'))+
  scale_x_discrete(labels=c('No','Yes'))+
  labs(x='DEX usage',y='Heart rate (bpm)')+
  theme_stata()

t.test(hr_mean ~ dex_usage, df_match)


# HR Fluctuation ----------------------------------------------------------

require(ggthemes)
hrv <- fread('csv/Afib_hr_chart.csv')
hrv <- hrv[stay_id %in% df_match$id]
hrv_afib <- hrv[stay_id %in% df_match[afib_within_7days==1]$id]
hrv[, hour := rowid(stay_id)]
hrv[,date := as.Date(charttime)]
hrv[,row_id := rowid(stay_id)]
x <- hrv[row_id==1,.(stay_id, dex_use)]
y <- hrv[,.(min_hr = min(value), max_hr = max(value)), by=c('stay_id','date')]
hrv_fluct <- left_join(y,x, by=c('stay_id'))
hrv_fluct[,day_id:=rowid(stay_id)]
hrv_fluct %>% View()



hrv_fluct[,dex_use:= as.factor(dex_use)]
hrv_fluct[,fluctuation:=max_hr-min_hr]

hrv_fluct[fluctuation<500 & day_id <30][,.(mean_fluc=mean(fluctuation)),by=c('day_id','dex_use')] %>% 
  ggplot(aes(x=day_id, y=mean_fluc, color=dex_use))+
  geom_line(size=0.8)+
  facet_wrap(~dex_use)


# Figure1A. KM plot -----------------------------------------------------------------
require(survminer)
require(ggthemes)
require(dplyr)
require(survival)
require(survminer)
require(ggsci)
df_km <- as.data.frame(df_match)
summary(df_match[dex_usage==0]$obs_duration)
df_match[,obs_duration_day := obs_duration/24]
df_match[,dex_usage := factor(dex_usage, levels=c(1,0))]
fit <- survfit(Surv(obs_duration, afib_within_7days==1) ~ dex_usage,df_match)
km <- ggsurvplot(fit, 
                 data = df_match,
                 fun='event', #function
                 size = 0.3, 
                 xlim=c(0, 7),
                 ylim=c(0,0.3),
                 break.time.by=1,
                 censor=F,
                 # pval = T,
                 linetype = "strata",
                 legend.title="DEX",
                 legend.labs=c('DEX','Non-DEX'),
                 risk.table=T,
                 fontsize=2 # table 에 들어가는 숫자 크기
)
km$plot <- km$plot+
  theme_classic() +
  labs(x="Time (Days)",
       y="Cumulative incidence of new onset atrial fibrillation") +
  scale_y_continuous(breaks = seq(0,0.3,0.05))+
  theme(axis.title.y = element_text(size=5),
        axis.title.x = element_text(size=6),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6),
        panel.grid.major.y=element_line(color="grey90",
                                        linewidth = 0.3),
        legend.position = 'none',
        # legend.key.size = unit(0.5,'cm'),
        legend.text = element_text(size=7),
        legend.title = element_text(size=7),
        axis.line=element_line(size = 0.2),
        axis.ticks=element_line(size=0.2)) +
  scale_color_hue(direction=-1) +
  scale_color_d3() +
  annotate('text',x=0.9, y=0.2, label="italic(P)<0.001",
           size=2, parse=T) +
  annotate('text', x=6.8, y=0.29,  size=2,label='Non-DEX')+ #color='#ff7f0e'
  annotate('text', x=6.7, y=0.245, size=2,label='DEX') #,color='#1f77b4'

km$table <- km$table + 
  theme_void() +
  # theme_stata(base_family = 'serif') +
  labs(x="", y="") + 
  theme(
    # axis.title.y = element_text(size=10,angle = 0),
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 0, size=5,hjust=1),
        text= element_text(size=6) #number at risk
        )

km
require(patchwork)
figure2A <- km$plot/km$table +
  plot_layout(heights = c(7.5,2.5))
ggsave(filename = "result/JAMA network/figure2A.pdf",
       plot = figure2A,
       device = "pdf",
       width = 84,
       height = 63,
       units = 'mm',
       dpi = 300)

require(survival)
cox.zph(fit = coxph(Surv(inhos_duration, inhos_mortality==1)~ dex_usage, df_match),
        transform = "km")
?cox.zph
# Figure1B: afib 발생한 환자들의 dex 사용여부에 따른 사망률 -----------------
fit <- survfit(Surv(inhos_duration, inhos_mortality==1) ~ dex_usage, data=df_match)
km <- ggsurvplot(fit, 
                 data = df_match,
                 # fun='event',
                 xlim=c(0,50),
                 size = 0.3,
                 # ylim=c(0,0.25),
                 break.time.by=10,
                 censor=F,
                 # pval = T,
                 linetype = "strata",
                 legend.title="DEX",
                 legend.labs=c('DEX','Non-DEX'),
                 risk.table=T,
                 fontsize=2,
                 font.family="sans")
km$plot <- km$plot+
  theme_classic(base_family = 'sans') +
  labs(x="Time (Days)",
       y="Survival probability") +
  scale_y_continuous(breaks = seq(0,1,0.2))+
  theme(text=element_text(family="sans"),
        panel.grid.major.y=element_line(color="grey90",
                                        linewidth = 0.3),
        axis.title = element_text(size=6),
        axis.title.y = element_text(size=6),
        axis.title.x = element_text(size=6),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6),
        legend.position = 'none',
        legend.text = element_text(size=7),
        legend.title = element_text(size=7),
        axis.line=element_line(size = 0.2),
        axis.ticks=element_line(size=0.2)) +
  scale_color_hue(direction=-1) +
  scale_color_d3() +
  annotate('text',x=5, y=0.6, label="italic(P)<0.001",
           family='sans', size=2, parse=T) +
  annotate('text', x=48, y=0.72,  size=2,label='Non-DEX',color='#ff7f0e')+
  annotate('text', x=47, y=0.85, size=2,label='DEX',color='#1f77b4')

km$table <- km$table + 
  theme_void(base_family = 'sans') +
  # theme_stata(base_family = 'serif') +
  labs(x="", y="") + 
  theme(
    # axis.title.y = element_text(size=10,angle = 0),
    axis.text.y = element_text(angle = 0, size=5,hjust=1),
    text= element_text(size=6) #number at risk
  )
figure2B <- km$plot/km$table +
  plot_layout(heights = c(7.5,2.5))
ggsave(filename = "result/JAMA network/figure2B.pdf",
       plot = figure2B,
       device = 'pdf',
       width = 84,
       height = 63,
       units = 'mm',
       dpi = 300)

# Figure 2. forest plot ---------------------------------------------------

require(forester)
sub_result <- data.table::data.table(
        check.names = FALSE,
           Subgroup = c("Sex","Female","Male",
                        "Age","< 65","≥ 65","Ethnicity","African-American",
                        "Non-African-American","ICU Unit","CVICU","Non-CVICU",
                        "Sepsis","No","Yes","Cardiac surgery","No","Yes",
                        "Mechanical ventilation support","No","Yes","CRRT","No",
                        "Yes","Total"),
                DEX = c(NA,"104/708","267/1398",NA,
                        "97/1178","274/928",NA,"11/143","360/1963",NA,
                        "261/834","110/1272",NA,"104/651","267/1455",NA,
                        "340/2026","31/80",NA,"24/305","347/1801",NA,
                        "352/2034","19/72","373/2112"),
          `non-DEX` = c(NA,"437/2067","886/3842",
                        NA,"304/3144","1019/2765",NA,"55/410","1268/5499",
                        NA,"792/2217","531/3692",NA,"348/1848","975/4061",
                        NA,"1235/5671","88/238",NA,"103/893","1220/5016",NA,
                        "1252/5687","71/222","1359/5907"),
      `HR.(95%.CI)` = c(NA,"0.61 (0.49-0.75)",
                        "0.75 (0.65-0.86)",NA,"0.79 (0.63-1.00)",
                        "0.68 (0.59-0.77)",NA,"0.51 (0.27-0.97)","0.71 (0.63-0.80)",NA,
                        "0.80 (0.70-0.92)","0.52 (0.42-0.64)",NA,
                        "0.79 (0.63-0.98)","0.67 (0.59-0.77)",NA,"0.69 (0.61-0.78)",
                        "0.94 (0.62-1.41)",NA,"0.60 (0.39-0.94)","0.71 (0.63-0.80)",
                        NA,"0.70 (0.63-0.79)","0.70 (0.42-1.17)",
                        "0.68 (0.61-0.77)"),
                 HR = c(NA,0.61,0.75,NA,0.79,
                        0.68,NA,0.51,0.71,NA,0.8,0.52,NA,0.79,0.67,NA,
                        0.69,0.94,NA,0.6,0.71,NA,0.7,0.7,0.7),
              lower = c(NA,0.49,0.65,NA,0.63,
                        0.59,NA,0.27,0.63,NA,0.7,0.42,NA,0.63,0.59,NA,
                        0.61,0.62,NA,0.39,0.63,NA,0.63,0.42,0.63),
              upper = c(NA,0.75,0.86,NA,1,0.77,
                        NA,0.97,0.8,NA,0.92,0.64,NA,0.98,0.77,NA,0.78,
                        1.41,NA,0.94,0.8,NA,0.79,1.17,0.79),
  P.for.interaction = c("0.10",NA,NA,"0.24",NA,
                        NA,"0.30",NA,NA,"<0.001",NA,NA,"0.23",NA,NA,
                        "0.17",NA,NA,"0.48",NA,NA,"1",NA,NA,NA)
)
sub_result
subgroups <- c(2,3,5,6,8,9,11,12,14,15,17,18,20,21,23,24)
sub_result$Subgroup[subgroups] <- paste("     ",sub_result$Subgroup[subgroups])

sub_result <- sub_result %>% mutate(lower= ifelse(!is.na(lower),format(round(as.numeric(lower),2),nsmall=2), NA))
sub_result <- sub_result %>% mutate(upper= ifelse(!is.na(upper),format(round(as.numeric(upper),2),nsmall=2), NA))
sub_result <- sub_result %>% mutate(HR= ifelse(!is.na(HR),format(round(as.numeric(HR),2),nsmall=2), NA))
sub_result <- sub_result %>% mutate(P.for.interaction= ifelse(P.for.interaction=="<0.001", '<0.001', 
                                              ifelse(!is.na(P.for.interaction),format(round(as.numeric(P.for.interaction),2),nsmall=2), NA)))

# sub_result <- sub_result %>% mutate(HR= ifelse(HR=="1 (ref)", '1 (Ref.)',
#                                                ifelse(!is.na(HR),format(round(as.numeric(HR),2),nsmall=2),NA)))
sub_result <- sub_result %>% mutate(`HR (95% CI)` = ifelse(!is.na(HR), paste0(HR, ' (', lower, '-', upper, ')'),""))
sub_result <-sub_result %>% rename('P for heterogeneity '='P.for.interaction')
setnames(sub_result, 'non-DEX',' Non-DEX')
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
         right_side_data= sub_result[,c(9,8)],
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
         # nudge_x = ,
         # ggplot_width = 30,
         font_family = "sans",
         arrows = T,
         arrow_labels=c('DEX favored', 'Non-DEX favored'),
         render_as = 'pdf'
)

citation("forester")

mytable(dex_usage ~ afib_within_7days, df_match)
# foreset2 ----------------------------------------------------------------

temp
subgroups <- c(2,3,4,5,7,8,9,10)
temp$Subgroup[subgroups] <- paste("     ",temp$Subgroup[subgroups])

temp <- temp %>% mutate(lower= ifelse(!is.na(lower),format(round(as.numeric(lower),2),nsmall=2), NA))
temp <- temp %>% mutate(upper= ifelse(!is.na(upper),format(round(as.numeric(upper),2),nsmall=2), NA))
temp <- temp %>% mutate(P.for.interaction= ifelse(P.for.interaction=="<0.001", '<0.001', 
                                                              ifelse(!is.na(P.for.interaction),format(round(as.numeric(P.for.interaction),2),nsmall=2), NA)))

temp <- temp %>% mutate(HR= ifelse(HR=="1 (ref)", '1 (reference)',
                                               ifelse(!is.na(HR),format(round(as.numeric(HR),2),nsmall=2),NA)))
temp <- temp %>% mutate(`HR (95% CI)` = ifelse(!is.na(HR), paste0(HR, ' (', lower, '-', upper, ')'),""))
temp <-temp %>% rename('P for interaction'='P.for.interaction')
temp <-temp %>% rename('Mean ± SD'='Mean.±.SD')
shape <- rep(16, times=10)
sizes <- rep(3.25, times=10)
require(forester)
forester(left_side_data = temp[,1:5],
         right_side_data= temp[,c(10,9)],
         estimate = as.numeric(temp[['HR']]),
         estimate_col_name = "HR (95% CI)",
         estimate_precision = 2,
         ci_low = as.numeric(temp$lower),
         ci_high= as.numeric(temp$upper),
         # stripe_colour = "#ffffff",
         # ci_sep="  ", 
         display = T,
         null_line_at = 1,
         xlim=c(0,2),
         dpi=1500,
         nudge_x=0.5,
         nudge_width = 1,
         nudge_height = 0,
         ggplot_width = 20,
         justify=c(0,0,0.5,0.5, 0.5, 0.5,0.5), # 정렬
         point_shapes= shape,
         point_sizes = sizes,
         # nudge_x = ,
         # ggplot_width = 30,
         font_family = "serif",
         arrows = T,
         
         arrow_labels=c('DEX favored', 'Non-DEX favored'),
         render_as = 'jpg')
# DEX 사용 여부에 따른 시간별 HR 추이 -------------------------------------------------
hrv[,dex_use := as.factor(ifelse(dex_use==1,1,0))]
hrv[, hours:=format(charttime, format='%H')]
hrv
hrv_per_hour <- hrv[hrv[,.I[1L], by=c('stay_id','date', 'hours')]$V1]
hrv_per_hour
hrv_per_hour[,hour:=rowid(stay_id)]
hrv_per_hour[hour<200,.(mean_hr=  mean(value),
                        min_hr = min(value),
                        max_hr = max(value),
                        sd_hr = sd(value)), by=c('hour','dex_use')] %>% View()
hrv_per_hour[hour<=170 & value <200,.(mean_hr=  mean(value),
                min_hr = min(value),
                max_hr = max(value),
                sd_hr = sd(value)), by=c('hour','dex_use')] %>% 
  ggplot(aes(x=hour, y=mean_hr))+
  geom_smooth(aes(color=dex_use, 
                  fill=dex_use,
                  linetype=dex_use), 
              size=1) +
  scale_fill_discrete(name='DEX',labels=c('No','Yes'))+
  scale_color_discrete(name='DEX',labels=c('No','Yes'))+
  scale_linetype_discrete(name='DEX',labels=c('No','Yes'))+
  labs(x= 'Hour', y='Heart rate (bpm)')+
  ggthemes::theme_stata()
  # facet_grid(~dex_use)
  # geom_line(aes(color=dex_use))+

require(Publish)
hrv_per_hour[hour<=170 & value <200,.(mean_hr=  mean(value),
                                     min_hr = min(value),
                                     max_hr = max(value),
                                     se = as.numeric(ci.mean(value)[2])), by=c('hour','dex_use')] %>% 
  ggplot(aes(x=hour, y=mean_hr))+
  geom_ribbon(aes(ymin=mean_hr - se, ymax=mean_hr + se, fill=dex_use), alpha=.3)+
  geom_line(aes(color=dex_use, linetype=dex_use), size=1.0)+
  # scale_color_manual(values=c('red','darkgreen'))+
  scale_fill_discrete(name='DEX',labels=c('No','Yes'))+
  scale_color_discrete(name='DEX',labels=c('No','Yes'))+
  scale_linetype_discrete(name='DEX',labels=c('No','Yes'))+
  labs(x= 'Hour', y='Heart rate (bpm)')+
  ggthemes::theme_stata()
  # ylim(c(0,120))


# Figure E2. Matching 결과 --------------------------------------------
require(ggthemes)
require(forcats)
require(cobalt)
#https://cran.rstudio.com/web/packages/cobalt/vignettes/cobalt_A4_love.plot.html
new.name <- c(age='Age',
              sepsis_Yes='Sepsis',
              icuunit_CVICU='ICU Unit: Cardiovascular ICU',
              icuunit_SICU='ICU Unit: SICU',
              icuunit_MICU ='ICU Unit: MICU',
              icuunit_CCU = 'ICU Unit: CCU',
              icuunit_ETC = 'ICU Unit: Other',
              ventilation = 'Mechanical ventilation',
              crrt = 'Continuous kidney replacement therapy',
              sex_M = 'Sex: Male',
              ethnicity_Other = 'Race and ethnicity: Other',
              `ethnicity_African-American`= 'Race and ethnicity: African American',
              ethnicity_Hispanic = 'Race and ethnicity: Hispanic',
              ethnicity_Caucasian = 'Race and ethnicity: White',
              ethnicity_Asian = 'Race and ethnicity: Asian',
              heart_surgery = 'Cardiac surgery',
              cci='Charlson comorbidity index',
              sofa_score='SOFA',
              creatinine_max='Creatinine',
              bun_max='BUN',
              heart_rate_max = 'Pulse rate',
              temperature_max='Temperature',
              vis = 'Vasoactive Inotropic Score',
              platelets_min = 'Platelet',
              potassium_max= 'Potassium',
              calcium_min = 'Calcium',
              sodium_max = 'Sodium',
              bicarbonate_min='Bicarbonate',
              wbc_max = 'White blood cell',
              spo2_min = 'Spo₂',
              lactate_max='Lactate',
              chloride_max = 'Chloride',
              resp_rate_max = 'Respiratory rate',
              sapsii = 'SAPS II',
              sepsis = 'Sepsis',
              hemoglobin_min = 'Hemoglobin',
              map_min = 'Mean arterial pressure',
              magnesium_min = 'Magnesium',
              bmi = 'Body mass index',
              ph_min = 'pH',
              po2_min = 'po₂',
              pco2_max = 'pCo₂'
              )
require(cobalt)
smd_plot <- love.plot(m, abs=T, thresholds = 0.1,
          drop.distance = T,
          binary='std',
          position='bottom',
          var.order = 'unadjusted',
          var.names = new.name,
          size = 2,
          shapes=c('circle filled','circle')
          )
require(ggthemes)
smd_plot <- smd_plot + 
  theme_classic(base_family = 'sans')+ 
  labs(x='Absolute Standardized Mean Difference')+
  theme(axis.text.y = element_text(angle = 0,size=8),
        axis.text.x = element_text(size=8),
        axis.title = element_text(size=8),
        axis.line = element_line(linewidth=0.3),
        axis.ticks = element_line(linewidth=0.3),
        title=element_blank(),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        panel.grid.major.y = element_line(color='#F1F1F1'),
        legend.position='bottom')
smd_plot
ggsave(plot=smd_plot,
       filename='result/JAMA network/revised_figures/eFigure1.wmf',
       device = 'wmf',
       width=150,
       height=120,
       units='mm',
       dpi=300)
# love.plot(m, stat="mean.diffs", binary='std',
#           var.order = 'unadjusted',
# )
bal.tab(m)

smd_plot
# Figure E3_A. dex dose quantile에 따른 afib 발생 비율 -------------------------------------------------------
require(ggthemes)
with( dexRateSummary,
      chisq.test(afib_yn,quantile)
)

dexRateSummary <- dexRate[,.(sum_rate=sum(rate),
                             sum_total_amount = sum(total_amount)),by=stay_id]

dexRateSummary <- dexRateSummary[stay_id %in% df_match$stay_id]
dexRateSummary[,quantile := as.factor(ntile(sum_total_amount, 4))]
dexRateSummary[,.(min = min(sum_total_amount),
                  max = max(sum_total_amount)),by=quantile]
dexRateSummary[,afib_yn := as.factor(ifelse(stay_id %in% afib_dex[afib_within_7days==1, stay_id],1,0))]
xtickLabel <- c('1st','2nd','3rd','4th')
dexRateSummary[,.N, by=.(quantile,afib_yn)]
temp <- dexRateSummary[stay_id %in% afib_dex$stay_id,.N, by=.(quantile,afib_yn)][,pct := round(N/sum(N),2)*100, by=.(quantile)]
x <- with(dexRateSummary[stay_id %in% afib_dex$stay_id],
          table(quantile, afib_yn))
require(rcompanion)
rcompanion::pairwiseNominalIndependence(
  x,
  fisher=F, gtest=F,
  chisq = T
)

dexRateSummary[,mean(sum_total_amount),by=quantile]
label <- c('1st quarter\n(<0.93mg)',
           '2nd quarter\n(0.93–3.39mg)',
           '3rd quarter\n(3.39–12.57mg)',
           '4th quarter\n(≥12.57mg)')
comp <- list(c('1','2'),c('1','3'),c('1','4'),c('2','3'),c('2','4'),c('3','4')) 
bargraph <- temp[afib_yn==1]%>% 
  ggplot(aes(x=as.factor(quantile), y=pct)) +
  geom_col(fill='#D6EAF8')+
  theme_classic(base_family = 'sans')+
  labs(x='Cumulative dose of dexmedetomidine divided into four groups by quartiles',
       y= 'New-onset atrial fibrillation (%)')+
  ylim(c(0,25))+
  scale_x_discrete(labels=label)+
  theme(axis.title.x = element_text(size=6),
        axis.title.y = element_text(size=6),
        axis.text = element_text(size=6),
        axis.line=element_line(size = 0.2),
        axis.ticks=element_line(size=0.2))+
  annotate('text',x=3.3, y=25, label='italic(P)',
           family='sans', size=2.5, parse=T)+
  annotate('text',x=3.85, y=25, label='for trend<.001',
           family='sans', size=2.5)
bargraph
ggsave(plot=bargraph, 
       filename = 'result/JAMA network/revised_figures/eFigure2.wmf',
       width=84,
       height=63,
       units = 'mm',
       dpi=300,device = 'wmf')
#   
#   geom_path(x=c(1,1,3,3),y=c(22,23.5,23.5,22))+
#   annotate('text',x=2, y=24.5, label='<0.001 ***', family='serif')+
#   
#   geom_path(x=c(1,1,4,4),y=c(25,26.5,26.5,25))+
#   annotate('text', x=2.5, y=27.5, label='<0.001 ***', family='serif') +
#   
#   geom_path(x=c(2,2,4,4),y=c(15,16.5,16.5,15))+
#   annotate('text',x=3,y=17.5,label='0.05 *', family='serif')
# scale_x_discrete('DEX dose quantile', labels=c('1st'=1,'2nd'=2,'3rd'=3,'4th'=4))



# p for trend
prop.trend.test(x=c(92,139,132,210),n = c(1031,1031,1030,1030))

require(ggmosaic)
require(forcats)
str(dexQuantileDf)
dexQuantileDf
ggplot(data=dexQuantileDf)+
  geom_mosaic(aes(x=product(quantile), fill= afib_within_7days)) + 
  # geom_mosaic_text(aes(x=product(quantile), fill= afib_within_7days))+
  theme_stata(base_family = 'serif') +
  scale_fill_brewer(palette='Set1', 
                    direction = -1,
                    label=c('No','Yes'),
                    name='AF') +
  # scale_fill_manual(values=c('blue','red'), label=c('No','Yes'),
  #                   name='AF')+
  scale_x_productlist(labels = c('1st','2nd','3rd','4th'), 
                      name = 'DEX Dose') + 
  scale_y_productlist(labels=c('No','Yes'), name="Atrial fibrillation")+
  guides(fill=guide_legend(reverse=T)) # legend의 순서 뒤집기


# figure E3_B: 사용시간에 따른 NOAF 발생률 --------------------------------------------

dexRate[stay_id %in% df_match$stay_id] %>% 
  ggplot(aes(x=duration_min))+
  geom_histogram()

dexRate <- dexRate[stay_id %in% df_match$stay_id]
boxplot(dexRate$duration_min)
dexRate[,duration_q4 := ntile(duration_min,4) %>% as.factor()]
dexRate[,afib_yn := ifelse(stay_id %in% df_match[afib_within_7days==1, stay_id],1,0)]

# p for trend
dexRate[,.N, by=duration_q4]
prop.trend.test(x=c(133,88,72,78),n = c(527,527,526,526))

dexRate[,.(min=min(duration_min),
           max=max(duration_min)),by=duration_q4]
label <- c('Quartile 1\n(≤223)',
           'Quartile 2\n(224–868)',
           'Quartile 3\n(869–2,710)',
           'Quartile 4\n(≥2,710)')
label
figureE3_B <- dexRate[stay_id %in% df_match$stay_id][,.N,by=.(duration_q4,afib_yn)][,.(prop=N/sum(N)),by=duration_q4][prop<0.3] %>% 
  ggplot(aes(duration_q4, prop))+
  geom_col(position = 'dodge',
           fill='#D6EAF8')+
  labs(x='Quartiles of dexmedetomidine infusion duration (minute)',
       y='New-onset atrial fibrillation (%)')+
  scale_y_continuous(labels = scales::percent_format(suffix=""),
                     breaks = seq(0,0.30,0.05))+
  scale_x_discrete(labels=label)+
  theme_classic(base_family = 'sans')+
  theme(axis.title.x = element_text(size=6),
        axis.title.y = element_text(size=6),
        axis.text = element_text(size=6),
        axis.line=element_line(size = 0.2),
        axis.ticks=element_line(size=0.2))+
  annotate('text',x=3.3, y=0.25, label='italic(P)',
           family='sans', size=2.5, parse=T)+
  annotate('text',x=3.9, y=0.25, label='for trend<0.001',
           family='sans', size=2.5)
figureE3_B  
ggsave(plot=figureE3_B, 
       filename = 'result//efigureE3_B.pdf',
       width=84,
       height=63,
       units = 'mm',
       dpi=300,device = 'png')
#   
# Figure E4. fluctuation box plot -------------------------------------------------------------

heart_rate
heart_rate <- fread('csv/Afib_heartrate.csv')
heart_rate <- heart_rate[stay_id %in% df_match$stay_id]
# setnames(heart_rate,'value','hr')
# 
heart_rate[, hours:=format(charttime, format='%H')]
heart_rate[,dates:=format(charttime, format='%Y%m%d')]
heart_rate[,row_num:=rowid(stay_id)]
heart_rate[,dex_use:=as.factor(ifelse(dex_use==0,'No','Yes'))]

# 6시간 주기 또는 12시간 주기별 컬럼 만들기
heart_rate_afib <- heart_rate[stay_id %in% df_match[afib_within_7days==1]$stay_id]
temp <- heart_rate_afib[heart_rate_afib[,.I[1L],.(stay_id,dates,hours)]$V1]
temp[,n6:=gl(.N, 6, length=.N), by=.(stay_id)]
temp <- temp[,.(min_hr=min(value), max_hr=max(value)), by=.(stay_id, n6)]
# temp[,.(min_hr=min(hr), max_hr=max(hr)), by=.(stay_id,dates, (seq(nrow(temp))-1) %/% 5)]
# heart_rate[heart_rate[,.I[1L],.(stay_id,dates,hours)]$V1][,.(min_hr = min(hr), max_hr = max(hr)),by=.(stay_id, dates, hours-0:5)]
# heart_rate[,hours:=as.numeric(hours)]
# heart_rate[,.(min_hr = min(hr), max_hr = max(hr)),by=.(stay_id, dates,hours-0:3)]



# y <- heart_rate[,.(min_hr = min(hr), max_hr = max(hr)), by=.(stay_id, dates)]

x <- heart_rate[row_num==1,.(stay_id, dex_use)] # stay_id 뽑아내기
hrv_fluct <- left_join(temp,x, by=c('stay_id'))
hrv_fluct[,n6:=as.numeric(n6)*6]
hrv_fluct[,dex_use := factor(dex_use,levels=c('Yes','No'))]
# hrv_fluct[,row_id := rowid(stay_id)]
hrv_fluct[,fluctuation:= max_hr-min_hr]
hrv_fluct[fluctuation<10,.(mean(fluctuation)),by=dex_use]
plt2 <- hrv_fluct[n6<=36 & fluctuation<100] %>% 
  ggplot(aes(x=as.factor(n6), y= fluctuation))+
  geom_violin(aes(fill=dex_use), alpha=0.5)+
  stat_boxplot(geom='errorbar', 
               aes(color=dex_use),
               size=0.4)+
  geom_boxplot(aes(color=dex_use),
               alpha=1,
               fatten = 1,
               outlier.size=0.2) +
  theme_classic(base_family = 'sans') + 
  theme(legend.position = 'top',
        legend.key.size = unit(0.5,'cm'),
        legend.text = element_text(size=5),
        axis.text = element_text(size=5),
        axis.title = element_text(size=6),
        axis.line = element_line(size=0.3),
        axis.ticks = element_line(size=0.3))+
  labs(x='Time from ICU admission (hours)', 
       y=expression("HR"["max"]- "HR"["min"]),
       fill='DEX',color='DEX') +
  scale_fill_d3(name='',labels=c('DEX','Non-DEX'))+
  scale_color_d3(name='',labels=c('DEX','Non-DEX'))
plt2

for(i in seq(6,36,6)){
  with(hrv_fluct[n6==i],
    print(t.test(fluctuation ~ dex_use))
       )
}
hrv_fluct[n6<=36,.(mean=mean(fluctuation),
                  sd = sd(fluctuation)),.(dex_use, n6)]
hrv_table <- hrv_fluct[n6<=36] %>% 
  group_by(n6) %>% 
  nest() %>% 
  mutate(x = purrr::map(data, ~tidy(t.test(fluctuation ~ dex_use, data=.)))) %>% 
  unnest(x) %>% 
  select(n6, estimate1, estimate2, p.value)
hrv_table
rst <- data.table(
  ' ' = c('DEX','Non-DEX','p value'),
  `6 hr` = c("14.9 ± 10.5",'17.6 ± 12.5', '< 0.001'),
  `12 hr` = c('12.9 ± 8.7', '14.3 ± 11.4','0.016'),
  `18 hr` = c('12.6 ± 10.4', '14.2 ± 11.5','0.016'),
  `24 hr` = c('14.4 ± 10.1', '15.6 ± 12.1', '0.06'),
  `30 hr` = c('15.1 ± 11.3', '15.9 ± 13.6','0.312'),
  `36 hr` = c('15.7 ± 13.6', '15.5 ± 12.7','0.832')
)

rownames(rst) <- c('DEX','non-DEX','p value')

## gt 사용 + ggplot과 함께 사용하기
# bstfun 과 함꼐 사용
require(bstfun) # as_ggplot(gt)
require(gt)
tab <- rst %>% 
  gt() %>% 
  cols_align(align='center') %>% 
  opt_table_font(
    font=c('arial')
  )
figureE4 <- plt2/as_ggplot(tab)+
  plot_layout(heights = c(7.5,2.5))
figureE4
ggsave('result/figures/figureE4.tiff',
       figureE4,
       width = 150,
       height = 150,
       units='mm')
require(gtsummary)
require(patchwork)

## ggtexttable 사용
# font 변경 불가
tab <- ggtexttable(rst,
            theme = ttheme(
              rownames.style = rownames_style(
                face = 'plain'),
              colnames.style=colnames_style(
                color = "black",
                face = "bold",
                size = 12,
                fill = "white",
                linewidth = 1,
                linecolor = "white"),
              tbody.style = tbody_style(fill='white',  linecolor = "white"))) %>% 
  tab_add_hline(at.row = c(1,2,3,4), row.side = 'top', linewidth = 1, linetype = 1) %>% 
  tab_add_hline(at.row = c(4.5), row.side = 'bottom', linewidth = 1, linetype = 1) +
  geom_text(family='Times New Roman')
plt2/tab + 
  plot_layout(heights = c(2.5,1))


## gridExtra 사용
# 테이블 선 그리는 것 복잡.
theme <- gridExtra::ttheme_default(
  core=list(bg_params=list(fill='white')),
  colhead =list(bg_params=list(fill='white')),
  base_family = 'serif')

tab <- tableGrob(rst, theme=theme)
grid.arrange(plt2, tab, heights=c(0.7, 0.3))

require(gridExtra)
ggtexttable(tab)
gridExtra::grid.arrange(plt2,tableGrob(tab))
column <- paste('hr',hrv_table$n6)



#HRV ---------------------------------------------------------------------

with(hrv, t.test(value ~ dex_use))
hrv
hrv[hour<170,.(hr= mean(value)),by=c('dex_use','hour')] %>% 
  ggplot(aes(x=hour, y=hr, 
             fill=as.factor(dex_use),
             color=as.factor(dex_use))) +
  geom_smooth( se = T )+
  scale_fill_discrete(name='DEX', labels=c('No','Yes'))+
  scale_color_discrete(name='DEX', labels=c('No','Yes'))+
  theme_stata()+
  ylim(c(80,95))+
  labs(x= 'Hours', y='Heart rate')+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

hrv_afib[hour<170,.(hr= mean(value)),by=c('dex_use','hour')] %>% 
  ggplot(aes(x=hour, y=hr, 
             fill=as.factor(dex_use),
             color=as.factor(dex_use))) +
  geom_smooth()+
  ylim(c(80,100))


# LOS ---------------------------------------------------------------------

df_match[,.(mean=mean(los_hospital)),by=dex_usage] %>% 
  ggplot(aes(x=dex_usage,y=mean, fill=dex_usage))+
  geom_col(width=.7, color="black")+
  geom_text(aes(label=paste(round(mean,1),'days')), vjust=-.5)+
  labs(y='Hospital LOS')+
  scale_x_discrete(label=c('No','Yes'))+
  scale_fill_discrete(label=c('No','Yes'),name="Dexmedetomidine")

df_match[,.(mean=mean(los_icu)),by=dex_usage] %>% 
  ggplot(aes(x=dex_usage,y=mean, fill=dex_usage))+
  geom_col(width=.7, color="black")+
  geom_text(aes(label=paste(round(mean,1),'days')), vjust=-.5)+
  labs(y='ICU LOS')+
  scale_x_discrete(label=c('No','Yes'))+
  scale_fill_discrete(label=c('No','Yes'),name="Dexmedetomidine")


# 90 Mortality ------------------------------------------------------------


mytable(dex_usage ~ mortality_90, df_match)
mytable(dex_usage ~ inhos_mortality, df_match)
x <- df_match[,.N, by=c('dex_usage','mortality_90')][,pct:=round(N/sum(N)*100,1), by="dex_usage"][order(mortality_90)]


require(forcats)
x[,ylabel:= cumsum(pct)-0.5*pct, by='dex_usage']
x %>% 
  ggplot(aes(x=dex_usage,y=pct, fill= fct_rev(mortality_90)))+ 
  geom_col(width=.7, color="black")+
  geom_text(aes(label=paste0(pct,'%'), y=ylabel)) +
  scale_x_discrete(label=c('No','Yes'))+
  scale_fill_discrete(label=c('Yes','No'), name="In-Hospital mortality")+
  theme(legend.position = 'bottom') +
  labs(y="%")


x <- cohort_mt[,.N, by=c('mt_90','dex_usage')][,pct:= round(N/sum(N)*100,1),by='dex_usage']
x[,label:=cumsum(pct)-0.5*pct, by=dex_usage]
x %>% 
  ggplot(aes(x=dex_usage,y=pct, fill=fct_rev(mt_90)))+
  geom_col(width=0.7, alpha=.7, color='black')+
  geom_text(aes(y=label, label=paste0(pct,'%')))+
  labs(title="90 Mortality by Dexmedetomidine Usage")+
  scale_x_discrete(labels=c('No','Yes'))+
  scale_fill_discrete(labels=c('Dead','Alive'), name="~")+
  guides(fill=guide_legend(reverse = F, title="90 Mortality"))

# ggplot legend title 변경방법
# 1. scale_fill_discrete(name="")
# 2. guides(fill=guide_legend(title=""))

# ggplot stack bar plot 색상 뒤집기
# 1. ggplot(aes(fill=forcats::fct_rev())): 쌓은 순서만 바꾸기
# 2. geom_col(position=position_fill(reverse=TRUE)): 쌓은 순서 + 색상까지 변경


require(forcats)
x %>% ggplot(aes(x=dex_usage,y=pct, fill=mt_90))+
  geom_col(width=.7)

x %>% 
  ggplot(aes(x=dex_usage,y=pct, fill=mt_90))+
  geom_col(width=0.7, position=position_stack(reverse = T))

x %>% 
  ggplot(aes(x=dex_usage,y=pct, fill=fct_rev(mt_90)))+
  geom_col(width=0.7)
  

# SANKEY ------------------------------------------------------------------

# dex 사용에 따른 afib 발생
require(ggalluvial)
require(scales)
require(forcats)
sankey <- df_match2[,.N, by=.(dex_usage, afib_within_7days, inhos_mortality)][order(dex_usage,afib_within_7days, inhos_mortality)]
sankey
sankey[,`:=`(
  dex_usage = ifelse(dex_usage==0,'No','Yes'),
  afib_within_7days = ifelse(afib_within_7days ==0,'No','Yes'),
  inhos_mortality = ifelse(inhos_mortality == 'No', 'No','Yes')
)]
sankey$dex_usage <- factor(sankey$dex_usage, levels=c('Yes','No'))
sankey[,prop:=round(N/sum(N)*100,1)]
ggplot(sankey,
       aes(y=N, axis1=dex_usage, axis2=afib_within_7days, axis3=inhos_mortality))+
  geom_alluvium(aes(fill=fct_rev(dex_usage),
                    color=fct_rev(dex_usage),
                    alpha=fct_rev(inhos_mortality)),
                decreasing=T,
                width=1/12, 
                knot.pos=.4) +
  geom_stratum(color='grey', decreasing=T,
               width=.2) +
  geom_text(aes(label=after_stat(stratum)), stat='stratum', decreasing=T,
            family='serif')+
  # geom_label(stat='stratum', aes(label=after_stat(stratum)), decreasing=T) +
  scale_x_discrete(limits=c('dex_usage','afib_within_7days','inhos_mortality'),
                   labels=c('DEX', 'Afib w/ 7 days','Death'),
                   expand= c(.01,.01)) +
  scale_y_continuous(labels=comma)+
  # scale_fill_discrete(name='BMI status')+
  scale_fill_manual(name='DEX',values=c('firebrick3','deepskyblue3'))+
  scale_color_manual(name='DEX',values=c('firebrick3','deepskyblue3'))+
  scale_alpha_manual(name='DEX',values = c(0.9, 0.4), labels=c('No','Yes'))+
  theme_void()+
  theme(legend.position = 'bottom',
        
        legend.text=element_text(family='serif'),
        legend.title = element_text(family='serif'))+
  labs(y='No. of patients')

temp2 <- df_match[,.(id, dex_usage,afib_within_7days, inhos_mortality)]
temp2[,`:=`(
  dex_usage = ifelse(dex_usage==0,'No','Yes'),
  afib_within_7days = ifelse(afib_within_7days ==0,'No','Yes'),
  inhos_mortality = ifelse(inhos_mortality == 0, 'No','Yes')
)]

temp2
melt(temp2, id.vars = 'id') %>% 
  ggplot(aes(x=variable,
             stratum=value,
             alluvium = id,
             fill=value,
             label=value,
             alpha=value))+
  geom_text(stat='stratum')+
  geom_flow(stat='alluvium', lode.guidance='frontback')+
  geom_stratum(alpha=.5)+
  scale_fill_brewer(type='qual', palette = 'Set2')+
  scale_x_discrete(expand=c(.1, .1)) +
  scale_alpha_manual(name='DEX',values = c(0.9, 0.4), labels=c('No','Yes'))


# bradycardia hypotension -------------------------------------------------
require(kableExtra)
countBradyHypo %>% kbl() %>% kable_classic()
countBradyHypo %>% 
  ggplot(aes(x=as.factor(dex_usage), y=IR_brady))+
  geom_col(width=0.6,aes(fill=as.factor(dex_usage)))+
  geom_line(aes(x=dex_usage + 1, y=HR_brady/45))+
  geom_point(aes(y=HR_brady/45),shape=15)+
  scale_x_discrete(label=c('Non-Dex','Dex'))+
  scale_fill_discrete(label=c('Non-Dex','Dex'), name='DEX')+
  geom_errorbar(aes(ymin=lower_brady/45, ymax=upper_brady/45),width=.05)+
  scale_y_continuous(sec.axis = sec_axis(~ .*45, name='Hazard ratio (95% CI)'),
                     limits=c(0,0.03))+
  labs(x='DEX', y='Incidence rate (1000 person-hours)') + 
  theme_stata(base_family = 'serif')+
  geom_hline(yintercept = 0.0223, linetype='dashed')+
  theme(axis.title.y = element_text(face = 'bold'),
        axis.title.x = element_text(face='bold'))
  

countBradyHypo %>% 
  ggplot(aes(x=as.factor(dex_usage), y=IR_hypo))+
  geom_col(width=0.6,aes(fill=as.factor(dex_usage)))+
  geom_line(aes(x=dex_usage + 1, y=HR_hypo/58))+
  geom_point(aes(y=HR_hypo/58),shape=15)+
  scale_x_discrete(label=c('Non-Dex','Dex'))+
  scale_fill_discrete(label=c('Non-Dex','Dex'), name='DEX')+
  geom_errorbar(aes(ymin=lower_hypo/58, ymax=upper_hypo/58),width=.05)+
  scale_y_continuous(sec.axis = sec_axis(~ .*58, name='Hazard ratio (95% CI)'),
                     limits=c(0,0.02))+
  labs(x='DEX', y='Incidence rate (1000 person-hour)') +
  theme_stata(base_family = 'serif')+
  geom_hline(yintercept = 0.0173, linetype='dashed')+
  theme(axis.title.y = element_text(face = 'bold'),
        axis.title.x = element_text(face='bold'))
  

countBradyHypo
# supple: Mortality cox regression forest plot ----------------------------

df <- data.table::data.table(
    Variable = c("Dexmedetomidine","No","Yes","Sex",
                 "Female","Male","Age","<60",">=60","Afib","Non-Afib","Afib",
                 "ICU unit","Non CVICU","CVICU","Ethnicity","etc",
                 "African-American","CRRT","No","Yes","Ventilator","No","Yes",
                 "Hypertension","No","Yes","Sepsis","No","Yes","CCI","SOFA"),
          HR = c(NA,"1 (ref.)","0.66",NA,"1 (ref.)",
                 "0.8",NA,"1 (ref.)","1.34",NA,"1 (ref.)","1.23",NA,
                 "1 (ref.)","0.21",NA,"1 (ref.)","0.84",NA,"1 (ref.)","1.35",NA,
                 "1 (ref.)","0.66",NA,"1 (ref.)","0.95",NA,"1 (ref.)",
                 "1.64","1.08","1.14"),
       lower = c(NA,NA,0.58,NA,NA,0.71,NA,NA,1.16,
                 NA,NA,1.05,NA,NA,0.17,NA,NA,0.66,NA,NA,1.01,NA,NA,
                 0.54,NA,NA,0.8,NA,NA,1.34,1.06,1.12),
       upper = c(NA,NA,0.75,NA,NA,0.91,NA,NA,1.56,
                 NA,NA,1.44,NA,NA,0.26,NA,NA,1.07,NA,NA,1.82,NA,NA,
                 0.81,NA,NA,1.14,NA,NA,1.98,1.11,1.16),
           p = c("< 0.001",NA,NA,"< 0.001",NA,NA,
                 "< 0.001",NA,NA,"0.008",NA,NA,"< 0.001",NA,NA,"0.161",NA,
                 NA,"0.046",NA,NA,"< 0.001",NA,NA,"0.578",NA,NA,
                 "< 0.001",NA,NA,NA,"< 0.001")
)
subgroups <- c(2,3,5,6,8,9,11,12,14,15,17,18,20,21,23,24,26,27,29,30)
df$Variable[subgroups] <- paste("     ",df$Variable[subgroups])


df <- df %>% mutate(lower= ifelse(!is.na(lower),format(round(as.numeric(lower),2),nsmall=2), NA))
df <- df %>% mutate(upper= ifelse(!is.na(upper),format(round(as.numeric(upper),2),nsmall=2), NA))
df <- df %>% mutate(p= ifelse(p=="< 0.001", '< 0.001', 
                                              ifelse(!is.na(p),format(round(as.numeric(p),3),nsmall=3), NA)))

df
df <- df %>% mutate(HR= ifelse(HR=="1 (ref.)", 1,
                                               ifelse(!is.na(HR),format(round(as.numeric(HR),2),nsmall=2),NA)))
df %>% View()
df <- df %>% mutate(`HR (95% CI)` =ifelse(HR==1, "1 (Ref.)",
                                                 ifelse(!is.na(HR), paste0(HR, ' (', lower, ', ', upper, ')'),"")))
df <-df %>% rename('P for interaction'='p')
df
shape <- rep(16, times=32)
sizes <- rep(3.25, times=32)

# for(i in totals){
#   shape[i] <- 18
#   sizes[i] <- 7
# }

df %>% View()
require(forester)
forester(left_side_data = df[,1],
         right_side_data= df[,c(6,5)],
         estimate = as.numeric(df[['HR']]),
         estimate_col_name = "HR (95% CI)",
         estimate_precision =2,
         ci_low = as.numeric(df$lower),
         ci_high= as.numeric(df$upper),
         # stripe_colour = "#ffffff",
         # ci_sep="  ", 
         display = T,
         null_line_at = 1,
         xlim=c(0,2),
         dpi=2000,
         justify=c(0,0.5,0.5,0,0), # 정렬
         point_shapes= shape,
         point_sizes = sizes,
         nudge_x = 0.5,
         # ggplot_width = 30,
         font_family = "serif",
         arrows = T,
         arrow_labels=c('Mortality ↓ ', 'Mortality ↑'),
         render_as = 'jpg'
)


