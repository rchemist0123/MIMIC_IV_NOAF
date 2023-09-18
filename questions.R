# subgroup for inhos-mortality --------------------------------------------

sub_df <- df_match[,.( obs_duration, afib_within_7days, age65, race, cvicu , dex_usage ,  
                       sex,vis,  sepsis  ,  cci , crrt_before_afib , heart_surgery,
                       ventilator, inhos_mortality,
                       inhos_duration)]
sub_df[,dex_usage := factor(dex_usage, levels = c(0,1))]
sub_fit <- coxph(Surv(obs_duration, afib_within_7days==1) ~ 
                   dex_usage, data=sub_df)



sub_df[,afib_within_7days:=as.numeric(ifelse(afib_within_7days==1,1,0))]
sub_df[,inhos_mortality:= ifelse(inhos_mortality=='Yes',1,0)]
sub_fit <- coxph(Surv(inhos_duration, inhos_mortality==1) ~ dex_usage,
                 sub_df)
sub_df[,vis_quantile := as.factor(ntile(vis,4))]
target <- c('sex','age65', 
            'race','cvicu',
            'sepsis','heart_surgery', 
            'ventilator','crrt_before_afib')
sub_df[,(target):=lapply(.SD, as.factor),.SDcols=target]
require(Publish)
subgroupAnalTable(fit=sub_fit,trt='dex_usage', 
                  subgroup = target, data=sub_df)

sub_fit %>% summary()


# 2. SNUBH RERI -----------------------------------------------------------
interaction_df <- bestMatch[,.(dex_usage, afib_within_7days, inhos_mortality)]
interaction_df[,c('dex_usage','afib_within_7days'):=lapply(.SD, function(x) as.numeric(ifelse(x==0,0,1))),
               .SDcol=c('dex_usage','afib_within_7days')]
interaction_df
# epiR
interaction_df[,group:=as.factor(fifelse(dex_usage==1 & afib_within_7days==0,0,
                                         ifelse(dex_usage==1 & afib_within_7days==1,1,
                                                ifelse(dex_usage==0 & afib_within_7days==0,2,3))))]
interaction_df[,.N,by=group]
epi_fit2 <- glm(inhos_mortality ~ group, data=interaction_df, family='binomial')
summary(epi_fit2)
epiR::epi.interaction(epi_fit2, coef=c(2,3,4), param='dummy')

x <- summary(epi_fit2)
exp(coef(epi_fit2))
rr1 <- exp(epi_fit2$coefficients[2])
rr2 <- exp(epi_fit2$coefficients[3])
rr3 <- exp(epi_fit2$coefficients[4])


ORplot(epi_fit2, show.CI = T)


# SOFA, APACHE ------------------------------------------------------------

bestMatch[,apache_group := ifelse(apache_score<10,0,
                                  ifelse(apache_score<16,1,
                                         ifelse(apache_score<21,2,
                                                ifelse(apache_score<26,3,
                                                       ifelse(apache_score<31,4,
                                                              ifelse(apache_score<36,5,6))))))]

table(bestMatch$apache_group)


bestMatch[apache,glm(inhos_mortality ~ dex_usage, family='binomial')] 
bestMatch[,dex_usage := factor(dex_usage, levels=c(0,1))]
rst <- bestMatch %>% 
  group_by(apache_group) %>% 
  do(model = tidy(glm(inhos_mortality ~ dex_usage,
                 family='binomial',
                 data=.,))) %>% 
  ungroup()
rst$model
for(i in seq_len(length(rst$model))){
  if(i>=2){
    print(cbind(exp(coef(rst$model[[i]])), exp(confint(rst$model[[i]]))))
  }

}

bestMatch %>% 
  group_by(apache_group) %>%
  summarise(apache_min = min(apache_score),
            apache_max=  max(apache_score)) %>% 
  mutate(OR = c(1.00,0.74,1.12,0.89,1.08,1.20,0.98)) %>% 
  ggplot(aes(x=as.factor(apache_group), y=OR))+
  geom_col() +
  scale_x_discrete(labels= c('<10','10-15','16-20','21-25',
                             '26-30','31-35','≥36'))+
  labs(x='APACHE',y='Odds ratio')+
  ggthemes::theme_few(base_family = 'serif')




df_match[,sofa_group := ntile(sofa_score,4)]
df_match$sofa_group
df_match[sofa_group==4,glm(inhos_mortality ~ dex_usage, family='binomial')] %>%
  summary()
df_match[,dex_usage:=factor(dex_usage, levels=c(0,1))]
rst <- df_match %>% 
  group_by(sofa_group) %>% 
  do(model=tidy(glm(inhos_mortality ~ dex_usage, 
              family='binomial',
              data=.))) %>% 
  ungroup()

rst$model
for(i in rst$model){
  print(cbind(exp(coef(i)), exp(confint(i))))
}

df_match %>% 
  group_by(sofa_group) %>%
  summarise(sofa_min = min(sofa_score),
            sofa_max=  max(sofa_score)) %>% 
  mutate(OR = c(0.62,0.47,0.54,0.41)) %>% 
  ggplot(aes(x=as.factor(sofa_group), y=OR))+
  geom_col() +
  scale_x_discrete(labels= c('<4','4-7','7-9','9-23'))+
  labs(x='SOFA',y='Odds ratio')+
  ggthemes::theme_few(base_family = 'serif')

df_match %>% 
  group_by(sofa_group) %>%
  summarise(sofa_min = min(sofa_score),
            sofa_max=  max(sofa_score)) %>% 
  mutate(OR = c(0.62,0.47,0.54,0.41)) %>% 
  ggpubr::ggbarplot(data=.,
                    x='sofa_group',
                    y='OR')
data.frame()



rst
rst$rn <- 1:nrow(rst)
rst <- rst %>% dplyr::filter(rn%%2==0)
exp(rst$estimate)



