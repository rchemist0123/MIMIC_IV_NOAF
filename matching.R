require(MatchIt)
require(survival)


df_match2

df_comp[,dex_usage := as.factor(d)]



# full cohort matching ----------------------------------------------------



coxph(Surv(obs_duration, afib_within_7days==1)~dex_usage*sepsis, df_match2)


# second matching for subgroup analysis -----------------------------------


m2_2 <- matchit(heart_surgery ~  sex + age + ethnicity + bmi +
                  icuunit + vis + sepsis + sofa_score + cci + 
                  ventilator+ crrt_before_afib +
                  map + heart_rate + resp_rate + temperature +
                  hemoglobin + wbc + creatinine + bun + spo2 + 
                  lactate + platelet + sodium + chloride + potassium + calcium+
                  bicarbonate + magnesium,
                data =df_match2,
                reestimate = T,
                discard = 'both',
                method='nearest')

m2_2
match.data(m2_2)

m2_2_df <- as.data.table(match.data(m2_2, distance = 'distance2',weights = 'weight2',
                      subclass = 'subclass2'))

sub_fit <- coxph(Surv(obs_duration, afib_within_7days==1)~ dex_usage, m2_2_df)


subgroupAnalTable(fit=sub_fit, subgroup = 'heart_surgery', trt = 'dex_usage',data = m2_2_df)

# by subgroup -------------------------------------------------------------

temp_match <- matchit(dex_usage ~
                        sex + age + ethnicity + bmi +
                        icuunit + vis + sepsis + sofa_score + cci + 
                        ventilator+ crrt_before_afib +
                        map + heart_rate + resp_rate + temperature +
                        hemoglobin + wbc + creatinine + bun + spo2 + 
                        lactate + platelet + sodium + chloride + potassium + calcium+
                        bicarbonate + magnesium,
                      data=df_comp[heart_surgery=='Yes'],
                      ratio = 2,
                      caliper = 0.1,
                      method = 'nearest')

temp_match_df <- match.data(temp_match)
temp_match_df




# 여자: 0.63 (0.51-0.77)
coxph(Surv(obs_duration, afib_within_7days==1)~ dex_usage, temp_match_df) %>% summary()

# 남자: 0.61 (0.53-0.69)
coxph(Surv(obs_duration, afib_within_7days==1)~ dex_usage, temp_match_df) %>% summary()


# 심장수술 No : 0.60 (0.53-0.67)
# 심장수술 Yes: 0.80 (0.53-1.21)
coxph(Surv(obs_duration, afib_within_7days==1)~ dex_usage*heart_surgery, temp_match_df) %>% summary()


