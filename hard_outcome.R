
vals <- grep('time',names(mortality), value=T)
mortality[,(vals):=lapply(.SD, function(x) as.Date(as.character(x))), .SDcols=vals]

# inhospital mortality: 퇴원 전에 죽었는지 아닌지
mortality[,inhos_mortality:= as.factor(fifelse(dod <= dischtime, 1,0))]

# 90 mortality: icu 입실 이후 90일 이내 죽었는지 살았는지
mortality[,mortality_90 := as.factor(ifelse(dod<=icu_intime+lubridate::days(90),1,0))]

vals <- grep('mortality', names(mortality), value=T)
mortality[,(vals):=lapply(.SD, function(x) as.factor(fifelse(is.na(x),0, fifelse(x==1,1,0)))),
           .SDcols=vals]


mytable(dex_usage ~ mortality_90, mortality)

mytable(dex_usage ~ inhos_mortality, mortality)





