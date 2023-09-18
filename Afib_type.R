require(data.table)
afib_type <- fread('csv/Afib_type_check.csv')
afib_type
cohort3 <- afib_type[cohort2, on='stay_id']
cohort3 %>% View()

cohort3[!is.na(afib_type),.(stay_id,afib_within_7days)]
colnames(cohort2)
mytable(dex_usage~ afib_type,  cohort3)
?mytable

rst <- cohort3[!is.na(afib_type),.N, by=c('dex_usage','afib_type')][,pct:=round(N/sum(N)*100,1), by=dex_usage][order(-afib_type)]
rst[,y_label := cumsum(pct) - 0.5*pct, by='dex_usage']
require(ggplot2)
rst %>% ggplot(aes(x=dex_usage, y=pct, fill=afib_type))+
  geom_col(width=.5)+
  geom_text(aes(y=y_label, label=paste0(pct,'%','\n','(n=',N,')')))+
  scale_x_discrete(labels=c('No','Yes'))+
  theme(legend.position = 'top')

