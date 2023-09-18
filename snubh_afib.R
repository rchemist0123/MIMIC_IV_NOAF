require(fread)
require(readxl)
a1 <- readxl::read_excel('CDW/cdw_65982_20211008174639_1.xlsx') 
a2 <- readxl::read_excel('CDW/cdw_65982_20211008174703_2.xlsx')
a3 <- readxl::read_excel('CDW/cdw_65982_20211008174726_3.xlsx')
a4 <- readxl::read_excel('CDW/cdw_65982_20211008174750_4.xlsx')
a5 <- readxl::read_excel('CDW/cdw_65982_20211008174813_5.xlsx')
a6 <- readxl::read_excel('CDW/cdw_65982_20211008174836_6.xlsx')
a7 <- readxl::read_excel('CDW/cdw_65982_20211008174859_7.xlsx')
a8 <- readxl::read_excel('CDW/cdw_65982_20211008174923_8.xlsx')
a9 <- readxl::read_excel('CDW/cdw_65982_20211008174947_9.xlsx')
a10 <- readxl::read_excel('CDW/cdw_65982_20211008175011_10.xlsx')
a11 <- readxl::read_excel('CDW/cdw_65982_20211008175027_11.xlsx')


cdw_df <- bind_rows(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) %>% as.data.table()
cdw_df
colnames(cdw_df) <- c('person_id','name','sex','birth','diag_date','store_date',
                      'cat1','cat_attribute','cat_value','record_type')

cdw_df[,.(min(diag_date), max(diag_date))]
cdw_df[tolower(cat_value) %like% 'afib']
cdw_df[tolower(cat_value) %like% 'atrial' | tolower(cat_value) %like% 'afib'][,(.N), by=person_id] %>% summary(V1)
cdw_df[,event_duration := as.numeric(difftime(store_date,diag_date, units='hours'))]
cdw_df[tolower(cat_value) %like% 'atrial' | tolower(cat_value) %like% 'afib',event_duration] %>% summary(event_duration)

cdw_df_afib <- cdw_df[tolower(cat_value) %like% 'atrial' | tolower(cat_value) %like% 'afib']
cdw_df_afib[,first(event_duration),by=person_id] %>% 
  ggplot(aes(x=V1)) + geom_histogram()+
  scale_x_continuous(limits=c(0,1000))+
  labs(x='Afib time to event')
  
cdw_df_afib[,first(event_duration),by=person_id] %>% summary()
# total patient number: 12200
cdw_df[,name,by='person_id']
length(unique(cdw_df$person_id))