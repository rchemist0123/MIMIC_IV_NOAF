library(data.table)

df = fread("csv/af_dex_df.csv")
patient = df[,.(stay_id, icu_intime, icu_outtime, dod)]
afib = df[!is.na(afib_onset_time),.(stay_id, afib_onset_time)]
df |> names()
afib

# Patients who has afib within 48 hours after ICU admission
## non-equi join
names(afib)
library(lubridate)
patient[,afib_margin_time := icu_intime + days(7)]

afib
patient
patient[afib, on=.(stay_id, 
                    icu_intime <= afib_onset_time,
                    afib_margin_time > afib_onset_time),
                    .(n=.N), by=.EACHI][n==1]

afib[patient, on=.(stay_id,
                  afib_onset_time >= icu_intime,
                  afib_onset_time < afib_margin_time), .N, by=.EACHI]


df[difftime(afib_onset_time, icu_intime, units="days") %between% c(0,7),.N]

setkey(df, gender, ethnicity)
df[,.(group = .GRP), by=.(gender,ethnicity, is.na(dod))]

