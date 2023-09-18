-- afib 발생자 분류 cohort
-- drop table afib_cohort;
-- create table mimic_derived.afib_cohort as
with cohort as (
    select temp2.*, id.subject_id, id.hadm_id, weight, i2.first_careunit,
       id.dod, id.gender, round(id.admission_age,0) as age, id.ethnicity,
           anchor_year_group,id.icu_outtime, id.admittime, id.dischtime, id.los_hospital, id.los_icu,
       case when afib_onset_time >= temp2.icu_intime then 1 else 0 end as afib_newonset
    from (
        select temp.stay_id, icu_intime,
               charttime[array_position(heart_rhythm, 'AF (Atrial Fibrillation)')] as afib_onset_time,
               charttime[array_position(heart_rhythm, 'A Flut (Atrial Flutter) ')] as afl_onset_time
        from (
            select distinct on (r.subject_id)
                   r.subject_id,r.stay_id, id.icu_intime,
                   array_agg(charttime order by charttime) charttime,
                   array_agg(heart_rhythm order by charttime) heart_rhythm
            from rhythm r
            inner join icustay_detail id on r.stay_id = id.stay_id
            group by r.subject_id, r.stay_id, id.icu_intime
            order by subject_id,icu_intime
                 ) temp
             ) temp2
    inner join first_day_weight fdw on temp2.stay_id = fdw.stay_id
    inner join icustay_detail id on temp2.stay_id = id.stay_id
    inner join icustays i2 on temp2.stay_id = i2.stay_id
    inner join mimic_core.patients p on p.subject_id = id.subject_id

)
, height as (
    select stay_id,
       (array_agg(height order by charttime))[1] as height
    from height
    -- where charttime < datetime_add(charttime, interval '48' hour)
    group by stay_id
)
, vital as (
    select fdv.stay_id,
           heart_rate_min, heart_rate_max,
           mbp_min as map_min, mbp_max as map_max,
           resp_rate_min, resp_rate_max,
           temperature_min, temperature_max,
           spo2_min, spo2_max,
           heart_rate_mean, mbp_mean as map_mean,
           resp_rate_mean, temperature_mean, spo2_mean
    from first_day_vitalsign fdv
--     inner join cohort c on c.stay_id = fdv.stay_id
)
, lab as (
    select stay_id,
           wbc_min, wbc_max,
           hemoglobin_min, hemoglobin_max,
           platelets_min, platelets_max,
           sodium_min, sodium_max,
           calcium_min ,calcium_max,
           potassium_min, potassium_max,
           chloride_min, chloride_max,
           creatinine_min, creatinine_max,
           bun_min, bun_max,
           bicarbonate_min, bicarbonate_max,
           magnesium_min, magnesium_max
    from first_day_lab
)
, bg as (
    select temp2.stay_id,
       coalesce(fdba.lactate_min, temp2.lactate_min, null) as lactate_min,
       coalesce(fdba.lactate_max, temp2.lactate_max, null) as lactate_max,
       coalesce(fdba.ph_min, temp2.ph_min, null) as ph_min,
       coalesce(fdba.ph_max, temp2.ph_max, null) as ph_max,
       coalesce(fdba.po2_min, temp2.po2_min, null) as po2_min,
       coalesce(fdba.po2_max, temp2.po2_max, null) as po2_max,
       coalesce(fdba.pco2_min, temp2.pco2_min, null) as pco2_min,
       coalesce(fdba.pco2_max, temp2.pco2_max, null) as pco2_max

    from (
        select min(stay_id) stay_id,
            coalesce(min(valuenum) filter ( where item = 'lactate'), null) as lactate_min,
            coalesce(max(valuenum) filter ( where item = 'lactate'),null) as lactate_max,
            coalesce(min(valuenum) filter ( where item = 'po2'),null) as po2_min,
            coalesce(max(valuenum) filter ( where item = 'po2'),null) as po2_max,
            coalesce(min(valuenum) filter ( where item = 'pco2'),null) as pco2_min,
            coalesce(max(valuenum) filter ( where item = 'pco2'),null) as pco2_max,
            coalesce(min(valuenum) filter ( where item = 'ph'),null) as ph_min,
            coalesce(max(valuenum) filter ( where item = 'ph'),null) as ph_max
        from (
            select stay_id,
                   case when itemid = '225668' then 'lactate'
                       when itemid = '220224' then 'po2'
                       when itemid = '220235' then 'pco2'
                       when itemid = '223830' then 'ph' end as item,
                    valuenum
            from chartevents
            where itemid in ('225668','220224','220235','223830')
                AND charttime < datetime_add(charttime, interval '24' hour )
                          ) temp
        group by stay_id
             ) temp2
    inner join first_day_bg_art fdba on fdba.stay_id = temp2.stay_id
)
, sepsis as (
    select s.stay_id, sepsis3 as sepsis
    from sepsis3 s
    order by s.stay_id
)
, sofa as (
select stay_id, sofa as sofa_score
from first_day_sofa
)
, ventil as (
    select min(stay_id) as stay_id,
           (array_agg(starttime order by starttime))[1] as starttime,
           (array_agg(endtime order by endtime desc))[1] as endtime
    from ventilation
    group by stay_id
)
, crrt as (
    select min(stay_id) as stay_id,
           (array_agg(charttime order by charttime))[1] as starttime
    from crrt
    group by stay_id
)
, cardioversion as (
    select distinct on (p.stay_id)
    p.stay_id as stay_id,
      case when starttime is not null then 1 else 0 end as cardioversion
    from procedureevents p
    inner join cohort c on p.stay_id = c.stay_id
    where itemid = '225464'
    and c.icu_intime <= p.starttime
)
, hs as (
    select p.hadm_id, chartdate
    from procedures_icd p
    inner join cohort c on c.hadm_id = p.hadm_id
    where icd_code in (select icd_code from d_icd_procedures where long_title like '%heart') or
          icd_code in (select icd_code from d_icd_procedures where long_title like '%Heart')
    and p.chartdate <= c.icu_intime
)
, dex as (
    select min(stay_id) stay_id,
           (array_agg(i.starttime order by i.starttime))[1] as starttime
    from inputevents i
    where itemid in ('229420','225150')
    group by stay_id
)
, cci as (
    select ii.stay_id, charlson_comorbidity_index as cci
    from charlson c
    inner join icustays ii on ii.hadm_id = c.hadm_id
)
, saps as (
    select stay_id, sapsii
    from sapsii
)
, vis as (
    select min(temp2.stay_id) stay_id,
       max(norepinephrine*100 + epinephrine*100 +
           dobutamine + dopamine +
           milrinone * 10 +
           vasopressin*10000) as vis

    from (
        select stay_id, starttime,
               sum(norepinephrine) norepinephrine,
               sum(epinephrine) epinephrine,
               sum(dobutamine) dobutamine,
               sum(dopamine) dopamine,
               sum(milrinone) milrinone,
               sum(vasopressin) vasopressin

        from (
            select i.stay_id,
                   substring(starttime::varchar,1,13) as starttime,
            --                starttime,
            --                extract(epoch from age(i.endtime, i.starttime)/60) as duration_min,
                           case when itemid = '221906' then  rate else 0 end as norepinephrine,
                           case when itemid = '221289' then rate else 0 end as epinephrine,
                           case when itemid = '221653' then rate else 0 end as dobutamine,
                           case when itemid = '221662' then rate else 0 end as dopamine,
                           case when itemid = '221986' then rate else 0 end as milrinone,
                           case when itemid = '222315' then rate/i.patientweight/60 else 0 end as vasopressin,
                   rate, rateuom
            from inputevents i
            inner join cohort c on c.stay_id = i.stay_id
            where itemid in ('221906','221289', '221653', '221662','221986','222315')
                      AND i.stay_id in (select stay_id from sepsis3)
                      AND i.starttime>=c.icu_intime
                      AND i.starttime < DATETIME_ADD(c.icu_intime, INTERVAL '24' HOUR )
                 ) temp
        group by stay_id, starttime
                      ) temp2
    group by stay_id
)
, trt as (
    select distinct on (i.stay_id)
           min(i.stay_id) stay_id,
           case when i.itemid = '225974' and avg(rate) > 0 then 1 else 0 end as metoprolol,
           case when i.itemid = '221429' and avg(rate) > 0 then 1 else 0 end as esmolol,
           case when i.itemid = '221468' and avg(rate) > 0 then 1 else 0 end as diltiazem,
           case when i.itemid = '227440' and avg(rate) > 0 then 1 else 0 end as digoxin,
           case when i.itemid in ('222011','227523','227524') and avg(rate) > 0 then 1 else 0 end as magnesium,
           case when i.itemid = '221347' and max(rate) > 0 then 1 else 0 end as amiodarone
    from inputevents i
    inner join cohort c on c.stay_id = i.stay_id
    where itemid in ('225974','221429','221468','227440','222011','227523','227524')
        AND i.starttime>= c.afib_onset_time

    group by i.stay_id, i.itemid
)
, pills as (
    select min(c.stay_id) as stay_id,
           case when drug = 'Amiodarone' then 1 else 0 end as amiodarone2,
           case when drug = 'Sotalol' then 1 else 0 end as sotalol,
           case when drug = 'Dronedarone' then 1 else 0 end as dronedarone,
           case when drug = 'Flecainide' then 1 else 0 end as flecainide,
           case when drug = 'propafenone' then 1 else 0 end as propafenone,
           case when drug = 'Quinidine' then 1 else 0 end as quinidine,
           case when drug like '%Magnesium%' then 1 else 0 end as magnesium2
    from prescriptions p
    inner join cohort c on c.hadm_id = p.hadm_id
    AND drug in ('Flecainide','propafenone','Quinidine','Sotalol','Dronedarone','Amiodarone')
    AND p.starttime >= c.afib_onset_time
    group by drug, stay_id
)
, bradyhypo as (
    select stay_id,
           max(brady) brady, max(brady2) brady2,
           max(hypo) hypo, max(hypo2) hypo2
    from (
        select t.stay_id,
               case when variable='hr' and min(value::numeric) <50.0 then 1 else 0 end as brady,
               case when variable='sbp' and min(value::numeric) <90.0 then 1 else 0 end as hypo,
               case when variable='hr' and min(value::numeric) <60.0 then 1 else 0 end as brady2,
               case when variable='sbp' and min(value::numeric) <80.0 then 1 else 0 end as hypo2
        from (
            select ce.stay_id,
                   case when itemid='220045' then 'hr'
                        when itemid='225309' then 'sbp' end as variable,
                   value
            from chartevents ce
            inner join cohort c on c.stay_id = ce.stay_id
            where itemid in ('220045','225309') and charttime >= c.icu_intime
                 ) t
        group by stay_id, variable
        ) t2
    group by stay_id
)

select distinct on (c.stay_id) c.stay_id, c.subject_id, c.hadm_id,
                               anchor_year_group, icu_intime,icu_outtime, admittime, dischtime,
       afib_onset_time, weight, height, first_careunit, dod, gender, age, ethnicity,  los_hospital, los_icu, afib_newonset,

       --vital
        heart_rate_min, heart_rate_max, map_min, map_max, resp_rate_min, resp_rate_max, temperature_min, temperature_max,spo2_min, spo2_max,
       heart_rate_mean, map_mean, resp_rate_mean, temperature_mean, spo2_mean,

       --lab
        wbc_min, wbc_max, hemoglobin_min, hemoglobin_max, platelets_min, platelets_max, sodium_min, sodium_max,
       calcium_min ,calcium_max, potassium_min, potassium_max, chloride_min, chloride_max, creatinine_min, creatinine_max,
       bun_min, bun_max, bicarbonate_min, bicarbonate_max, magnesium_min, magnesium_max,
       ph_min, ph_max, po2_min, po2_max, pco2_min, pco2_max, lactate_min, lactate_max,

        -- drugs
       coalesce(trt.digoxin,0) digoxin,
       coalesce(trt.diltiazem,0) diltiazem,
       coalesce(trt.esmolol,0) esmolol,
       coalesce(trt.metoprolol,0) metoprolol,
       coalesce(trt.magnesium,0) magnesium,
       coalesce(trt.amiodarone,0) amiodarone,
       coalesce(pills.amiodarone2,0) amiodarone2,
       coalesce(pills.magnesium2,0) magnesium2,
       coalesce(pills.Dronedarone,0) dronedarone,
       coalesce(pills.Quinidine,0) quinidine,
       coalesce(pills.sotalol,0) sotalol,
       coalesce(pills.Flecainide,0) flecainide,
       coalesce(pills.Propafenone,0) propafenone,

       case when c.stay_id in (select stay_id from dex) then 1 else 0 end as dexmedetomidine,
       dex.starttime as dex_starttime,
       case when c.stay_id in (select stay_id from sepsis) then 1 else 0 end as sepsis,
       sofa.sofa_score, sapsii,
       case when c.stay_id in (select stay_id from ventil)
            and ventil.starttime <= datetime_add(c.icu_intime, interval '24' HOUR) then 1 else 0 end as ventilation,
       case when c.stay_id in (select stay_id from crrt)
            and crrt.starttime <= datetime_add(c.icu_intime, interval '24' HOUR) then 1 else 0 end as crrt,
       coalesce(cardioversion.cardioversion,0) cardioversion,
       case when c.hadm_id in (select hadm_id from hs) then 1 else 0 end as heart_surgery,
       coalesce(vis,0) as vis, cci, brady, hypo, brady2, hypo2
from cohort c
left join height h on h.stay_id = c.stay_id
left join lab on lab.stay_id = c.stay_id
left join bg on bg.stay_id = c.stay_id
left join vital on vital.stay_id = c.stay_id
left join sofa on sofa.stay_id = c.stay_id
left join crrt on crrt.stay_id = c.stay_id
left join ventil on ventil.stay_id = c.stay_id
left join hs on hs.hadm_id = c.hadm_id
left join cardioversion on cardioversion.stay_id = c.stay_id
left join dex on dex.stay_id = c.stay_id
left join cci on cci.stay_id = c.stay_id
left join saps on saps.stay_id = c.stay_id
left join vis on vis.stay_id = c.stay_id
left join trt on trt.stay_id = c.stay_id
left join pills on pills.stay_id = c.stay_id
left join bradyhypo on bradyhypo.stay_id = c.stay_id;
-- where exclusion=0; -- icu 입실 이전 afib 발생


select * from afib_cohort;

select v.stay_id, v.starttime, ac.icu_intime, ac.afib_onset_time,
       case when starttime < datetime_add(icu_intime, interval '24' HOUR) then 1 else 0 end as MV_DAY1,
       case when starttime < datetime_add(icu_intime, interval '48' HOUR) then 1 else 0 end as MV_DAY2,
       case when starttime < datetime_add(icu_intime, interval '72' HOUR) then 1 else 0 end as MV_DAY3,
       case when starttime < datetime_add(icu_intime, interval '96' HOUR) then 1 else 0 end as MV_DAY4,
       case when starttime < datetime_add(icu_intime, interval '120' HOUR) then 1 else 0 end as MV_DAY5,
       case when starttime < datetime_add(icu_intime, interval '144' HOUR) then 1 else 0 end as MV_DAY6,
       case when starttime < datetime_add(icu_intime, interval '48' HOUR) then 1 else 0 end as MV_DAY7

from ventilation v

left join afib_cohort ac on ac.stay_id = v.stay_id
order by ac.stay_id;

select * from prescriptions
where drug like '%Amiodarone%';

select * from mimic_core.patients;


select stay_id,
       max(brady) brady,
       max(hypo) hypo
from (
    select ce.stay_id,
                case when itemid = '220045'  then 1 else 0 end as brady,
                case when itemid = '225309'  then 1 else 0 end as hypo
    from chartevents ce
         ) t
group by stay_id;

select stay_id, max(brady) brady,
       max(hypo) hypo
from (
    select stay_id,
           case when variable='hr' and min(value::integer) <50 then 1 else 0 end as brady,
           case when variable='sbp' and min(value::integer) <90 then 1 else 0 end as hypo
    from (
        select stay_id,
               case when itemid='220045' then 'hr'
                    when itemid='225309' then 'sbp' end as variable,
               value
        from chartevents
        where itemid in ('220045','225309')
             ) t
    group by stay_id, variable
    ) t2
group by stay_id;

    select * from chartevents
    where itemid =225309; -- SBP
    select * from d_items
    where label like '%Heart%'

-- afib 약물
drop table afib_drugs;
-- af_sedatives
select stay_id,
       drugname,
       sum(rate) rate,
       min(rateuom) rate_unit,
       sum(amount) amount,
       min(amountuom) amount_unit,
       max(endtime) - min(starttime) as dur,
       extract(epoch from age(max(endtime),min(starttime))/3600) as duration_hour
from (
    select i.stay_id
         ,case
            when itemid = '222168' then 'propofol'
            when itemid = '225154' then  'morphine'
            when itemid = '221712' then 'ketamin'
            when itemid in ('221744','225942','225972') then 'fentanyl' else 'midazolam'
            end as drugname,
           amount,
           amountuom,
           rate,
           rateuom,
           starttime, endtime
    from inputevents i
    left join afib_cohort ac on ac.stay_id = i.stay_id
    where itemid in ('222168','225154','221712', '221744','225942','225972', '221668')
      and i.starttime >= ac.icu_intime
    and i.starttime < datetime_add(ac.icu_intime, interval '7' day)
--         group by i.stay_id, itemid
         ) as temp
group by stay_id, drugname;

select * from d_items
where label like '%Fenta%';
