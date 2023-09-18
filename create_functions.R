

# table 3 만들기 ----------------------------------------------------------


fit1 <- coxph(Surv(obs_duration, afib_within_7days==1)~icuunit, match_data)
fit2 <- coxph(Surv(obs_duration, afib_within_7days==1)~icuunit+ sex + age, match_data)
fit3 <- coxph(Surv(obs_duration, afib_within_7days==1)~icuunit + sex + age + dex_usage + sepsis, match_data)

summary(fit2)[[8]][]

# factor 변수의 level 길이
length(levels(match_data$icuunit))

# 변수의 개수
match_data[['race']]
createTable3 <- function(mainVar, fit1, fit2, fit3, data){
  require(data.table)
  t1 <- summary(fit1)[[8]] %>% as.data.table()
  t2 <- summary(fit2)[[8]] %>% as.data.table()
  t3 <- summary(fit3)[[8]] %>% as.data.table()
  
  p1 <- summary(fit1)[[7]] %>% as.data.table()
  p2 <- summary(fit2)[[7]] %>% as.data.table()
  p3 <- summary(fit3)[[7]] %>% as.data.table()
  
  refs <- as.data.table("1 (Ref.)")
  varnames <- levels(data[[mainVar]]) %>% as.data.table()
  names(varnames) <- mainVar
  varLen <- length(levels(data[[mainVar]]))
  t1[,model1 := paste0(format(round(`exp(coef)`,3),nsmall=3),' (',
                    format(round(`lower .95`,3),nsmall=3),'-',
                    format(round(`upper .95`,3),nsmall=3),')')]
  p1[,p:= ifelse(`Pr(>|z|)`<0.001,'<0.001',format(round(`Pr(>|z|)`,3),nsmall=3))]
  t1_1 <- bind_rows(refs, t1)
  p1_1 <- bind_rows(refs, p1)
  t1_new = cbind(t1_1[,.(model1)], p1_1[,.(p)])
  t1_new2 = cbind(varnames, t1_new)
  
  t2[,model2 := paste0(format(round(`exp(coef)`,3),nsmall=3),' (',
                    format(round(`lower .95`,3),nsmall=3),'-',
                    format(round(`upper .95`,3),nsmall=3),')')]
  p2[,p:= ifelse(`Pr(>|z|)`<0.001,'<0.001',format(round(`Pr(>|z|)`,3),nsmall=3))]
  
  t2_1 <- bind_rows(refs, t2)
  p2_1 <- bind_rows(refs, p2)
  t2_new = cbind(t2_1[1:varLen,.(model2)], p2_1[1:varLen,.(p)])
  t3[,model3 := paste0(format(round(`exp(coef)`,3),nsmall=3),' (',
                    format(round(`lower .95`,3),nsmall=3),'-',
                    format(round(`upper .95`,3),nsmall=3),')')]
  p3[,p:= ifelse(`Pr(>|z|)`<0.001,'<0.001',format(round(`Pr(>|z|)`,3),nsmall=3))]
  t3_1 <- bind_rows(refs, t3)
  p3_1 <- bind_rows(refs, p3)
  t3_new = cbind(t3_1[1:varLen,.(model3)], p3_1[1:varLen,.(p)])
  
  result = cbind(t1_new2,t2_new, t3_new)
  message('** The result of the < Table 3 > is **\n')  
  return(result)
}
createTable3(mainVar='icuunit',fit1,fit2,fit3)


# multivariate regression ----------------------------------------
multiRegression <- function(fit){
  p <- summary(fit)[[7]] %>% as.data.table(keep.rownames = T)
  setnames(p,"Pr(>|z|)",'p')
  p <- p[,.(rn,p)]
  p[,p:=ifelse(p<0.001,'<0.001',format(round(p,3),nsmall=3))]
  
  hr <- summary(fit)[[8]] %>% as.data.table(keep.rownames = T)
  names(hr) <- c('rn','HR','tete','lower','upper')
  hr[,`HR (95% CI)` := paste0(format(round(HR,2),nsmall=2),
                              ' (',
                              format(round(lower,2),nsmall=2), '-',
                              format(round(upper,2),nsmall=2),')')]
  hr2 <- hr[,.(rn,`HR (95% CI)`)]
  return(inner_join(hr2,p, by='rn'))
}
# %>% 
#   DT::datatable(
#     colnames=c('Variable', 'HR (95% CI)', 'P value'),
#     options = list(dom='Bftrip',button='copy'),
#     extensions='Buttons')
multiRegression(fit)


# 변수별 class 출력 테이블 --------------------------------------------------------

classTable <- function(x){
  varname <- c()
  classname <- c()
  for (i in colnames(x)){
    varname <- c(varname,i)
    classname <- c(classname, class(x[[i]]))
  }
  tab <- as.data.frame(cbind(varname,classname))
  colnames(tab) <- c('Variable','Class')
  return(tab)
}
classTable(df_match)



# subgroup 분석결과 -----------------------------------------------------------

require(Publish)
subgroupAnalTable <- function(fit, trt, subgroup, data = data){
  require(gt)
  rst <- subgroupAnalysis(object = fit, data = data,
                          treatment = trt,
                          subgroups = subgroup)
  result <- rst %>% as.data.table()%>% 
    mutate(`HR (95% CI)` = paste0(format(round(HazardRatio,2),nsmall=2), 
                                  ' (', format(round(Lower,2),nsmall=2), '-', 
                                  format(round(Upper,2),nsmall=2),
                                  ')'),
           control = paste0(event_0,'/',sample_0),
           case = paste0(event_1,'/',sample_1)) 
  
  return (gt(result[,.(subgroups, level, `Non-DEX` = control, DEX = case, `HR (95% CI)`, 
                       hr = format(round(HazardRatio,2),nsmall=2), 
                       lower = format(round(Lower,2),nsmall=2), 
                       upper = format(round(Upper,2),nsmall=2), 
                       Pint=format(round(pinteraction,2),nsmall=2))]))
}


subgroupResult <- function(strata, time, outcome, data, vars, main){
  
  require(data.table)
  require(Publish)
  
  # cox regression part 
  
  rst <- by(data, data[[strata]],function(sub_df){
    form <- paste(main,"+", vars, collapse=' + ')
    forms <- paste('Surv(', time, ',', outcome, '==1)', '~', form)
    fit <- coxph(as.formula(forms) , data=sub_df)
    
    t1 <- summary(fit)[[8]] %>% as.data.table()
    p1 <- summary(fit)[[7]] %>% as.data.table()
    
    varnames <- levels(sub_df[[main]]) %>% as.data.table()
    names(varnames) <- 'level'
    varLen <- length(levels(sub_df[[main]]))
    t1[,estimate := paste0(format(round(`exp(coef)`,2),nsmall=2),' (',
                         format(round(`lower .95`,2),nsmall=2),'-',
                         format(round(`upper .95`,2),nsmall=2),')')]
    p1[,p:= ifelse(`Pr(>|z|)`<0.001,'<0.001',format(round(`Pr(>|z|)`,3),nsmall=3))]
    tp <- bind_cols(t1[,.(estimate)],p1[,.(p)])
    refs <- data.table('estimate'="1 (Ref.)", 'p'="")
    tp_new <- bind_rows(refs, tp)
    tp_new <- tp_new[1:varLen]
    # tp_new2 = bind_cols(varnames, tp_new)
  })
  
  # subgroupAnalysis Part
  tabs <- data.table(do.call(bind_rows,rst))
  form <- paste(strata,"+", vars , collapse=' + ')
  forms <- paste('Surv(', time, ',', outcome, '==1)', '~', form)
  fit <- coxph(as.formula(forms) , data=data)
  subRst <- setDT(subgroupAnalysis(object=fit,
                             treatment= strata,
                             subgroups = main,
                             data=data))
  
  # strata의 factor 레벨 수대로 데이터 추출후 합치기
  level <- levels(data[[strata]])
  l <- list()
  for(i in level){
    target <- names(subRst[,names(subRst) %like% i, with=F])
    merge_target <- c('level',target,'pinteraction')
    l[[i]] <- subRst[,..merge_target]
    names(l[[i]]) <- c('level', 'n','event','duration','pint')
  }
  rst2 <- data.table(do.call(bind_rows,l))
  rst2[,rate := round(event/duration*1000,2)]
  final_result <- bind_cols(rst2,tabs)
  subgroup <- as.data.table(rep(levels(data[[strata]]),each=length(levels(data[[main]]))))
  names(subgroup) <- "subgroup"
  final <- bind_cols(subgroup,final_result)
  return(final[,.(subgroup,level,n,event,duration,rate,estimate,pint=format(round(pint,3),nsmall=3))])
}

subgroupResult(strata = 'ethnicity',
               time='obs_duration',
               data=df_match2,
               outcome='afib_within_7days',
               vars = c('dex_usage','age','sepsis'),
               main='icuunit')
target <- names(subRst[,names(subRst) %like% 'Female',with=F])
rst2
l <- list()
data.table(do.call(bind_rows,l))

l
target
subRst
merge_target <- c('level',target)
length(c('level',target))
subRst[,..merge_target]

user_topic <- function(data, strata, target, fun='mean'){
  #' @param fun select descriptive function (e.g., mean, min, max)
  # data: data
  # strata: 그룹별로 나눌 열 (column)
  # target: 최댓값을 계산하고자 하는 열들 (columns).
  data$Topic <- as.factor(data$Topic) # factor 변환
  topic.levels <- levels(data$Topic) # Topic의 레벨: economy, microsoft
  mylist <- list() # 결과들을 담을 리스트 생성
  
  for (i in topic.levels){
    dat <- switch(i, data[data[[strata]]==i,]) # Topic 선택: economy or microsoft
    for (j in target){ 
      value <- switch(fun, 
                      mean = mean(dat[[j]],na.rm=T),
                      min = min(dat[[j]],na.rm=T),
                      max = max(dat[[j]],na.rm=T),
                      median = median(dat[[j]],na.rm=T))
      mylist[[i]][j] <- value # 리스트에 결과물 저장
    }
  }
  rst <- as.data.frame(do.call(bind_rows,mylist)) # 리스트를 데이터 프레임으로 변환
  rownames(rst) <- topic.levels # 행의 이름 지정
  return(rst)
}
