require(medflex)
require(mediation)
data('UPBdata')
head(UPBdata)

data('framing')
head(framing)

med.fit <- lm(emo ~ treat  + age + educ + gender + income, framing)
out.fit <- glm(cong_mesg ~ emo + treat + age +educ + gender + income, 
               framing, family=binomial(link = 'probit'))

# ACME: Average Causal Mediation Effects (= Indirect Effects)
# ADE: Average Direct Effects (= Direct Effects)

med.out <- mediate(med.fit, out.fit, treat="treat", mediator = "emo",
                   robustSE = T, 
                   sims = 100 # simulation
                   )
summary(med.out)
# moderated mediation 
# 1. set covariate
med.age20 <- mediate(med.fit, out.fit, treat='treat', mediator='emo',
                     covariates = list(age=20), sims=100)

med.age60 <- mediate(med.fit, out.fit, treat='treat', mediator='emo',
                     covariates = list(age=60), sims=100)

# 2. test.modmed
med.init <- mediate(med.fit, out.fit, treat='treat', 
                    mediator='emo',sims=2)
test.modmed(med.init, covariates.1 = list(age=20),
            covariates.2 = list(age=60),sims=100)

summary(med.age60)

summary(med.out)
plot(med.out)

# 3. sensitivity analysis
# Only if no moderation effect 
sens.out <- medsens(med.out, rho.by = 0.1, effect.type = 'indirect',sims=100)
summary(sens.out)


med.fit <- glm(afib_within_7days ~ heart_surgery*dex_usage + sex + age, data=bestDf,
               family=binomial())
out.fit <- glm(inhos_mortality ~ afib_within_7days + heart_surgery*dex_usage + sex + age,
               data=bestDf,
               family=binomial())

med.dex0 <- mediate(med.fit, out.fit, treat='heart_surgery', mediator='afib_within_7days',
                     covariates = list(dex_usage=0), sims=100)

med.dex1 <- mediate(med.fit, out.fit, treat='heart_surgery', mediator='afib_within_7days',
                     covariates = list(dex_usage=1), sims=100)


summary(med.dex0)
summary(med.dex1)

med.out <- mediate(med.fit, out.fit, treat='heart_surgery', mediator='afib_within_7days',
                   robustSE=T,
                   boot = T,
                   sims=100)

summary(med.out)[[40]]
plot(med.out)

test.modmed(med.out,
            covariates.1 = list(dex_usage=0),
            covariates.2 = list(dex_usage=1),
            sims = 100)

test.TMint(med.out, conf.level = .95)
summary(med.out)
plot(med.out)


listReg <- function(data, regType, strata, formula){
  L <- list()
  rst <- by(data, data[[strata]],function(sub_df) {
    if(regType=='logistic') {
      lm <- glm(formula, data=sub_df, family=binomial())
    } else {
      xx <- coxph(Surv(data[[duration]], data[[y]]) ~ data[[x]], data=sub_df)
    }
  })
  for (i in (names(rst))){
    L[[i]] <- tbl_regression(rst[[i]], exponentiate = T,)
  }
  return(L)
}

listRe(g)

by(bestDf, bestDf$sex, function(sub_df){
  glm(afib_within_7days ~ dex_usage, 
      data=sub_df,family=binomial())
})


x <- listReg(bestDf, regType = 'logistic',strata= 'surgery',
        formula = 'afib_within_7days ~ dex_usage')
tbl_merge(x)
y <- listReg(bestDf, regType = 'cox', strata=bestDf$sex,
             formula = )
L[[1]] <-  tbl_regression(x$F, exponentiate = T)
L[[2]] <- tbl_regression(x$M, exponentiate = T)
t1 <- tbl_regression(x$F, exponentiate = T)
t2 <- tbl_regression(x$M, exponentiate = T)

x[['F']]
L <- list()
for (i in (names(x))){
  L[[i]] <- tbl_regression(x[[i]], exponentiate = T)
}
tbl_merge(tbls = L,tab_spanner = names(x)) %>% 
  as_gt() %>%
  tab_header('hi') %>% 
  tab_options(table.font.names = 'Times New Roman')

cat(paste(L, collapse = '\n'))
summary(temp[[1]])[[8]]
summary(temp[[2]])[[8]][1]
