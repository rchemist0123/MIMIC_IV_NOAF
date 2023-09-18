devtools::install_github('paul-buerkner/brms')
install.packages('brms')
options("install.lock"=FALSE)
require(readr)
chimp <- read_delim('https://raw.githubusercontent.com/rmcelreath/rethinking/master/data/chimpanzees.csv',
           delim=';')
chimp
require(brms)
data('kidney')
head(kidney)
bestMatch$inhos_mortality
require(splines2)
bayes_fit <- brm(inhos_duration | cens(inhos_mortality) ~ dex_usage|apache_group,
                 data=bestMatch)

summary(bayes_fit,waic=T,bic=T)
require(dplyr)
bayes_fit %>% posterior_interval(prob=c(0.9))

bayes_fit %>% posterior_summary()

conditional_effects(bayes_fit)
pp_check(bayes_fit, type='ecdf_overlay')
head(airquality)
coalesce(airquality$Ozone,airquality$Solar.R, airquality$Wind) %>% head() %>% sum()
