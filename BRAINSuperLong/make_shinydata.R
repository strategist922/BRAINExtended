## -- Data management no matter what ---------------------------------------------------------------
load('/home/thomps23/ICUDelirium/BRAINICU/braindata.Rdata')

library(tidyverse)

## -- Create data set: One record per time point with RBANS scores, with age, IQCODE merged on -----
## We only want to look at patients with 4/6y outcomes
pts.4or6 <- unique(c(subset(brain.fu, fu.period == '4 year' & !is.na(rbans.global.score))$id,
                     subset(brain.fu, fu.period == '6 year' & !is.na(rbans.global.score))$id))

test.data <- brain.fu %>%
  filter(fu.period != '12 Month Mortality' & id %in% pts.4or6) %>%
  left_join(subset(brain.oneobs, select = c(id, age.enroll, iqcode.score.e)), by = 'id') %>%
  mutate(time.pt = ifelse(fu.period == '3 Month', 90,
                   ifelse(fu.period == '12 Month', 365,
                   ifelse(fu.period == '4 year', 365*4,
                   ifelse(fu.period == '6 year', 365*6, NA))))) %>%
  select(id, age.enroll, iqcode.score.e, fu.period, time.pt, test.score = rbans.global.score)

save(test.data, file = 'data/shinydata.Rdata')

# ## -- Options: -------------------------------------------------------------------------------------
# ## What IQCODE level should MCI be classified as?
# mci.iqcode <- 3.0
# 
# ## Should age be categorized? If so, how?
# cat.age <- 'None' ## None; <50, 50-65, >65
# 
# test.data$mci.baseline <- with(test.data, factor(iqcode.score.e > mci.iqcode,
#                                                  labels = c('Normal', 'MCI')))
# 
# if(cat.age == 'None'){
#   test.data$age.cat <- factor(1, labels = 'All Patients')
# } else if(cat.age == '<=65/>65'){
#   test.data$age.cat <- with(test.data, factor(ifelse(is.na(age.enroll), NA,
#                                               ifelse(age.enroll <= 65, 1, 2)),
#                                               levels = 1:2, labels = c('<=65', '>65')))
# } else{
#   test.data$age.cat <- with(test.data, factor(ifelse(is.na(age.enroll), NA,
#                                               ifelse(age.enroll <= 50, 1,
#                                               ifelse(age.enroll <= 65, 2, 3))),
#                                               levels = 1:3, labels = c('<=50', '50-65', '>65')))
# }
# 
# ggplot(data = test.data, aes(x = time.pt, y = test.score, group = id)) +
#   facet_grid(age.cat ~ mci.baseline) +
#   annotate(geom = 'rect', ymin = 0, ymax = 78, xmin = 0, xmax = 365*7,
#            fill = '#B85C00', alpha = 0.1) +
#   annotate(geom = 'rect', ymin = 78, ymax = 122, xmin = 0, xmax = 365*7,
#            fill = '#007D51', alpha = 0.1) +
#   geom_line(alpha = 0.2, colour = '#003D79') +
#   scale_x_continuous(breaks = c(90, 365, 365*4, 365*6),
#                      labels = c('3 Months', '12 Months', '4 Years', '6 Years'),
#                      name = NULL) +
#   scale_y_continuous(name = 'Test Score') +
#   theme_bw()
