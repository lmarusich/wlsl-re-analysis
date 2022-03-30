library(dplyr)
library(tidyr)
#need estimateDataAll


#need to figure out the unit conversion for distance
mydat <- estimateDataAll %>%
  mutate(dist_meters = ifelse(estimate_type == "Distance" & units_used == "feet",
                           subject_estimate/3.281,
                           subject_estimate),
         estimate_err = ifelse(estimate_type == "Distance", dist_meters - stim_distance,
                               ifelse(estimate_type == "Heading", abs(subject_estimate) - abs(stim_heading),
                                      subject_estimate - stim_number)),
         subject = as.factor(subject))

oldsubs <- sort(levels(mydat$subject))
newsubs <- paste("sub",1:length(oldsubs),sep = "")

mydat$subject <- mydat$subject %>%
  fct_relabel(~newsubs[match(.x,oldsubs)])

  #        )
  #
  #
  #
  # mutate(estimate_err = ifelse(estimate_type == "Distance", dist_meters - stim_distance,
  #                              ifelse(estimate_type == "Heading", abs(subject_estimate) - abs(stim_heading),
  #                              subject_estimate - stim_number)))

numdat <- mydat %>% filter(estimate_type == "Number")
headdat <- mydat %>% filter(estimate_type == "Heading")

numdat <- numdat %>%
  mutate(iscorrect = ifelse(stim_number == subject_estimate, 1, 0))


b <- numdat %>% select(subject,condition,stim_distance,stim_heading, stim_number,
                       estimate_type,subject_estimate,estimate_err)

plot(numdat$stim_distance, numdat$estimate_err)
hist(numdat$estimate_err)


bysub <- numdat %>% group_by(subject, stim_distance) %>%
  summarize(mean_dev = mean(abs(estimate_err)))

a <- numdat %>%
  select(subject,condition, stim_distance, estimate_err) %>%
  mutate(abs_err = abs(estimate_err)) %>%
  pivot_wider(names_from = stim_distance, values_from = abs_err, values_fn = mean, id_cols = subject) %>%
  select(subject,`15`,`25`,`50`,`75`,`100`,`150`,`200`)

write.csv(a,file = "number_estimates.csv", row.names = F)

boxplot(numdat$estimate_err ~ numdat$subject)
boxplot(headdat$estimate_err ~ headdat$subject)

filterednum <- numdat %>% filter(subject != "sub15" & subject != "sub2"& subject != "sub3") %>%
  filter(abs(estimate_err) < 12)

boxplot(filterednum$estimate_err ~ filterednum$subject)

a <- filterednum %>%
  select(subject,condition, stim_distance, estimate_err) %>%
  mutate(abs_err = abs(estimate_err)) %>%
  pivot_wider(names_from = stim_distance, values_from = abs_err, values_fn = mean, id_cols = c(subject,condition)) %>%
  select(subject, condition, `15`,`25`,`50`,`75`,`100`,`150`,`200`)

write.csv(a,file = "filtered_number_estimates.csv", row.names = F)

iscorrect_summary <- numdat %>%
  group_by(subject) %>%
  summarize(mean_iscorrect = mean(iscorrect))






