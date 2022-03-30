#clean formatted data
#need "sublist"

library(dplyr)

nsubs <- length(sublist)

#make separate dataframes for each type of data (estimates, visibility, combine demographics and freeresponse)

demographics <- data.frame(subject = character(nsubs),
                        condition = character(nsubs), age = character(nsubs),
                        gender = character(nsubs), mil = character(nsubs),
                        leo = character(nsubs), col = character(nsubs),
                        comments = character(nsubs), total_time = integer(nsubs),
                        stringsAsFactors = F)

for (i in 1:nsubs){
  demographics[i,1] <- sublist[[i]]$demographics$subject[1]
  demographics[i, 2] <- sublist[[i]]$demographics$conditiona[1]
  tempdemo <- strsplit(sublist[[i]]$demographics$responses, ',\"')[[1]]
  tempdemo <- sub('\\{\"Age\":\"', "", tempdemo)
  tempdemo <- sub('Gender\":"', "", tempdemo)
  tempdemo <- sub('Mil\":\"', "", tempdemo)
  tempdemo <- sub('Law\":\"', "", tempdemo)
  tempdemo <- sub('Col\":\"', "", tempdemo)
  tempdemo <- gsub('\"', "", tempdemo)
  tempdemo <- gsub('\\}', "", tempdemo)

  demographics[i, 3:7] <- tempdemo

  tempfreeresp <- sublist[[i]]$freeresponse$responses
  tempfreeresp <- sub('\\{\"Q0\":\"', "", tempfreeresp)
  tempfreeresp <- sub('\"\\}', "", tempfreeresp)
  demographics[i, 8] <- tempfreeresp
  demographics[i, 9] <- sublist[[i]]$freeresponse$time_elapsed
}


estimateDataAll <- NULL
visDataAll <- NULL

for (i in 1:nsubs){
  estimatedata <- sublist[[i]]$estimate %>%
    select(subject, condition = conditiona, stim_distance = distance,
           stim_heading = orientation, stim_number = number,
           estimate_type, rt, subject_estimate = estimate, units_used, nswitches,
           time_in_SL, time_in_WL, firstview, lastview,
           first_instr, browserinfo, windowWidth, windowHeight)

  tempcondition <- estimatedata$condition[1]
  print(tempcondition)

  if (tempcondition != "SL") {
    visdata <- sublist[[i]]$visibility
    visDataAll <- rbind(visDataAll, visdata)
  }

  estimateDataAll <- rbind(estimateDataAll, estimatedata)


}

write.csv(demographics, "WLSL_demographics.csv", row.names = F)
write.csv(estimateDataAll, "WLSL_estimate_data.csv", row.names = F)
write.csv(visDataAll, "WLSL_visibility_test_data.csv", row.names = F)
