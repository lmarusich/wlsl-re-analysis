
library(XML)
library(dplyr)
dat <- xmlParse("fulldata.txt")
rootnode <- xmlRoot(dat)

#how many subjects?
nsubs <- xmlSize(rootnode)

sublist <- list()
index <- 1

# allIDs <- data.frame(completed = integer(46), ID = character(46))
for (i in 1:nsubs){
  # allIDs[i,2] <- xmlToList(rootnode[[i]][[1]])[["uid"]]
  tempnode <- xmlValue(rootnode[[i]][[2]][[1]])
  tempdat <- read.csv(textConnection(tempnode), stringsAsFactors = F)
  if (dim(tempdat)[1] == 0){
  #   allIDs[i,1] <- 0
    next
  }

  # allIDs[i,1] <- 1
  # close(textConnection(tempnode))
  sublist[[index]] <- list()

  subID <- xmlToList(rootnode[[i]][[1]])[["uid"]]
  tempdat <- tempdat %>% mutate(subject = subID)

  sublist[[index]]$estimate <- tempdat %>%
    filter(test_part == "estimate")
  if (tempdat$conditiona[1] != "SL"){
    sublist[[index]]$estimate <- sublist[[index]]$estimate %>%
      select(!(animation_sequence:question_order))
  }

  sublist[[index]]$demographics <- tempdat %>%
    filter(test_part == "demographics") %>%
    select(subject,rt,responses,test_part,conditiona,question_order)

  sublist[[index]]$freeresponse <- tempdat %>%
    filter(test_part == "freeresponse") %>%
    select(subject,rt,responses,test_part,conditiona, time_elapsed)

  # if (tempdat$conditiona[1] != "SL")

  sublist[[index]]$visibility <- tempdat %>%
    filter(test_part == "visibility")
  if (tempdat$conditiona[1] != "SL"){
    sublist[[index]]$visibility <- sublist[[index]]$visibility %>%
      select(subject,rt,test_part,animation_sequence,stimulus,key_press,order,vis_distance)
  }

  index <- index + 1

}
# nsubs <- length(sublist)
# bigdataframe <- sublist[[1]]
# bigdataframe$Subject <- "1"
#
# for (i in 2:nsubs){
#
#   tempdat <- sublist[[i]]
#   tempdat$Subject <- as.character(i)
#   bigdataframe <- rbind(bigdataframe, tempdat)
#
# }

closeAllConnections()
