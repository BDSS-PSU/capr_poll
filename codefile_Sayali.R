###############################################
#### Descriptives for background variables ####
###############################################

rm(list=ls())
data <- load("capr.RData")


#### Create single column that sums the time spent on all pages
capr$tot.time <- NA
for (i in 1:length(capr$tot.time)){
  capr[i,"tot.time"] <- sum(capr[i, 57:82])
}

summary(capr$tot.time)
boxplot(capr$tot.time)


#### Create a column that separates 5 separate ballots
capr$actual.ballot <- NA

for (i in 1:length(capr$tot.time)){
  if (as.character(capr[i, "ballot"]) == "Ballot I"){
    capr[i, "actual.ballot"] <- "Ballot 1"
  } else {
    if (as.character(capr[i, "ballot"]) == "Ballot II" &
       as.character(capr[i, "treat_instruct"]) == "No instructions shown"){
      capr[i, "actual.ballot"] <- "Ballot 2A"
    } else {
      if (as.character(capr[i, "ballot"]) == "Ballot II" &
          as.character(capr[i, "treat_instruct"]) == "Instructions shown"){
        capr[i, "actual.ballot"] <- "Ballot 2B"
      } else {
        if (as.character(capr[i, "ballot"]) == "Ballot III" &
            as.character(capr[i, "treat_instruct"]) == "No instructions shown"){
          capr[i, "actual.ballot"] <- "Ballot 3A"
        } else {
          capr[i, "actual.ballot"] <- "Ballot 3B"
        }
      }
    }
  }
}

capr$ballot.five.cat <- as.factor(capr$actual.ballot)
capr$actual.ballot <- NULL
summary(capr$ballot.five.cat)


#### Create a dataset that removes total time above 30 minutes
data <- capr[-which(capr$tot.time > 1800),]


#### Plots of total time with background variables

## Boxplot with Ballot type
boxplot(data$tot.time ~ data$ballot.five.cat,
        col=topo.colors(5, alpha = 1),
        main="Total time on survey by Ballot type",
        ylab="Time in seconds")


## Boxplot with ideology
boxplot(data$tot.time ~ droplevels(data$ideo5),
        col=heat.colors(6, alpha = 1),
        main="Total time on survey by ideology",
        ylab="Time in seconds")


## Boxplot with party ID
boxplot(data$tot.time ~ droplevels(data$pid3),
        col=heat.colors(5, alpha = 1),
        main="Total time on survey by party ID",
        ylab="Time in seconds")


## Boxplot with employment
boxplot(data$tot.time ~ droplevels(data$employ),
        col=terrain.colors(9, alpha = 1),
        main="Total time on survey by employment type",
        ylab="Time in seconds")


## Boxplot with marital status
boxplot(data$tot.time ~ droplevels(data$marstat),
        col=heat.colors(6, alpha = 1),
        main="Total time on survey by marital status",
        ylab="Time in seconds")


## Boxplot with education
boxplot(data$tot.time ~ droplevels(data$educ),
        col=topo.colors(6, alpha = 1),
        main="Total time on survey by education",
        ylab="Time in seconds")


## Boxplot with race
boxplot(data$tot.time ~ droplevels(data$race),
        col=heat.colors(8, alpha = 1),
        main="Total time on survey by race",
        ylab="Time in seconds")


## Boxplot with gender
boxplot(data$tot.time ~ droplevels(data$gender),
        col=c("Red", "Blue"),
        main="Total time on survey by gender",
        ylab="Time in seconds")


## Boxplot with vote choice (votechoice)
boxplot(data$tot.time ~ droplevels(data$votechoice),
        col=heat.colors(11, alpha = 1),
        main="Total time on survey by vote choice",
        ylab="Time in seconds")



save(capr, file="capr.5.ballots.RData")


#### Characters in answers
## Below data was created by Cassie
data <- load("capr.5.ballots.charc.RData")


#### Plots of number of characters in answers with the same background variables as above
## Boxplot with Ballot type
boxplot(data$totalcharc ~ data$ballot.five.cat,
        col=topo.colors(5, alpha = 1),
        main="Total characters in answers by Ballot type",
        ylab="Number of characters")


## Boxplot with ideology
boxplot(data$totalcharc ~ droplevels(data$ideo5),
        col=heat.colors(6, alpha = 1),
        main="Total characters in answers by ideology",
        ylab="Number of characters")


## Boxplot with party ID
boxplot(data$totalcharc ~ droplevels(data$pid3),
        col=heat.colors(5, alpha = 1),
        main="Total characters in answers by party ID",
        ylab="Number of characters")


## Boxplot with employment
boxplot(data$totalcharc ~ droplevels(data$employ),
        col=terrain.colors(9, alpha = 1),
        main="Total characters in answers by employment type",
        ylab="Number of characters")


## Boxplot with marital status
boxplot(data$totalcharc ~ droplevels(data$marstat),
        col=heat.colors(6, alpha = 1),
        main="Total characters in answers by marital status",
        ylab="Number of characters")


## Boxplot with education
boxplot(data$totalcharc ~ droplevels(data$educ),
        col=topo.colors(6, alpha = 1),
        main="Total characters in answers by education",
        ylab="Number of characters")


## Boxplot with race
boxplot(data$totalcharc ~ droplevels(data$race),
        col=heat.colors(8, alpha = 1),
        main="Total characters in answers by race",
        ylab="Number of characters")


## Boxplot with gender
boxplot(data$totalcharc ~ droplevels(data$gender),
        col=c("Red", "Blue"),
        main="Total characters in answers by gender",
        ylab="Number of characters")


## Boxplot with vote choice (votechoice)
boxplot(data$totalcharc ~ droplevels(data$votechoice),
        col=heat.colors(11, alpha = 1),
        main="Total characters in answers by vote choice",
        ylab="Number of characters")



