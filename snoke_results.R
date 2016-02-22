load("~/Box Sync/CAPRpoll/CAPR-R-format/capr.5.ballots.charc.Rdata")

#####
## Remove time outliers (people who took longer than 30 minutes)
#####
capr_no_outliers = capr[capr$tot.time < 1800, ]

#---------------------------
# Considering time taken to complete ballots and total character lengths
#---------------------------

cor(capr_no_outliers$totalcharc, capr_no_outliers$tot.time) ## unsurprisingly correlated

#####
## create ballot time vectors for t.tests
#####
ballot1Times = capr_no_outliers[capr_no_outliers$ballot.five.cat == "Ballot 1", "tot.time"]

ballot2ATimes = capr_no_outliers[capr_no_outliers$ballot.five.cat == "Ballot 2A", "tot.time"]

ballot2BTimes = capr_no_outliers[capr_no_outliers$ballot.five.cat == "Ballot 2B", "tot.time"]

ballot3ATimes = capr_no_outliers[capr_no_outliers$ballot.five.cat == "Ballot 3A", "tot.time"]

ballot3BTimes = capr_no_outliers[capr_no_outliers$ballot.five.cat == "Ballot 3B", "tot.time"]

#####
## simple pairwise t.tests for mean ballot time
#####
t.test(ballot2ATimes, ballot2BTimes) ## only one of real interest
t.test(ballot3ATimes, ballot3BTimes)

t.test(ballot1Times, ballot2ATimes)
t.test(ballot1Times, ballot2BTimes)

t.test(ballot1Times, ballot3ATimes)
t.test(ballot1Times, ballot3BTimes)

t.test(ballot3ATimes, ballot2BTimes)
t.test(ballot3BTimes, ballot2BTimes)

t.test(ballot3ATimes, ballot2ATimes)
t.test(ballot3BTimes, ballot2ATimes)

#####
## Multiple regression models for time, AIC model selection for important predictor variables
#####
timeLM = lm(tot.time ~ ballot.five.cat + gender + educ + race + votechoice + inputstate +
                religpew + employ + pid3 + pid7 + marstat + ideo5 + faminc + pew_bornagain + birthyr +
                pew_churatd, 
            data = capr_no_outliers)

stepTime = stepAIC(timeLM) ## chosen predictors: ballot, 7 point political scale, birth year
summary(stepTime)

anova(stepTime)

#-----------------------------------

#####
## create ballot character length vectors for t.tests
#####
ballot1Char = capr_no_outliers[capr_no_outliers$ballot.five.cat == "Ballot 1", "totalcharc"]

ballot2AChar = capr_no_outliers[capr_no_outliers$ballot.five.cat == "Ballot 2A", "totalcharc"]

ballot2BChar = capr_no_outliers[capr_no_outliers$ballot.five.cat == "Ballot 2B", "totalcharc"]

ballot3AChar = capr_no_outliers[capr_no_outliers$ballot.five.cat == "Ballot 3A", "totalcharc"]

ballot3BChar = capr_no_outliers[capr_no_outliers$ballot.five.cat == "Ballot 3B", "totalcharc"]

#####
## simple pairwise t.tests for mean ballot character length
#####
t.test(ballot2AChar, ballot2BChar) ## 
t.test(ballot3AChar, ballot3BChar)

t.test(ballot1Char, ballot2AChar)
t.test(ballot1Char, ballot2BChar) ##

t.test(ballot1Char, ballot3AChar) ##
t.test(ballot1Char, ballot3BChar) ##

t.test(ballot3AChar, ballot2BChar)
t.test(ballot3BChar, ballot2BChar)

t.test(ballot3AChar, ballot2AChar) ##
t.test(ballot3BChar, ballot2AChar) ##

#####
## Multiple regression models for character length, AIC model selection for important predictor variables
#####
charLM = lm(totalcharc ~ ballot.five.cat + gender + educ + race + votechoice + inputstate +
                religpew + employ + pid3 + pid7 + marstat + ideo5 + faminc + pew_bornagain + birthyr +
                pew_churatd, 
            data = capr_no_outliers)

stepChar = stepAIC(charLM) ## chosen predictors: ballot, education, race, employment, 
                            ## 3 point political scale, 5 point ideology scale, birthyear
summary(stepChar)

anova(stepChar)





