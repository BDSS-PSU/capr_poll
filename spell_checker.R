# Ryan McMahon, Christopher Boylan, Orly 
# Date: 02/20/2016
# Automated spell checking for CAPR

# Credit to Peter Norvig: http://www.sumsar.net/blog/2014/12/peter-norvigs-spell-checker-in-two-lines-of-r/
rm(list=ls())
load("~/Downloads/capr.RData")
setwd("~/Dropbox/capr_poll/")
library(stringr);library(qdap)
# Read in words that are correctly spelled
sorted_words <- names(sort(table(strsplit(tolower(paste(readLines("big.txt"), 
                                                        collapse = " ")), 
                                          "[^a-z]+")), decreasing = TRUE))

# Use Peter Norvig's function to compare distances:
correct <- function(word) { 
  c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)],
    word)[1] 
  }

# Example:
correct(word = "speling")
# spelling

################################################################################


# Start with a column that already has a hand cleaned version so we can 
# compare:

# Ballot 2, Angry (Var = II2):

angry_2 <- capr$II2
angry_2[which(angry_2 == "__NA__")] <- NA

# Create empty vector to hold edited strings:
angry_2_auto <- NA

# create list to hold corrections:
corrections <- list()

for(i in 1:length(angry_2)){
  
  if(is.na(angry_2[i])==T){
    angry_2_auto[i] <- NA
  } else{
    temp <- NA
        
    # split strings on spaces, lowercase, and remove non-alphanumeric
    a <- unlist(str_split(angry_2[i], pattern = " "))
    a <- tolower(a)
    a <- gsub(pattern = "[^[:alnum:] ]", x = a, replacement = " ")
    
    if(length(which(a==" "))>0){
      a <- a[-which(a==" ")]
    }
    a <- paste(a, collapse = " ")
    
    temp <- which_misspelled(a, suggest = T)
    
    if(length(temp)>0){
      a <- unlist(str_split(a, " "))
      temp$word.no <- as.integer(temp$word.no)
      for(j in 1:nrow(temp)){
        a[temp[j,1]] <- temp[j,3]
      }
      corrections[[i]] <- temp
    } else{
      corrections[[i]] <- NULL
    }
  
    angry_2_auto[i] <- paste(a, collapse = " ")
    
  }
}

angry_2_comparison <- cbind(angry_2, angry_2_auto)
colnames(angry_2_comparison) <- c("original", "automated")



################################################################################

preprocess_spelling <- function(x){
   # Takes a column, x, and generates an auto-corrected vector, processed.
   # Stores the changes in a list object, corrections.
  
  errors <- NA
  processed <- NA
  corrections <- list()
  
  for(i in 1:length(x)){
    if(is.na(x[i])==T){
      processed[i] <- NA
      errors[i] <- NA
    } else{
      temp <- NA
      
      # split strings on spaces, lowercase, and remove non-alphanumeric
      a <- unlist(str_split(x[i], pattern = " "))
      a <- tolower(a)
      a <- gsub(pattern = "[^[:alnum:] ]", x = a, replacement = " ")
      
      if(length(which(a==" "))>0){
        a <- a[-which(a==" ")]
      }
      a <- paste(a, collapse = " ")
      
      temp <- which_misspelled(a, suggest = T)
      
      if(length(temp)>0){
        a <- unlist(str_split(a, " "))
        errors[i] <- nrow(temp)/length(a)
        temp$word.no <- as.integer(temp$word.no)
        for(j in 1:nrow(temp)){
          a[temp[j,1]] <- temp[j,3]
        }
        corrections[[i]] <- temp
      } else{
        corrections[[i]] <- NULL
        errors[i] <- 0
      }
      
      processed[i] <- paste(a, collapse = " ")
      
    }
  }
  object <- list(processed = processed, corrections = corrections, errors = errors)
  return(object)
}

test <- preprocess_spelling(x = angry_2)
names(test)



mass_preprocessing <- function(data){
  text_responses <- function(data){
    temp <- NA
    for(i in 1:ncol(data)){
      temp[i] <- class(data[,i])
    }
    return(temp)
  }
  
  cat(paste0("finding text response variables...", " \n\n"))
  marks <- text_responses(data)
  marks <- ifelse(test = marks == "character", yes = T, no = F)
  
  cat(paste0("Found ", length(which(marks==T)), " columns...", "\n\n"))
  
  cat(paste0("Now processing text for ", length(which(marks==T)), " columns...", "\n\n"))
  
  processed_cols <- apply(data[,which(marks==T)], 2, preprocess_spelling)
  
  ret_obj <- list(cleaned = processed_cols, columns_used = marks)
  
  return(ret_obj)
  
}

test2 <- mass_preprocessing(capr)

cleaned_answers <- test2$cleaned[[1]]$processed
for(i in 2:length(test2$cleaned)){
  cleaned_answers <- cbind(cleaned_answers, test2$cleaned[[i]]$processed)
}

cleaned_answers[which(cleaned_answers == "  na  ")] <- NA
colnames(cleaned_answers) <- names(test2$cleaned)


errors <- test2$cleaned$I1$errors
for(i in 2:length(test2$cleaned)){
  errors <- cbind(errors, test2$cleaned[[i]]$errors)
}
colnames(errors) <- names(test2$cleaned)

errors[which(is.na(cleaned_answers)==T)] <- NA

a <- lm(errors[,1] ~ capr$I1A + capr$votechoice + capr$ideo5.int + capr$page_I1_pg_timing, weights = capr$weight)
summary(a)

models <- list()
for(i in 1:14){
  models[[i]] <- lm(errors[,i] ~ capr$ideo5.int + capr$educ + 
             capr$votereg + capr$page_I1_pg_timing, weights = capr$weight)
}

for(i in 1:14){
  print(summary(models[[i]]))
}
