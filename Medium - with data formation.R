#########################################################################################
# PART I
#PREPARE THE DATA

#--------------------------------------------------------------------------------------------#
# Step A: This step cleans the memory, reads in the data, separates the predictors and response in the data and stores them in different 
# variables.

rm(list=ls())

start.time=proc.time()[3]  # Starts a clock to measure run time

require(data.table)
source("http://www.stanford.edu/~bayati/oit367/T367_utilities_09.R")

setwd("Dropbox/Stanford/Academia/Year 2/Winter 2013/OIT367 - Big Data/Medium/")   

## DECLARE CONSTANTS

manual_invite_date <- as.Date("08/05/13", format ="%m/%d/%y") # Cutoff date for manual invites. After this date invites are programmatic.
programmatic_invite_date <- as.Date("10/25/13", format ="%m/%d/%y") # Cutoff date for programmatic invites. After this date registration is open
redesign_date <- as.Date("12/04/13", format ="%m/%d/%y") # Cutoff date for redesign.
last_date <- as.Date("2014-03-11", format="%Y-%m-%d") # Date to count back from

## READ IN AND FORMAT DATA

data <- data.table(read.csv("data-v4-published-posts.csv"))

data$publish_date <- substring(data$publish_hour,1,10)
data$publish_time <- as.numeric(substring(data$publish_hour,12))
data$latest_save_date <- substring(data$latest_save_hour,1,10)
data$latest_save_time <- as.numeric(substring(data$latest_save_hour,12))
data$image_count <- as.numeric(data$image_count)
data$word_count <- as.numeric(data$word_count)

## REMOVE JUNK POSTS

data = subset(data, (blacklisted_user != 1) & # Remove blacklisted users
                (latest_save_hour != '') & # Remove posts published before save events
                (word_count > 4)) # Remove test posts
#                (author_identifier != 'XysERH/ezaE.trXO4nfWOaaVgwSHLQ.') & 
#                (author_identifier != 'FqIMr.7c4iNuYq3g0Nh2if94ldOUxge') &
#                (author_identifier != 'YoyEpu.Cj0YpYYkHo9TZcsVqVp5qQx2') &
#                (author_identifier != 'Yv1uhorLTs9h522qNln64iDHWY3luAK') &
#                (author_identifier != 'FDy54jMsnKCEm12WbSOJmqoNOMXq9ui') &
#                (author_identifier != 'xKlqxk7GchQ.r2dzUD4fKw9eNqqaqO6') & 
#                (author_identifier != 'g8yfjMv0JXO.5vOgHv1e9tm0/OPBa7W') & 
#                (author_identifier != 'gEAWO3cauZBZXjyy7wPUcpyfkxQWz2C') & 
#                (author_identifier != 'gYQtZY2xSh7esTKltV5tmZoolSt8zgq') & 
#                (author_identifier != 'G9fybbB76OhgegG6LDoCjaYGJoSzC2y') &
#                (author_identifier != 'xkEJAcl6v8xNobe5.Qnz..hxKPnNFFW') &
#                (author_identifier != 'HYI0Uj3hIuu4c2L0PlVt1DUzMwIz.9y') &
#                (author_identifier != 'GP0U/vopYglYGMoHXObwo0qcVhXxa02') &
#                (author_identifier != 'gf.ELymoCjRJ39epnK.CKVWCyNby3su') &
#                (author_identifier != 'FJb13BNrmmaXD4hvmous6A1NdwSum6O') &
#                (author_identifier != 'fPwO7IqLGFJz9yll/AbIw4gWwnQBYmG') &
#                (author_identifier != 'hvkNaJBXjTC4DPDoSJuIm/2ytlTCBhG') &
#                (author_identifier != 'gO7YdbtMXzs69unz5M9fsn3azR4DhVm') &
#                (author_identifier != 'X5PRkQ5PzobaRThPhLdUOI2arxHESPK') &
#                (author_identifier != 'X.okpz7j8qnP1r49U60Etbej0RWugCe') &
#                (author_identifier != 'EdJVjVDtLOgsCTsFxyyZWhzcH8.LB4G') &
#                (author_identifier != 'yUQo/6X9Y4Zn/rEZhCNwt/XLA6I9XrO') &
#                (author_identifier != 'XtzPHY9Pohx1fsNyRmxS63EgDZlNRH2'))

## PREDICTOR GENERATION

data$manual_invite <- as.numeric(as.Date(data$author_sign_up_day, format ="%m/%d/%y") < manual_invite_date)
data$programmatic_invite <- as.numeric((as.Date(data$author_sign_up_day, format ="%m/%d/%y") >= manual_invite_date) & (as.Date(data$author_sign_up_day, format ="%m/%d/%y") < programmatic_invite_date))
data$join_after_redesign <- as.numeric((as.Date(data$author_sign_up_day, format ="%m/%d/%y") >= redesign_date))
data$post_after_redesign <- as.numeric((as.Date(data$publish_date, format ="%Y-%m-%d") >= redesign_date))

data$post_en <- as.numeric(data$detected_language == 'en')

data$invite_only_days <- as.numeric(programmatic_invite_date - as.Date(data$author_sign_up_day, format ="%m/%d/%y"))
data$post_age <- as.numeric(last_date - as.Date(data$publish_date, format ="%Y-%m-%d"))
data$last_save_age <- as.numeric(last_date - as.Date(data$last_save_date, format ="%Y-%m-%d"))

data[, last_save_date := max(latest_save_date), by=author_identifier]
data[, last_save_time := max(latest_save_time), by=author_identifier]
data[, last_post_date := max(publish_date), by=author_identifier]
data[, last_post_time := max(publish_time), by=author_identifier]
data[, total_posts := length(unique(post_identifier)), by=author_identifier]

data = data[order(data$author_identifier,data$publish_date,data$publish_time),] # Order the data chronologically to count previous posts
for(i in 1:nrow(data)){
  j <- i - 1
  if (j > 0){
    if (data$author_identifier[j] == data$author_identifier[i]) {
      data$prev_posts[i] = data$prev_posts[j] + 1
      data$days_since_prev[i] = as.numeric(as.Date(data$publish_date[i], format ="%Y-%m-%d") - as.Date(data$publish_date[j], format ="%Y-%m-%d"))
    } else {
      data$prev_posts[i] = 0
    }
  } else {
    data$prev_posts[i] = 0
  }
}

data$prev_posts_5_over <- as.numeric(data$prev_posts >= 5)
data$is_latest_post <- as.numeric(data$prev_posts == data$total_posts - 1)
data$is_last_activity <- as.numeric((as.Date(data$publish_date, format ="%Y-%m-%d") >= as.Date(data$last_save_date, format ="%Y-%m-%d"))
                                    & (data$publish_time >= data$last_save_time))

## PARTITION TEST DATA AND ASSIGN RESPONSE VARIABLE TO TRAINING AND VALIDATION DATA

#language.binned <- bin.cat.data(data$detected_language)
prev_posts.binned <- bin.num.data(data$prev_posts, N=12)
#data <- cbind(data, language.binned, prev_posts.binned)
data <- cbind(data, prev_posts.binned)

test.data = subset(data, (is_last_activity == 1) & (last_save_age < 14))
tv.data = subset(data, (is_last_activity == 0) | (last_save_age >= 14))

#tv.data$response = as.numeric((tv.data$latest_post == FALSE) & (tv.data$total_posts > tv.data$prev_posts)) # 1 if the author ever wrote again
tv.data$response = as.numeric((tv.data$is_last_activity == FALSE))

#########################################################################################
# PART II

#--------------------------------------------------------------------------------------------#
# Step A: Randomize and partition the training and validation sets using cross-validation

tv.data <- as.data.frame(tv.data[sample(nrow(tv.data)),])

nfolds = 10

flds <- createFolds(seq(1, nrow(tv.data)), nfolds, list = TRUE, returnTrain = FALSE)

logreg.aucs = rep(0,nfolds)
tree.aucs = rep(0,nfolds)
svm.aucs = rep(0,nfolds)
combined.aucs =  rep(0,nfolds)

for(i in 1:nfolds){
  ind = flds[[i]]
  training = tv.data[-ind,]
  validation = tv.data[ind,]
  
  #--------------------------------------------------------------------------------------------#
  # Step B: Define some predictors. Leave old models in comments. Use agrep() for categorical predictor names.
  
  ### Declare predictors for the models
  predictor.names = c('word_count',
                      'image_count',
                      'has_cover_image',
                      'days_as_draft',
                      'collaborators',
                      'in_ed_picks',
                      'in_matter',
                      'in_curated_collection',
                      'views',
                      'views_from_internal',
                      'views_from_facebook',
                      'views_from_twitter',
                      'recs',
                      'hrs_read',
                      'notes',
                      'notes_public',
                      'collections_accepted',
                      'collections_pending_or_rejected',
                      'follow_ups',
                      #'publish_time',
                      'manual_invite',
                      'programmatic_invite',
                      'join_after_redesign',
                      'post_after_redesign',
                      'invite_only_days',
                      'post_age',
                      #'prev_posts',
                      'days_since_prev',
                      #'latest_post',
                      'post_en',
                      'X.from.1.to.2',
                      'X.from.18.to.Inf',
                      'X.from.2.to.3',
                      'X.from.3.to.5',
                      'X.from.5.to.8',
                      'X.from.8.to.18')
  
  
  #--------------------------------------------------------------------------------------------#
  # Step C: Run some models
  
  model.logreg = buildModel("response", predictors=predictor.names, training, type ='classify', method = 'logisticRegression')
  pred.logreg  = genPred(model.logreg, newdata=validation, method='logisticRegression')
  
  model.tree = buildModel("response", predictors=predictor.names, training, type ='classify', method = 'decisionTree', prune.tree=TRUE)
  pred.tree  = genPred(model.tree, newdata=validation, method='decisionTree')
  
  #model.svm = buildModel("response", predictors=predictor.names, training, type ='classify', method = 'nonlinearSVM')
  #pred.svm  = genPred(model.svm, newdata=validation, method='nonlinearSVM')
  
  #combined.preds = (pred.logreg + pred.tree + pred.svm)/3
  
  logreg.aucs[i] = auc(validation$response, pred.logreg)
  tree.aucs[i] = auc(validation$response, pred.tree)
  #svm.aucs[i] = auc(validation$response, pred.svm)
  
  #combined.aucs[i] = auc(validation$response, combined.preds)
  
  cat("Finished fold ", i, " of ", nfolds, "\n" )
  
}

cat("Average AUC of tree = ", mean(tree.aucs), " with standard error = ", sd(tree.aucs)/sqrt(nfolds), "\n" )
cat("Average AUC of logreg = ", mean(logreg.aucs), " with standard error = ", sd(logreg.aucs)/sqrt(nfolds), "\n" )
#cat("Average AUC of svm = ", mean(svm.aucs), " with standard error = ", sd(svm.aucs)/sqrt(nfolds), "\n" )
#cat("Average AUC of combined model = ", mean(combined.aucs), " with standard error = ", sd(combined.aucs)/sqrt(nfolds), "\n" )

summary(model.logreg)

#########################################################################################
# PART II
# Play around with association rules

library("arules")

assoc.numericals = c('word_count',
                     'image_count',
                     'days_as_draft',
                     'collaborators',
                     #'views',
                     #'views_from_internal',
                     #'views_from_facebook',
                     #'views_from_twitter',
                     'recs',
                     #'hrs_read',
                     'notes',
                     'notes_public',
                     'collections_accepted',
                     'collections_pending_or_rejected',
                     'follow_ups')

assoc.categoricals = c(
  'has_cover_image',
  #'publish_time',
  #'
  #'
  'manual_invite',
  'programmatic_invite',
  'join_after_redesign',
  #'invite_only_days',
  #'post_age',
  #'prev_posts',
  #'days_since_prev',
  #'latest_post',
  'post_after_redesign',
  'post_en',
  'X.from.1.to.2',
  'X.from.19.to.Inf',
  'X.from.2.to.3',
  'X.from.3.to.5',
  'X.from.5.to.9',
  'X.from.9.to.19',
  'response')

assoc.matrix = as.matrix(training[,assoc.categoricals])

assoc.matrix = cbind(assoc.matrix,bin.num.data(as.matrix(training[,assoc.numericals]), N=10))

assoc.trans = as(assoc.matrix, "transactions")

rules = apriori(assoc.trans, parameter = list(supp=0.2, conf=0.5), appearance = list(rhs = c("response"), default="lhs")) # generates the ruleset
sorted.rules = sort(rules,by="lift") # Sorts by lift
inspect(sorted.rules) # Outputs the sorted ruleset