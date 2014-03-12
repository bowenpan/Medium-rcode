
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

## READ IN AND FORMAT DATA

data <- data.table(read.csv("data.csv"))

## PARTITION TEST DATA AND ASSIGN RESPONSE VARIABLE TO TRAINING AND VALIDATION DATA

prev_posts.binned <- bin.num.data(data$prev_posts, N=12)
data <- cbind(data, prev_posts.binned)

test.data = subset(data, (is_last_activity == 1) & (last_save_age < 14))
tv.data = subset(data, (is_last_activity == 0) | (last_save_age >= 14))

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
  predictor.names = c(#'word_count',
                      #'image_count',
                      'has_cover_image',
                      #'days_as_draft',
                      #'collaborators',
                      #'in_ed_picks',
                      #'in_matter',
                      'in_curated_collection',
                      'views',
                      #'views_from_internal',
                      #'views_from_facebook',
                      #'views_from_twitter',
                      'recs',
                      'hrs_read',
                      #'notes',
                      'notes_public',
                      'collections_accepted',
                      'collections_pending_or_rejected',
                      'follow_ups',
                      #'publish_time',
                      #'manual_invite',
                      #'programmatic_invite',
                      #'join_after_redesign',
                      'post_after_redesign',
                      #'invite_only_days',
                      'post_age',
                      #'prev_posts',
                      'days_since_prev',
                      #'latest_post',
                      #'post_en',
                      'X.from.1.to.2',
                      'X.from.18.to.Inf',
                      'X.from.2.to.3',
                      'X.from.3.to.5',
                      'X.from.5.to.8',
                      'X.from.8.to.18'
                      )
  
  
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

plot.tree(model.tree)

summary(model.tree)

summary(model.logreg)

#########################################################################################
# PART II
# Play around with association rules

library("arules")

assoc.numericals = c('word_count',
                     'image_count',
                     'days_as_draft',
                     'collaborators',
                     'recs',
                     'notes',
                     'notes_public',
                     'collections_accepted',
                     'collections_pending_or_rejected',
                     'follow_ups')

assoc.categoricals = c(
  'has_cover_image',
  'manual_invite',
  'programmatic_invite',
  'join_after_redesign',
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