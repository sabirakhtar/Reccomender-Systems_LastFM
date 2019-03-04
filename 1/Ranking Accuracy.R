###########################
### Evaluation: Ranking ###
###########################

########################
### Evaluation: NDCG ###
########################
library(recommenderlab)

### Read In data ###
####################

data("Jester5k")
JesterMatrix <- as(Jester5k,"matrix")
########################################################################################################################################

### score a model ###
#####################

# Load Models
setwd("/Users/stijngeuens/Google Drive/Zelfstandig/IESEG/Recommendation Tools Course/Course/1. Introduction")
source("functions.R")


# Split train - Test
set.seed(2)
train_rows = sample(1:nrow(JesterMatrix), 0.7*nrow(JesterMatrix))

train <- JesterMatrix[train_rows,]
test <- JesterMatrix[-train_rows,]

### score item-based
#Item 10
st <- proc.time()
Item10 = ItemBasedCF(train, test, N=10, NN=10, onlyNew=TRUE)
(proc.time() - st)
#Item 15
st <- proc.time()
Item15 = ItemBasedCF(train, test, N=10, NN=15, onlyNew=TRUE)
(proc.time() - st)

### score user-based
#User 10
st <- proc.time()
User10 = UserBasedCF(train, test, N=10, NN=10, onlyNew=TRUE)
(proc.time() - st)
print("user = 462 / system = 95 / elapsed = 558")

#User 15
st <- proc.time()
User15 = UserBasedCF(train, test, N=10, NN=15, onlyNew=TRUE)
(proc.time() - st)
#print("user = 462 / system = 95 / elapsed = 558")

########################################################################################################################################

### Compute NDCG ###
####################

library(StatRank)

# Preprocess

for (u in rownames(test)){

  # compute ranking
  rank <- sort(-rank(Item15$prediction[u,]))[1:threshold]

  # Create NDCG vector
  if ( u == rownames(test)[1]){
    NDCG_vect <- Evaluation.NDCG(rank, test[u, names(rank)])
  }else{
    NDCG_vect <- rbind(NDCG_vect, Evaluation.NDCG(rank, test[u,names(rank)]))
  }
  NDCG_vect[is.na(NDCG_vect)] <- 0
}

# Compute avarege NDCG
NDCG <- colMeans(NDCG_vect, na.rm=T)
names(NDCG) <- "NDCG"

NDCG

### NDCG as a Function ###
##########################

NDCG <- function(real, prediction, TopN){
  for (u in rownames(real)){

    # compute ranking
    rank <- sort(-rank(prediction[u,]))[1:TopN]


    # Create NDCG vector
    if ( u == rownames(real)[1]){
      NDCG_vect <- Evaluation.NDCG(rank, real[u, names(rank)])
    }else{
      NDCG_vect <- rbind(NDCG_vect, Evaluation.NDCG(rank, real[u, names(rank)]))
    }
  }

  # Compute avarege NDCG
  NDCG_vect[is.na(NDCG_vect)] <- 0
  NDCG <- colMeans(NDCG_vect, na.rm=T)
  names(NDCG) <- "NDCG"
  return(NDCG)
}

### Evaluating the created algorithms ###
#############################################

NDCG(test, Item10$prediction, 5)

NDCG(test, Item15$prediction, 5)

NDCG(test, User10$prediction, 5)

NDCG(test, User15$prediction, 5)

########################
### Evaluation: AUC ###
########################

#install.packages("AUC")

library(AUC)

### AUC as a Function ###
##########################

AUC <- function(real, prediction, threshold){

  pred <- ifelse(prediction >= threshold, 1, 0)
  real <- ifelse(real >= threshold, 1, 0)

  real[is.na(real)] <- 0
  pred[is.na(pred)] <- 0

  ROC <- roc(factor(prediction), factor(real))

  plot(ROC)

  AUC <- auc(ROC)
  return(AUC)
}

### Predicting for the created algorithms ###
#############################################

AUC(test, Item10$prediction, 5)

AUC(test, Item15$prediction, 5)

AUC(test, User10$prediction, 5)

AUC(test, User15$prediction, 5)

