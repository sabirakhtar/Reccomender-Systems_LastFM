#######################
### Item-based CF ###
#######################
library(recommenderlab)

### Read In data ###
####################

data("Jester5k")
JesterMatrix <- as(Jester5k,"matrix")
########################################################################################################################################

### UBCF explained - Step 1: Compute similarity ###
###################################################

### The hard way ###

ptm <- proc.time()

# Initialize empty similarity matrix
similarity_matrix = matrix(, ncol=ncol(JesterMatrix), nrow=ncol(JesterMatrix), dimnames = list(colnames(JesterMatrix), colnames(JesterMatrix)))

rowmeans = rowMeans(JesterMatrix)

# Calculate Pearson correlation between all ITEM pairs
for(i in 1:nrow(JesterMatrix)) {
  for(j in 1:nrow(JesterMatrix)) {
    if (i < j){
      sim <- sum((JesterMatrix[i, ]-mean(JesterMatrix[i, ],na.rm=T))*(JesterMatrix[j,]-mean(JesterMatrix[j, ],na.rm=T)), na.rm=TRUE)/
        sqrt(sum((JesterMatrix[i, ]-mean(JesterMatrix[i, ],na.rm=T)))^2 * sum((JesterMatrix[j, ]-mean(JesterMatrix[j, ],na.rm=T)))^2)
      similarity_matrix_THW[i,j] <- sim
      similarity_matrix_THW[j,i] <- sim
    }
  }
}

Time <- (proc.time() - ptm)
Time

# My Mac = +/- 2.15 sec
# -> Much more efficient than user-based (+/- 260 sec)

### Make use of R packages ###

library('proxy')

ptm <- proc.time()

similarity_matrix <- as.matrix(simil(t(JesterMatrix), method="cosine"))

Time <- (proc.time() - ptm)
Time
# My Mac = +/- 0.267 sec
# -> Much more efficient than user-based (+/- 12 sec)

### Step 2: Retain nearest neighbors ###
########################################

### Keep N nearest neighbors ###
NN=10

similarity_matrix_NN <- similarity_matrix

for (k in 1:ncol(similarity_matrix_NN)){
  crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
  similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
}

similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0


### Step 3: Prediction ###
##########################
N = 3

#JesterMatrix[is.na(JesterMatrix)] <- 0

prediction <- matrix(, nrow=nrow(JesterMatrix), ncol=ncol(JesterMatrix), dimnames=list(rownames(JesterMatrix), colnames(JesterMatrix)))
TopN <- matrix(, nrow=nrow(JesterMatrix), N, dimnames=list(rownames(JesterMatrix)))

for (u in rownames(JesterMatrix)){
  ### Numerator ###
  # Calculate Numerator
  Num <-  JesterMatrix[u, ] %*% similarity_matrix_NN

  ### Denominator ###
  Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)

  prediction[u, ] <- Num/Denom
  TopN[u, ] <- names(sort(-prediction[u, ]))[1:N]
}


########################################################################################################################################
### Item-based CF as a function ###
###################################

library(proxy)

ItemBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  # # Similarity
  # similarity_matrix <- matrix(, nrow = nrow(test_data), ncol = nrow(train_data),
  #                             dimnames = list(rownames(test_data), rownames(train_data)))
  #
  # # Calculate Pearson correlation between all ITEM pairs
  # for(i in 1:ncol(test_data)) {
  #   print(i)
  #   for(j in 1:ncol(train_data)) {
  #     if (i < j){
  #       sim <- sum(((test_data[,i])-(mean(test_data[, i],na.rm=T)))
  #                  *((train_data[,j])-(mean(train_data[,j ],na.rm=T))), na.rm=TRUE)/
  #         sqrt((sum((test_data[,i]) - (mean(test_data[,i ],na.rm=T)), na.rm = T)^2) *
  #                (sum((train_data[,j ])-(mean(train_data[,j ],na.rm=T)), na.rm = T)^2))
  #       similarity_matrix[i, j] <- sim
  #       similarity_matrix[j, i] <- sim
  #     }
  #   }
  # }

  similarity_matrix <- as.matrix(simil(t(train_data), method="correlation"))

  print("Similarity calculation done")
  # Nearest Neighbor
  similarity_matrix_NN <- similarity_matrix

  #similarity_matrix <- as.matrix(simil(t(train_data), method="cosine"))

  print("Similarity calculation done")
  # Nearest Neighbor
  similarity_matrix_NN <- similarity_matrix

  for (k in 1:ncol(similarity_matrix_NN)){
    sim_matrix_NN_vec <- similarity_matrix_NN[,k]
    sim_matrix_NN_vec[is.infinite(sim_matrix_NN_vec)] <- NA
    sim_matrix_NN_vec[is.nan(sim_matrix_NN_vec)] <- NA
    #dummy=data.frame(sim_matrix_NN_vec)
    #crit_val <- -sort(-sim_matrix_NN_vec[k])[NN]
    crit_val <- -sort(-sim_matrix_NN_vec)[NN]
    #crit_val <- -sort(-dummy[,k])[NN]
    #similarity_matrix_NN[,k] <- ifelse(sim_matrix_NN_vec[k] >= crit_val, similarity_matrix_NN[,k], NA)
    similarity_matrix_NN[,k] <- ifelse(sim_matrix_NN_vec >= crit_val, similarity_matrix_NN[,k], NA)
  }

  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0

  train_data[is.na(train_data)] <- 0

  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0

  print("Nearest neighbor selection done")

  ### Prediction ###
  prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data),
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data),
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))

  for (u in rownames(test_data)){
    # Numerator
    Num <-  test_data2[u, ] %*% similarity_matrix_NN

    # Denominator
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)

    # Prediction
    prediction[u, ] <- Num/Denom

    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }

    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])

  }

  print("Prediction done")

  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')

  return(res)
}

########################################################################################################################################

### Split in train and test ###
###############################
JesterMatrix <- as(Jester5k,"matrix")
train <- JesterMatrix[1:4000,]
test <- JesterMatrix[4001:5000,]

### Recommend for one user ###
##############################
#u2841 & u238

u1 = 'u2841'
u2 = 'u238'

tm <- matrix(, nrow=2, ncol=ncol(test), dimnames=(list(list(u1, u2), colnames(test))))
tm[1, ] <- t(as.matrix(train[u1,]))
tm[2, ] <- t(as.matrix(train[u2,]))

ResultsIBCFUser <- ItemBasedCF(train, tm, 3, NN=10, onlyNew=TRUE)

predictionUser <- as.data.frame(ResultsIBCFUser$prediction)

TopNUser <- as.data.frame(ResultsIBCFUser$topN)

for (u in 1:nrow(TopNUser)){
  for (i in 1:3){
    print(paste("Recommendation", i, "for user", u, ":"))
    cat(JesterJokes[as.character(TopNUser[[i]])[u]], sep = "\n\n")
    print("________________")
  }
}

### Recommend for all test users ###
####################################

ResultsIBCF <- ItemBasedCF(train, test, 3, NN=10, onlyNew=TRUE)

prediction <- as.data.frame(ResultsIBCF$prediction)

TopN <- as.data.frame(ResultsIBCF$topN)
