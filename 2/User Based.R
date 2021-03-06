#######################
### User - based CF ###
#######################
library(recommenderlab)

### Step 0. Read In data ###
############################

data("Jester5k")
JesterMatrix <- as(Jester5k,"matrix")
dim(JesterMatrix)

########################################################################################################################################

### UBCF explained - Step 1: Compute cosine similarity ###
##########################################################


### The hard way ###
ptm <- proc.time()

# Initialize empty similarity matrix
similarity_matrix_THW <- matrix(, nrow = nrow(JesterMatrix), ncol = nrow(JesterMatrix),
                                dimnames = list(rownames(JesterMatrix), rownames(JesterMatrix)))





# # Calculate Pearson correlation between all ITEM pairs
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
# My Mac = +/- 260 sec

### Make use of R packages ###

#install.packages('proxy')
library('proxy')

ptm <- proc.time()

similarity_matrix <- as.matrix(simil(JesterMatrix, method="cosine"))

Time <- (proc.time() - ptm)
Time
# My Mac = +/- 12 sec

### UBCF explained - Step 2: Retain nearest neighbors ###
#########################################################

### Keep nearest neighbors based on similarity threshold ###
hist(similarity_matrix)

threshold = 0.5

similarity_matrix_threshold <- similarity_matrix
similarity_matrix_threshold[similarity_matrix_threshold < threshold] <- NA

### Keep N nearest neighbors ###
NN=10

similarity_matrix_NN <- similarity_matrix

for (k in 1:nrow(similarity_matrix_NN)){
  crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
  similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
}


### UBCF explained - Step 3: Prediction ###
###########################################
N = 3

prediction <- matrix(, nrow=nrow(JesterMatrix), ncol(JesterMatrix), dimnames=list(rownames(JesterMatrix), colnames(JesterMatrix)))
TopN <- matrix(, nrow=nrow(JesterMatrix), N, dimnames=list(rownames(JesterMatrix)))
#Preparation: Compute (r - mean(ri))

for (u in rownames(JesterMatrix)){
  similarity_vector <- na.omit(similarity_matrix_NN[u, ])

  NN_norm <- JesterMatrix[rownames(JesterMatrix) %in% names(similarity_vector),]

  CM <- colMeans(JesterMatrix, na.rm=TRUE)
  for (l in 1:ncol(NN_norm)){
    NN_norm[,l] <- NN_norm[,l] - CM[l]
  }
  NN_norm[is.na(NN_norm)] <- 0

  # Numerator
  Num = similarity_vector %*% NN_norm

  #Prediction
  prediction[u, ] =  mean(JesterMatrix[u, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))
  TopN[u, ] <- names(sort(-prediction[u, ]))[1:N]
}

########################################################################################################################################

### UBCF as a function - User-based CF for one user ###
#######################################################

UserBasedCFOneUser <- function(dataset, user, N, NN, onlyNew=TRUE){

  ### similarity ###
  similarity_vect <- vector(, nrow(dataset))
  names(similarity_vect) <- rownames(dataset)
  for (i in rownames(dataset)){
    if (i != user){
      sim <- sum(dataset[user, ]*dataset[i,], na.rm=TRUE)/sqrt(sum(dataset[user, ]^2, na.rm=TRUE) * sum(dataset[i, ]^2, na.rm=TRUE))
      similarity_vect[i] <- sim
    }
  }

  ### Nearest Neighbors ###
  crit_val <- -sort(-similarity_vect)[NN]
  similarity_vect <- na.omit(ifelse(similarity_vect >= crit_val, similarity_vect, NA))

  ### Prediction ###
  # Prepare
  NN_norm <- dataset[rownames(dataset) %in% names(similarity_vect),]
  CM <- colMeans(dataset, na.rm=TRUE)
  for (l in 1:ncol(NN_norm)){
    NN_norm[,l] <- NN_norm[,l] - CM[l]
  }
  NN_norm[is.na(NN_norm)] <- 0

  # Numerator
  Num = similarity_vect %*% NN_norm

  #Prediction
  prediction = mean(dataset[user, ], na.rm=TRUE) + (Num/sum(similarity_vect, na.rm=TRUE))
  names(prediction) = colnames(dataset)

  if (onlyNew == TRUE){
    unseen <- names(dataset[user, is.na(dataset[user,])])
    prediction <- prediction[names(prediction) %in% unseen]
  }
  TopN <- head(-sort(-prediction), N)

  return(TopN)
}

### UBCF as a function - User-based CF for all users ###
########################################################

UserBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){

  ### similarity ###
  similarity_matrix <- matrix(, nrow = nrow(test_data), ncol = nrow(train_data),
                              dimnames = list(rownames(test_data), rownames(train_data)))

  for(i in 1:nrow(test_data)) {
    for(j in 1:nrow(train_data)) {
      if (i < j){
        sim <- sum((test_data[i, ]-mean(test_data[i, ],na.rm=T))*(train_data[j,]-mean(train_data[j, ],na.rm=T)), na.rm=TRUE)/sqrt(sum((test_data[i, ]-mean(test_data[i, ],na.rm=T)))^2 * sum((train_data[j, ]-mean(train_data[j, ],na.rm=T)))^2)
        similarity_matrix[i,j] <- sim
      }
    }
  }



  print("similarity calculation done")
  ### Nearest Neighbors ###
  similarity_matrix_NN <- similarity_matrix

  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
  }

  print("Nearest Neighbor selection done")
  ### Prediction ###
  # Prepare
  prediction <- matrix(, nrow=nrow(test_data), ncol(test_data),
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data),
                        dimnames=list(rownames(test_data), colnames(test_data)))

  TopN <- matrix(, nrow=nrow(test_data), ncol=N, dimnames=list(rownames(test_data)))
  ### Numerator ###
  for (u in rownames(test_data)){
    similarity_vector <- na.omit(similarity_matrix_NN[u, ])

    NN_norm <- train_data[rownames(train_data) %in% names(similarity_vector),]

    CM <- colMeans(train_data, na.rm=TRUE)
    for (l in 1:ncol(NN_norm)){
      NN_norm[,l] <- NN_norm[,l] - CM[l]
    }
    NN_norm[is.na(NN_norm)] <- 0

    # Numerator
    Num = similarity_vector %*% NN_norm

    #Prediction
    prediction[u, ] =  mean(test_data[u, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))


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

### Using the functions ###
###########################

### Split in train and test ###
###############################

train <- JesterMatrix[1:4000,]
test <- JesterMatrix[4001:5000,]

### Recommend for one user ###
##############################

#2841
res <- UserBasedCFOneUser(train, user='u2841', N=10, NN=10, onlyNew=TRUE)
res
cat(JesterJokes[names(res)], sep = "\n\n")

#u238
res <- UserBasedCFOneUser(train, user='u238', N=10, NN=10, onlyNew=TRUE)
res
cat(JesterJokes[names(res)], sep = "\n\n")

### Recommend for all test users ###
####################################

ResultsUBCF <- UserBasedCF(train, test, N=3, NN=10, onlyNew=TRUE)

ResultsUBCF$prediction
ResultsUBCF$topN
