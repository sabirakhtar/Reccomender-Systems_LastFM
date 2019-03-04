########################################################
### UBCF as a function - User-based CF for all users ###
########################################################

UserBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  
  ### similarity ###
  similarity_matrix <- matrix(, nrow = nrow(test_data), ncol = nrow(train_data),
                              dimnames = list(rownames(test_data), rownames(train_data)))
  
  ### similarity using correlation ###
  for(i in 1:nrow(test_data)) {
    for(j in 1:nrow(train_data)) {
      if (i < j){
        sim <- sum(((test_data[i, ])-(mean(test_data[i, ],na.rm=T)))
                   *((train_data[j,])-(mean(train_data[j, ],na.rm=T))), na.rm=TRUE)/
          sqrt((sum((test_data[i,]) - (mean(test_data[i, ],na.rm=T)), na.rm = T)^2) *
                 (sum((train_data[j, ])-(mean(train_data[j, ],na.rm=T)), na.rm = T)^2))
        similarity_matrix[i,j] <- sim
      }
    }
  }
  
  
  
  print("similarity calculation done")
  ### Nearest Neighbors ###
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    sim_matrix_NN_vec <- similarity_matrix_NN[k,]
    sim_matrix_NN_vec[is.infinite(sim_matrix_NN_vec)] <- NA
    sim_matrix_NN_vec[is.nan(sim_matrix_NN_vec)] <- NA
    crit_val <- -sort(-sim_matrix_NN_vec)[NN]
    similarity_matrix_NN[k,] <- ifelse(sim_matrix_NN_vec >= crit_val, similarity_matrix_NN[k,], NA)
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

#########################################################
### IBCF as a function - Items-based CF for all Items ###
#########################################################

ItemBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  
  # Similarity matrix using Correlation method
  # similarity_matrix <- as.matrix(simil(t(train_data), method="correlation"))
  
  # Initialize empty similarity matrix
  similarity_matrix = matrix(, ncol=ncol(test_data), nrow=ncol(train_data), 
                             dimnames = list(colnames(test_data), colnames(train_data)))
  
  # Similarity matrix using Correlation method
  for(i in 1:ncol(test_data)) {
    for(j in 1:ncol(train_data)) {
      if (i < j){
        sim <- sum(((test_data[,i])-(mean(test_data[,i],na.rm=T)))
                   *((train_data[,j])-(mean(train_data[,j],na.rm=T))), na.rm=TRUE)/
          sqrt((sum((test_data[,i]) - (mean(test_data[,i],na.rm=T)), na.rm = T)^2) *
                 (sum((train_data[,j])-(mean(train_data[,j],na.rm=T)), na.rm = T)^2))
        similarity_matrix[i,j] <- sim
      }
    }
  }
  
  print("Similarity calculation done")
  # Nearest Neighbor
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:ncol(similarity_matrix_NN)){
    sim_matrix_NN_vec <- similarity_matrix_NN[,k]
    sim_matrix_NN_vec[is.infinite(sim_matrix_NN_vec)] <- NA
    sim_matrix_NN_vec[is.nan(sim_matrix_NN_vec)] <- NA
    crit_val <- -sort(-sim_matrix_NN_vec)[NN]
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



######################################
### Cluster based CF as a function ###
######################################

ClusterBasedCF <- function(data, N, centers, iter, onlyNew=TRUE){
  
  data2 <- data
  
  # fill with average product rating
  colmeans <- colMeans(data2, na.rm=TRUE)
  
  for (j in colnames(data2)){
    data2[, j] <- ifelse(is.na(data2[ ,j]), colmeans[j], data2[, j])
  }
  
  km <- kmeans(data2, centers=centers, iter.max=iter)
  
  head(km$cluster)
  head(km$centers)
  
  
  # Statistics of the groups
  tab <- table(km$cluster)
  
  # Assign users to groups
  RES <- cbind(data, as.data.frame(km$cluster))
  
  # Calculate average ratings for everi cluster
  aggregation <- aggregate(RES, list(RES$"km$cluster"), mean, na.rm=T)
  aggregation <- aggregation[,-1]
  
  # Make a prediction
  users <- as.data.frame(RES$"km$cluster")
  users <- cbind(users, rownames(RES))
  colnames(users) <- c("km$cluster", 'rn')
  
  
  prediction = merge(users, aggregation, by="km$cluster")
  rownames(prediction) <- prediction$rn
  
  prediction  <- prediction[order(rownames(prediction)), -1:-2]
  
  prediction2 <- matrix(, nrow=nrow(prediction), ncol(prediction),
                        dimnames=list(rownames(prediction), colnames(prediction)))
  colnames(prediction2) <- colnames(prediction)
  rownames(prediction2) <- rownames(prediction)
  
  for (u in rownames(prediction)){
    if (onlyNew == TRUE){
      unseen <- names(data[u, is.na(data[u,])])
      
      prediction2[u, ] <- as.numeric(t(ifelse(colnames(prediction) %in% unseen, prediction[u, ], as.numeric(NA))))
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
  }
  
  # TopN
  TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), 5))))
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}


#########################################
### Content-based similarity function ###
#########################################   

ContentBased <- function(product_data, test_data, N, NN, onlyNew=TRUE){
  
  # Similarity calculation (stolen from user-based CF)
  similarity_matrix <- as.matrix(simil(product_data, method="cosine"))
  
  print("Similarity calculation done")
  
  # Set Nearest neighbors (stolen from user-based CF)
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], 0)
  }
  
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction (stolen from item-based CF) ###
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



############################################
### Evaluation - Root mean squared error ###
############################################

RMSE <- function(prediction, real){
  
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    RSME = sqrt( sum( (prediction - real)^2 , na.rm = TRUE ) / (nrow(prediction) * ncol(prediction)) )
    return(RSME)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

########################################
### Evaluation - Mean Absolute error ###
########################################

MAE <- function(prediction, real){
  
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    res = mean( abs((prediction - real)) , na.rm = TRUE )
    return(res)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

##################################################################################
### Evaluation - Classification based on Recall, Precision, Accuracy, F1-score ###
##################################################################################

Classification <- function(prediction, real, threshold=NA){
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    # Threshold #
    if (!is.na(threshold)){
      TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
      FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
      FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
      TN = sum(ifelse(prediction < threshold & real < threshold, 1, 0), na.rm=T)
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      Accuracy = (TP+TN)/(TP+TN+FP+FN)
      F1 = 1/(((1/Recall)+(1/Precision))/2)
      Class_Thres = list(Recall, Precision, Accuracy, F1)
      names(Class_Thres) = c("Recall", "Precision", "Accuracy", "F1")
      return(Class_Thres)
    }
    
    if (is.na(threshold)){
      return("You have to specify the 'Threshold' parameter!")
    }
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}