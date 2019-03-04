
user_artists_mat <- spread(user_artists, artist_name, weight_discrete)
#user_artists_mat[is.na(user_artists_mat)] <- 0
View(user_artists_mat[1:100,1:10])

row.names(user_artists_mat) <- user_artists_mat$userID
user_artists_mat$userID <- NULL

user_artists_mat <- as(user_artists_mat,"matrix")
View(user_artists_mat[1:100,1:10])

### 2) Read In Functions ###
############################


### 3) Split train - Test ###
#############################

train_rows <- sample(1:nrow(user_artists_mat), 0.7*nrow(user_artists_mat))

train <- user_artists_mat[train_rows,]
test <- user_artists_mat[-train_rows,]


#######################################################

### 4) User-based CF ###
########################

### a) Based on function

ptm <- proc.time()

UBCF <- UserBasedCF(train, test,N=5, NN=10, onlyNew=TRUE)


TimeUBCF <- (proc.time() - ptm)
TimeUBCF

test2 <- test
for (i in 1:ncol(test2)){
  test2[, i] <- ifelse(is.na(test2[, i]), mean(test2[,i],na.rm=T), test2[, i] )
}

predicted=as.numeric(UBCF$prediction)
predicted[is.nan(predicted)]=0
predicted=as.data.frame(predicted)


actual=as.numeric(test2)
actual[is.nan(actual)]=0
actual=as.data.frame(actual)

RMSE(actual$actual, predicted$predicted)

meanabserror <- function(actual,predicted)
{
  mean(abs(actual - predicted))
}

meanabserror(actual$actual,predicted$predicted)


Classification <- function(prediction, real, threshold=NA, TopN=NA){
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    # Threshold #
    if (!is.na(threshold)){
      TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
      TN = sum(ifelse(prediction <= threshold & real <= threshold, 1, 0), na.rm=T)
      FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
      FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      Accuracy=(TP+TN)/(TP+TN+FP+FN)
      Class_Thres = list(Recall, Precision, Accuracy)
      names(Class_Thres) = c("Recall", "Precision","Accuracy")
    }
    if (!is.na(TopN)){
      TP = vector(, length = nrow(prediction))
      TN = vector(, length = nrow(prediction))
      FP = vector(, length = nrow(prediction))
      FN = vector(, length = nrow(prediction))

      for (i in nrow(prediction)){
        threshold_pred = -sort(-prediction[i, ])[TopN]
        threshold_real = -sort(-real[i, ])[TopN]
        TP[i] = sum(ifelse(prediction[i, ] >= threshold_pred & real[i, ] >= threshold_real, 1, 0), na.rm=T)
        TN[i] = sum(ifelse(prediction[i, ] <= threshold_pred & real[i, ] <= threshold_real, 1, 0), na.rm=T)
        FP[i] = sum(ifelse(prediction[i, ] >= threshold_pred & real[i, ] < threshold_real, 1, 0), na.rm=T)
        FN[i] = sum(ifelse(prediction[i, ] < threshold_pred & real[i, ] >= threshold_real, 1, 0), na.rm=T)
      }
      TP = sum(TP[i])
      TN = sum(TN[i])
      FP = sum(FP[i])
      FN = sum(FN[i])

      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      Accuracy=(TP+TN)/TP+TN+FP+FN
      Class_Thres = list(Recall, Precision, Accuracy)
      names(Class_Thres) = c("Recall", "Precision","Accuracy")
    }


    if (!is.na(threshold) & !is.na(TopN)){
      Class = list(Class_Thres, Class_TopN)
      names(Class) = c("Threshold", "TopN")
    }else if (!is.na(threshold) & is.na(TopN)) {
      Class = Class_Thres
    }else if (is.na(threshold) & !is.na(TopN)) {
      Class = Class_TopN
    }else{
      Class = "You have to specify the 'Threshold' or 'TopN' parameter!"
    }
    return(Class)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

# Classification Item 15
f=Classification(UBCF$prediction, test, threshold=5)

f[3]
Recall=unlist(f[1])
Prec=unlist(f[2])

F_Score=(2*Recall*Prec)/(Recall+Prec)
names(F_Score)="F Score"
F_Score



### 5) Item-based CF ###
########################

ptm <- proc.time()

IBCF <- ItemBasedCF(train, test, N=10, NN=15, onlyNew=TRUE)

TimeIBCF <- (proc.time() - ptm)
TimeIBCF


#######################################################


# Classification Item 10
#Classification(IBCF$prediction, test, threshold=3)
