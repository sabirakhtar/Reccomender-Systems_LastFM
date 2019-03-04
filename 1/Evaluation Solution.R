#################
### Excercise ###
#################

### Steps to execute:
# 1)   Read in data
# 2)   Read in functions
# 3)   Split data in train test (70% - 30% split)
# 4    Run models
# 5 a) Evaluate in terms of RMSE
# 5 b) Evaluate in terms of Recall/Precisiopn
# 5 c) Evaluate in terms of MAE

meanabserror <- function(actual,predicted)
{
  mean(abs(actual - predicted))
}


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

meanabserror(actual$actual,predicted$predicted)


Classification <- function(prediction, real, threshold=NA, TopN=NA){
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    # Threshold #
    if (!is.na(threshold)){
      TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
      FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
      FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      Class_Thres = list(Recall, Precision)
      names(Class_Thres) = c("Recall", "Precision")
    }
    if (!is.na(TopN)){
      TP = vector(, length = nrow(prediction))
      FP = vector(, length = nrow(prediction))
      FN = vector(, length = nrow(prediction))

      for (i in nrow(prediction)){
        threshold_pred = -sort(-prediction[i, ])[TopN]
        threshold_real = -sort(-real[i, ])[TopN]
        TP[i] = sum(ifelse(prediction[i, ] >= threshold_pred & real[i, ] >= threshold_real, 1, 0), na.rm=T)
        FP[i] = sum(ifelse(prediction[i, ] >= threshold_pred & real[i, ] < threshold_real, 1, 0), na.rm=T)
        FN[i] = sum(ifelse(prediction[i, ] < threshold_pred & real[i, ] >= threshold_real, 1, 0), na.rm=T)
      }
      TP = sum(TP[i])
      FP = sum(FP[i])
      FN = sum(FN[i])

      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      Class_Thres = list(Recall, Precision)
      names(Class_Thres) = c("Recall", "Precision")
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


Recall=unlist(f[1])
Prec=unlist(f[2])

F_Score=(2*Recall*Prec)/(Recall+Prec)
names(F_Score)="F Score"
F_Score


# Classification Item 10
#Classification(IBCF$prediction, test, threshold=3)


