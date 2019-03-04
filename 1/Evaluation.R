# Mean Absolute Error
meanabserror <- function(actual,predicted)
{
  mean(abs(actual - predicted))
}

# Example data
actual <- c(4, 6, 9, 10, 4, 6, 4, 7, 8, 7)
predicted <- c(5, 6, 8, 10, 4, 8, 4, 9, 8, 9)

meanabserror(actual,predicted)
MLmetrics::mae(actual,predicted)
forecast::accuracy(actual,predicted)


# F1 Score
F1_Score(actual, predicted)


f1score=function(tp,fp,fn)
{
  f=(2*tp)/(2*tp+fp+fn)
  return(f)
}
