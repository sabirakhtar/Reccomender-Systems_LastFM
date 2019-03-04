
library(proxy)

##### 1) Load the Data ######
#############################
source('01_Pre-processing.R')

### 2) Load the Functions ###
#############################
source('02_Functions.R')

# Spread the data to convert in matrix form
user_artists_mat <- spread(user_artists, artist_name, weight_discrete)

# Row names should be considered as user names and columns as artists
row.names(user_artists_mat) <- user_artists_mat$userID
user_artists_mat$userID <- NULL

# Convert to matrix
user_artists_mat <- as(user_artists_mat,"matrix")


### 3) Split train - Test ###
#############################
set.seed(2)
train_rows <- sample(1:nrow(user_artists_mat), 0.7*nrow(user_artists_mat))

train <- user_artists_mat[train_rows,]
test <- user_artists_mat[-train_rows,]


#######################################################

### 4) User-based CF ###
########################
ptm <- proc.time()
UBCF <- UserBasedCF(train, test,N=5, NN=10, onlyNew=TRUE)
Time <- (proc.time() - ptm)
Time

save(UBCF, file = "Data/UBCF.RData")


### 5) Item-based CF ###
########################
ptm <- proc.time()
IBCF <- ItemBasedCF(train, test,N=5, NN=10, onlyNew=TRUE)
Time <- (proc.time() - ptm)
Time

save(IBCF, file = "Data/IBCF.RData")



### 6) Cluster-based CF ###
###########################
ptm <- proc.time()
CBCF <- ClusterBasedCF(user_artists_mat, N = 5, centers = 200, iter = 100)
Time <- (proc.time() - ptm)
Time

save(CBCF, file = "Data/CBCF.RData")


### 7) Content-based CF ###
###########################

# Row names should be considered as artists
row.names(user_taggedartists_content) <- user_taggedartists_content$name
user_taggedartists_content$name <- NULL

# Convert to matrix
user_taggedartists_content <- as(user_taggedartists_content,"matrix")

ptm <- proc.time()
CB <- ContentBased(user_taggedartists_content, test, 5, 10, onlyNew=T)
Time <- (proc.time() - ptm)
Time

save(CB, file = "Data/CB.RData")


##################
### Evaluation ###
##################

### Evaluate ###
# RMSE
RMSE(CB$prediction, test)
RMSE(IBCF$prediction, test)
RMSE(UBCF$prediction, test)
RMSE(as.matrix(CBCF$prediction[row.names(test),]), test)

# MAE
MAE(CB$prediction, test)
MAE(IBCF$prediction, test)
MAE(UBCF$prediction, test)
MAE(as.matrix(CBCF$prediction[row.names(test),]), test)

# Classification
Classification(CB$prediction, test, threshold=5)
Classification(IBCF$prediction, test, threshold=5)
Classification(UBCF$prediction, test, threshold=5)
Classification(as.matrix(CBCF$prediction[row.names(test),]), test, threshold=5)
