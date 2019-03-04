
user_artists_mat <- spread(user_artists, artist_name, weight_discrete)
user_artists_mat[is.na(user_artists_mat)] <- 0
View(user_artists_mat[1:100,1:10])

row.names(user_artists_mat) <- user_artists_mat$userID
user_artists_mat$userID <- NULL

user_artists_mat <- as(user_artists_mat,"matrix")
View(user_artists_mat[1:100,1:10])

### 2) Read In Functions ###
############################

source("functions.R")


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

UBCF <- UserBasedCF(train, test, N=3, NN=10, onlyNew=TRUE)

predictionUBCF <- as.data.frame(UBCF$prediction)
topNUBCF <- as.data.frame(UBCF$topN)

TimeUBCF <- (proc.time() - ptm)
TimeUBCF

#######################################################

