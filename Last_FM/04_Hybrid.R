
### Load in the data ###
source('01_Pre-processing.R')

### Load in the function ###
source('02_Functions.R')

# Spread the data to convert in matrix form
user_artists_mat <- spread(user_artists, artist_name, weight_discrete)

# Row names should be considered as user names and columns as artists
row.names(user_artists_mat) <- user_artists_mat$userID
user_artists_mat$userID <- NULL

# Convert to matrix
user_artists_mat <- as(user_artists_mat,"matrix")

### Split train - Test ###
set.seed(2)
train_rows = sample(1:nrow(user_artists_mat), 0.7*nrow(user_artists_mat))

train <- as(user_artists_mat, "matrix")[train_rows,]
test <- as(user_artists_mat, "matrix")[-train_rows,]
test1 <- test[1:284, ]
test2 <- test[285:568, ]

### Load individual models ###
load("Data/CB.RData")
load("Data/CBCF.RData")
load("Data/UBCF.RData")
load("Data/IBCF.RData")

CB <- CB$prediction[row.names(test1),]
IBCF <- IBCF$prediction[row.names(test1),]
CBCF <- as.matrix(CBCF$prediction[row.names(test1),])
UBCF <- UBCF$prediction[row.names(test1),]


### Transform results to lists (to be able to use the rowMeans function) ###
CB_list <- as.list(CB)
IBCF_list <- as.list(IBCF)
UBCF_list <- as.list(UBCF)
CBCF_list <- as.list(CBCF)

#####################################
### Hybrid Model 1 - Compute Mean ###
#####################################
hybrid <- rowMeans(cbind(as.numeric(CB_list),as.numeric(IBCF_list),
                         as.numeric(UBCF_list), as.numeric(CBCF_list)), na.rm=T)

### Transform list back to matrix with correct number of dimensions ###
Hybrid_prediction <- matrix(hybrid, nrow=nrow(test1), ncol=ncol(test1))
rownames(Hybrid_prediction) <- rownames(test1)
colnames(Hybrid_prediction) <- colnames(test1)

### Evaluate ###
# RMSE
RMSE(CB, test1)
RMSE(IBCF, test1)
RMSE(UBCF, test1)
RMSE(CBCF, test1)
RMSE(Hybrid_prediction, test1)

# MAE
MAE(CB, test1)
MAE(IBCF, test1)
MAE(UBCF, test1)
MAE(CBCF, test1)
MAE(Hybrid_prediction, test1)

# Classification
Classification(CB, test1, threshold=5)
Classification(IBCF, test1, threshold=5)
Classification(UBCF, test1, threshold=5)
Classification(CBCF, test1, threshold=5)
Classification(Hybrid_prediction, test1, threshold=5)

##########################################
### Hybrid Model 2 - Linear Regression ###
##########################################

# Train a linear Regression
### flatten test1 dataset
test_list <- as.list(test1)

### Transform list and matrices to dataframe
test_df <- data.frame(matrix(unlist(test_list), byrow=T))
CB_df <- data.frame(matrix(unlist(CB_list), byrow=T))
IBCF_df <- data.frame(matrix(unlist(IBCF_list), byrow=T))
UBCF_df <- data.frame(matrix(unlist(UBCF_list), byrow=T))
CBCF_df <- data.frame(matrix(unlist(CBCF_list), byrow=T))

### Combine created dataframes
input <- cbind(test_df, CB_df, IBCF_df, UBCF_df, CBCF_df)
colnames(input) <- c('TARGET', 'CB', 'IBCF','UBCF', 'CBCF')

### Train the linear regression
fit <- lm(TARGET ~ CB + IBCF + UBCF + CBCF, data=input)
summary(fit)


### Transform results to lists (to be able to use the rowMeans function) ###
load("Data/CB.RData")
load("Data/CBCF.RData")
load("Data/UBCF.RData")
load("Data/IBCF.RData")

### Score Models
CB <- CB$prediction[row.names(test2),]
IBCF <- IBCF$prediction[row.names(test2),]
CBCF <- as.matrix(CBCF$prediction[row.names(test2),])
UBCF <- UBCF$prediction[row.names(test2),]

### Matrix to list
test_list2 <- as.list(test2)
CB_list <- as.list(CB)
IBCF_list <- as.list(IBCF)
UBCF_list <- as.list(UBCF)
CBCF_list <- as.list(CBCF)

### List to dataframe
test_df2 <- data.frame(matrix(unlist(test_list2), byrow=T))
CB_df2 <- data.frame(matrix(unlist(CB_list), byrow=T))
IBCF_df2 <- data.frame(matrix(unlist(IBCF_list), byrow=T))
UBCF_df2 <- data.frame(matrix(unlist(UBCF_list), byrow=T))
CBCF_df2 <- data.frame(matrix(unlist(CBCF_list), byrow=T))

### combine dataframes to have an input dataset for linear regression
input2 <- cbind(test_df2, CB_df2, IBCF_df2, UBCF_df2, CBCF_df2)
colnames(input2) <- c('TARGET', 'CB', 'IBCF','UBCF', 'CBCF')

### Predict using the model calculated on test2 dataset
Hybrid_lin_reg <- predict(fit, input2)

### Transform the list of results to matrix with the correct dimensions
Hybrid_lin_reg <- matrix(Hybrid_lin_reg, nrow=nrow(test2), ncol=ncol(test2))
rownames(Hybrid_lin_reg) <- rownames(test2)
colnames(Hybrid_lin_reg) <- colnames(test2)

# Evaluation
# RSME
RMSE(CB, test2)
RMSE(IBCF, test2)
RMSE(UBCF, test2)
RMSE(CBCF, test2)
RMSE(Hybrid_lin_reg, test2)

# MAE
MAE(CB, test2)
MAE(IBCF, test2)
MAE(UBCF, test2)
MAE(CBCF, test2)
MAE(Hybrid_lin_reg, test2)

# Classification
Classification(CB, test2, threshold=5)
Classification(IBCF, test2, threshold=5)
Classification(UBCF, test2, threshold=5)
Classification(CBCF, test2, threshold=5)
Classification(Hybrid_lin_reg, test2, threshold=5)

# We see that we achieve a better RMSE for hybrid model using linear regression but,
# Cluster based filtering achieves betters results for other evaluation parameters.
# Henceforth, we chose Cluster based collaborative filtering output as the best recommendation
################################################################################
