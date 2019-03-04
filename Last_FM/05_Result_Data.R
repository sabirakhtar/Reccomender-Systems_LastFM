

df=as.data.frame(CBCF$prediction) ########## Reading the Cluster Based predictions #########
df$UserID=rownames(df)            ########## Creating UserID Column


################ Restructing the data set#################
library(reshape)  
mdata <- melt(df, id="UserID")
data=mdata[order(-mdata$value),]

data1=data %>% group_by(UserID) %>% slice(1:5)
Artists <- read_delim("Data/Artists.dat", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)

####### Merging Artist and CBCF to get final datsets #########################
library(dplyr)

final_data=merge(data1,Artists,by.x="variable",by.y="name",all.x = T)
final_data$url=NULL
final_data$pictureURL=NULL
names(final_data)
 
names(final_data)<-c("Artist","UserID","Score","ArtistID")
final_data=final_data[,c(2,4,1,3)]

write.csv(final_data,"Data/data.csv",row.names = F)

