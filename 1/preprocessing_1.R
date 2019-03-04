df=CBCF$prediction
df$UserID=rownames(df)
library(reshape)
mdata <- melt(df, id="UserID")
sortdata=sort(mdata$value,decreasing = T)
head(sortdata)
final_data=mdata[order(-mdata$value),]
