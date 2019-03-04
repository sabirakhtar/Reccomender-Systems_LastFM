library(readr)
library(tidyr)
library(dplyr)

Artists <- read_delim("Artists.dat",
                      "\t", escape_double = FALSE, trim_ws = TRUE)
tags <- read_delim("tags.dat", "\t",
                   escape_double = FALSE, trim_ws = TRUE)
user_artists <- read_delim("user_artists.dat",
                           "\t", escape_double = FALSE, trim_ws = TRUE)
user_taggedartists <- read_delim("user_taggedartists.dat",
                                 "\t", escape_double = FALSE, trim_ws = TRUE)


#### Pre-processing ####
Artists <- Artists[,c("id","name")]
user_taggedartists <- user_taggedartists[,c("userID","artistID","tagID")]

#### Keeping users and artist same in both user_artists and user_taggedartists ###

user_taggedartists <- user_taggedartists[user_taggedartists$userID %in% intersect(user_artists$userID, user_taggedartists$userID)
                                         &
                                        user_taggedartists$artistID %in% intersect(user_artists$artistID, user_taggedartists$artistID),]


user_artists <- user_artists[user_artists$userID %in% intersect(user_artists$userID, user_taggedartists$userID)
                             &
                               user_artists$artistID %in% intersect(user_artists$artistID, user_taggedartists$artistID) ,]

length(unique(user_artists$artistID)) == length(unique(user_taggedartists$artistID))
length(unique(user_artists$userID)) == length(unique(user_taggedartists$userID))



user_artists <- user_artists[user_artists$userID %in% intersect(user_artists$userID, user_taggedartists$userID)
                             &
                               user_artists$artistID %in% intersect(user_artists$artistID, user_taggedartists$artistID) ,]


user_taggedartists <- user_taggedartists[user_taggedartists$userID %in% intersect(user_artists$userID, user_taggedartists$userID)
                                         &
                                           user_taggedartists$artistID %in% intersect(user_artists$artistID, user_taggedartists$artistID),]

length(unique(user_artists$artistID)) == length(unique(user_taggedartists$artistID))
length(unique(user_artists$userID)) == length(unique(user_taggedartists$userID))


###################################################

user_artists <- merge(x = user_artists, y = Artists,
                      by.x = "artistID", by.y = "id", all.x = T)

# Categorize the continuous data
summary(user_artists$weight)
boxplot(user_artists$weight)


user_artists$weight_discrete <- ifelse(
                    user_artists$weight <= as.numeric(quantile(user_artists$weight, 0.1)),
                    1,ifelse(user_artists$weight <= as.numeric(quantile(user_artists$weight, 0.2)),
                    2,ifelse(user_artists$weight <= as.numeric(quantile(user_artists$weight, 0.3)),
                   3,ifelse(user_artists$weight <= as.numeric(quantile(user_artists$weight, 0.4)),
                  4,ifelse(user_artists$weight <= as.numeric(quantile(user_artists$weight, 0.5)),
                 5,ifelse(user_artists$weight <= as.numeric(quantile(user_artists$weight, 0.6)),
                6,ifelse(user_artists$weight <= as.numeric(quantile(user_artists$weight, 0.7)),
               7,ifelse(user_artists$weight <= as.numeric(quantile(user_artists$weight, 0.8)),
              8,ifelse(user_artists$weight <= as.numeric(quantile(user_artists$weight, 0.9)),
             9,10)))))))))

user_artists <- user_artists[,c("userID", "name","weight_discrete")]
names(user_artists) <- c("userID", "artist_name","weight_discrete")

table(user_artists$weight_discrete)


user_taggedartists <- merge(x = user_taggedartists, y = Artists,
                            by.x = "artistID", by.y = "id", all.x = T)

user_taggedartists <- merge(x = user_taggedartists, y = tags,
                            by.x = "tagID", by.y = "tagID", all.x = T)

user_taggedartists <- user_taggedartists[,c("userID","name","tagValue")]
names(user_taggedartists) <- c("userID", "artist_name","tag_value")

nrow(unique(user_taggedartists))
nrow(unique(user_artists))
length(unique(user_artists$userID))
length(unique(user_artists$artist_name))
length(unique(user_taggedartists$userID))
length(unique(user_taggedartists$artist_name))
length(unique(user_taggedartists$tag_value))

rm(Artists)
rm(tags)

user_taggedaartist_content <- user_taggedartists %>%
  group_by(artist_name,tag_value) %>%
  summarise(tag_count = n())

summary(user_taggedaartist_content$tag_count)
hist(user_taggedaartist_content$tag_count)

user_taggedaartist_content <- spread(user_taggedaartist_content, tag_value, tag_count)
user_taggedaartist_content[is.na(user_taggedaartist_content)] <- 0
#View(user_taggedaartist_content[1:100,1:10])
