
#### Load the librarires ####
library(readr)
library(tidyr)
library(dplyr)


#### Load the data ####
Artists <- read_delim("Data/Artists.dat", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)
tags <- read_delim("Data/tags.dat", "\t", 
                   escape_double = FALSE, trim_ws = TRUE)
user_artists <- read_delim("Data/user_artists.dat", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)
user_taggedartists <- read_delim("Data/user_taggedartists.dat", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)


#### Keep the Id and Name of the artist in Artists ####
Artists <- Artists[,c("id","name")]


#### Keeping users and artist same in both user_artists and user_taggedartists - Start ###

user_taggedartists <- user_taggedartists[user_taggedartists$userID %in% intersect(user_artists$userID, user_taggedartists$userID)
                                         & user_taggedartists$artistID %in% intersect(user_artists$artistID, user_taggedartists$artistID),]

user_artists <- user_artists[user_artists$userID %in% intersect(user_artists$userID, user_taggedartists$userID)
                             & user_artists$artistID %in% intersect(user_artists$artistID, user_taggedartists$artistID) ,]

# Checking if the users and artists are unique or not
length(unique(user_artists$artistID)) == length(unique(user_taggedartists$artistID))
length(unique(user_artists$userID)) == length(unique(user_taggedartists$userID))

# As the users and artits are not unique perform the following steps again to make them unique
user_artists <- user_artists[user_artists$userID %in% intersect(user_artists$userID, user_taggedartists$userID)
                             & user_artists$artistID %in% intersect(user_artists$artistID, user_taggedartists$artistID) ,]

user_taggedartists <- user_taggedartists[user_taggedartists$userID %in% intersect(user_artists$userID, user_taggedartists$userID)
                                         & user_taggedartists$artistID %in% intersect(user_artists$artistID, user_taggedartists$artistID),]

# Checking if the users and artists are unique or not
length(unique(user_artists$artistID)) == length(unique(user_taggedartists$artistID))
length(unique(user_artists$userID)) == length(unique(user_taggedartists$userID))

# Now they are unique

#### Keeping users and artist same in both user_artists and user_taggedartists - End ###


#### Data Manipulation for Users vs Artists - Start ####

# Merge the artist name to make the data more intuitive
user_artists <- merge(x = user_artists, y = Artists,
                      by.x = "artistID", by.y = "id", all.x = T)

# Check the summary statistics for number of times song has been listened
summary(user_artists$weight)
boxplot(user_artists$weight)

# Categorize the continuous data based of groups calculated from percentiles of 10
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

# Keep the required columns with relevant column names
user_artists <- user_artists[,c("userID", "name","weight_discrete")]
names(user_artists) <- c("userID", "artist_name","weight_discrete")

table(user_artists$weight_discrete)

#### Data Manipulation for Users vs Artists - End ####


#### Data Manipulation for Artists vs Tags - Start ####

# Group the years in VeryOld, Old, Recent and New
user_taggedartists$VeryOld <- ifelse(user_taggedartists$year <= 2005, 1, 0)
user_taggedartists$Old <- ifelse(user_taggedartists$year >= 2006 & user_taggedartists$year <= 2007, 1, 0)
user_taggedartists$Recent <- ifelse(user_taggedartists$year >= 2008 & user_taggedartists$year <= 2009, 1, 0)
user_taggedartists$New <- ifelse(user_taggedartists$year >= 2010 & user_taggedartists$year <= 2011, 1, 0)

# Group the month from January to December
user_taggedartists$Jan <- ifelse(user_taggedartists$month == 1,1,0)
user_taggedartists$Feb <- ifelse(user_taggedartists$month == 2,1,0)
user_taggedartists$Mar <- ifelse(user_taggedartists$month == 3,1,0)
user_taggedartists$Apr <- ifelse(user_taggedartists$month == 4,1,0)
user_taggedartists$May <- ifelse(user_taggedartists$month == 5,1,0)
user_taggedartists$Jun <- ifelse(user_taggedartists$month == 6,1,0)
user_taggedartists$Jul <- ifelse(user_taggedartists$month == 7,1,0)
user_taggedartists$Aug <- ifelse(user_taggedartists$month == 8,1,0)
user_taggedartists$Sep <- ifelse(user_taggedartists$month == 9,1,0)
user_taggedartists$Oct <- ifelse(user_taggedartists$month == 10,1,0)
user_taggedartists$Nov <- ifelse(user_taggedartists$month == 11,1,0)
user_taggedartists$Dec <- ifelse(user_taggedartists$month == 12,1,0)

# Removing the unwated columns
user_taggedartists$day <- NULL
user_taggedartists$month <- NULL
user_taggedartists$year <- NULL


# Merge the Artists name to make the data more intuitive
user_taggedartists <- merge(x = user_taggedartists, y = Artists,
                            by.x = "artistID", by.y = "id", all.x = T)

# Merge the Tag name to make the data more intuitive
user_taggedartists <- merge(x = user_taggedartists, y = tags,
                            by.x = "tagID", by.y = "tagID", all.x = T)

# Removing the unwated columns
user_taggedartists$userID <- NULL
user_taggedartists$artistID <- NULL

# Get a frequency count for each tag
user_taggedartists_2 <- user_taggedartists %>%
  group_by(tagValue) %>%
  summarise(cnt = n()) %>%
  arrange(desc(cnt))

# Group the tags based on frequency of tags Top 50, Top 50 to 100, Mid 100 to 500, Mid 500 to 1000, Bottom 1000 to 2500 and Bottom
user_taggedartists_2$Tag_type <- ifelse(user_taggedartists_2$cnt >= user_taggedartists_2$cnt[50], "Top 50",
                                        ifelse(user_taggedartists_2$cnt >= user_taggedartists_2$cnt[100], "Top 50-100",
                                               ifelse(user_taggedartists_2$cnt >= user_taggedartists_2$cnt[500], "Mid 100-500",
                                                      ifelse(user_taggedartists_2$cnt >= user_taggedartists_2$cnt[1000], "Mid 500-1000",
                                                             ifelse(user_taggedartists_2$cnt >= user_taggedartists_2$cnt[2500], "Bottom 1000-2500",
                                                                    "Bottom"
                                                                    
                                                             )))))

user_taggedartists_2$cnt <- NULL

# Merge the tag categories created
user_taggedartists <- merge(x = user_taggedartists, y = user_taggedartists_2,
                            by.x = "tagValue", by.y = "tagValue", all.x = T)

# Remove unwanted column
user_taggedartists$tagValue <- NULL
user_taggedartists$tagID <- NULL
user_taggedartists$Tag_type_cnt <- 1

# Aggregate the data per Artist name
user_taggedartists_content <- user_taggedartists %>%
  group_by(name) %>%
  summarise(VeryOld = sum(VeryOld),
            Old = sum(Old),
            Recent = sum(Recent),
            New = sum(New),
            Jan = sum(Jan),
            Feb = sum(Feb),
            Mar = sum(Mar),
            Apr = sum(Apr),
            May = sum(May),
            Jun = sum(Jun),
            Jul = sum(Jul),
            Aug = sum(Aug),
            Sep = sum(Sep),
            Oct = sum(Oct),
            Nov = sum(Nov),
            Dec = sum(Dec),
            Top_50 = sum(Tag_type == "Top 50"),
            Top_50to100 = sum(Tag_type == "Top 50-100"),
            Mid_100to500 = sum(Tag_type == "Mid 100-500"),
            Mid_500to1000 = sum(Tag_type == " Mid 500-1000"),
            Bottom_1000to2500 = sum(Tag_type == "Bottom 1000-2500"),
            Bottom = sum(Tag_type == "Bottom")
  )

#### Data Manipulation for Artists vs Tags - End ####

# Remove unwanted data
rm(user_taggedartists)
rm(Artists)
rm(tags)
rm(user_taggedartists_2)
