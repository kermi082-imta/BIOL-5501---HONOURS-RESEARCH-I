library(tidyverse)
setwd("~/Desktop/BIOL 5501/BIOL 5501 RESEARCH/overton_withSara")

df <- read_delim("cleanDB_wip.csv") %>%
  mutate(common = str_to_title(common))
View(df)

#Grouping Common and Latin names with NAs
na_commonandlatin <- df %>%
  filter(is.na(common), is.na(latin)) %>% #Filtered for species that contain NAs in their common and latin columns. 
  group_by(pub.title) %>% #Grouped them by the pub.title column. 
  summarise(count = n())
head(na_commonandlatin) 
#Showed only on pub.title doesn't have either common or latin names which makes sense since this assessment is on the Athabasca endemics which encompasses multiple species. Also shows that there are 63 citations from this assessment. 

na_sources <- df %>%
  filter(is.na(source)) %>%
  group_by(common) %>%
  summarise(count = n())
# Shows how many NAs per "common" species are found in the "source" column. 
View(na_sources)


#Wanted to check if there's a more efficient way. Found a function online that selects the max value. Used on "year" column while grouping "common" column. 
cleanestdb <- df %>%
  group_by(common) %>%
  slice_max(order_by = year) #Found function online that allows us to select max value. 
View(cleanestdb) 

df_latest_countsource <- cleanestdb %>%
  count(source)
View(df_latest_countsource)
#Searched df for "American Eel" and the year provided, showed 2012 which is the most recent supported by the filter and distinct functions. 
write_delim(cleanestdb, "cleanestdb.csv", delim = ',')

View(cleanestdb) 

df_latest_unique_sources <- df_latest %>%
  distinct(common, source) %>%
  group_by(common) %>%
  summarise(count_sources = n())

View(df_latest_unique_sources) 
