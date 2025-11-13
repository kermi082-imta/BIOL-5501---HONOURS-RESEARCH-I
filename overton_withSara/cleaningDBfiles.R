library(tidyverse)
setwd("~/Desktop/BIOL 5501/BIOL 5501 RESEARCH/overton_withSara")

df <- read_delim("cleanDB_wip.csv")
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

dupyears <- read_delim("dupYears.csv")
View(dupyears)
#Opening the "dupYears.csv" previously created. 

df %>%
  filter(common == "American eel") %>%
  distinct(year) # 2012 and 2006

# Searching for the most recent assessment. 
df %>%
  filter(common == "American marten") %>%
  distinct(year) # 2022 and 2007

df %>%
  filter(common == "Bering cisco") %>%
  distinct(year) #2005 and 1970

df %>%
  filter(common == "American eel") %>%
  distinct(year) # 2012

df %>%
  filter(common == "American marten") %>%
  distinct(year) # 2022

df %>%
  filter(common == "Bird's-foot violet") %>%
  distinct(year) # 2003 and 2002

df %>%
  filter(common == "Black redhorse") %>%
  distinct(year) # 2015 and 2005

df %>%
  filter(common == "Blue shark") %>%
  distinct(year) # 2017 and 2006

df %>%
  filter(common == "Bluehearts") %>%
  distinct(year) 

#Wanted to check if there's a more efficient way. Found a function online that selects the max value. Used on "year" column while grouping "common" column. 
df_latest <- df %>%
  group_by(common) %>%
  slice_max(order_by = year) #Found function online that allows us to select max value. 
#Searched df for "American Eel" and the year provided, showed 2012 which is the most recent supported by the filter and distinct functions. 

#ASK SARA: Would there be a way to get rid of all the duplicates? Instead of having one species list a year multiple times, can it just list it once? 
View(df_latest)

