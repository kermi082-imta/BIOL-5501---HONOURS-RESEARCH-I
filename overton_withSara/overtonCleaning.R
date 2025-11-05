library(tidyverse)

# load in the reports CSV and make a test data set of five obs to try things out.
df <- read_delim("overton_withSara/reports.csv") #loading in the "reports.csv" file into R. 
test <- df[1:5,] %>% # choosing the first 5 entries from the reports file. 
  select("Title of citing document") %>% 
  # Only having the first 5 entries and their "Title of Citing Document" Column. 
  rename(title = "Title of citing document") 
# Renamed the long column name to "title" for more efficient coding. 

# testing regex to extract strings and make new column with date 
str_extract(test$title,"(\\d\\d\\d\\d)+(?=E)") 
#allowed us to extract the 4 wildcard digits that occur before the "E" from the title in "test". 
# we know that they should all be 2010, checking to see if the code worked in the way we wanted it to. 

test %>% 
  mutate(year = str_extract(test$title,"(\\d\\d\\d\\d)+(?=E)")) 
# using the string extract function, the 4 wildcard digits that are before the "E" from the title in "test" is taken and mutated to create a new column in the "test" data set called "year". 

# testing the separate for species name pull
test %>%
  separate(title, into = c(NA, "int1"), sep = "COSEWIC assessment and status report on the ") %>% 
# In the title column of the "test" data set, the contents are separated. The separator is the "COSEWIC...." with the contents prior to the separator being ignored while the contents after the separator are placed into the column labelled "int1". 
  separate(int1, into = c("full.spp", NA), sep = ", in Canada") %>%
# Using the separate function, the contents of int1 column is separated with the separator being ", in Canada", keeping the contents prior to the separator and placing it into a column labelled "full.spp" and ignoring the contents after the separator.  
  separate(full.spp, into = c("Common", "Latin"), sep = ', ')
# Using the separate function, the contents of full.spp column is separated with the separator being ", ", keeping the contents on both side of the separator with the contents prior placed in the "Common" column and the contents after the separator placed in the "Latin" column. 

# full df clean - kinda janky but keeping the full.test$year as separate from the int.clean obj makes it easier to pull on previously created obj

df.pub.year <- df %>% #cleaning title names and separating the year of publication from the pub.date column. 
  rename(title = "Title of citing document", pub.date = "Published on date of citing document") %>%
# from the loaded overton database called "df", the long titles were renamed for more efficient coding.
  separate(pub.date, into = c("pub.year"), sep = '-', extra = "drop") 
#separates the first part of the pub.date using the "-" separator and drops the other parts. 

# making intermediate obj so string extract works properly
int.year <- df.pub.year %>%
  mutate(year = str_extract(df.pub.year$title,"(\\d\\d\\d\\d)+(?=E)"))
#string extract from title. 

# make int obj to replace year with pub.year if year is NA 
int.year.narepl <- int.year %>%
  mutate(year = ifelse(is.na(int.year$year), int.year$pub.year, int.year$year))
#ASK SARA!!! - How to explain this code? 
# New column called "year" which already exists in the int.year object. If there's no information in the int.year column pull the information from pub.year. IFELSE FUNCTIION!!!

# create clean df with spp names pulled out into Common & Latin columns
df.clean <- int.year.narepl %>%
  mutate(pub.title = title) %>%
  separate(title, into = c(NA, "int1"), sep = "COSEWIC assessment and status report on ") %>%
  # trial and error. separator can't include "the" because of the way they wrote the title. 
  separate(int1, into = c("full.spp", NA), sep = ", in Canada") %>%
  separate(full.spp, into = c("common", "latin"), sep = ', ') %>%
  select(common, latin, year, `Cited source or journal`, pub.year, pub.title)

# some NAs in species name based on differing titles of reports - mostly that some reports are 'update status' rather than 'status'
#Some species were listed as NA in the common name, the titles were slightly different. 

# pull all the species that have NA as Common because they are update reports 
update.spp <- df.clean %>%
  filter(is.na(common))

# separate all names based on update status report 
update.clean <- update.spp %>%
  separate(pub.title, into = c(NA, "int1"), sep = "COSEWIC assessment and update status report on ", remove = F) %>%
  separate(int1, into = c("full.spp", NA), sep = ", in Canada") %>%
  separate(full.spp, into = c("common", "latin"), sep = ', ') %>%
  select(common, latin, year, `Cited source or journal`, pub.year, pub.title)

# it worked! no NAs 
update.clean %>%
  distinct(common) %>%
  filter(is.na(common))

head(df.clean)
head(update.clean)

# remove all spp with Common = NA (because those spp are in update.clean now)
df.clean.noups <- df.clean %>%
  filter(!is.na(common))

# combine status report spp with update status report spp
df.clean.combo <- bind_rows(df.clean.noups, update.clean)

# check for any NAs in common name 
missing.common <- df.clean.combo %>%
  filter(is.na(common)) # none! 

# check for any NAs in Latin name
missing.latin <- df.clean.combo %>%
  filter(is.na(latin)) #872 :( 

clean.missing.lat <- missing.latin %>%
  separate(common, into = c(NA, "common"), sep = 'the ') %>%
  separate(common, into = c("common", NA), sep = ' in Canada.') %>%
  separate(common, into = c("common", "latin"), sep = ' \\(')

clean.missing.lat$latin <- gsub(")", "", clean.missing.lat$latin)
# In the latin column a gsub ")" replace it with noting in the clean.missing.lat$latin column which will be save in the clean.missing.lat

# remove spp with NAs in Latin
final.clean <- df.clean.combo %>%
  filter(!is.na(latin)) %>% 
  bind_rows(clean.missing.lat) %>%
  separate(common, into = c(NA, "common"), sep = 'the ') %>%
  rename(source = "Cited source or journal")
#final.clean are missing species that don't have a latin name. 

# find the entries that will need manual changing bc they do not all have the same patterns to match
missing.latin.final <- final.clean %>% 
  filter(is.na(latin)) %>%
  distinct(pub.title) # only NA 
# All reports in the current data frame that don't have a latin name. 

missing.common.final <- final.clean %>%
  filter(is.na(common)) %>%
  distinct(latin)
# Manually place the common name. 

# find entries with more than one year to manually choose which report to keep
dup.years <- final.clean %>%
  select(-c(source)) %>%
  group_by(common, latin) %>%
  summarise(n.years = n_distinct(year), .groups = "drop") %>%
  filter(n.years > 1)
# 66 species that have more than 2 observation. Find more recent year and choose that one. 

# write out CSVs 
write_delim(missing.latin.final, "overton_withSara/missingLatin.csv", delim = ',')
write_delim(missing.common.final, "overton_withSara/missingCommon.csv", delim = ',')
write_delim(dup.years, "overton_withSara/dupYears.csv", delim = ',')
write_delim(final.clean, "overton_withSara/cleanDB.csv", delim = ',')

