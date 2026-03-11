library(tidyverse)
library(readxl)
library("readr")
library(ggplot2)

# read in data from Excel spreadsheet
df <- read_excel("~/Desktop/BIOL 5501/BIOL 5501 RESEARCH/Schedule 1 - Part 2_ Endangered Fish Species - BIOL 5501 (WIP).xlsx", sheet = 1)
View(df)

# clean data for test
chub <- df[2:23,] %>% # keep only rows associated with Chub
  select(-Species) %>% # get rid of species column 
  filter(Journal == "Journal of Great Lakes Research") %>% # keeping only target journal
  rename(tot.cit.2yr = "Total Citable Items (Previous 2 Years)") %>% # renaming to make it easier to code with
  mutate(JIF = as.numeric(JIF), # make JIF a number
         tot.cit.2yr = as.numeric(tot.cit.2yr)) # make a number (instead of character)
View(chub)

# calculating mean JIF for the journal and mean total citable items
mean_jif <- mean(chub$JIF) # mean JIF for JGLR is 1.55

# calculating COSEWIC listing IF (ESA listing IF equivalent)
n.cit <- chub %>%
  group_by(Journal) %>% # group by journal
  summarize(mean.cit = mean(tot.cit.2yr), # average total citable items per journal
            count = n()) %>% # count number of times journal is cited in report 
  mutate(cos.if = count/mean_cit) # calculate COSEWIC listing IF

# test on more journals 
all.chub <- df[2:23,] %>% # keep only rows associated with Chub
  select(-Species) %>% # get rid of species column 
  rename(tot.cit.2yr = "Total Citable Items (Previous 2 Years)") %>% # renaming to make it easier to code with
  mutate(JIF = as.numeric(JIF), # make JIF a number
         tot.cit.2yr = as.numeric(tot.cit.2yr)) %>% # make a number (instead of character)
  na.omit()

n.cit.all.chub <- all.chub %>%
  group_by(Journal) %>% # group by journal
  summarize(mean.jif = mean(JIF), # average JIF per journal
            mean.cit = mean(tot.cit.2yr), # average total citable items per journal
            count = n()) %>% # count number of times journal is cited in report 
  mutate(cos.if = count/mean_cit) # calculate COSEWIC listing IF

setwd("/Users/khristianne_e/Desktop/BIOL 5501/BIOL 5501 RESEARCH")
View(overton)
# Group by species. Search for most recently updated. Omit the older ones. 
# String split. Group by species name. Group them by species name, manually look for the most recent one. 

test.chub <- overton %>%
  filter()

n.cit.all.chub <- all.chub %>%
  group_by(Journal) %>%
  summarize(mean.jif = mean(JIF),
            mean.cit = mean(tot.cit.2yr),
            count = n()) %>%
  mutate(cos.if = count/mean.cit)
