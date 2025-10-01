library(tidyverse)
library(readxl)

# read in data from Excel spreadsheet
df <- read_excel("~/Desktop/Schedule 1 - Part 2_ Endangered Fish Species - BIOL 5501 (WIP).xlsx", sheet = 1)

# clean data 
chub <- df[2:23,] %>% # keep only rows associated with Chub
  select(-Species) %>% # get rid of species column 
  filter(Journal == "Journal of Great Lakes Research") %>% # keeping only target journal
  rename(tot.cit.2yr = "Total Citable Items (Previous 2 Years)") %>% # renaming to make it easier to code with
  mutate(JIF = as.numeric(JIF), # make JIF a number
         tot.cit.2yr = as.numeric(tot.cit.2yr)) # make a number (instead of character)

# calculating mean JIF for the journal and mean total citable items
mean_jif <- mean(chub$JIF) 
mean_cit <- mean(chub$tot.cit.2yr)


