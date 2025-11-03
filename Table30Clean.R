library(tidyverse)
library(readxl)
library("readr")
library(ggplot2)
library(dplyr)
library(ggpubr)
library(knitr)


setwd("/Users/khristianne_e/Desktop/BIOL 5501/BIOL 5501 RESEARCH")

top30updated <- read_csv("updatedtop30journals.csv")
# Pulled the updated top 30 journals csv file from the given excel sheet from overton database. 
View(top30updated)

clean_top30 <- na.omit(top30updated)
# Took out all the journals without values. While majority of the JIFs were found on the clarivate database, some were unavailable and were manually searched for from each of their publishing websites. Some were not found.
View(clean_top30)

table_clean_top30 <- clean_top30[, c("journals_cited", "count", "jif")]
print(table_clean_top30)
View(table_clean_top30)
# Making sure that there's proper columns needed. 

tablecleantop30_sorted <- table_clean_top30 %>% 
  arrange(desc(count)) %>% # Organizing the data set by descending order of "counts" (i.e., the amount of times these journals were cited).
  rename("Cited Journals"=journals_cited, "Count" = count, "Journal Impact Factor (JIF)" = jif) # Renamed the columns to have proper column names necessary for the final table print.
View(tablecleantop30_sorted)


### suggestion from Sara: make a second table, same as above, but sorted in descending order by JIF

table_plot <- ggtexttable( tablecleantop30_sorted,rows = NULL,  # Hide row numbers
  theme = ttheme( colnames.style = colnames_style(color = "white", fill = "steelblue", face = "bold", hjust = 0.5), tbody.style = tbody_style(fill = c("white", "gray95"), hjust = 0.5))) #Found this function online. 

table_plot
ggsave("top30_table_plot.pdf", table_plot, width = 8, height = 10)

# How to make the table with ggplot? 
# Why can't I make it an Rmd file? Why is it an R file? confusion. 
