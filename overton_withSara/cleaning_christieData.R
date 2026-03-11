library(tidyverse)
library(readxl)

# save file path to a variable
file <- "~/Desktop/MRU_Faculty/Teaching/03_Honours/2025_26/Khris/christie_data.xlsx"

# grab all names of tabs in excel file
sheet_names <- excel_sheets(file)

# read in and combine all sheets into one, rename columns to match, fix data types, keep source ID of sheet 
combined_data <- sheet_names %>%
  set_names() %>% 
  map_dfr(~{
    df <- read_excel(path = file, sheet = .x) # read in each sheet
    df %>% 
      rename(journal_name = contains("Journal name"), # standardize column names
        impact_factor = contains("JIF"),
        citable_items = contains("citable items")) %>%
      mutate(impact_factor = as.numeric(impact_factor), # fix data types
        citable_items = as.numeric(citable_items)) %>%
      mutate(year = .x) %>% # add the year column based on the sheet name
      select(journal_name, year, impact_factor, citable_items)
  }, .id = "source_sheet") # .id is optional, tracks which sheet it came from

clean_data <- combined_data %>%
  distinct(journal_name, year, .keep_all = TRUE) # removes duplicated entries ensuring only one unique journal name per year is kept

# calculate avg citables for prev 2 years
final_df <- clean_data %>%
  arrange(journal_name, year) %>% 
  group_by(journal_name) %>% # group by journal so lags within each journal
  mutate(avg_citable_2yr = (lag(citable_items, 1) + lag(citable_items, 2)) / 2) %>% # create new column with lagged average calculation
  ungroup() # stops futures ops from lagging/slowing

# check which years are NA 
final_df %>%
  filter(is.na(avg_citable_2yr)) %>%
  group_by(year) %>%
  count() %>%
  print(n=28)

# get rid of NAs (for now - probably add it back in manually in the spring, but this is fine for Honours right now)
final_clean_df <- final_df %>%
  filter(!is.na(avg_citable_2yr))

