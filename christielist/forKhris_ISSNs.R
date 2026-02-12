khris <- read_delim("testkhris.csv") %>%
  rename(journal = "journals_cited") %>%
  mutate(journal = str_to_upper(journal)) %>%
  select(-count)
christie <- read_delim("christielist.csv") %>%
  rename(journal = "Journal title") %>%
  select(journal, ISSN, eISSN)

test <- left_join(khris,christie, by = c("journal" = "journal"))

i.nas <- is.na(test$ISSN)
sum(i.nas)
e.nas <- is.na(test$eISSN)
sum(e.nas)
