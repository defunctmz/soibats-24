# Packages ----
install.packages("pacman")
pacman::p_load(pacman,tidyverse)

# EB group - Overall - Data cleaning ----

# Paper publications distribution by year, region and categories

# import datasheet
eb_basedt <- read_csv("eb_citations_rawdata.csv",
                      skip = 1,
                      name_repair = "universal",
                      trim_ws = T)

View(eb_basedt)

# assign unique id for joins
eb_basedt <- eb_basedt %>% 
  mutate(id = seq.int(1,nrow(eb_basedt),1))

# long form categories
eb_dt_cat <- eb_basedt %>%
  select(citation:Toxicology,id) %>% 
  pivot_longer(cols = Diet:Toxicology,
               names_to = "category",
               values_drop_na = T) 

# long form states
eb_dt_location <- eb_basedt %>% 
  select(citation,Andaman.and.Nicobar.Islands:West.Bengal,id) %>% 
  pivot_longer(cols = Andaman.and.Nicobar.Islands:West.Bengal,
               names_to = "location",
               values_drop_na = F)


test <- eb_dt_location %>% select(-location) %>% group_by(id,value) %>% 
  unique()

