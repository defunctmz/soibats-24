# Packages ----
install.packages("pacman")
pacman::p_load(pacman,tidyverse)

# EB group - Overall - Data cleaning ----

# Paper publications distribution by year, region and categories

# import datasheet
eb_basedt <- read_csv("eb_citations_data.csv",
                      skip = 1,
                      name_repair = "universal",
                      trim_ws = T)

View(eb_basedt)

# assign unique id for joins
eb_basedt <- eb_basedt %>% 
  mutate(id = seq.int(1,nrow(eb_basedt),1))

# long form categories----
eb_dt_cat <- eb_basedt %>%
  select(citation:Toxicology,id) %>% 
  pivot_longer(cols = Diet:Toxicology,
               names_to = "category",
               values_drop_na = T) 

# long form states----
eb_dt_location <- eb_basedt %>% 
  select(citation,Andaman.and.Nicobar.Islands:West.Bengal,id) %>% 
  pivot_longer(cols = Andaman.and.Nicobar.Islands:West.Bengal,
               names_to = "location",
               values_drop_na = T)

# check all citations are included - ignore this df
test <- eb_dt_location %>% select(-location) %>% group_by(id,value) %>% unique()

# Plowright et al 2019 missing (id = 87) because no location provided (can be assigned Kerala if necessary)
# letting it be

# long form year of study----
eb_dt_year <- eb_basedt %>% 
  select(citation,...2024:id) %>% 
  pivot_longer(cols = ...2024:...1960,
               names_to = "year",
               values_drop_na = T)

# fixing data formats
eb_dt_year$year <- str_remove(eb_dt_year$year,"...")
eb_dt_year$year <- as.numeric(eb_dt_year$year)

# id 17 is a pre-print, assigned the year 2024 given its year of upload


# join tables- id column is key ----

ebd_join_catloc <- full_join(eb_dt_cat,eb_dt_location,"id")
ebd_join_catloc <- ebd_join_catloc %>% select(id,citation.x,category,location)

ebd_join_complete <- left_join(ebd_join_catloc,eb_dt_year,"id")

# check if both citations columns are identical
identical(ebd_join_complete$citation.x,ebd_join_complete$citation)

ebd_join_complete$category <- str_replace_all(ebd_join_complete$category,
                                              "Disease.Ecology..and.Health.",
                                              "Disease.Ecology.and.Health")

# clean up into a df - ebd stands for ecology biology data
ebd <- ebd_join_complete %>% select(id,category,location,year,citation)

# Plotting ----

# Line graph - citations by year and category

pub_years <- ebd %>% 
  select(-location,-citation) %>% 
  unique() %>% 
  group_by(year) %>% 
  mutate(pub_count = n())

ggplot(pub_years) +
  geom_line(aes(year,pub_count,colour = category))
  