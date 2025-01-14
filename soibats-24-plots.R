# Packages ----
install.packages("pacman")
pacman::p_load(pacman,tidyverse,lubridate)

# EB group - Overall - Data cleaning ----

# import datasheet
eb_basedt <- read_csv("eb_citations_data_v2.csv",
                      skip = 1,
                      name_repair = "universal",
                      trim_ws = T)

View(eb_basedt)

# assign unique id for joins
eb_basedt <- eb_basedt %>% 
  mutate(id = seq.int(1,nrow(eb_basedt),1))

# long form categories ----
eb_dt_cat <- eb_basedt %>%
  select(citation:Toxicology,id) %>% 
  pivot_longer(cols = Diet:Toxicology,
               names_to = "category",
               values_drop_na = T) 

eb_dt_cat[which(duplicated(eb_dt_cat$id)),]

View(eb_dt_cat)
# long form states (location) ----
eb_dt_location <- eb_basedt %>% 
  select(citation,Andaman.and.Nicobar.Islands:West.Bengal,id) %>% 
  pivot_longer(cols = Andaman.and.Nicobar.Islands:West.Bengal,
               names_to = "location",
               values_drop_na = T)

View(eb_dt_location)


# check for duplicated entries

nrow(eb_dt_location)
View(eb_dt_location[which(duplicated(eb_dt_location$id)),])
nrow(eb_dt_location) - count(eb_dt_location[which(duplicated(eb_dt_location$id)),])

# check for missing citation IDs

# takes the ID numbers from the table, compares to a seq of all numbers b/w min and max
# this works here because the seq starts from one - elsewhere, if need be, generate a seq from the first desired number to the last manually and compare to seq from data source

# check any missing citation ID
nrow(eb_dt_location) - count(eb_dt_location[which(duplicated(eb_dt_location$id)),])

idchk_seq <- eb_dt_location$id
idchk_seq2 <- min(eb_dt_location$id):max(eb_dt_location$id)
idchk_seq2[!idchk_seq2 %in% idchk_seq]

# missing are OK for eb_citations_data_v2, location not applicable for missing IDs

# long form year of study ----
eb_dt_year <- eb_basedt %>% 
  select(citation,...2024:id) %>% 
  pivot_longer(cols = ...2024:...1960,
               names_to = "year",
               values_drop_na = T)

# fixing data formats
eb_dt_year$year <- str_remove(eb_dt_year$year,"...")
eb_dt_year$year <- format(as.Date(eb_dt_year$year,
                                  format = "%Y"),
                          "%Y") ; View(eb_dt_year)

# id 17 is a pre-print, assigned the year 2024 given its year of upload

# check any missing citation ID 

# check any missing citation ID

idchk_seq <- eb_dt_year$id
idchk_seq2 <- min(eb_dt_year$id):max(eb_dt_year$id)
idchk_seq2[!idchk_seq2 %in% idchk_seq]

# desired output of above chunk is integer[0]; If any are missing, those IDs will show instead. Year should not be missing for citations

# join tables - id column is key ----

ebd_join_catloc <- full_join(eb_dt_cat,eb_dt_location,"id")
ebd_join_catloc <- ebd_join_catloc %>% select(id,citation.x,category,location)

ebd_join_complete <- left_join(ebd_join_catloc,eb_dt_year,"id")

# check if both citations columns are identical
identical(ebd_join_complete$citation.x,ebd_join_complete$citation)
# intended output is TRUE

# minor correction
ebd_join_complete$category <- str_replace_all(ebd_join_complete$category,
                                              "Disease.Ecology..and.Health.",
                                              "Disease.Ecology.and.Health")

# clean up into a df - ebd stands for ecology biology data
ebd <- ebd_join_complete %>%
  select(id,category,location,year,citation) %>% 
  arrange(year)

View(ebd)

# fix formatting of year column

ebd$year <- as.Date(ebd$year,
                    format = "%Y")
ebd$year <- year(ebd$year)

str(ebd)

# optionally turn into factor in case of issues of ordering of cumulative numbers
# ebd$year <- as.factor(ebd$year)

# number of citations by category
ebd %>% 
  select(id,category) %>% 
  unique() %>% 
  group_by(category) %>% 
  mutate(count = n()) %>% 
  select(-id) %>% 
  unique() %>% 
  View()

ebd$year <- as.factor(ebd$year)

# Plotting ----

# Cumulative number of citations by categories

# data formatting 
pub_years <- ebd %>% 
  select(-location,-citation) %>% 
  unique() %>%
  group_by(year,category) %>%
  mutate(pub_count = n()) %>% 
  select(-id) %>% 
  unique()

View(pub_years)

pub_cumsum <- pub_years %>% 
  ungroup() %>% 
  group_by(category) %>% 
  mutate(pub_cumsum = cumsum(pub_count))

View(pub_cumsum)

# check pub_cumsum endpoint totals match that of category wise citations totals, code available above 


# Variable declarations for tweaks ----
lwd = 1

# plot 
ggplot(pub_cumsum,aes(year,pub_cumsum, group = category, colour = category)) +
  geom_line(linewidth = lwd)+
  labs(x = "Year of Publication",
       y = "Cumulative number of publications")+
  theme_classic()

View(pub_years)

pub_cumsum <- pub_years %>% 
  ungroup() %>% 
  group_by(category) %>% 
  mutate(pub_cumsum = cumsum(pub_count))

View(pub_cumsum)

# tweaks - variable declarations ----
lwd = 1

# Year-wise publication trends by categories - cumulative
ggplot(pub_cumsum,aes(year,pub_cumsum,group = category, colour = category)) +
  geom_line(linewidth = lwd)+
  labs(x = "Year of Publication", y = "Cumulative number of publications")+
  theme_classic()

# State-wise heatmap (hm) - formatting data for QGIS - state names made to match the shapefile names
eb_hmdt <- eb_dt_location %>% 
  select(id,location) %>% 
  group_by(location) %>% 
  mutate(pub_count = n()) %>% 
  select(-id) %>% 
  unique() %>%
  arrange(desc(pub_count)) %>% 
  mutate(location = str_replace_all(location,"\\."," ")) %>%
  mutate(location = str_replace_all(location,"Andaman and Nicobar Islands", "Andaman & Nicobar")) %>% 
  write_csv("eb_heatmap_dt.csv")

# Ecosystem services group
a <- read_csv(file.choose())

bat_diet_long <- a %>%
  mutate(Diet = strsplit(Diet, ",\\s*")) %>%  # Split the Diet column by commas
  unnest(Diet) %>%  # Expand into multiple rows
  mutate(Diet = trimws(Diet))  # Remove surrounding whitespace from Diet values

# Step 3: Add Genera Column
bat_diet_long <- bat_diet_long %>%
  mutate(Genera = word(Diet, 1))  # Extract Genera (first word from Diet)
# file check and write
p_dt <- bat_diet_long

# remove sp. and similar suffixes as well as whitespace char
#  \\s+ is more than one whitespace, plus is for more than one
#  \\w is one, or more characters when paired with *
#  \\. is for period
#  | is OR condition
#  $ checks for the string being at the end only
p_dt$Diet <- str_remove_all(p_dt$Diet, "\\s+sp\\.$|\\s+sp\\w$")
p_dt$Diet <- str_remove_all(p_dt$Diet, "\\s+spp\\.$")
p_dt$Diet <- str_remove_all(p_dt$Diet, "D4")
p_dt$Diet <- str_remove_all(p_dt$Diet, "\\s+sp$")


compare <- cbind(p_dt,bat_diet_long)
colnames(compare) <- c("sp1","diet1","g1","sp2","diet2","g2")
compare <- compare %>% 
  select(diet1,diet2)

View(compare)

sp_list <- compare %>% 
  mutate(id = seq(1:nrow(compare))) %>% 
  select(id,diet1) %>%  # edited coloumn
  distinct(diet1,.keep_all = T)

colnames(sp_list) <- c("id","species") 

write_csv(sp_list,
          "soibats_es_pt_dietls_15012025.csv",
          col_names = T)
