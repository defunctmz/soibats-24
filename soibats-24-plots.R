# Packages ----
install.packages("pacman")
pacman::p_load(pacman,
               tidyverse,
               lubridate,
               igraph,
               tidygraph,
               ggraph,
               ggrepel,
               oaqc)

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

## long form categories ----
eb_dt_cat <- eb_basedt %>%
  select(citation:Toxicology,id) %>% 
  pivot_longer(cols = Diet:Toxicology,
               names_to = "category",
               values_drop_na = T) 

eb_dt_cat[which(duplicated(eb_dt_cat$id)),]

View(eb_dt_cat)
## long form states (location) ----
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

## long form year of study ----
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

## join tables - id column is key ----

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

## plotting ----

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


## variable declarations for tweaks ----
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

# tweaks - variable declarations
lwd = 1

# Year-wise publication trends by categories - cumulative
ggplot(pub_cumsum,aes(year,pub_cumsum,group = category, colour = category)) +
  geom_line(linewidth = lwd)+
  labs(x = "Year of Publication", y = "Cumulative number of publications")+
  theme_classic()

# State-wise heatmap (hm) - formatting data for QGIS - state names made to match the shapefile names ----
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

# Ecosystem services network - Data clean up ----

## Pteropotid network clean up ----
pt_dt <- read_csv(file.choose()) # use file named "Pteropodid_v1_20250111.csv" here

p_diet_long <- pt_dt %>%
  mutate(Diet = strsplit(Diet, ",\\s*")) %>%  # Split the Diet column by commas
  unnest(Diet) %>%  # Expand into multiple rows
  mutate(Diet = trimws(Diet))  # Remove surrounding whitespace from Diet values

# file check and write
p_dt <- p_diet_long

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

compare <- cbind(p_dt,p_diet_long)

colnames(compare) <- c("sp1","diet1","sp2","diet2")

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

## updated lists and merging revised data ----
# some part of this is a repeated code, to maintain flow and ease reruns on different systems

pt_dt <- read_csv(file.choose()) # use file named "Pteropodid_v1_20250111.csv" here

p_diet_long <- pt_dt %>%
  mutate(Diet = strsplit(Diet, ",\\s*")) %>%  # Split the Diet column by commas
  unnest(Diet) %>%  # Expand into multiple rows
  mutate(Diet = trimws(Diet))  # Remove surrounding whitespace from Diet values

# file check and write
p_dt <- p_diet_long

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

compare <- cbind(p_dt,p_diet_long)

colnames(compare) <- c("sp1","diet1","sp2","diet2")

sp_list <- compare %>%
  mutate(id = seq(1:nrow(compare))) %>% 
  select(id,sp1,diet1) %>%  # edited coloumn
  distinct(diet1,sp1,.keep_all = T) # id assigning for each row


pt_dt_rev <- read_csv(file.choose()) # use file name "soibats_es_pt_dietls_23012025.csv" here

# join the dataset using the old species name and connect the new species names - genera name isolation was done externally

pt_dt_rev_merge <- full_join(sp_list,
                             pt_dt_rev,
                             by = join_by(diet1 == species),
                             multiple = "all",
                             relationship = "many-to-many",
                             keep = T)

# check duplicated rows in the original dataset
dupli_df <- p_dt[which(duplicated(p_dt)),]
View(dupli_df)

# check the correct number of duplicated rows are out in the merged dataset
nrow(p_dt)
nrow(dupli_df)
nrow(p_dt)-nrow(dupli_df)

rm(dupli_df)

# clean up dataframe
pt_dt_v2 <- pt_dt_rev_merge %>% 
  select(-id.y,
         -species) %>% 
  distinct(sp1,
           sp_update,
           .keep_all = T)

# readable colnames
colnames(pt_dt_v2) <- c("id","sp","diet_old","diet","diet_genus")

# reorder cols
pt_dt_v2 <- pt_dt_v2 %>% 
  select(id,sp,diet,diet_genus,diet_old)

View(pt_dt_v2)

# network data formatting
pt_dt_v2_genera <- pt_dt_v2 %>% 
  drop_na() %>% 
  select(sp,diet_genus) %>% 
  unique()

View(pt_dt_v2_genera)

# calculate quick numbers for species and diet genera - ignore
pt_dt_stats <- pt_dt_v2 %>% 
  drop_na() %>% 
  select(sp,diet_genus) %>% 
  unique() %>% 
  group_by(sp) %>% 
  mutate(diet_genera_count = n()) %>% 
  select(-diet_genus) %>% 
  unique() %>% 
  arrange(desc(diet_genera_count))

View(pt_dt_stats)

## ggraph plots -----

pt_graph <- tbl_graph(edges = pt_dt_v2_genera, directed = F)

pt_graph <- pt_graph %>% 
  mutate(type = ifelse(name %in% pt_dt_v2_genera$sp, "Bat", "Diet"),
         size = centrality_degree())

# check for size attribute being correctly calculated
pt_dt_v2_genera %>%
  group_by(diet_genus) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count)) %>% 
  distinct(diet_genus,.keep_all = T)

# check graph object for size attribute, compare with above
pt_nodes <- as_tibble(pt_graph, active = "nodes") ; View(pt_nodes)

# additional edges check
pt_edges <- as_tibble(pt_graph, active = "edges") ; View(pt_edges)

# ### clustering groups - only for testing
# pt_communities <- cluster_fast_greedy(as.igraph(pt_graph))  # Detect communities
# 
# pt_graph <- pt_graph %>%
#   mutate(cluster = as.factor(membership(pt_communities)))


# layout to make nodes with higher centrality plotted towards the center of the graph
set.seed(0)
layout_centrality <- layout_with_kk(as.igraph(pt_graph),
                                    weights = E(as.igraph(pt_graph))$weight)

## visual vars ----

layout = layout_centrality
labelsize = 4
edgecol = "lightgray" ; edgewd = 0.6 ; edgealpha = 0.2
batshp = 17 ; batcol = "turquoise"
dietshp = 16 ; dietcol = "violet"
node_sizerange = c(2,6)
title = "Pteropotid Bats and diet network (Node Size = Number of Connections)"

## plot ----
ggraph(pt_graph,
       layout = layout) +
  geom_edge_link(aes(edge_alpha = edgealpha),
                 color = edgecol,
                 width = edgewd,
                 show.legend = F) +
  geom_node_point(aes(color = type, # color by type, bat or prey
                      size = size, # size by number of connections
                      shape = type)) +  # shape by cluster
  geom_node_text(aes(label = name), # label by name
                 repel = T,
                 size = labelsize) +
  scale_shape_manual(values = c("Bat" = batshp,
                                 "Diet" = dietshp)) +
  scale_color_manual(values = c("Bat" = batcol,
                                "Diet" = dietcol)) +
  scale_size_continuous(range = node_sizerange,
                        guide = "none") + # Adjust node size scale 
  ggtitle(title) +
  theme_void()

## Non pteropotid network - clean up ----
npt_dt <- read_csv(file.choose())

str(npt_dt)  # Check the structure of the dataset

# long from diet entries
npt_diet_long <- npt_dt %>%
  mutate(Diet = strsplit(Diet, ",\\s*")) %>%  # Split the Diet column by commas
  unnest(Diet) %>%  # Expand into multiple rows
  mutate(Diet = trimws(Diet))  # Remove surrounding whitespace from Diet values

# prep for igraph format
npt_dt <- data.frame(sp = npt_diet_long$Species,
                     diet = npt_diet_long$Diet)

## ggraph plots ----

npt_graph <- tbl_graph(edges = npt_dt, directed = F)

npt_graph <- npt_graph %>% 
  mutate(type = ifelse(name %in% npt_dt$sp, "Bat", "Prey"),
         size = centrality_degree())

# check for size attribute being correctly calculated
npt_dt %>%
  group_by(diet) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count)) %>% 
  distinct(diet,.keep_all = T)

# check graph object for size attribute, compare with above
npt_nodes <- as_tibble(npt_graph, active = "nodes") ; View(npt_nodes)

# additional edges check
npt_edges <- as_tibble(npt_graph, active = "edges") ; View(npt_edges)

### clustering groups - only for testing
communities <- cluster_fast_greedy(as.igraph(npt_graph))  # Detect communities
npt_graph <- npt_graph %>%
  mutate(cluster = as.factor(membership(communities)))
###

# layout to make nodes with higher centrality plotted towards the center of the graph
npt_layout_centrality <- layout_with_kk(as.igraph(npt_graph),
                                    weights = E(as.igraph(npt_graph))$weight)

## visual vars ----

layout = npt_layout_centrality
labelsize = 4
edgecol = "lightgray" ; edgewd = 0.8 ; edgealpha = 0.1
batshp = 16 ; batcol = "turquoise"
preyshp = 1 ; preycol = "purple"
node_sizerange = c(2,8)
title = "Non-pteropotid Bats and prey network (Node Size = Number of Connections)"

## plot ----
ggraph(npt_graph,
       layout = layout) +
  geom_edge_link(aes(edge_alpha = edgealpha),
                 color = edgecol,
                 width = edgewd,
                 show.legend = F) +
  geom_node_point(aes(color = cluster, # color by type, bat or prey
                      size = size, # size by number of connections
                      shape = type)) +  # shape by cluster
  geom_node_text(aes(label = name), # label by name
                 repel = T,
                 size = labelsize) +
  # scale_shape_manual(values = c("Bat" = batshp,
  #                               "Prey" = preyshp)) +
  # scale_color_manual(values = c("Bat" = batcol,
  #                               "Prey" = preycol)) +
  scale_size_continuous(range = node_sizerange,
                        guide = "none") + # Adjust node size scale 
  ggtitle(title) +
  theme_void()
  
### Revised NPT dataset ----

unique(npt_dt$diet)

# replace non insect diet items to grouped taxa
npt_dt_v2 <- npt_dt

# replace spiders with arachnida
npt_dt_v2$diet <- gsub("\\w*Spider$", "Arachnida", npt_dt_v2$diet)

# check correctly replaced
which(grepl("Spider",npt_dt$diet))
which(grepl("Arachnida",npt_dt_v2$diet))

# replace mites with arachnida
npt_dt_v2$diet <- gsub("\\w*mites$", "Arachnida",npt_dt_v2$diet)

# check correctly replaced
which(grepl("mites",npt_dt$diet))
which(grepl("Arachnida",npt_dt_v2$diet))

# replace non_insect items with vertebrata

# check remaining "Non" items
which(grepl("Non",npt_dt_v2$diet))

# replace
npt_dt_v2$diet <- gsub("Vertebrata.\\w*.\\w*.\\w*.\\w*", "Vertebrata",npt_dt_v2$diet)

# check same indices replaced
which(grepl("Vertebrata",npt_dt_v2$diet))

npt_dt_v2

# check all replacements are correct
cbind(npt_dt$diet,npt_dt_v2$diet) %>% 
View()

# write updated file
write_csv(npt_dt_v2,"soibats_es_npt_dietls_v2_30012025.csv")

### graph ----

npt_dt_v2 <- read_csv(file.choose()) # choose "soibats_es_npt_dietls_v2_30012025.csv"
 
npt_graph <- tbl_graph(edges = npt_dt_v2, directed = F)

npt_graph <- npt_graph %>% 
  mutate(type = ifelse(name %in% npt_dt_v2$sp, "Bat", "Diet"),
         size = centrality_degree())

# check for size attribute being correctly calculated
npt_dt_v2 %>%
  group_by(diet) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count)) %>% 
  distinct(diet,.keep_all = T)

# check graph object for size attribute, compare with above
npt_nodes <- as_tibble(npt_graph, active = "nodes") ; View(npt_nodes)

# additional edges check
npt_edges <- as_tibble(npt_graph, active = "edges") ; View(npt_edges)

# layout to make nodes with higher centrality plotted towards the center of the graph
set.seed(0) 
npt_layout_centrality <- layout_with_gem(as.igraph(npt_graph))

### visual vars ----
layout_fixed = npt_layout_centrality
labelsize = 4
edgecol = "lightgray" ; edgewd = 0.6 ; edgealpha = 0.2
batshp = 17 ; batcol = "turquoise"
dietshp = 16 ; dietcol = "violet"
node_sizerange = c(1,6)
title = "Non-pteropotid bats and diet network (Node Size = Number of Connections)"

### plot ----

ggraph(npt_graph,
       layout = layout_fixed) +
  geom_edge_link(aes(edge_alpha = edgealpha),
                 color = edgecol,
                 width = edgewd,
                 show.legend = F) +
  geom_node_point(aes(color = type, # color by type, bat or prey
                      size = size, # size by number of connections
                      shape = type)) +  # shape by cluster
  geom_node_text(aes(label = name), # label by name
                 repel = T,
                 size = labelsize) +
  scale_shape_manual(values = c("Bat" = batshp,
                                 "Diet" = dietshp)) +
  scale_color_manual(values = c("Bat" = batcol,
                                "Diet" = dietcol)) +
  scale_size_continuous(range = node_sizerange,
                        guide = "none") + # Adjust node size scale 
  ggtitle(title) +
  theme_void()

# Taxonomy -----

## randomly generated data as a demo ----
tx_dt <- tibble(year = rep(seq(1950,2024,1),3),
                sp_count = sample(1:1,
                                  NROW(seq(1950,2024,1))*3,
                                  replace = T),
                sp_group = sample(LETTERS[1:3],
                                  NROW(seq(1950,2024,1))*3,
                                  replace = T))

View(tx_dt)

tx_dt_csum <- tx_dt %>% 
  group_by(year,sp_group) %>% 
  mutate(sp_total = sum(sp_count)) %>% 
  select(-sp_count) %>% 
  unique() %>% 
  ungroup() %>% 
  group_by(sp_group) %>% 
  arrange(year) %>% 
  mutate(cumsum = cumsum(sp_total))

## vars ----   
lwd = 0.8

## plot ----

ggplot(tx_dt_csum,aes(year,
                      cumsum,
                      group = sp_group,
                      colour = sp_group)) +
  geom_line(linewidth = lwd)+
  labs(x = "Year", y = "cumulative number of species described")+
  theme_classic()

# Citizen Science ----

## Data clean up ----

## IBP data ----

ibp_raw <- read_csv(file.choose()) # use file name "IBP-withmedia-raw-24012025.csv" here

ibp_dt <- ibp_raw %>% 
  select(catalogNumber,
         locationLat,
         locationLon,
         rank,
         scientificName,
         order:genus) %>% 
  mutate(database = "ibp")

## iNat data ----

inat_raw <- read_csv(file.choose()) # use file name "Inat-raw-24012025.csv" here

inat_dt <- inat_raw %>% 
  select(id,
         quality_grade,
         latitude,
         longitude,
         scientific_name,
         taxon_id) %>% 
  mutate(database = "inat")

csci_occ_dt <- bind_rows(ibp_dt,inat_dt)

csci_occ_dt <- csci_occ_dt %>% 
  mutate(id = if_else(database == "ibp",catalogNumber,id)) %>% 
  mutate(latitude = if_else(database == "ibp", locationLat,latitude)) %>% 
  mutate(longitude = if_else(database == "ibp",locationLon,longitude)) %>% 
  mutate(scientific_name = if_else(database == "ibp",scientificName,scientific_name)) %>% 
  select(database,
         id,
         latitude,
         longitude,
         scientific_name,
         rank,
         taxon_id)

write_csv(csci_occ_dt,"csci_occ_dt_27012025.csv")
