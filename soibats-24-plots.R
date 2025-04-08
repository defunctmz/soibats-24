# Packages ----
install.packages("pacman")
pacman::p_load(pacman,
               tidyverse,
               lubridate,
               igraph,
               tidygraph,
               ggraph,
               ggrepel,
               oaqc,
               taxize)

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

# PT - Revised ----
## updated lists and merging revised data ----
# some part of this is repeated code, to maintain flow and ease of reruns on different systems

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
set.seed(0) # make sure to run set seed with zero before each run for exaact reproduction
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

# Non pteropotid network - clean up ----
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

# ### clustering groups - only for testing
# communities <- cluster_fast_greedy(as.igraph(npt_graph))  # Detect communities
# npt_graph <- npt_graph %>%
#   mutate(cluster = as.factor(membership(communities)))
# ###

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
  
# NPT Revised ----

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
set.seed(1) 
npt_layout_centrality <- layout_with_kk(as.igraph(npt_graph),
                                        weights = E(as.igraph(npt_graph))$weight)

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

# test revisions ----
## manual concentric layout ---- 

# Get node data
nodes <- as_tibble(npt_graph, active = "nodes")
nodes <- nodes %>% mutate(id = row_number())

# Separate bats and diet items
bats <- nodes %>% filter(type == "Bat") %>% arrange(desc(size))
diet <- nodes %>% filter(type == "Diet")

# Number of each
n_bats <- nrow(bats)
n_diet <- nrow(diet)

# Assign inner circle layout for diet nodes
diet$angle <- seq(0, 2 * pi, length.out = n_diet + 1)[- (n_diet + 1)]
diet$x <- cos(diet$angle) * 1
diet$y <- sin(diet$angle) * 1

# Assign outer circle layout for bats (sorted by degree)
bats$angle <- seq(0, 2 * pi, length.out = n_bats + 1)[- (n_bats + 1)]
bats$x <- cos(bats$angle) * 2
bats$y <- sin(bats$angle) * 2

# Combine layout data
layout_df <- bind_rows(diet, bats) %>%
  arrange(id) %>%
  select(x, y)

# Plot using manual layout
ggraph(npt_graph, layout = "manual", x = layout_df$x, y = layout_df$y) +
  geom_edge_link(aes(edge_alpha = edgealpha),
                 color = edgecol,
                 width = edgewd,
                 show.legend = FALSE) +
  geom_node_point(aes(color = type, 
                      size = size,
                      shape = type)) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = labelsize) +
  scale_shape_manual(values = c("Bat" = batshp, "Diet" = dietshp)) +
  scale_color_manual(values = c("Bat" = batcol, "Diet" = dietcol)) +
  scale_size_continuous(range = node_sizerange, guide = "none") +
  ggtitle(title) +
  theme_void()

## bats inwards rev ----

# --- Load Data
npt_dt_v2 <- read_csv(file.choose())  # Choose "soibats_es_npt_dietls_v2_30012025.csv"

# --- Create Graph
npt_graph <- tbl_graph(edges = npt_dt_v2, directed = FALSE) %>%
  mutate(
    type = ifelse(name %in% npt_dt_v2$sp, "Bat", "Diet"),
    size = centrality_degree()
  )

# --- Layout: KK Layout 
set.seed(1)
kk_layout <- layout_with_kk(as.igraph(npt_graph))

# Combine layout with node metadata
layout_df <- as_tibble(kk_layout) %>%
  rename(x = V1, y = V2) %>%
  bind_cols(as_tibble(npt_graph, active = "nodes"))

# --- Radial Adjustment: Mild push/pull for clarity
layout_df <- layout_df %>%
  mutate(
    angle = atan2(y, x),
    radius = sqrt(x^2 + y^2),
    radius = case_when(
      type == "Diet" ~ radius * 1.15,  # Slight push
      type == "Bat"  ~ radius * 0.9,   # Slight pull inward
      TRUE ~ radius
    ),
    x = radius * cos(angle),
    y = radius * sin(angle)
  )

# --- Plot Settings
labelsize <- 3.5
edgecol <- "gray70"
edgewd <- 0.6
edgealpha <- 0.5
batshp <- 17
batcol <- "turquoise3"
dietshp <- 16
dietcol <- "orchid"
node_sizerange <- c(2, 6)
title <- "Non-pteropotid bats and diet network\n(Node Size = Degree Centrality)"

# --- Plot
ggraph(npt_graph, layout = "manual", x = layout_df$x, y = layout_df$y) +
  geom_edge_link(color = edgecol,
                 alpha = edgealpha,
                 width = edgewd,
                 show.legend = FALSE) +
  geom_node_point(aes(color = type,
                      size = size,
                      shape = type)) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = labelsize,
                 family = "sans") +
  scale_shape_manual(values = c("Bat" = batshp, "Diet" = dietshp)) +
  scale_color_manual(values = c("Bat" = batcol, "Diet" = dietcol)) +
  scale_size_continuous(range = node_sizerange, guide = "none") +
  ggtitle(title) +
  theme_void()

# bats inwards rev 2 - spacing increase ----

# --- Load Data
npt_dt_v2 <- read_csv(file.choose())  # Choose "soibats_es_npt_dietls_v2_30012025.csv"

# --- Create Graph
npt_graph <- tbl_graph(edges = npt_dt_v2, directed = FALSE) %>%
  mutate(
    type = ifelse(name %in% npt_dt_v2$sp, "Bat", "Diet"),
    size = centrality_degree()
  )

# --- Layout: KK ----
set.seed(1)
kk_layout <- layout_with_kk(as.igraph(npt_graph))

layout_df <- as_tibble(kk_layout) %>%
  rename(x = V1, y = V2) %>%
  bind_cols(as_tibble(npt_graph, active = "nodes"))

# --- Radial Adjustment with Overall Spacing ----
# Gently scale all nodes outward, and slightly emphasize diet vs. bat spacing
layout_df <- layout_df %>%
  mutate(
    angle = atan2(y, x),
    radius = sqrt(x^2 + y^2),
    radius = radius * 1.3,  # Uniform radial expansion for spacing
    radius = case_when(
      type == "Diet" ~ radius * 1.05,  # Slightly more outward for diets
      type == "Bat"  ~ radius * 0.95,  # Slightly less for bats
      TRUE ~ radius
    ),
    x = radius * cos(angle),
    y = radius * sin(angle)
  )

# --- Plot Settings ----
labelsize <- 3.5
edgecol <- "gray70"
edgewd <- 0.6
edgealpha <- 0.5
batshp <- 17
batcol <- "turquoise3"
dietshp <- 16
dietcol <- "orchid"
node_sizerange <- c(2, 6)
title <- "Non-pteropotid bats and diet network\n(Node Size = Degree Centrality)"

# --- Plot ----
ggraph(npt_graph, layout = "manual", x = layout_df$x, y = layout_df$y) +
  geom_edge_link(color = edgecol,
                 alpha = edgealpha,
                 width = edgewd,
                 show.legend = FALSE) +
  geom_node_point(aes(color = type,
                      size = size,
                      shape = type)) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = labelsize,
                 family = "sans") +
  scale_shape_manual(values = c("Bat" = batshp, "Diet" = dietshp)) +
  scale_color_manual(values = c("Bat" = batcol, "Diet" = dietcol)) +
  scale_size_continuous(range = node_sizerange, guide = "none") +
  ggtitle(title) +
  theme_void()

# top 10 prey concentric rev 3 ----

# --- Load Data ----
# Choose "soibats_es_npt_dietls_v2_30012025.csv" when prompted
npt_dt_v2 <- read_csv(file.choose())

# --- Create Graph ----
npt_graph <- tbl_graph(edges = npt_dt_v2, directed = FALSE) %>%
  mutate(
    type = ifelse(name %in% npt_dt_v2$sp, "Bat", "Diet"),
    size = centrality_degree()
  )

# --- Prepare Node Data ---
# Extract node data from graph
node_df <- as_tibble(npt_graph, active = "nodes")

# Identify top 10 prey nodes by degree (size)
top_prey <- node_df %>%
  filter(type == "Diet") %>%
  arrange(desc(size)) %>%
  head(10)

# All other prey nodes
other_prey <- node_df %>%
  filter(type == "Diet" & !(name %in% top_prey$name))

# Bat nodes remain as is
bats <- node_df %>%
  filter(type == "Bat")

# --- Compute Custom Layout Positions ---
# We use polar coordinates to assign nodes to layers:
#   - Top Prey: small circle at center (radius ~0.5)
#   - Bats: arranged on an intermediate circle (radius ~1.5)
#   - Remaining Prey: on outer circle (radius ~3.0)

# Function to assign positions on a circle given radius and number of nodes
# takes the number of nodes from nrow of the df
# angles holds a seq of equidistant angles from 0 to 2pi, equal to the number of nodes, i.e. n
# create n+1 nodes, then remove the last node - seq() will have the first and last angles both in the sequence
# but those two, first and last, will overlap in a circle, so you remove the n + 1th position angle from the seq 
# x and y get the coordinates for the nodes to be plotted

assign_circle <- function(df, radius) {
  n <- nrow(df)
  angles <- seq(0, 2*pi, length.out = n + 1)[- (n + 1)]
  df %>% mutate(x = radius * cos(angles),
                y = radius * sin(angles))
}

# Apply the function for each group with chosen radii:
top_prey <- assign_circle(top_prey, radius = 0.5)
bats     <- assign_circle(bats, radius = 1.5)
other_prey <- assign_circle(other_prey, radius = 3.0)

# --- Combine the Layout ---
# Combine the three groups back together
# store the coordinates, x and y, and other node attributes together
new_layout <- bind_rows(top_prey, bats, other_prey)

# Ensure that the layout ordering matches the original node order (by name)
new_layout <- new_layout %>% arrange(match(name, node_df$name))

# --- Plot Settings ---
labelsize <- 3.5
edgecol <- "gray58"
edgewd <- 0.6
edgealpha <- 0.3
batshp <- 17
batcol <- "turquoise3"
dietshp <- 16
dietcol <- "orchid"
node_sizerange <- c(2, 6)
title <- "Custom Network Layout\n(Top 10 Prey at Center, Bats in Middle, Rest of Prey Outer)"

# --- Plot the Graph using Manual Layout ---
ggraph(npt_graph, layout = "manual", x = new_layout$x, y = new_layout$y) +
  geom_edge_link(color = edgecol, alpha = edgealpha, width = edgewd, show.legend = FALSE) +
  geom_node_point(aes(color = type, size = size, shape = type)) +
  geom_node_text(aes(label = name), repel = TRUE, size = labelsize) +
  scale_shape_manual(values = c("Bat" = batshp, "Diet" = dietshp)) +
  scale_color_manual(values = c("Bat" = batcol, "Diet" = dietcol)) +
  scale_size_continuous(range = node_sizerange, guide = "none") +
  ggtitle(title) +
  theme_void()

# Taxonomy -----

## randomly generated data as a demo
# tx_dt <- tibble(year = rep(seq(1950,2024,1),3),
#                 sp_count = sample(1:1,
#                                   NROW(seq(1950,2024,1))*3,
#                                   replace = T),
#                 sp_group = sample(LETTERS[1:3],
#                                   NROW(seq(1950,2024,1))*3,
#                                   replace = T))
# 
# View(tx_dt)

## read in data ----
tx_dt <- tibble(read_csv("raw_data_files/taxonomy_13032025.csv"))
# use file taxonomy_13032025.csv here

tx_dt_csum <- tx_dt %>% 
  mutate(csum = cumsum(sp_count))

## vars ----   
lwd = 0.8
col = "darkgreen"
## plot ----

ggplot(tx_dt_csum,aes(year,
                      csum)) +
  annotate(geom = "rect",
           xmin = -Inf, xmax = 1947,
           ymin = -Inf, ymax = Inf,
           fill = "lightgrey", alpha = 0.5) +
  annotate("text",
           x = 1850, y = 105,
           label = "Pre-independence",
           size = 5, fontface = "bold.italic", alpha = 0.8) +
  annotate(geom = "rect",
           xmin = 1947, xmax = 2023,
           ymin = -Inf, ymax = Inf,
           fill = "lightblue", alpha = 0.5) +
  annotate("text",
           x = 1985, y = 105,
           label = "Post-independence",
           size = 5, fontface = "bold.italic", alpha = 0.8) +
  geom_vline(xintercept = 1947,
             linetype = "dashed",
             color = "darkgrey",
             linewidth = 0.8) +
  scale_y_continuous(breaks = c(seq(0,150,25))) +
  scale_x_continuous(breaks = c(seq(1758,2023,50))) +
    geom_line(linewidth = lwd, col = col) +
  labs(x = "Year", y = "Cumulative number of species described")+
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Citizen Science ----

# Fig 1 cit-sci ----

## data input ----

cs_fig1_dt <- read_csv(file.choose()) 
## use cs_fig1_year_time_series_17032025.csv 

## processing ----
cs_fig1 <- cs_fig1_dt %>% 
  pivot_longer(cols = c(inat,ibp),
               names_to = "dataset",
               values_to = "count")

## calculate cumulative sum for each dataset
cs_fig1 <- cs_fig1 %>% 
  group_by(dataset) %>%
  arrange(year) %>% 
  mutate(csum = cumsum(count))

## time series line graph ----

## vars ----
ibpcol =  "steelblue"
inatcol = "violet"

# plot
ggplot(cs_fig1,
       aes(x = year, y = csum, color = dataset)) +
  geom_line(linewidth = 0.8,
            position = "jitter") +
  scale_y_continuous(breaks = c(seq(0,300,300),seq(500,3000,500))) +
  scale_x_continuous(breaks = c(seq(1994,2024,5))) +
  scale_color_manual(labels = c("IBP","Inaturalist"),
                     values = c("ibp" = ibpcol,
                              "inat" = inatcol),
                     name = "Dataset") +
  labs(x = "Year",
       y = "Cumulative number of observations") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
  
# Fig 2 cit-sci ----

## data input ----

cs_fig2_dt <- read_csv(file.choose())
# use cs_fig2_monthly_obs_17032025.csv

## processing ----
cs_fig2 <- cs_fig2_dt %>%
  pivot_longer(cols = c(inat,ibp),
               names_to = "dataset",
               values_to = "count")

## vars ----
ibpcol =  "steelblue"
inatcol = "violet"

## plot ----

ggplot(cs_fig2,
       aes(x = sr, y = count, color = dataset)) +
  geom_line(linewidth = 0.8) +
  geom_label(aes(label = count), col = "black") +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  scale_y_continuous(breaks = c(seq(0,40,10),seq(50,350,50),400)) +
  scale_color_manual(values = c("ibp" = ibpcol,
                                "inat" = inatcol),
                     labels = c("IBP","Inaturalist"),
                     name = "Dataset") +
  labs(x = "Month",
       y = "Number of observations (1994-2024)") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 0.5,
                                   hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

## merged datasets version
cs_fig2_dtmerge <- cs_fig2 %>% 
  ungroup() %>% 
  select(-dataset) %>% 
  group_by(sr) %>% 
  mutate(monthly_counts = sum(count)) %>% 
  select(-count) %>% 
  unique()

## merged datasets plot ----

ggplot(cs_fig2_dtmerge,
       aes(x = sr, y = monthly_counts)) +
  geom_line(linewidth = 0.8, col = "steelblue") +
  geom_label(aes(label = monthly_counts), col = "black") +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  labs(x = "Month",
       y = "Number of observations (1994-2024)") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 0.5,
                                   hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

# Fig 5 Proportion of representation of bat taxa in datasets (by family) ----

## - data input ----

cs_fig5_masterlist_dt <- read_csv(file.choose())
# use "cs_fig5_sp_master_list_18032025.csv"
# this will be used to derive the number of species in each family present in India

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

# processing - combining the two datasets into one larger set 

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
  mutate(id = if_else(database == "ibp", catalogNumber, id)) %>%
  mutate(latitude = if_else(database == "ibp", locationLat, latitude)) %>%
  mutate(longitude = if_else(database == "ibp", locationLon, longitude)) %>%
  mutate(scientific_name = if_else(database == "ibp", scientificName, scientific_name)) %>%
  select(database,
         id,
         latitude,
         longitude,
         scientific_name,
         rank,
         taxon_id)

write_csv(csci_occ_dt,"csci_occ_dt_27012025.csv")

## combined datasheet input ----

cs_fig5_datasets_dt <- read_csv(file.choose())
# use "csci_occ_dt_27012025.csv" 

## Explainer ---- 
# steps below - 
# filter out unique taxon names
# fetch family names then to get a list of species names with families recorded in the two datasets
# total up numbers by family names to get the proportions
# Match these numbers to the number of species in each family reported as in the master list for the country 

cs_fig5_sp_family <-  cs_fig5_datasets_dt %>% 
  select(scientific_name) %>% 
  group_by(scientific_name) %>% 
  mutate(obs_count = n()) %>% 
  unique()
  
# using package taxise to fetch family names from scientific names

cs_fig5_sp_family$family <- tax_name(sci = cs_fig5_sp_family$scientific_name,
                                 get = "family",
                                 db = "itis")
