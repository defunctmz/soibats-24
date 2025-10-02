# Packages ----
# Install pacman if not available to load required pkgs at once
install.packages("pacman")

# Load listed packages (except  pacman) individually if pacman gives issues
pacman::p_load(pacman, # can be ignored
               tidyverse,
               lubridate,
               igraph,
               tidygraph,
               ggraph,
               ggrepel,
               ggalt, # can be ignored
               oaqc, # can be ignored
               taxize) # can be ignored

## Throughout this script, datasets are loaded using file.choose() - the raw dataset files are available within the folders named raw_data_files and processed_data_files. At each line where a dataset is to be loaded, a comment mentions the name of the file to be used. Please select the files carefully 

# EB - Data clean up ----
## Data input -----
eb_basedt <- read_csv(file.choose(), # use eb_citations_data_v3_10042025.csv
                      skip = 1,
                      name_repair = "universal",
                      trim_ws = T)

View(eb_basedt)

# assign unique id for joins
eb_basedt <- eb_basedt %>% 
  mutate(id = seq.int(1,
                      nrow(eb_basedt),
                      1))

## Long form - Categories (of studies) ----
eb_dt_cat <- eb_basedt %>%
  select(citation:Toxicology,id) %>% 
  pivot_longer(cols = Diet:Toxicology,
               names_to = "category",
               values_drop_na = T) 

# see which papers are mentioned in more than one category
eb_dt_cat[which(duplicated(eb_dt_cat$id)),]

View(eb_dt_cat)

###  Revision to paper set ----

## remove habitat category literature that isn't a direct field study 
## (selection criteria explained elsewhere in the report)

eb_dt_cat_v2 <- eb_basedt %>%
  select(citation:Toxicology,id, selected) %>% 
  pivot_longer(cols = Diet:Toxicology,
               names_to = "category",
               values_drop_na = T)

# see which papers are mentioned in more than one category
eb_dt_cat_v2[which(duplicated(eb_dt_cat_v2$id)),]

# total entries in dataset
base_count <- nrow(eb_dt_cat_v2)

# total number of habitat papers in the original set
nrow(eb_dt_cat_v2[(eb_dt_cat_v2$category == "Habitat"),])

# number of papers from habitat category that will be selected
habitat_count_rev <- nrow(eb_dt_cat_v2[(eb_dt_cat_v2$category == "Habitat" & !(is.na(eb_dt_cat_v2$selected))), ])

# list out (temporary) which habitat papers are being removed
View(eb_dt_cat_v2[(eb_dt_cat_v2$category == "Habitat" & is.na(eb_dt_cat_v2$selected)),])

# number of entries being filtered out
removed_count <- nrow(eb_dt_cat_v2[(eb_dt_cat_v2$category == "Habitat" & is.na(eb_dt_cat_v2$selected)),]) ; message("subtract this number from the total number of entries, thats the expected reduced number of entries")

# list the ID numbers of citations that got filtered out - to be used later, please run this chunk
removed_citations <- eb_dt_cat_v2$category == "Habitat" & is.na(eb_dt_cat_v2$selected)

removed_ids <- eb_dt_cat_v2$id[removed_citations]

# Revise the paper set now
# Filter out (remove) entries that are habitat category and not NA for column named "selected" 

eb_dt_cat_v3 <- eb_dt_cat_v2 %>% 
  filter(!(category == "Habitat" & is.na(selected)))

base_count ; removed_count ; nrow(eb_dt_cat_v3)
# see to it that this revised number of entries is the expected reduced number

## Long form - States (location) ----
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

# check for citations that are missing, by their IDs, by comparing which ID numbers in the series are missing in the list

# takes the ID numbers from the table, compares to a seq of all numbers b/w min and max
# this works here because the seq starts from one - elsewhere, if need be, generate a seq from the first desired number to the last manually and compare to seq from data source

# check any missing citation ID
nrow(eb_dt_location) - count(eb_dt_location[which(duplicated(eb_dt_location$id)),])

idchk_seq <- eb_dt_location$id
idchk_seq2 <- min(eb_dt_location$id):max(eb_dt_location$id)
idchk_seq2[!idchk_seq2 %in% idchk_seq]

# missing IDs OK for eb_citations_data_v2, location not applicable for missing IDs
# Missing IDs OK for as well as eb_citations_data_v3_10042025 as well


## Long form - Year (of studies) ----
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

# ID 17 is a pre-print, assigned the year 2024 given its year of upload

# check any missing citation ID

idchk_seq <- eb_dt_year$id
idchk_seq2 <- min(eb_dt_year$id):max(eb_dt_year$id)
idchk_seq2[!idchk_seq2 %in% idchk_seq] ; message("Desired output of this line is integer[0]; \n Year should not be missing for citations ; \n If any entries have missing year, \n corresponding IDs will output here, check for issues")

## Join tables - id column is key ----

ebd_join_catloc <- full_join(eb_dt_cat_v3,eb_dt_location,"id")

# Ignore the many to many warning if shown in console

## Match IDs of habitat citations removed at the category stage to the IDs that show up as NA on category
# Check that only those are removed at the next step

removed_matcher <- is.na(ebd_join_catloc$category)

removed_matcher_ids <- unique(ebd_join_catloc$id[removed_matcher])

setdiff(removed_ids,removed_matcher_ids) ; message("It is expected to get 133 as output, that ID will not match. This citation is filtered out at category stage and has no location, so it gets removed at category stage and the ID is recorded, but it gets removed at the location stage as well, those IDs are not recorded so it hits as a non matched ID. Any other number here is a problem, check")

# clean up the combined citation dataset now
ebd_join_catloc <- ebd_join_catloc %>% select(id,citation.x,category,location)

# remove rows with category as NA, these are the habitat category papers intended to be filtered out

# base entry count
rowcount_base <- nrow(ebd_join_catloc)

# number of entries removed
rowcount_rm <- nrow(ebd_join_catloc[is.na(ebd_join_catloc$category),])

# remove entries
ebd_join_catloc_v2 <- ebd_join_catloc %>% 
  filter(!(is.na(category)))

# entries before, entries removed, revised entries count
rowcount_base ; rowcount_rm ; nrow(ebd_join_catloc_v2)

# add year data
ebd_join_complete <- left_join(ebd_join_catloc_v2,eb_dt_year,"id")

# compare citation IDs of this dataset with the IDs of removed citations

# see that number of non-matching IDS here are the same as the number of removed IDs at category stage
NROW(removed_ids) ; NROW(setdiff(removed_ids, ebd_join_complete$id))

# check none of the removed IDs are in the new dataset
removed_ids %in% ebd_join_complete$id ; message("expected all elements to output false")

# check if any other ID is missing from the list
seq <- seq(min(ebd_join_complete$id),
           max(ebd_join_complete$id),
           1)

excl_IDS <- setdiff(seq,ebd_join_complete$id)

setdiff(removed_ids,excl_IDS); message("Expected to see 138 as output. The test seq in the previous step takes min and max of the revised dataset that ends at 137 entries, ID 138 is beyond the revised min:max range, was removed at category stage, so, expected to be an unmatched element here, ignore. Check for any other numbers appearing here")

# check if both citations columns are identical
identical(ebd_join_complete$citation.x,ebd_join_complete$citation) ; message("intended output is TRUE")

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

# revised expected number of papers in habitat category should match this variable below
habitat_count_rev

ebd$year <- as.factor(ebd$year)

# EB Fig 1 ---- 

# Cumulative number of citations by categories

## Data formatting ---- 
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

# pub count by category
slice_max(pub_cumsum,
          order_by = pub_cumsum,
          n = 1)

# check pub_cumsum endpoint totals match that of category wise citations totals, code available above 

# Review revision 02-10-2025 - Reorder the categories
  
# convert categories as factor
pub_cumsum$category <- as_factor(pub_cumsum$category)

# converting as factor seems to somehow give the desired order of factors already, so not doing it explicitly anymore

## Variables for tweaks ----

# small df to create category specific labels
label_df <- pub_cumsum %>% slice_max(order_by = pub_cumsum,
                                      n = 1)
lwd = 0.8

## Plot ----
eb_fig1 <- ggplot(pub_cumsum,
                  aes(year,pub_cumsum,
                      group = category,
                      colour = category)) +
  geom_line(linewidth = lwd)+
  labs(x = "Year of Publication",
       y = "Cumulative number of publications")+
  scale_color_manual(values = c("steelblue",
                                "violet",
                                "orange",
                                "darkgray",
                                "turquoise"),
                     labels = c("Disease Ecology",
                                "Diet",
                                "Movement Ecology",
                                "Toxicology",
                                "Habitat Use"),
                     name = "Category")+
  geom_point(data = label_df,
             aes(shape = category,
             colour = category),
             size = 3) +
  scale_shape_manual(values = c(17,15,26,8,19), #ignore warning, one invalid pch value for shape given intentionally to create a void shape
                     labels = c("Disease Ecology",
                                "Diet",
                                "Movement Ecology",
                                "Toxicology",
                                "Habitat Use"),
                     name = "Category") +
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 40,
                                   hjust = 0.9,
                                   vjust = 0.9),
        axis.title.x = element_text(vjust = -0.5),
        axis.title.y = element_text(vjust = 2),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, hjust = 0.5))

# implementing ggsave instead of image saving for consistency here - original figures were made at below resolution

 fig_width_pixels = 2159
 fig_height_pixels = 1439
 dpi = 172

ggsave(filename = "eb_fig1_v4_02102025.pdf",
       plot = eb_fig1,
       width = fig_width_pixels/dpi, # only for raster outputs
       height = fig_height_pixels/dpi, # only for raster outputs
       units = "in", 
       dpi = dpi) # only for raster outputs

# EB Heatmap data cleanup ----
## Formatting data for QGIS -----

# state names made to match the shapefile names
eb_hmdt <- ebd %>% 
  select(id,location) %>% 
  group_by(location) %>% 
  mutate(pub_count = n()) %>% 
  select(-id) %>% 
  unique() %>%
  arrange(desc(pub_count)) %>% 
  mutate(location = str_replace_all(location,"\\."," ")) %>%
  mutate(location = str_replace_all(location,"Andaman and Nicobar Islands", "Andaman & Nicobar")) %>% 
  write_csv("eb_heatmap_dt_v2_11042025.csv")

# This figure (Map) is further created with QGIS

# ES - Pteropotid network - clean up ----

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

# calculate quick numbers for species and diet genera - ignore this chunk
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

### ggraph setup -----

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

set.seed(0) # make sure to run set seed with zero before each run for exact reproduction
layout_centrality <- layout_with_kk(as.igraph(pt_graph),
                                    weights = E(as.igraph(pt_graph))$weight)

### Visual tweak vars ----

layout = layout_centrality
labelsize = 4
edgecol = "lightgray" ; edgewd = 0.4 ; edgealpha = 0.4
batshp = 17 ; batcol = "turquoise"
dietshp = 16 ; dietcol = "violet"
node_sizerange = c(2,6)
title = "Pteropotid Bats and diet network (Node Size = Number of Connections)"
legend_lbl = "Node Type"

### Plotting ----
ggraph(pt_graph,
       layout = layout) +
  geom_edge_link(aes(edge_alpha = edgealpha),
                 color = edgecol,
                 width = edgewd,
                 show.legend = F) +
  geom_node_point(aes(color = type, # color by type, bat or prey
                      size = size, # size by number of connections
                      shape = type)) +  # shape by cluster
  geom_node_text(aes(label = paste0("italic('", name, "')")), # label by name, italicised
                 parse = TRUE,
                 repel = TRUE,
                 size = labelsize) +
  scale_shape_manual(values = c("Bat" = batshp,
                                 "Diet" = dietshp),
                     name = legend_lbl) +
  scale_color_manual(values = c("Bat" = batcol,
                                "Diet" = dietcol),
                     name = legend_lbl) +
  scale_size_continuous(range = node_sizerange,
                        guide = "none") + # Adjust node size scale
  theme_void() +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.spacing = unit(8, "pt"),
        legend.box.spacing = unit(0, "pt")) +
  guides(color = guide_legend(override.aes = list(size = 2.5))) # change legend icons with size

# ES - Non-Pteropotid Network - Clean up ----
npt_dt <- read_csv(file.choose()) # use Non_Pteropodid_v1_20250111.csv 

str(npt_dt) # Check the structure of the dataset

# convert to long from diet entries
npt_diet_long <- npt_dt %>%
  mutate(Diet = strsplit(Diet, ",\\s*")) %>%  # Split the Diet column by commas
  unnest(Diet) %>%  # Expand into multiple rows
  mutate(Diet = trimws(Diet))  # Remove surrounding whitespace from Diet values

# prep for igraph format
npt_dt <- data.frame(sp = npt_diet_long$Species,
                     diet = npt_diet_long$Diet)

## Data revisions ----
unique(npt_dt$diet)

# replace non insect diet items to grouped taxa
npt_dt_v2 <- npt_dt

# replace spiders with arachnida
npt_dt_v2$diet <- gsub("\\w*Spider$", "Arachnida", npt_dt_v2$diet)

# check correctly replaced
which(grepl("Spider",npt_dt$diet))
which(grepl("Arachnida",npt_dt_v2$diet))
# index positions of above two line should match

# replace mites with arachnida
npt_dt_v2$diet <- gsub("\\w*mites$", "Arachnida",npt_dt_v2$diet)

# check correctly replaced
which(grepl("mites",npt_dt$diet))
which(grepl("Arachnida",npt_dt_v2$diet))

# replace non_insect items with vertebrata

# check remaining "Non" items
which(grepl("Non",npt_dt_v2$diet))

# replace
npt_dt_v2$diet <- gsub("Non\\w*",
                       "Vertebrata",
                       npt_dt_v2$diet)

# replace
npt_dt_v2$diet <- gsub("Vertebrata.\\w*.\\w*.\\w*.\\w*",
                       "Vertebrata",
                       npt_dt_v2$diet)

# check same indices replaced
which(grepl("Vertebrata",npt_dt_v2$diet))

npt_dt_v2

# check all replacements are correct
cbind(npt_dt$diet,npt_dt_v2$diet) %>% 
View()

# write updated file
write_csv(npt_dt_v2,"soibats_es_npt_dietls_v2_30012025.csv")

# minor revision 

# combine rhinopomma microphyllum and rhinopoma m kinneari as one sp, reclassify diet items 
npt_dt_v3 <- npt_dt_v2 %>% 
  mutate(sp = if_else(sp == "Rhinopoma microphyllum kinneari",
                      "Rhinopoma microphyllum",
                      sp)) %>% 
  unique()

# slight name revisions - Pipistrillus to Pipistrellus
npt_dt_v3 <- npt_dt_v3 %>% 
  mutate(sp = if_else(sp == "Pipistrillus coromandra",
                      "Pipistrellus coromandra",
                      sp)) %>% 
  mutate(sp = if_else(sp == "Pipistrillus mimus",
                      "Pipistrellus tenuis",
                      sp))
  
# write updated file
write_csv(npt_dt_v3,"soibats_es_npt_dietls_v3_02052025.csv")

# review revision 02/10/2025 - Rename some species
npt_dt_v4 <- npt_dt_v3 %>% 
  mutate(sp = if_else(sp =="Megaderma (Lyroderma) lyra",
                      "Lyroderma lyra",
                      sp)) %>%
  mutate(sp = if_else(sp =="Pipistrellus coromandra",
                      "Alionoctula coromandra",
                      sp)) %>%
  mutate(sp = if_else(sp =="Pipistrellus tenuis",
                      "Alionoctula tenuis",
                      sp))

# write updated file
write_csv(npt_dt_v4,"soibats_es_npt_dietls_v4_02102025.csv")

## Plot - Top prey concentric rings version ----

### Load Data ----

# choose "soibats_es_npt_dietls_v4_02102025.csv"
npt_dt_v4 <- read_csv(file.choose())

# Graph Object
npt_graph <- tbl_graph(edges = npt_dt_v4, directed = FALSE) %>%
  mutate(
    type = ifelse(name %in% npt_dt_v4$sp, "Bat", "Diet"),
    size = centrality_degree()
  )

### Prepare Node Data ----

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

### Compute Node Positions ----

# Use polar coordinates to assign nodes to layers:
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
other_prey <- assign_circle(other_prey, radius = 2.5)

### Define Layout ----

# Combine the three groups back together
# store the coordinates, x and y, and other node attributes together
new_layout <- bind_rows(top_prey, bats, other_prey)

# Ensure that the layout ordering matches the original node order (by name)
new_layout <- new_layout %>% arrange(match(name, node_df$name))

### Variables for tweaks ----
labelsize <- 3.5
edgecol <- "lightgray"
edgewd <- 0.4
edgealpha <- 1
batshp <- 17
batcol <- "turquoise3"
dietshp <- 16
dietcol <- "violet"
node_sizerange <- c(2, 6)
title <- "Custom Network Layout\n(Top 10 Prey at Center, Bats in Middle, Rest of Prey Outer)"
legend_lbl = "Node Type"

### Plot ----
npt_network <- ggraph(npt_graph,
       layout = "manual",
       x = new_layout$x,
       y = new_layout$y) +
  geom_edge_link(color = edgecol,
                 alpha = edgealpha,
                 width = edgewd,
                 show.legend = FALSE) +
  geom_node_point(aes(color = type,
                      size = size,
                      shape = type)) +
  geom_node_text(aes(label = ifelse(type == "Bat", paste0("italic('", name, "')"), name)),
                 parse = TRUE, # required for label to be parsed as an expression, for italic function to work
                 repel = TRUE,
                 size = labelsize) +
  scale_shape_manual(values = c("Bat" = batshp,
                                "Diet" = dietshp),
                     name = legend_lbl) +
  scale_color_manual(values = c("Bat" = batcol,
                                "Diet" = dietcol),
                     name = legend_lbl) +
  scale_size_continuous(range = node_sizerange, guide = "none") +
  #ggtitle(title) + # add title if needed
  theme_classic() +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.spacing = unit(8, "pt"),
        legend.box.spacing = unit(0, "pt"),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 2.5))) # change legend icons with size

# export to eps and pdf

ggsave(filename = "es_fig2_npt_network_v3_02102025.eps",
       plot = npt_network,
       width = fig_width_pixels/dpi, # only for raster outputs
       height = fig_height_pixels/dpi, # only for raster outputs
       units = "in", 
       dpi = dpi) # only for raster outputs

# Taxonomy -----

## Data input ----

# use file taxonomy_13032025.csv here
tx_dt <- tibble(read_csv("raw_data_files/taxonomy_13032025.csv"))

# Calculate cumulative numbers of species by year
tx_dt_csum <- tx_dt %>% 
  mutate(csum = cumsum(sp_count))

## Variables for tweaks ----   
lwd = 0.8
col = "darkturquoise"

## Plot ----

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

## CS Fig 1 ----

### Data input ----

## use cs_fig1_year_time_series_17032025.csv 
cs_fig1_dt <- read_csv(file.choose()) 

### Processing ----
cs_fig1 <- cs_fig1_dt %>% 
  pivot_longer(cols = c(inat,ibp),
               names_to = "dataset",
               values_to = "count")

## calculate cumulative sum for each dataset
cs_fig1 <- cs_fig1 %>% 
  group_by(dataset) %>%
  arrange(year) %>% 
  mutate(csum = cumsum(count))

### Variables for tweaks ----
ibpcol =  "darkturquoise"
inatcol = "violet"

### Plot ----
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
        axis.title.y = element_text(vjust = 1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
  
## CS Fig 2 ----

### Data Input ----

# use cs_fig2_monthly_obs_17032025.csv
cs_fig2_dt <- read_csv(file.choose())

### Processing ----
cs_fig2 <- cs_fig2_dt %>%
  pivot_longer(cols = c(inat,ibp),
               names_to = "dataset",
               values_to = "count")

### Variables for tweaks ----
ibpcol =  "darkturquoise"
inatcol = "violet"

### Plot ----

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
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 0.6,
                                   hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

#### Merged datasets version ----
cs_fig2_dtmerge <- cs_fig2 %>% 
  ungroup() %>% 
  select(-dataset) %>% 
  group_by(sr) %>% 
  mutate(monthly_counts = sum(count)) %>% 
  select(-count) %>% 
  unique()

#### Plot ----

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

# Obsolete Code (Ignore) ----

## ES data clean up ----
# # use file named "Pteropodid_v1_20250111.csv" here
# pt_dt <- read_csv(file.choose())
# 
# p_diet_long <- pt_dt %>%
#   mutate(Diet = strsplit(Diet, ",\\s*")) %>%  # Split the Diet column by commas
#   unnest(Diet) %>%  # Expand into multiple rows
#   mutate(Diet = trimws(Diet))  # Remove surrounding whitespace from Diet values
# 
# # file check and write
# p_dt <- p_diet_long
# 
# # Remove sp. and similar suffixes as well as whitespace char
# 
# #  \\s+ is more than one whitespace, plus is for more than one
# #  \\w is one, or more characters when paired with *
# #  \\. is for period
# #  | is OR condition
# #  $ checks for the string being at the end only
# 
# p_dt$Diet <- str_remove_all(p_dt$Diet, "\\s+sp\\.$|\\s+sp\\w$")
# p_dt$Diet <- str_remove_all(p_dt$Diet, "\\s+spp\\.$")
# p_dt$Diet <- str_remove_all(p_dt$Diet, "D4")
# p_dt$Diet <- str_remove_all(p_dt$Diet, "\\s+sp$")
# 
# # data formatting and checks 
# compare <- cbind(p_dt,p_diet_long)
# 
# colnames(compare) <- c("sp1","diet1","sp2","diet2")
# 
# compare <- compare %>% 
#   select(diet1,diet2)
# 
# View(compare)
# 
# sp_list <- compare %>%
#   mutate(id = seq(1:nrow(compare))) %>%
#   select(id,diet1) %>%  # edited coloumn
#   distinct(diet1,.keep_all = T)
# 
# colnames(sp_list) <- c("id","species")
# 
# write_csv(sp_list,
#           "soibats_es_pt_dietls_15012025.csv",
#           col_names = T)


## ES NPT Network plot V1 ----
# 
## # Plotting setup ----
# 
# # choose "soibats_es_npt_dietls_v3_02052025.csv"
# npt_dt_v3 <- read_csv(file.choose()) 
# npt_graph <- tbl_graph(edges = npt_dt_v3, directed = F)
# 
# npt_graph <- npt_graph %>% 
#   mutate(type = ifelse(name %in% npt_dt_v3$sp, "Bat", "Diet"),
#          size = centrality_degree())
# 
# # check for size attribute being correctly calculated
# npt_dt_v3 %>%
#   group_by(diet) %>% 
#   mutate(count = n()) %>% 
#   arrange(desc(count)) %>% 
#   distinct(diet,.keep_all = T)
# 
# # check graph object for size attribute, compare with above
# npt_nodes <- as_tibble(npt_graph, active = "nodes") ; View(npt_nodes)
# 
# # additional edges check
# npt_edges <- as_tibble(npt_graph, active = "edges") ; View(npt_edges)
# 
# # layout to make nodes with higher centrality plotted towards the center of the graph
# set.seed(1) 
# npt_layout_centrality <- layout_with_kk(as.igraph(npt_graph),
#                                         weights = E(as.igraph(npt_graph))$weight)
# 
## ## visual vars ----
# layout_fixed = npt_layout_centrality
# labelsize = 4
# edgecol = "lightgray" ; edgewd = 0.6 ; edgealpha = 0.2
# batshp = 17 ; batcol = "turquoise"
# dietshp = 16 ; dietcol = "violet"
# node_sizerange = c(1,6)
# title = "Non-pteropotid bats and diet network (Node Size = Number of Connections)"
# 
## ### Plot ----
# 
# ggraph(npt_graph,
#        layout = layout_fixed) +
#   geom_edge_link(aes(edge_alpha = edgealpha),
#                  color = edgecol,
#                  width = edgewd,
#                  show.legend = F) +
#   geom_node_point(aes(color = type, # color by type, bat or prey
#                       size = size, # size by number of connections
#                       shape = type)) +  # shape by cluster
#   geom_node_text(aes(label = name), # label by name
#                  repel = T,
#                  size = labelsize) +
#   scale_shape_manual(values = c("Bat" = batshp,
#                                 "Diet" = dietshp)) +
#   scale_color_manual(values = c("Bat" = batcol,
#                                 "Diet" = dietcol)) +
#   scale_size_continuous(range = node_sizerange,
#                         guide = "none") + # Adjust node size scale 
#   ggtitle(title) +
#   theme_void()


# Dropped
## CS Fig 5 - figure dropped ----
# Proportion of representation of bat taxa in datasets (by family)
# 
# ## - data input
# 
# cs_fig5_masterlist_dt <- read_csv(file.choose())
# # use "cs_fig5_sp_master_list_18032025.csv"
# # this will be used to derive the number of species in each family present in India
# 
# ## IBP data
# 
# ibp_raw <- read_csv(file.choose()) # use file name "IBP-withmedia-raw-24012025.csv" here
# 
# ibp_dt <- ibp_raw %>% 
#   select(catalogNumber,
#          locationLat,
#          locationLon,
#          rank,
#          scientificName,
#          order:genus) %>% 
#   mutate(database = "ibp")
# 
# ## iNat data
# 
# inat_raw <- read_csv(file.choose()) # use file name "Inat-raw-24012025.csv" here
# 
# # processing - combining the two datasets into one larger set 
# 
# inat_dt <- inat_raw %>% 
#   select(id,
#          quality_grade,
#          latitude,
#          longitude,
#          scientific_name,
#          taxon_id) %>% 
#   mutate(database = "inat")
# 
# csci_occ_dt <- bind_rows(ibp_dt,inat_dt)
# 
# csci_occ_dt <- csci_occ_dt %>%
#   mutate(id = if_else(database == "ibp", catalogNumber, id)) %>%
#   mutate(latitude = if_else(database == "ibp", locationLat, latitude)) %>%
#   mutate(longitude = if_else(database == "ibp", locationLon, longitude)) %>%
#   mutate(scientific_name = if_else(database == "ibp", scientificName, scientific_name)) %>%
#   select(database,
#          id,
#          latitude,
#          longitude,
#          scientific_name,
#          rank,
#          taxon_id)
# 
# write_csv(csci_occ_dt,"csci_occ_dt_27012025.csv")
# 
# ## combined datasheet input
# 
# cs_fig5_datasets_dt <- read_csv(file.choose())
# # use "csci_occ_dt_27012025.csv" 
# 
# ## Explainer

# # steps below - 
# # filter out unique taxon names
# # fetch family names then to get a list of species names with families recorded in the two datasets
# # total up numbers by family names to get the proportions
# # Match these numbers to the number of species in each family reported as in the master list for the country 
# 
# cs_fig5_sp_family <-  cs_fig5_datasets_dt %>% 
#   select(scientific_name) %>% 
#   group_by(scientific_name) %>% 
#   mutate(obs_count = n()) %>% 
#   unique()
#   
# # using package taxise to fetch family names from scientific names
# 
# cs_fig5_sp_family$family <- tax_name(sci = cs_fig5_sp_family$scientific_name,
#                                  get = "family",
#                                  db = "itis")
