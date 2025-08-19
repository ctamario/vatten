

#setwd("//storage-ume.slu.se/home$/caio0001/My Documents/git_new/vatten")

library(pacman)

p_load(dplyr, sf, leaflet, ggplot2, ggpubr, stringr)


# Load background river data

#rivers_without_HARO <- read_sf("C:/Users/caio0001/Documents/test/vd_l_2016_3_RivEX.shp")
rivers_without_HARO <- read_sf("//storage-ume.slu.se/home$/caio0001/My Documents/test/vd_l_2016_3_RivEX.shp")

HARO <- read_sf("C:/data/rawdata/VM_Avrinningsomraden/SW_HARO_2016_1.shp")

#rivers_with_HARO <- st_join(rivers_without_HARO, HARO, largest = T)

#write_sf(obj=rivers_with_HARO, dsn="C:/Users/caio0001/Documents/test/vd_l_2016_3_RivEX_with_HARO.shp")

#rivers_with_HARO <- read_sf("C:/Users/caio0001/Documents/test/vd_l_2016_3_RivEX_with_HARO.shp")
rivers_with_HARO <- read_sf("//storage-ume.slu.se/home$/caio0001/My Documents/test/vd_l_2016_3_RivEX_with_HARO.shp")

rivers_with_HARO$HARO_factor <- as.factor(rivers_with_HARO$HARO)

rivers <- rivers_with_HARO

rivers$shape_length_full <- st_length(rivers)


# Load barrier data

merged_barriers_including_removals <- read.csv(file = "C:/data/merged_barriers20241215_20250123.csv", sep = ";", dec = ",", fileEncoding = "Latin1")

table(merged_barriers_including_removals$fishwaytype_merged_CT, useNA="always")

# Remove all the removed barriers from the dataset.
merged_barriers <- merged_barriers_including_removals %>% dplyr::filter(is.na(fishwaytype_merged_CT) | fishwaytype_merged_CT != "Utrivning")

table(merged_barriers$fishwaytype_merged_CT, useNA = "always")
table(merged_barriers$fishway_overall_CT, useNA = "always")
table(merged_barriers$type_CT, useNA = "always")

### Remove these dams manually ... lol 

merged_barriers <- merged_barriers %>% dplyr::filter(overallID_Calle != 29027)


#one_seg <- "62989281532963"
#one_seg <- "72434501791827"
one_seg <- "64807900173708" # Main inlet of Skagerack

# Function to find upstream segments directly connected to a given segment.
up_seg <- function(in_df, seg){
  # Find all segments whose `RSTID_NED` (downstream segment) matches `seg`.
  up_seg <- in_df$RSTID[which(in_df$RSTID_NED == seg)]
  # Return the upstream segments.
  return(up_seg) 
}

# Test `up_seg` on the river network for `one_seg`.
up_seg(rivers, one_seg)

# Extract the specific segment `one_seg` as a spatial feature.
one_segment_sf <- rivers[which(rivers$RSTID == one_seg),]

# Extract all segments directly upstream of `one_seg`.
up_segment_sf <- rivers[which(rivers$RSTID %in% up_seg(rivers, one_seg)),]

# Recursive function to find all segments upstream of a given segment.
up_seg_all2 <- function(in_df, seg){
  next_up <- up_seg(in_df, seg) # Get direct upstream segments.
  list_up <- next_up # Initialize the list of all upstream segments.
  
  # Continue adding upstream segments until no new ones are found.
  while(length(next_up) > 0) {
    new_up <- up_seg(in_df, next_up[1]) # Get upstream of the first in `next_up`.
    new_up <- new_up[!new_up %in% list_up]  # Exclude already found segments.
    list_up <- c(list_up, new_up) # Add new segments to the list.
    next_up <- c(next_up, new_up) # Add new segments to the queue.
    next_up <- next_up[-1] # Remove processed segment from the queue.
  }
  return(list_up) # Return all upstream segments.
}

#up_all_segment_sf <- rivers[which(rivers$RSTID %in% up_seg_all2(rivers, one_seg)),]


# leaflet() %>% #addProviderTiles("Esri.WorldImagery") %>%
#   addPolylines(data=one_segment_sf %>% st_transform(crs = 4326), popup=~RW_PopNamn) %>%
#   addPolylines(data=up_all_segment_sf %>% st_transform(crs = 4326), color="red", popup=~RW_PopNamn)

# function that takes into account dams. This is probably the 
# one that needs modification to take into account 
# where on a polyline the dam is. 

up_seg_all3 <- function(in_df, seg, in_barriers){
  next_up <- up_seg(in_df, seg)
  list_up <- next_up
  while(length(next_up) > 0) {
    new_up <- up_seg(in_df, next_up[1])
    new_up <- new_up[!new_up %in% list_up]  # Remove segments already in list_up
    list_up <- c(list_up, new_up)
    new_up <- new_up[!new_up %in% in_barriers$RSTID]
    
    next_up <- c(next_up, new_up)
    next_up <- next_up[-1]
  }
  return(c(seg, list_up))
}

find_blocking_set_of_dams <- function(in_barriers, in_segments){
  in_barriers[which(in_barriers$RSTID %in% in_segments$RSTID),]
  
}

find_first_blocking_dams <- function(in_blocking_barriers){
  b <- in_blocking_barriers
  remove_these_barriers <- b %>% arrange(!desc(Per_Along)) %>% group_by(RSTID) %>% slice_tail(n = 1)
  return(remove_these_barriers)
}


# This one calculates how much of the 
how_much_length <- function(in_segment_list, in_blocking_barriers){
  
  riv_df <- rivers[which(rivers$RSTID %in% in_segment_list),]
  barr_df <- in_blocking_barriers
  
  riv_df$RSTID <- as.character(riv_df$RSTID)
  barr_df$RSTID <- as.character(barr_df$RSTID)
  
  riv_df$shape_length <- st_length(riv_df)
  
  riv_df <- as.data.frame(riv_df)
  barr_df <- as.data.frame(barr_df)
  
  riv_df$prop_length_counts <- 1
  barr_df$prop_length_counts <- 1 - (barr_df$Per_Along/100)
  
  result <- riv_df %>%
    left_join(barr_df %>% dplyr::select(RSTID, prop_length_counts), by = "RSTID", suffix = c("", ".key")) %>%
    mutate(prop_length_counts = if_else(!is.na(prop_length_counts.key), prop_length_counts.key, prop_length_counts)) %>%
    dplyr::select(-ends_with(".key"))
  
  return(result)
  
}


######## THREE DIFFERENT SEARCHES vvvvvvv
######## THREE DIFFERENT SEARCHES vvvvvvv
######## THREE DIFFERENT SEARCHES vvvvvvv


####
#### 1.1 Run the search for FIRST BARRIER regardless of type
####

# Run the search! 
up_fragment_segment_sf <- rivers[which(rivers$RSTID %in% up_seg_all3(rivers, one_seg, merged_barriers)),]

# Find the dams in the furthest segments.
blocking_set_of_dams <- find_blocking_set_of_dams(merged_barriers, up_fragment_segment_sf)

# Note that there can be more than one dam per furthest segment, 
# so the following function extracts only the most downstream dam
first_blocking_dams <- find_first_blocking_dams(blocking_set_of_dams)

# The following function extracts the location along the river
# segment for each barrier
test123 <- how_much_length(up_fragment_segment_sf$RSTID, first_blocking_dams)
nrow(test123)
nrow(up_fragment_segment_sf)
up_fragment_segment_sf <- up_fragment_segment_sf %>% left_join(test123 %>% dplyr::select(RSTID, shape_length, prop_length_counts), by = c("RSTID"))



###
### 1.2 Run the search for FIRST BARRIER but removing the culverts and road passages!
###

### Removing culverts and road passages (assuming that they are passable)
merged_barriers_without_culverts_etc <- merged_barriers %>%
  dplyr::filter(type_CT != "road passage") %>%
  dplyr::filter(type_CT != "culvert")


# Run the search! 
up_fragment_segment_sf_wo_culverts <- rivers[which(rivers$RSTID %in% up_seg_all3(rivers, one_seg, merged_barriers_without_culverts_etc)),]

# Find the dams in the furthest segments.
blocking_set_of_dams_without_culverts <- find_blocking_set_of_dams(merged_barriers_without_culverts_etc, first_blocking_dams_without_culverts)

# Note that there can be more than one dam per furthest segment, 
# so the following function extracts only the most downstream dam
first_blocking_dams_without_culverts <- find_first_blocking_dams(blocking_set_of_dams_without_culverts)

# The following function extracts the location along the river
# segment for each barrier
test123 <- how_much_length(up_fragment_segment_sf_wo_culverts$RSTID, first_blocking_dams_without_culverts)
nrow(test123)
nrow(up_fragment_segment_sf_wo_culverts)
up_fragment_segment_sf_wo_culverts <- up_fragment_segment_sf_wo_culverts %>% left_join(test123 %>% dplyr::select(RSTID, shape_length, prop_length_counts), by = c("RSTID"))


###
### 1.3 Run the search while accounting for fishways!
###


# Removing dams with fishways, assuming that they are passable
merged_barriers_accounting_for_fishways <- merged_barriers_without_culverts_etc %>% filter(fishway_overall_CT == 0)


# Run the search! 
up_fragment_segment_sf_fishways <- rivers[which(rivers$RSTID %in% up_seg_all3(rivers, one_seg, merged_barriers_accounting_for_fishways)),]

# Find the dams in the furthest segments.
blocking_set_of_dams_fishways <- find_blocking_set_of_dams(merged_barriers_accounting_for_fishways, up_fragment_segment_sf_fishways)


# Note that there can be more than one dam per furthest segment, 
# so the following function extracts only the most downstream dam
first_blocking_dams_fishways <- find_first_blocking_dams(blocking_set_of_dams_fishways)

# The following function extracts the location along the river
# segment for each barrier
test123_fishways <- how_much_length(up_fragment_segment_sf_fishways$RSTID, first_blocking_dams_fishways)
nrow(test123_fishways)
nrow(up_fragment_segment_sf_fishways)
up_fragment_segment_sf_fishways <- up_fragment_segment_sf_fishways %>% left_join(test123_fishways %>% dplyr::select(RSTID, shape_length, prop_length_counts), by = c("RSTID"))


######## THREE DIFFERENT SEARCHES ^^^^^^^^
######## THREE DIFFERENT SEARCHES ^^^^^^^^
######## THREE DIFFERENT SEARCHES ^^^^^^^^

###### 2 Filtering the river segment datasets! vvvvv
###### 2 Filtering the river segment datasets! vvvvv
###### 2 Filtering the river segment datasets! vvvvv

###
### 2.0 All rivers 
###

rivers_all <-  rivers %>% filter(LINJEKOD != 26)
rivers_all$SE <- 0
rivers_all$SE[str_detect(rivers_all$DISTRICT, "SE")] <- 1
rivers_all$SE[str_detect(rivers_all$DISTRICT, "SE5101")] <- 0

# there are two river polylines with wrong LINJEKOD which are not removed in the filtering above.
rivers_all <- rivers_all[which(rivers_all$RSTID != 7247692812341),]
rivers_all <- rivers_all[which(rivers_all$RSTID != 73089740878985),]
rivers_all_filtered <- rivers_all %>% filter(SE == 1)

ggplot(data=rivers_all_filtered) + geom_sf()



###
### 2.1 all barriers 
###

rivers_acc21 <- up_fragment_segment_sf %>% filter(LINJEKOD != 26)
rivers_acc21$SE <- 0
rivers_acc21$SE[str_detect(rivers_acc21$DISTRICT, "SE")] <- 1
rivers_acc21$SE[str_detect(rivers_acc21$DISTRICT, "SE5101")] <- 0

# there are two river polylines with wrong LINJEKOD which are not removed in the filtering above.
rivers_acc21 <- rivers_acc21[which(rivers_acc21$RSTID != 7247692812341),]
rivers_acc21 <- rivers_acc21[which(rivers_acc21$RSTID != 73089740878985),]
rivers_acc21_filtered <- rivers_acc21 %>% filter(SE == 1)

ggplot() + 
  geom_sf(data = rivers_all_filtered %>% filter(!RSTID %in% rivers_acc21_filtered$RSTID), color="#c2e0f8") +
  geom_sf(data = rivers_acc21_filtered, color = "steelblue1")+
  theme_classic()

ggsave("21_how_far_into_rivers_white.png", bg = "transparent", dpi = 500, width = 4, height = 7)


###
### 2.2 barriers without culverts and road passages
###

rivers_acc22 <- up_fragment_segment_sf_wo_culverts %>% filter(LINJEKOD != 26)
rivers_acc22$SE <- 0
rivers_acc22$SE[str_detect(rivers_acc22$DISTRICT, "SE")] <- 1
rivers_acc22$SE[str_detect(rivers_acc22$DISTRICT, "SE5101")] <- 0

# there are two river polylines with wrong LINJEKOD which are not removed in the filtering above.
rivers_acc22 <- rivers_acc22[which(rivers_acc22$RSTID != 7247692812341),]
rivers_acc22 <- rivers_acc22[which(rivers_acc22$RSTID != 73089740878985),]
rivers_acc22_filtered <- rivers_acc22 %>% filter(SE == 1)

ggplot() + 
  geom_sf(data=rivers_all_filtered %>% filter(!RSTID %in% rivers_acc1_filtered$RSTID), color="steelblue4") +
  geom_sf(data = rivers_acc22_filtered, color = "steelblue1")+
  theme_black_test2()

ggsave("22_how_far_into_rivers.png", bg = "transparent", dpi = 700, width = 5, height = 6)


###
### 2.3 barriers accounting for fishways
###


rivers_acc23 <- up_fragment_segment_sf_fishways %>% filter(LINJEKOD != 26)
rivers_acc23$SE <- 0
rivers_acc23$SE[str_detect(rivers_acc23$DISTRICT, "SE")] <- 1
rivers_acc23$SE[str_detect(rivers_acc23$DISTRICT, "SE5101")] <- 0

# there are two river polylines with wrong LINJEKOD which are not removed in the filtering above.
rivers_acc23 <- rivers_acc23[which(rivers_acc23$RSTID != 7247692812341),]
rivers_acc23 <- rivers_acc23[which(rivers_acc23$RSTID != 73089740878985),]
rivers_acc23_filtered <- rivers_acc23 %>% filter(SE == 1)

ggsave("22_how_far_into_rivers.png", bg = "transparent", dpi = 700, width = 5, height = 6)

ggplot()+
  geom_sf(data=rivers_all_filtered %>% filter(!RSTID %in% c(rivers_acc21_filtered$RSTID, rivers_acc22_filtered$RSTID, rivers_acc23_filtered$RSTID)), color="#c2e0f8") +
  geom_sf(data=rivers_acc23_filtered, color = "orange")+
  geom_sf(data=rivers_acc22_filtered, color = "steelblue1")+
  geom_sf(data=rivers_acc21_filtered, color = "steelblue1")+
  #theme_black_test2()
  theme_classic()

ggsave("22_23_how_far_into_rivers_white.png", bg = "transparent", dpi = 500, width = 4, height = 7)


###### 2 Filtering the river segment datasets! ^^^^^
###### 2 Filtering the river segment datasets! ^^^^^
###### 2 Filtering the river segment datasets! ^^^^^

###### 3 Creating the summary datasets vvvvv
###### 3 Creating the summary datasets vvvvv
###### 3 Creating the summary datasets vvvvv

HARO_not_eligible <- c("114000", "114115", "115000", "115116", "116000", "116117")

acc_summary <- rivers_all_filtered %>% as.data.frame() %>% group_by(HARO_factor, AREAL) %>% summarize(sum_total_length = sum(shape_length_full))
acc_summary <- acc_summary %>% mutate(HARO = as.integer(as.character(HARO_factor)))
acc_summary <- acc_summary %>% filter(!HARO_factor %in% HARO_not_eligible)

acc_summary <- acc_summary %>% left_join(rivers_acc21_filtered %>% as.data.frame() %>% group_by(HARO_factor) %>% summarize(sum_accessible_length21 = sum(shape_length * prop_length_counts, na.rm=T)), by = c("HARO_factor"))
acc_summary <- acc_summary %>% mutate(accessible_prop21 = sum_accessible_length21 / sum_total_length)

acc_summary <- acc_summary %>% left_join(rivers_acc22_filtered %>% as.data.frame() %>% group_by(HARO_factor) %>% summarize(sum_accessible_length22 = sum(shape_length * prop_length_counts, na.rm=T)), by = c("HARO_factor"))
acc_summary <- acc_summary %>% mutate(accessible_prop22 = sum_accessible_length22 / sum_total_length)

acc_summary <- acc_summary %>% left_join(rivers_acc23_filtered %>% as.data.frame() %>% group_by(HARO_factor) %>% summarize(sum_accessible_length23 = sum(shape_length * prop_length_counts, na.rm=T)), by = c("HARO_factor"))
acc_summary <- acc_summary %>% mutate(accessible_prop23 = sum_accessible_length23 / sum_total_length)


###### 3 Creating the summary datasets ^^^^^^
###### 3 Creating the summary datasets ^^^^^^
###### 3 Creating the summary datasets ^^^^^^

###### 4 Making the HARO plots vvvvvv
###### 4 Making the HARO plots vvvvvv
###### 4 Making the HARO plots vvvvvv

HARO_with_extra <- HARO %>% 
  #filter(!HARO %in% HARO_not_eligible) %>%
  left_join(acc_summary, by = c("HARO"))

##

plot_catchments1 <- ggplot(data = HARO_with_extra) + 
  geom_sf(color = alpha("black",0.2), aes(fill=as.numeric(accessible_prop21))) +
  labs(fill = "Proportion of \n catchment accessible") + 
  scale_fill_gradient(low = "#c2e0f8", high = "steelblue1")+
  #theme_black_test2()
  theme_classic()

plot_catchments1 

ggsave("21_accessible_proportion_catchment_white.png", bg = "transparent", dpi = 300, width = 6, height = 9)



ggplot(data = HARO_with_extra) + 
  geom_sf(color = alpha("white",0.2), aes(fill=as.numeric(accessible_prop22))) +
  labs(fill = "Proportion accessible") + 
  theme_black_test2()

plot_catchments2 <- ggplot(data = HARO_with_extra) + 
  geom_sf(color = alpha("black",0.2), aes(fill=as.numeric(accessible_prop22))) +
  labs(fill = "Proportion of \n catchment accessible") + 
  scale_fill_gradient(low = "#c2e0f8", high = "steelblue1")+
  #theme_black_test2()
  theme_classic()

ggsave("22_accessible_proportion_catchment.png", bg = "transparent", dpi = 700, width = 5, height = 6)

ggplot(data = HARO_with_extra) + 
  geom_sf(color = alpha("white",0.2), aes(fill=as.numeric(accessible_prop23))) +
  labs(fill = "Proportion accessible") + 
  theme_black_test2()

plot_catchments3 <- ggplot(data = HARO_with_extra) + 
  geom_sf(color = alpha("black",0.2), aes(fill=as.numeric(accessible_prop23))) +
  labs(fill = "Proportion of \n catchment accessible") + 
  scale_fill_gradient(low = "#c2e0f8", high = "steelblue1")+
  #theme_black_test2()
  theme_classic()

plot_catchments3

ggsave("23_accessible_proportion_catchment_white.png", bg = "transparent", dpi = 300, width = 6, height = 9)

cbind(HARO_with_extra, st_area(HARO_with_extra))

large_accessible_rivers21 <- HARO_with_extra %>% 
  filter(as.numeric(sum_total_length) > 2000000 & 
           as.numeric(accessible_prop21) > 0.1)

plot_catchments_scatter1 <- HARO_with_extra %>% 
  ggplot(aes(#x=as.numeric(sum_total_length), 
    x = AREAL.x,
    y=as.numeric(accessible_prop21))) + 
  geom_point(color = "black") +
  #geom_point(color = "white") +
  geom_text(data = large_accessible_rivers21, 
            aes(#x=as.numeric(sum_total_length), 
              x = AREAL.x,
              y=as.numeric(accessible_prop21)+0.05,
              #label = NAME), color = "white")+
              label = NAME), color = "black")+
  labs(y = "Proportion accessible",
       x = expression("Catchment size (m"^2*")"))+
  #theme_black_test2()
  theme_classic()

plot_catchments_scatter1

dev.copy2pdf(file="test_out_first.pdf", height=7, width=12)

ggsave("21_accessible_proportion_vs_catchment_size_white.png", bg = "transparent", dpi = 500, width = 5, height = 4)

large_accessible_rivers22 <- HARO_with_extra %>% 
  filter(as.numeric(sum_total_length) > 2000000 & 
           as.numeric(accessible_prop22) > 0.1)

HARO_with_extra %>% 
  ggplot(aes(x=as.numeric(sum_total_length), y=as.numeric(accessible_prop22))) + 
  geom_point(color = "white") +
  geom_text(data = large_accessible_rivers22, 
            aes(x=as.numeric(sum_total_length), 
                y=as.numeric(accessible_prop22)+0.05,
                label = NAME), color = "white")+
  labs(y = "Proportion accessible",
       x = "Catchment size (m)")+
  theme_black_test2()

ggsave("22_accessible_proportion_vs_catchment_size.png", bg = "transparent", dpi = 700, width = 5, height = 4)

large_accessible_rivers23 <- HARO_with_extra %>% 
  filter(as.numeric(sum_total_length) > 2000000 & 
           as.numeric(accessible_prop23) > 0.1)

HARO_with_extra %>% 
  ggplot(aes(#x=as.numeric(sum_total_length),
    x=AREAL.x,
    y=as.numeric(accessible_prop23))) + 
  #geom_point(color = "white") +
  geom_point()+
  geom_text(data = large_accessible_rivers23, 
            aes(#x=as.numeric(sum_total_length), 
              x=AREAL.x,
              y=as.numeric(accessible_prop23)+0.05,
              label = NAME), color = "black")+
  labs(y = "Proportion accessible",
       x = expression("Catchment size (m"^2*")"))+
  #theme_black_test2()
  theme_classic()

ggsave("23_accessible_proportion_vs_catchment_size_white.png", bg = "transparent", dpi = 500, width = 5, height = 4)


###### 4 Making the HARO plots ^^^^^^
###### 4 Making the HARO plots ^^^^^^
###### 4 Making the HARO plots ^^^^^^

###### 5 Making overall statistics vvvvvv
###### 5 Making overall statistics vvvvvv
###### 5 Making overall statistics vvvvvv

### How much is accessible until hitting the first artificial structure?
sum(acc_summary$sum_accessible_length21) / sum(acc_summary$sum_total_length)

### How much is accessible when having removed culverts and road passages?
sum(acc_summary$sum_accessible_length22) / sum(acc_summary$sum_total_length)

### How much is accessible when taking into account fishways?
sum(acc_summary$sum_accessible_length23) / sum(acc_summary$sum_total_length)

sum(acc_summary_1st$sum_accessible_length_1st_sim) / sum(acc_summary_1st$sum_total_length)

(sum(acc_summary_1st$sum_accessible_length_1st_sim) / sum(acc_summary_1st$sum_total_length)) / (sum(acc_summary$sum_accessible_length23) / sum(acc_summary$sum_total_length))



### How much is habitat increased by fishways?

(sum(acc_summary$sum_accessible_length23) / sum(acc_summary$sum_total_length)) / (sum(acc_summary$sum_accessible_length22) / sum(acc_summary$sum_total_length))

###### 5 Making overall statistics ^^^^^^
###### 5 Making overall statistics ^^^^^^
###### 5 Making overall statistics ^^^^^^

###### 6 Making river plots vvvvvv
###### 6 Making river plots vvvvvv
###### 6 Making river plots vvvvvv

ggplot()+
  geom_sf(data=rivers_all_filtered %>% filter(!RSTID %in% c(rivers_acc21_filtered$RSTID)), color="#2a343c") +
  geom_sf(data=rivers_acc21_filtered, color = "steelblue1")+
  theme_black_test2()
ggsave("21_accessible_rivers.png", bg = "transparent", dpi = 700, width = 5, height = 6)


ggplot()+
  geom_sf(data=rivers_all_filtered %>% filter(!RSTID %in% c(rivers_acc22_filtered$RSTID)), color="#2a343c") +
  geom_sf(data=rivers_acc22_filtered, color = "steelblue1")+
  theme_black_test2()
ggsave("22_accessible_rivers.png", bg = "transparent", dpi = 700, width = 5, height = 6)


ggplot()+
  geom_sf(data=rivers_all_filtered %>% filter(!RSTID %in% c(rivers_acc23_filtered$RSTID)), color="#2a343c") +
  geom_sf(data=rivers_acc23_filtered, color = "steelblue1")+
  theme_black_test2()
ggsave("23_accessible_rivers.png", bg = "transparent", dpi = 700, width = 5, height = 6)


ggplot()+
  geom_sf(data=rivers_all_filtered %>% filter(!RSTID %in% c(rivers_acc23_filtered$RSTID, rivers_acc22_filtered$RSTID)), color="#2a343c") +
  geom_sf(data=rivers_acc23_filtered %>% filter(!RSTID %in% c(rivers_acc21_filtered$RSTID)), color = "steelblue1")+
  geom_sf(data=rivers_acc21_filtered, color = "steelblue4")+
  theme_black_test2()
ggsave("23_accessible_rivers_show_fishways.png", bg = "transparent", dpi = 700, width = 5, height = 6)

###### 6 Making river plots ^^^^^^
###### 6 Making river plots ^^^^^^
###### 6 Making river plots ^^^^^^


###### 7 Habitat! vvvvvv
###### 7 Habitat! vvvvvv
###### 7 Habitat! vvvvvv

with(rivers_acc21_filtered, table(Strahler))
with(rivers_acc23_filtered, table(Strahler))

river_type_all <- rivers_all_filtered %>% as.data.frame() %>% group_by(Strahler) %>% summarise(sum_shape_length_all = sum(as.numeric(shape_length_full, na.rm=T)))
river_type21 <- rivers_acc21_filtered %>% as.data.frame() %>% group_by(Strahler) %>% summarise(sum_shape_length21 = sum(as.numeric(shape_length, na.rm=T)))
river_type23 <- rivers_acc23_filtered %>% as.data.frame() %>% group_by(Strahler) %>% summarise(sum_shape_length23 = sum(as.numeric(shape_length, na.rm=T)))

big_HAROs <- HARO %>% filter(AREAL > 1000)
big_HAROs$HARO_factor <- factor(big_HAROs$HARO)

national_rivers <- c("1000", "4000", "13000", "28000")

river_type_all <- rivers_all_filtered %>% as.data.frame() %>% filter(HARO_factor %in% big_HAROs$HARO_factor) %>% group_by(Strahler) %>% summarise(sum_shape_length_all = sum(as.numeric(shape_length_full, na.rm=T)))
river_type21 <- rivers_acc21_filtered %>% as.data.frame() %>% filter(HARO_factor %in% big_HAROs$HARO_factor) %>% group_by(Strahler) %>% summarise(sum_shape_length21 = sum(as.numeric(shape_length, na.rm=T)))
river_type23 <- rivers_acc23_filtered %>% as.data.frame() %>% filter(HARO_factor %in% big_HAROs$HARO_factor) %>% group_by(Strahler) %>% summarise(sum_shape_length23 = sum(as.numeric(shape_length, na.rm=T)))


river_type_all <- rivers_all_filtered %>% as.data.frame() %>% filter(!HARO_factor %in% national_rivers) %>% group_by(Strahler) %>% summarise(sum_shape_length_all = sum(as.numeric(shape_length_full, na.rm=T)))
river_type21 <- rivers_acc21_filtered %>% as.data.frame() %>% filter(!HARO_factor %in% national_rivers) %>% group_by(Strahler) %>% summarise(sum_shape_length21 = sum(as.numeric(shape_length, na.rm=T)))
river_type23 <- rivers_acc23_filtered %>% as.data.frame() %>% filter(!HARO_factor %in% national_rivers) %>% group_by(Strahler) %>% summarise(sum_shape_length23 = sum(as.numeric(shape_length, na.rm=T)))


river_types <- cbind(river_type_all, river_type21, river_type23[,2])

river_types$increase_in_habitat <- river_types$sum_shape_length23-river_types$sum_shape_length21

hm <- data.frame(Strahler = rep(1:7,3), 
                 group= c(rep("The rest",7),rep("Accessible",7),rep("Accessible through fishways",7)), 
                 river_length = c(river_types$sum_shape_length_all-river_types$sum_shape_length21,
                                  river_types$sum_shape_length21,
                                  river_types$sum_shape_length23-river_types$sum_shape_length21))

plot_habitat <- hm %>% ggplot(aes(x=factor(Strahler), y=river_length, fill=group))+
  geom_bar(position = "fill",stat = "identity") +
  labs(fill = "",
       y = "Proportion of river length",
       x = "Strahler")+
  #theme_black_test2()
  theme_classic()

ggarrange(plot_habitat, common.legend = T)

ggsave("21_22_23_accessible_habitats_white.png", bg = "transparent", dpi = 700, width = 5, height = 4)


###### 7 Habitat! ^^^^^^
###### 7 Habitat! ^^^^^^
###### 7 Habitat! ^^^^^^


###### 8 Simulation of removing first barrier vvvvvv
###### 8 Simulation of removing first barrier vvvvvv
###### 8 Simulation of removing first barrier vvvvvv

# Remove the first blocking barriers from the barrier dataset that
# already accounts for fishways. That way, we can simulate how that
# would change from the "current" situtation.

nrow(merged_barriers_accounting_for_fishways)
nrow(first_blocking_dams_fishways)

first_blocking_dams_fishways %>% st_as_sf(coords=c("SWEREF99tm_e", "SWEREF99tm_n"), crs = 3006)

ggplot() + 
  geom_sf(data=first_blocking_dams_fishways %>% 
            st_as_sf(coords=c("SWEREF99tm_e", "SWEREF99tm_n"), 
                     crs = 3006))

merged_barriers_remove_1st <- merged_barriers_accounting_for_fishways %>%
  dplyr::filter(!RSTID %in% first_blocking_dams_fishways$RSTID)

### Run simulation functions:


# Run the search! 
up_fragment_segment_sf_1st_sim <- rivers[which(rivers$RSTID %in% up_seg_all3(rivers, one_seg, merged_barriers_remove_1st)),]

# Find the dams in the furthest segments.
blocking_set_of_dams_1st_sim <- find_blocking_set_of_dams(merged_barriers_remove_1st, up_fragment_segment_sf_1st_sim)


# Note that there can be more than one dam per furthest segment, 
# so the following function extracts only the most downstream dam
first_blocking_dams_1st_sim <- find_first_blocking_dams(blocking_set_of_dams_1st_sim)

# The following function extracts the location along the river
# segment for each barrier
test123_1st_sim <- how_much_length(up_fragment_segment_sf_1st_sim$RSTID, first_blocking_dams_1st_sim)
nrow(test123_1st_sim)
nrow(up_fragment_segment_sf_1st_sim)
up_fragment_segment_sf_1st_sim <- up_fragment_segment_sf_1st_sim %>% left_join(test123_1st_sim %>% dplyr::select(RSTID, shape_length, prop_length_counts), by = c("RSTID"))



rivers_acc_1st_sim <- up_fragment_segment_sf_1st_sim %>% filter(LINJEKOD != 26)
rivers_acc_1st_sim$SE <- 0
rivers_acc_1st_sim$SE[str_detect(rivers_acc_1st_sim$DISTRICT, "SE")] <- 1
rivers_acc_1st_sim$SE[str_detect(rivers_acc_1st_sim$DISTRICT, "SE5101")] <- 0

# there are two river polylines with wrong LINJEKOD which are not removed in the filtering above.
rivers_acc_1st_sim <- rivers_acc_1st_sim[which(rivers_acc_1st_sim$RSTID != 7247692812341),]
rivers_acc_1st_sim <- rivers_acc_1st_sim[which(rivers_acc_1st_sim$RSTID != 73089740878985),]
rivers_acc_1st_sim_filtered <- rivers_acc_1st_sim %>% filter(SE == 1)

# Plot to see how much rivers are gained from removing first barrier
ggplot(data=rivers_acc_1st_sim_filtered) + geom_sf()
ggplot()+
  geom_sf(data=rivers_all_filtered %>% filter(!RSTID %in% c(rivers_acc23_filtered$RSTID, rivers_acc_1st_sim_filtered$RSTID)), color="#2a343c") +
  geom_sf(data=rivers_acc_1st_sim_filtered %>% filter(!RSTID %in% c(rivers_acc21_filtered$RSTID)), color = "steelblue1")+
  geom_sf(data=rivers_acc23_filtered, color = "steelblue4")+
  theme_black_test2()

ggsave("1st_sim_accessible_rivers.png", bg = "transparent", dpi = 700, width = 5, height = 6)


# Copy the acc_summary from above:
acc_summary_1st <- acc_summary

acc_summary_1st <- acc_summary_1st %>% left_join(rivers_acc_1st_sim_filtered %>% as.data.frame() %>% group_by(HARO_factor) %>% summarize(sum_accessible_length_1st_sim = sum(shape_length * prop_length_counts, na.rm=T)), by = c("HARO_factor"))
acc_summary_1st <- acc_summary_1st %>% mutate(accessible_prop_1st_sim = sum_accessible_length_1st_sim / sum_total_length)


# Calculating the number of barriers per HARO:
# First need to make a function to add HARO to barrier datasets
add_HARO <- function(in_barriers){
  
  in_barriers$RSTID <- as.character(in_barriers$RSTID)
  
  out_barriers <- in_barriers %>% left_join(rivers_all %>% as.data.frame() %>% dplyr::select(RSTID, HARO_factor),
                            by = c("RSTID"))
  
  return(out_barriers)
}

# Add HARO to barrier dataset
first_blocking_dams_1st_sim <- add_HARO(first_blocking_dams_1st_sim)

acc_summary_1st <- acc_summary_1st %>% left_join(first_blocking_dams_1st_sim %>% group_by(HARO_factor) %>% summarize(n_of_barriers = n()), by = c("HARO_factor"))

acc_summary_1st <- acc_summary_1st %>% mutate(gained_1st_simulation = sum_accessible_length_1st_sim - sum_accessible_length23)
acc_summary_1st <- acc_summary_1st %>% mutate(gained_per_dam_1st_simulation = gained_1st_simulation/n_of_barriers)


HARO_with_extra <- HARO %>% 
  #filter(!HARO %in% HARO_not_eligible) %>%
  left_join(acc_summary_1st, by = c("HARO"))

plot_1st_sim_proportion_catchments <- ggplot(data = HARO_with_extra) + 
  geom_sf(color = alpha("white",0.2), aes(fill=as.numeric(accessible_prop_1st_sim))) +
  labs(fill = "Proportion accessible") + 
  theme_black_test2()

ggarrange(plot_1st_sim_proportion_catchments, common.legend = T)

ggsave("1st_sim_accessible_proportion_catchment2.png", bg = "transparent", dpi = 700, width = 5, height = 6)


ggplot(data = HARO_with_extra) + 
  geom_sf(color = alpha("white",0.2), aes(fill=as.numeric(gained_1st_simulation))) +
  labs(fill = "Gained by removing first dam(s)") + 
  theme_black_test2()

ggplot(data = HARO_with_extra) + 
  geom_sf(color = alpha("white",0.2), aes(fill=as.numeric(gained_per_dam_1st_simulation/1000))) +
  labs(fill = "Distance (km) gained\nper removed dam ") + 
  scale_fill_gradient(trans = "log", breaks = c(2,10,50,220))+
  theme_black_test2()

ggsave("1st_sim_bang_for_buck.png", bg = "transparent", dpi = 700, width = 5, height = 6)


acc_summary_1st %>%
  ggplot(aes(x = as.numeric(gained_per_dam_1st_simulation))) + 
  geom_histogram() + 
  theme_black_test2()



###### 8 Simulation of removing first barrier ^^^^^^
###### 8 Simulation of removing first barrier ^^^^^^
###### 8 Simulation of removing first barrier ^^^^^^




with(first_blocking_dams_fishways, table(type_CT, fishway_types_CT))







######

######

######

######

######

######

######







ggplot() +
  geom_sf(data=HARO, alpha=0.2)+
  geom_sf(data=merged_barriers %>% st_as_sf(coords=c("SWEREF99tm_e", "SWEREF99tm_n"), crs = 3006), aes(color=type_CT)) +
  labs(color = "Barrier type")+
  scale_color_manual(values = unname(palette.colors(palette = "Okabe-Ito")[2:20]))+
  theme_black_test2()

palette.colors(palette = "Okabe-Ito")[2:20]

ggsave("barrier_types_on_map.png", bg = "transparent", dpi = 700, width = 7, height = 13)

ggplot(data=merged_barriers, aes(x=type_CT, fill = type_CT)) +
  geom_bar() +
  labs(x = "Barrier type")+
  scale_fill_manual(values = unname(palette.colors(palette = "Okabe-Ito")[2:20]))+
  theme_black_test2()+
  theme(legend.position = "none")

ggsave("barrier_types_in_dataset.png", bg = "transparent", dpi = 700, width = 5, height = 4)


#


















###
### First encountered barrier types by Strahler category
###

with(first_blocking_dams, table(type_CT, Strahler))
with(merged_barriers, table(type_CT, Strahler))

merged_barriers$type_CT <- factor(merged_barriers$type_CT, levels = c("dam", "culvert", "natural barrier", "road passage", "other"))
first_blocking_dams$type_CT <- factor(first_blocking_dams$type_CT, levels = c("dam", "culvert", "natural barrier", "road passage", "other"))

summary_data <- first_blocking_dams %>% 
  as.data.frame %>%
  count(Strahler, type_CT) %>%
  group_by(Strahler) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

factor(summary_data$type_CT)

total_sample_size <- summary_data %>%
  group_by(Strahler) %>%
  summarize(total_n = sum(n)) %>%
  ungroup()

p1 <- ggplot(summary_data, aes(x = factor(Strahler), y = proportion, fill = type_CT)) +
  geom_bar(stat = "identity") +
  geom_text(data = total_sample_size, 
            aes(x = factor(Strahler), y = 1.015, label = total_n), # Place labels above the bar (y = 1.05 for above the stack)
            inherit.aes = FALSE, 
            vjust = 0,
            color = "black") + # Adjust vertical alignment
  scale_y_continuous(
    breaks = c(0.25, 0.50, 0.75, 1.00),
    labels = c("25%", "50%", "75%", "100%"),
    limits = c(0, 1.02))+
  scale_fill_manual(values = unname(palette.colors(palette = "Okabe-Ito")[2:20]))+
  labs(
    x = "Strahler order",
    y = "Proportion",
    fill = "Barrier type:",
    title = "First encountered barrier types",
    tag = "A.)"
  ) +
  #theme_black()
  theme_classic()



###
### Barrier types by Strahler category across Sweden
###

summary_data <- merged_barriers %>% 
  as.data.frame %>%
  count(Strahler, type_CT) %>%
  group_by(Strahler) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

factor(summary_data$type_CT)

total_sample_size <- summary_data %>%
  group_by(Strahler) %>%
  summarize(total_n = sum(n)) %>%
  ungroup()

p2 <- ggplot(summary_data, aes(x = factor(Strahler), y = proportion, fill = type_CT)) +
  geom_bar(stat = "identity") +
  geom_text(data = total_sample_size, 
            aes(x = factor(Strahler), y = 1.015, label = total_n), # Place labels above the bar (y = 1.05 for above the stack)
            inherit.aes = FALSE, 
            vjust = 0,
            color = "black") + # Adjust vertical alignment
  scale_y_continuous(
    breaks = c(0.25, 0.50, 0.75, 1.00),
    labels = c("25%", "50%", "75%", "100%"),
    limits = c(0, 1.02))+
  scale_fill_manual(values = unname(palette.colors(palette = "Okabe-Ito")[2:20]))+
  labs(
    x = "Strahler order",
    y = "Proportion",
    fill = "Barrier type:",
    title = "Barrier types across Sweden",
    tag = "B.)"
  ) +
  #theme_black()
  theme_classic()

ggarrange(p1, p2, common.legend = T)


ggsave("barrier_types_png_white.png", bg = "transparent", dpi = 300, width = 10, height = 5)
dev.copy2pdf(file="barrier_types.pdf", width = 10, height = 5)





###
### Distribution of fishways across Strahler type for the first encounted barrier
###

#fishway_occurrences_among_first_barriers <- with(first_blocking_dams %>% filter(type_CT %in% c("dam")), table(fishway_Calle2, Strahler))
fishway_occurrences_among_first_barriers <- with(first_blocking_dams_without_culverts, table(fishway_overall_CT, Strahler))

#fishway_occurrences_among_first_barriers <- with(first_blocking_dams, table(fishwaytype_Calle3, type_Calle))

sum(fishway_occurrences_among_first_barriers)
percent_with_fishway <- data.frame(percent=(fishway_occurrences_among_first_barriers["1", ] / colSums(fishway_occurrences_among_first_barriers)) * 100, Strahler=1:7)


#with(merged_barriers, table(fishway_Calle2, fishwaytype_Calle3))

table(first_blocking_dams$Strahler, useNA = "always")

summary_data <- first_blocking_dams_without_culverts %>% 
  #filter(type_CT %in% c("dam")) %>%
  as.data.frame %>%
  count(Strahler, fishway_overall_CT) %>%
  group_by(Strahler) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

total_sample_size <- summary_data %>%
  group_by(Strahler) %>%
  summarize(total_n = sum(n)) %>%
  ungroup()

sum(total_sample_size$total_n)

p3 <- ggplot(summary_data, aes(x = factor(Strahler), y = proportion, fill = factor(fishway_overall_CT))) +
  geom_bar(stat = "identity") +
  geom_text(data = total_sample_size, 
            aes(x = factor(Strahler), y = 1.015, label = total_n), # Place labels above the bar (y = 1.05 for above the stack)
            inherit.aes = FALSE, 
            vjust = 0,
            color= "black") + # Adjust vertical alignment
  geom_text(data = percent_with_fishway, 
            aes(x = factor(Strahler), y = percent/100+0.025, label = paste(round(percent),"%")), # Place labels above the bar (y = 1.05 for above the stack)
            inherit.aes = FALSE, 
            vjust = 0,
            color= "black") + # Adjust vertical alignment
  scale_y_continuous(
    breaks = c(0.25, 0.50, 0.75, 1.00),
    labels = c("25%", "50%", "75%", "100%"),
    limits = c(0, 1.02))+
  #scale_fill_discrete(labels = c("No fishway", "With fishway"))+
  scale_fill_manual(labels = c("not equipped", "equipped"),
                    values = unname(palette.colors(palette = "Okabe-Ito")[2:20]))+
  labs(
    x = "Strahler order",
    y = "Proportion",
    fill = "Fishway:",
    title = "Distribution of fishways across\nStrahler order for first-blocking dams",
    tag = "A.)"
  ) +
  #theme_black()
  theme_classic()

ggarrange(p3, common.legend = T)


###
### Distribution of fishways across Strahler type for the first encounted barrier
###

merged_barriers$fishway_overall_CT[which(merged_barriers$fishway_overall_CT == 2)] <- 1

#test <- with(merged_barriers %>% filter(type_CT %in% c("dam")), table(fishway_Calle, Strahler))
test <- with(merged_barriers, table(fishway_overall_CT, Strahler))

percent_with_fishway <- data.frame(percent=(test["1", ] / colSums(test)) * 100, Strahler=1:7)

summary_data <- merged_barriers %>% 
  filter(type_CT %in% c("dam")) %>%
  as.data.frame %>%
  count(Strahler, fishway_overall_CT) %>%
  group_by(Strahler) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

total_sample_size <- summary_data %>%
  group_by(Strahler) %>%
  summarize(total_n = sum(n)) %>%
  ungroup()

sum(total_sample_size$total_n)

p4 <- ggplot(summary_data, aes(x = factor(Strahler), y = proportion, fill = factor(fishway_overall_CT))) +
  geom_bar(stat = "identity") +
  geom_text(data = total_sample_size, 
            aes(x = factor(Strahler), y = 1.015, label = total_n), # Place labels above the bar (y = 1.05 for above the stack)
            inherit.aes = FALSE, 
            vjust = 0, color = "black") + # Adjust vertical alignment
  geom_text(data = percent_with_fishway, 
            aes(x = factor(Strahler), y = percent/100+0.025, label = paste(round(percent),"%")), # Place labels above the bar (y = 1.05 for above the stack)
            inherit.aes = FALSE, 
            vjust = 0) + # Adjust vertical alignment
  scale_y_continuous(
    breaks = c(0.25, 0.50, 0.75, 1.00),
    labels = c("25%", "50%", "75%", "100%"),
    limits = c(0, 1.02))+
  #scale_fill_discrete(labels = c("No fishway", "With fishway"))+
  scale_fill_manual(labels = c("not equipped", "equipped"),
                    values = unname(palette.colors(palette = "Okabe-Ito")[2:20]))+
  labs(
    x = "Strahler order",
    y = "Proportion",
    fill = "Fishway:",
    title = "Distribution of fishways across\nStrahler order for all dams",
    tag = "B.)"
  ) +
  #theme_black()
  theme_classic()


ggarrange(p3, p4, common.legend = T)

dev.copy2pdf(file = "fishways_equipped_to_barriers.pdf", height = 5, width = 10)
ggsave("fishway_distribution_png_white.png", bg = "transparent", dpi = 300, width = 10, height = 5)



# leaflet() %>% 
#   addProviderTiles("Esri.WorldImagery") %>%
#   addPolylines(data = rivers_all_filtered %>% st_transform(crs = 4326), popup = ~paste(DISTRICT, RSTID))





# leaflet() %>% 
#   addProviderTiles("Esri.WorldImagery") %>%
#   addPolylines(data = rivers_acc1 %>% filter(SE == 1) %>% st_transform(crs = 4326), popup = ~paste(DISTRICT, RSTID)) %>%
#   addCircleMarkers(data = first_blocking_dams %>%
#                      st_as_sf(coords = c("SWEREF99tm_e", "SWEREF99tm_n"), crs = 3006) %>%
#                      st_transform(crs = 4326),
#                    radius = 2, stroke = F, color = "red", fillOpacity = 1, popup = ~type_Calle)
# 
# leaflet() %>% 
#   #addProviderTiles("Esri.WorldImagery") %>%
#   addTiles() %>%
#   addPolylines(data = rivers_acc1 %>% st_transform(crs = 4326), popup = ~paste(DISTRICT, RSTID)) %>%
#   addCircleMarkers(data = first_blocking_dams %>%
#                      st_as_sf(coords = c("SWEREF99tm_e", "SWEREF99tm_n"), crs = 3006) %>%
#                      st_transform(crs = 4326),
#                    radius = 2, stroke = F, color = "red", fillOpacity = 1, popup = ~type_Calle)


#######
####### So how much is available before the first *structure*?
#######
# 
# with(rivers %>% filter(LINJEKOD != 26), table(HARO, COUNTRY, useNA = "always"))
# 
# leaflet() %>% 
#   addTiles() %>%
#   addPolylines(data = rivers %>% filter(is.na(HARO)) %>% st_transform(crs = 4326)) %>%
#   addPolylines(data = rivers %>% filter(!is.na(HARO)) %>% st_transform(crs = 4326), color = "red")
# 
# leaflet() %>% 
#   addTiles() %>%
#   addPolylines(data = rivers_acc1 %>% filter(SE == 1) %>% st_transform(crs = 4326))
# 
# acc1_summary <- rivers_all_filtered %>% as.data.frame() %>% group_by(HARO_factor, AREAL) %>% summarize(sum_total_length = sum(shape_length_full))
# acc1_summary <- acc1_summary %>% left_join(rivers_acc1_filtered %>% as.data.frame() %>% group_by(HARO_factor) %>% summarize(sum_accessible_length = sum(shape_length * prop_length_counts)), by = c("HARO_factor"))
# 
# HARO_not_eligible <- c("114000", "114115", "115000", "115116", "116000", "116117")
# 
# acc1_summary <- acc1_summary %>% filter(!HARO_factor %in% HARO_not_eligible)
# 
# acc1_summary <- acc1_summary %>% mutate(accessible_prop = sum_accessible_length / sum_total_length,
#                                         HARO = as.integer(as.character(HARO_factor)))
# 
# HARO_with_extra <- HARO %>% 
#   #filter(!HARO %in% HARO_not_eligible) %>%
#   left_join(acc1_summary, by = c("HARO"))
# 
# plot(HARO_with_extra["accessible_prop"])
# # recreate the above plot with ggplot, please.
# 
# ggplot(data = HARO_with_extra) + 
#   geom_sf(color = alpha("white",0.2), aes(fill=as.numeric(accessible_prop))) +
#   labs(fill = "Proportion accessible") + 
#   theme_black_test2()
# 
# ggsave("accessible_proportion_catchment.png", bg = "transparent", dpi = 700, width = 5, height = 6)
# 
# 
# 
# large_accessible_rivers <- HARO_with_extra %>% 
#   filter(as.numeric(sum_total_length) > 2000000 & 
#            as.numeric(accessible_prop) > 0.1)
# 
# HARO_with_extra %>% 
#   ggplot(aes(x=as.numeric(sum_total_length), y=as.numeric(accessible_prop))) + 
#   geom_point(color = "white") +
#   geom_text(data = large_accessible_rivers, 
#             aes(x=as.numeric(sum_total_length), 
#                 y=as.numeric(accessible_prop)+0.05,
#                 label = NAME), color = "white")+
#   labs(y = "Proportion accessible",
#        x = "Catchment size (m)")+
#   theme_black_test2()
# 
# ggsave("accessible_proportion_vs_catchment_size.png", bg = "transparent", dpi = 700, width = 5, height = 4)










# 
# acc2_summary <- rivers_acc2_filtered %>% as.data.frame() %>% group_by(HARO_factor) %>% summarize(sum_accessible_length_fishways = sum(shape_length * prop_length_counts))
# 
# acc2_summary <- acc2_summary %>% mutate(HARO = as.integer(as.character(HARO_factor))) 
# 
# acc12_summary <- acc1_summary %>% left_join(acc2_summary, by = "HARO")
# 
# acc12_summary <- acc12_summary %>% mutate(increase_with_fishway = sum_accessible_length_fishways / sum_accessible_length,
#                                           accessible_prop_fishway = sum_accessible_length_fishways / sum_total_length)
# 

### Some statistics on that


p_load(units)

ggplot(data=acc12_summary, aes(x = log(AREAL), y = as.numeric(accessible_prop))) +
  geom_point() + 
  geom_segment(aes(x=log(AREAL), xend=log(AREAL), y = as.numeric(accessible_prop), yend = as.numeric(accessible_prop_fishway)))

hist(acc12_summary$increase_with_fishway)

acc12_summary <- acc12_summary %>% mutate(increase_accessible_length_fishways = sum_accessible_length_fishways - sum_accessible_length)

HARO_with_extra <- HARO %>% left_join(acc12_summary, by = c("HARO"))

plot(HARO_with_extra["increase_accessible_length_fishways"])

plot(data=as.data.frame(HARO_with_extra), accessible_prop~AREAL.y)

plot(data=as.data.frame(HARO_with_extra), accessible_prop_fishway~AREAL.y)

# leaflet() %>%
#   addTiles() %>%
#   addPolylines(rivers_acc2)

ggplot()+
  geom_sf(data=rivers_all_filtered %>% filter(!RSTID %in% c(rivers_acc2_filtered$RSTID, rivers_acc2_filtered$RSTID)), color="#2a343c") +
  geom_sf(data=rivers_acc2_filtered, color = "steelblue1")+
  geom_sf(data=rivers_acc1_filtered, color = "steelblue4")+
  theme_black_test2()

ggsave("accessibility_with_fishways.png", bg = "transparent", dpi = 700, width = 5, height = 6)


ggplot(data = HARO_with_extra) + 
  geom_sf(color = alpha("white",0.2), aes(fill=as.numeric(accessible_prop_fishway))) +
  labs(fill = "Proportion accessible") + 
  theme_black_test2()

ggsave("accessible_proportion_catchment_fishways.png", bg = "transparent", dpi = 700, width = 5, height = 6)










                                                                                                 