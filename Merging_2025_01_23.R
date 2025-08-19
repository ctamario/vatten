




# Is it possible to make a "final" file for the dams?

# Just load it and take a look.
# Do it in R?
# I've done it in GIS, but maybe the only real way forward is to do it in R. Then it's fully traceable and reproducible.
# It's also possible to change the workflow... Can't believe I'm back on square one... 

# Load packages

library(pacman)
p_load(tidyr, sf, dplyr, ggplot2, stringr, readxl)

# Clean slate

rm(list=ls())

# Load background rivers 

#rivers <- read_sf("C:/Users/caio0001/Documents/test/vd_l_2016_3_RivEX.shp")
rivers <- read_sf("//storage-ume.slu.se/home$/caio0001/My Documents/test/vd_l_2016_3_RivEX.shp")


### 1. Load SMHI dam barrier data

SMHI_dams <- read_sf("data/DAMM_PROD_2013_3.shp")

# Function to join nearest features and filter by distance
nearest_feature_within_distance <- function(source_sf, target_sf, distance_threshold, crs = NULL) {
  # Ensure both layers are in the same CRS (transform if needed)
  if (!is.null(crs)) {
    source_sf <- st_transform(source_sf, crs)
    target_sf <- st_transform(target_sf, crs)
  }
  
  # Perform nearest neighbor join
  nearest_indices <- st_nearest_feature(source_sf, target_sf)
  
  # Calculate distances to the nearest features
  distances <- st_distance(source_sf, target_sf[nearest_indices, ], by_element = TRUE)
  
  # Add the nearest feature attributes and distance to the source layer
  source_sf_with_attrs <- st_join(source_sf, target_sf[nearest_indices, ], join = st_nearest_feature)
  source_sf_with_attrs$dist_to_network <- as.numeric(distances)
  
  # Filter features within the distance threshold
  #source_sf_within_distance <- source_sf_with_attrs[source_sf_with_attrs$dist < distance_threshold, ]
  #return(source_sf_within_distance)
  return(source_sf_with_attrs)
}

# These occurrences have their original position, so they are not properly snapped
# However, they contain the information of the closest river polyline and
# a distance measure to it. So it's possible to filter them away later. 
# I think it would be preferable to keep data points as much as possible
# and then filter them away.

SMHI_dams_snapped <- nearest_feature_within_distance(SMHI_dams, rivers)

table(SMHI_dams_snapped$VERKSMHT, useNA = "always")
table(SMHI_dams_snapped$FISKVAG, useNA = "always")

#######
####### Preparing the fishways columns for the SMHI barrier database
#######

SMHI_dams_snapped$fishway_SMHI_CT <- ifelse(SMHI_dams_snapped$FISKVAG == 0 | SMHI_dams_snapped$FISKVAG == 9, 0, 1)

SMHI_dams_snapped <- SMHI_dams_snapped %>%
  mutate(fishwaytype_SMHI_CT = case_when(
    FISKVAG == 1  ~ "Kammartrappa",
    FISKVAG == 2  ~ "Denilränna",
    FISKVAG == 3  ~ "Slitsränna",
    FISKVAG == 4  ~ "Omlöp",
    FISKVAG == 5  ~ "Inlöp",
    FISKVAG == 6  ~ "Ålledare",
    FISKVAG == 8  ~ "Övrig",
    FISKVAG == 10 ~ "Övrig",
    TRUE ~ NA_character_  # For all other cases
  ))


# just diagnostics tables to see that it is correct:
with(SMHI_dams_snapped, table(fishwaytype_SMHI_CT, FISKVAG, useNA = "always"))

table(SMHI_dams_snapped$fishway_SMHI_CT, SMHI_dams_snapped$FISKVAG)

SMHI_dams_snapped$relevant_Calle <- ifelse(SMHI_dams_snapped$STATUS == 2, 0, 1)

# relevant_Calle is a variable I will use for if the dam should be "real" or not
# for removed dams, relevant_Calle will be 0, for example. 
table(SMHI_dams_snapped$relevant_Calle, SMHI_dams_snapped$STATUS)

# Source of the dam
SMHI_dams_snapped$source_Calle <- "SMHI"

# Type of the dam; as for SMHI, I will call them all dams. 
SMHI_dams_snapped$type_Calle <- "damm"

# check for self-duplicates
# I want to remove all double (or more) geometries

equals_matrix <- st_equals(SMHI_dams_snapped)
duplicates <- which(lengths(equals_matrix) > 1)

# Sort the duplicates by location and then by fishway (if present) on top which will be kept
dupes <- arrange(SMHI_dams_snapped[duplicates,], NORTH, desc(FISKVAG))

# create a dataset with all the occurrences to remove (i.e., the duplicate occurrences)
dupes <- dupes %>% group_by(EAST, NORTH) %>% slice(-1) %>% ungroup()

# And then remove them.
SMHI_dams_snapped2 <- SMHI_dams_snapped %>% filter(!DAMMID %in% dupes$DAMMID)


#######
####### Load and prepare Vandringshinderdatabasen (BIOTOP)
#######

BIOTOP_dams <- read.csv("//storage-ume.slu.se/home$/caio0001/My Documents/gis/rawdata/Vandringshinder_csv.csv", sep=";", dec=",", fileEncoding = "Latin1")

BIOTOP_dams$BIOTOP_ID_Calle <- 1:nrow(BIOTOP_dams)

BIOTOP_dams <- BIOTOP_dams[BIOTOP_dams$Northing > 4000000,] # wrong location!
BIOTOP_dams <- BIOTOP_dams[BIOTOP_dams$VandringshinderID != 85406,] # wrong location!

# It seems relevant is a variable that tells which datapost is the newest
# if a place has been visited several times
BIOTOP_dams <- BIOTOP_dams %>% dplyr::filter(Relevant == "TRUE")

# It seems some occurrences are visited more than one time anyway
table(as.data.frame(table(BIOTOP_dams$VandringshinderID))$Freq)

# Sort by date and keep only the latest "inventory"
BIOTOP_dams2 <- BIOTOP_dams %>% arrange(VandringshinderID, desc(Karteringsdatum)) %>% group_by(VandringshinderID) %>% slice(1) %>% ungroup()

# fixed!
table(as.data.frame(table(BIOTOP_dams2$VandringshinderID))$Freq)

# convert into an sf object

BIOTOP_dams2$SWEREF99tm_e <- BIOTOP_dams2$Easting
BIOTOP_dams2$SWEREF99tm_n <- BIOTOP_dams2$Northing

BIOTOP_dams_sf <- st_as_sf(BIOTOP_dams2, coords = c("SWEREF99tm_e", "SWEREF99tm_n"), crs = 3006)

# Snap to network!

BIOTOP_dams_sf <- nearest_feature_within_distance(BIOTOP_dams_sf, rivers)


# plot it and see if it looks good
ggplot() + geom_sf(data = BIOTOP_dams_sf) + geom_sf(data = SMHI_dams_snapped2, color = "red")
# yep they are on top of each other, looks good.

# specify the source using the same variable name as in the SMHI dataframe
BIOTOP_dams_sf$source_Calle <- "BIOTOP"

# just check which categories there are
table(BIOTOP_dams_sf$Vandringshindertyp, useNA = "always")
# there are some weird empty entries

# 
BIOTOP_dams_sf$Vandringshindertyp[which(BIOTOP_dams_sf$Vandringshindertyp == "")] <- "övrigt hinder"

table(BIOTOP_dams_sf$Vandringshindertyp, useNA = "always")

# specify the type of barrier
BIOTOP_dams_sf$type_Calle <- BIOTOP_dams_sf$Vandringshindertyp

### Check for self-duplicate geometries in BIOTOP_dams

equals_matrix <- st_equals(BIOTOP_dams_sf)
duplicates <- which(lengths(equals_matrix) > 1)

BIOTOP_dupes <- BIOTOP_dams_sf[duplicates,] %>%
  arrange(Northing) %>%
  group_by(Northing, Easting) %>%
  slice(-1) %>%
  ungroup()


# And then remove them.
BIOTOP_dams_sf2 <- BIOTOP_dams_sf %>% filter(!BIOTOP_ID_Calle %in% BIOTOP_dupes$BIOTOP_ID_Calle)



#######
####### Preparing the fishways columns for the BIOTOP database
#######


# Create the fishway yes/no variable
table(BIOTOP_dams_sf2$Fiskväg)
BIOTOP_dams_sf2$fishway_BIOTOP_CT <- ifelse(BIOTOP_dams_sf2$Fiskväg == T, 1, 0)

table(BIOTOP_dams_sf2$Fiskväg, BIOTOP_dams_sf2$fishway_BIOTOP_CT)

BIOTOP_dams_sf2$fishway_BIOTOP_CT[is.na(BIOTOP_dams_sf2$fishway_BIOTOP_CT)] <- 0

# Create the fishway type variable

BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT <- BIOTOP_dams_sf2$FiskvagTyp

BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT == "" & BIOTOP_dams_sf2$fishway_BIOTOP_CT == 1)] <- "Unknown"

BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT == "")] <- NA

table(BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT, useNA = "always")

BIOTOP_dams_sf2 %>% dplyr::select(fishway_BIOTOP_CT, FiskvagTyp) %>% filter(fishway_BIOTOP_CT == 1)

30750-30222

#Omlöp
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(str_detect(BIOTOP_dams_sf2$FiskvagTyp, regex("omlöp", ignore_case = T)))] <- "Omlöp"
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(str_detect(BIOTOP_dams_sf2$FiskvagTyp, regex("omlop", ignore_case = T)))] <- "Omlöp"
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(str_detect(BIOTOP_dams_sf2$FiskvagTyp, regex("olmlöp", ignore_case = T)))] <- "Omlöp"
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(str_detect(BIOTOP_dams_sf2$FiskvagTyp, regex("ömlöp", ignore_case = T)))] <- "Omlöp"
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(str_detect(BIOTOP_dams_sf2$FiskvagTyp, regex("naturl", ignore_case = T)))] <- "Omlöp"


#Ål (search for "ål" but exclude if it is part of "ålig" (from word dålig))
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(str_detect(BIOTOP_dams_sf2$FiskvagTyp, regex("ål(?!ig)", ignore_case = T)))] <- "Ålledare"

# Bassäng / trappa
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(str_detect(BIOTOP_dams_sf2$FiskvagTyp, regex("bassäng", ignore_case = T)))] <- "Kammartrappa"
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(str_detect(BIOTOP_dams_sf2$FiskvagTyp, regex("trap", ignore_case = T)))] <- "Kammartrappa"

# Utrivning
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(str_detect(BIOTOP_dams_sf2$FiskvagTyp, regex("utriv", ignore_case = T)))] <- "Utrivning"

# Denilränna
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(str_detect(BIOTOP_dams_sf2$FiskvagTyp, regex("denil", ignore_case = T)))] <- "Denilränna"


#I.e., these are the ones that I DON'T want to rename to "Övrig"
legible_fishway_types <- c("Denilränna", "Kammartrappa", "Inlöp", "Omlöp", "", "Slitsränna", "Utrivning", "Ålledare", NA)

#Remaining are the ones that I WANT to rename to "Övrig" 
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(!BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT %in% legible_fishway_types)] <- "Övrig"
BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT[which(BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT == "")] <- NA


table(BIOTOP_dams_sf2$fishwaytype_BIOTOP_CT, useNA = "always")



# Don't remove barrier objects far away yet! Try to keep everything. 
# BIOTOP_dams_filtered <- BIOTOP_dams_sf2 %>% filter(dist_to_network <= 100)

### Joining the two barrier layers
# Finding all the occurrences in the SMHI layer that is at least
# 100 meters away from any occurence in BIOTOP layer

# Find the nearest neighbor and calculate distances
# For each occurrence in SMHI, find the nearest occurrence in BIOTOP:
nearest_neighbors <- st_nearest_feature(SMHI_dams_snapped2, BIOTOP_dams_sf2)  # Index of nearest point in second layer
distances <- st_distance(SMHI_dams_snapped2, BIOTOP_dams_sf2[nearest_neighbors, ], by_element = TRUE)  # Distances

# Add the distance column to points1
SMHI_dams_snapped2_with_dist <- SMHI_dams_snapped2 %>%
  mutate(nearest_distance = as.numeric(distances))  # Convert distance to numeric for readability

SMHI_dams_filtered <- SMHI_dams_snapped2_with_dist %>% filter(STATUS != 2) %>% filter(nearest_distance > 100)


### See how it looks

ggplot() + geom_sf(data = BIOTOP_dams_sf2) + geom_sf(data = SMHI_dams_filtered, color = "red")
# looks good I guess. Only 2006 occurrences that aren't within BIOTOP


### Code for finding overlapping points between two layers... 
# 
# duplicates_SMHI_BIOTOP <- st_equals(BIOTOP_dams_sf, SMHI_dams_snapped2)
# duplicate_indices_SMHI_BIOTOP <- which(lengths(duplicates_SMHI_BIOTOP) > 0)
# 
# BIOTOP_dams_sf_nonoverlapping <- BIOTOP_dams_sf[-duplicate_indices_SMHI_BIOTOP,]


######
###### merge points with non-overlapping geometry
######

SMHI_BIOTOP_merged <- bind_rows(SMHI_dams_filtered, BIOTOP_dams_sf2)

table(SMHI_BIOTOP_merged$type_Calle, useNA = "always")


SMHI_BIOTOP_merged  %>% ggplot() + geom_sf(aes(color=source_Calle))
SMHI_BIOTOP_merged %>% ggplot() + geom_sf(aes(color=type_Calle))

table(SMHI_BIOTOP_merged$type_Calle, useNA = "always")

SMHI_BIOTOP_merged$SWEREF99tm_e <- st_coordinates(SMHI_BIOTOP_merged)[,1]
SMHI_BIOTOP_merged$SWEREF99tm_n <- st_coordinates(SMHI_BIOTOP_merged)[,2]

# some general summaries to see that everything looks fine
table(SMHI_BIOTOP_merged$Strahler, useNA = "always")

table(SMHI_BIOTOP_merged$type_Calle, SMHI_BIOTOP_merged$Strahler, useNA = "always")

# why not create an ID...
SMHI_BIOTOP_merged$overallID_Calle <- 1:nrow(SMHI_BIOTOP_merged)

# Export temporarily:
SMHI_BIOTOP_merged_reduced <- SMHI_BIOTOP_merged %>% dplyr::select(SWEREF99tm_e, 
                                                                   SWEREF99tm_n, 
                                                                   overallID_Calle)

write.table(SMHI_BIOTOP_merged_reduced, "SMHI_BIOTOP_merged_out_for_RivEX_2024_12_13.csv", sep = ";", dec = ",", row.names = F, fileEncoding = "Latin1")

head(SMHI_BIOTOP_merged_reduced)



### What about finding our where on the line each occurrence lies. 

# polyline layer
head(rivers)

# barrier layer
head(SMHI_BIOTOP_merged)

pos_info <- readxl::read_excel(path = "//storage-ume.slu.se/home$/caio0001/My Documents/ArcGIS/Projects/Sveriges_vatten/positional_info_2024_12_13_TableToExcel.xlsx")

head(pos_info)

SMHI_BIOTOP_merged_snapped <- SMHI_BIOTOP_merged %>% 
  dplyr::filter(dist_to_network < 100) %>% 
  dplyr::left_join(pos_info, by = c("overallID_Calle" = "SiteID"))

TEMP <- SMHI_BIOTOP_merged_snapped[which(is.na(SMHI_BIOTOP_merged_snapped$Per_Along)),]
TEMP$overallID_Calle

tabort <- SMHI_BIOTOP_merged_snapped %>% dplyr::filter(overallID_Calle %in% TEMP$overallID_Calle)

SMHI_BIOTOP_merged_snapped$RelPos[is.na(SMHI_BIOTOP_merged_snapped$Per_Along)] <- 0.5



# Incorporating ÅIV too. 

### Load ÅIV
getwd()

AIV <- read.csv("//storage-ume.slu.se/home$/caio0001/My Documents/gis/rawdata/AIV_Uttag_240902.csv", sep = ";", dec = ",", fileEncoding = "Latin1")

str(AIV)

AIV2 <- AIV %>% group_by(AtgardsId) %>% slice_tail(n=1) %>% select(Fardigstald, Namn1, Namn2, Namn3, Namn4, Visningsnamn.3, avg.punkt.x, avg.punkt.y)

AIV2 <- AIV2 %>% filter(avg.punkt.x != "NULL")

AIV2$avg.punkt.x <- str_replace_all(AIV2$avg.punkt.x, ",", ".")
AIV2$avg.punkt.y <- str_replace_all(AIV2$avg.punkt.y, ",", ".")

AIV2$avg.punkt.x <- as.numeric(AIV2$avg.punkt.x)
AIV2$avg.punkt.y <- as.numeric(AIV2$avg.punkt.y)

# Assign proper coordinate columns
AIV2$SWEREF99tm_e <- AIV2$avg.punkt.x
AIV2$SWEREF99tm_n <- AIV2$avg.punkt.y

head(AIV2, n = 20)

AIV2_konn <- AIV2 %>% filter(Namn2 == "Åtgärd för förändrad konnektivitet") %>% group_by(avg.punkt.x, avg.punkt.y) %>% mutate(n = n())

table(AIV2_konn$n)

print(AIV2_konn %>% filter(n > 1) %>% arrange(Fardigstald, desc = T) %>% arrange(avg.punkt.x) ,  n = 100)

AIV2_no_dupes <- AIV2_konn %>% arrange(Fardigstald, desc = T) %>% arrange(avg.punkt.x) %>% group_by(avg.punkt.x, avg.punkt.y) %>% slice_tail(n = 1)


table(AIV2_no_dupes$Namn3)

AIV2_no_dupes

AIV3 <- AIV2_no_dupes %>% filter(Namn3 != "Driftanpassning") %>% 
  filter(Namn3 != "Kulvert") %>%
  filter(Namn3 != "Vägpassage")

table(AIV3$Namn3)
table(AIV3$Namn4)

AIV3 %>% filter(Namn3 == "Naturlik faunapassage") %>% group_by(Namn4) %>% tally()
AIV3 %>% filter(Namn3 == "Teknisk faunapassage") %>% group_by(Namn4) %>% tally()


AIV3 <- AIV3 %>% filter(!Namn4 %in% c("Avledare med flyktöppning",
                                      "Fiskhiss", 
                                      "Fånga och transportera", 
                                      "Galler med flyktöppning/fiskavledare",
                                      "Styrning med galler (uppströmsvandring)",
                                      "Utterpassage",
                                      "Ålyngelledare",
                                      "Ålyngelsamlare",
                                      "Övrig nedströmspassage"))

AIV3 %>% filter(Namn3 == "Teknisk faunapassage") %>% group_by(Namn4) %>% tally()

AIV3$Namn4[AIV3$Namn4 == "NULL"] <- "Övrig"

AIV3_sf <- st_as_sf(AIV3, coords = c("avg.punkt.x", "avg.punkt.y"), crs = 3006)

AIV3_sf # Ready for merging I think!


AIV3_sf$fishway_AIV_CT <- 1
AIV3_sf$fishwaytype_AIV_CT <- AIV3_sf$Namn4

write.table(AIV3_sf, file = "C:/temp/AIV3_sf.csv", sep = ";", dec = ",", row.names = F, fileEncoding = "Latin1")

# I want to merge all the information in AIV3_sf to the merged SMHI_BIOTOP_merged_snapped

# Function to join nearest features and assign NA for distant matches
nearest_fishway_within_distance <- function(source_sf, target_sf, distance_threshold, crs = NULL) {
  # Ensure both layers are in the same CRS (transform if needed)
  # if (!is.null(crs)) {
  #   source_sf <- st_transform(source_sf, crs)
  #   target_sf <- st_transform(target_sf, crs)
  # }
  
  # Drop geometry from the target_sf to prevent conflicts
  target_attributes <- st_drop_geometry(target_sf)
  
  # Perform nearest neighbor join
  nearest_indices <- st_nearest_feature(source_sf, target_sf)
  
  # Calculate distances to the nearest features
  distances <- st_distance(source_sf, target_sf[nearest_indices, ], by_element = TRUE)
  distances_numeric <- as.numeric(distances)
  
  # Create a logical vector for features within the distance threshold
  within_threshold <- distances_numeric <= distance_threshold
  
  # Create a placeholder for joined attributes
  joined_attributes <- target_attributes[nearest_indices, ]
  joined_attributes[!within_threshold, ] <- NA
  
  # Bind the joined attributes and distances to the source_sf
  source_sf_with_attrs <- cbind(source_sf, joined_attributes)
  source_sf_with_attrs$dist_to_fishway <- ifelse(within_threshold, distances_numeric, NA)
  
  return(source_sf_with_attrs)
}

######
###### Snap the merged database with the AIV dataset
######

# Snap the merged database with the AIV dataset
SMHI_BIOTOP_merged_snapped_with_AIV <- nearest_fishway_within_distance(SMHI_BIOTOP_merged_snapped, AIV3_sf, distance_threshold = 100)

# Checking if the fishway 1/0 column has any NA
table(SMHI_BIOTOP_merged_snapped_with_AIV$fishway_SMHI_CT, useNA="always")
table(SMHI_BIOTOP_merged_snapped_with_AIV$fishway_BIOTOP_CT, useNA="always")
table(SMHI_BIOTOP_merged_snapped_with_AIV$fishway_AIV_CT, useNA="always")

# Replacing all the NA in the fishway 1/0 column
SMHI_BIOTOP_merged_snapped_with_AIV$fishway_SMHI_CT[is.na(SMHI_BIOTOP_merged_snapped_with_AIV$fishway_SMHI_CT)] <- 0
SMHI_BIOTOP_merged_snapped_with_AIV$fishway_BIOTOP_CT[is.na(SMHI_BIOTOP_merged_snapped_with_AIV$fishway_BIOTOP_CT)] <-0
SMHI_BIOTOP_merged_snapped_with_AIV$fishway_AIV_CT[is.na(SMHI_BIOTOP_merged_snapped_with_AIV$fishway_AIV_CT)] <- 0

# Checking again if the fishway 1/0 column has any NA
table(SMHI_BIOTOP_merged_snapped_with_AIV$fishway_SMHI_CT, useNA="always")
table(SMHI_BIOTOP_merged_snapped_with_AIV$fishway_BIOTOP_CT, useNA="always")
table(SMHI_BIOTOP_merged_snapped_with_AIV$fishway_AIV_CT, useNA="always")

# Create a variable (fishway_overall_CT) that checks if there is a
# fishway in at least one of the datasets for all objects
SMHI_BIOTOP_merged_snapped_with_AIV <- SMHI_BIOTOP_merged_snapped_with_AIV %>% mutate(fishway_overall_CT = ifelse(fishway_SMHI_CT+fishway_BIOTOP_CT+fishway_AIV_CT > 0,1,0))

## Working with the fishway type ...

temp_df <- SMHI_BIOTOP_merged_snapped_with_AIV %>%
  dplyr::select(fishwaytype_SMHI_CT,
                fishwaytype_BIOTOP_CT,
                fishwaytype_AIV_CT)


combine_fishway_types <- function(row_in){
  row_in <- as.data.frame(row_in)[,1:3]
  
  # Priority list for fishway types
  fishway_types <- c(
    "Utrivning", 
    "Omlöp", 
    "Inlöp", 
    "Kammartrappa", 
    "Slitsränna", 
    "Denilränna", 
    "Ålledare", 
    "Ramp/Upptröskling/Överlöp", 
    "Övrig uppströmspassage", 
    "Anpassning av damm-/sjötröskel",
    "Övrig"
  )
  
  get_first_fishway_type <- function(input_vector) {
    # Match input vector elements to the order in fishway_types
    matches <- match(input_vector, fishway_types)
    
    # Find the first non-NA match and return corresponding value from fishway_types
    if (any(!is.na(matches))) {
      return(fishway_types[min(matches, na.rm = TRUE)])
    } else {
      return(NA)  # If no match, return NA
    }
  }
  
  df <- row_in[!is.na(row_in)]
  
  if(is.null(nrow(df))){
    df_out <- NA
  }
  
  if(length(df) == 1){
    df_out <- df
  } else if(length(df) == 2 & df[1] == df[2]){
    df_out <- df[1]
  } else if(length(df) > 1){
    df_out <- get_first_fishway_type(df)
  }
  return(df_out)
}


for(i in 1:nrow(SMHI_BIOTOP_merged_snapped_with_AIV)){
  SMHI_BIOTOP_merged_snapped_with_AIV$fishwaytype_merged_CT[i] <- combine_fishway_types(temp_df[i,])
}



# Okay, so I think I have everything to go ahead with the connectivity analyses! 


merged_barriers <- SMHI_BIOTOP_merged_snapped_with_AIV %>% dplyr::select(geometry, SWEREF99tm_e, SWEREF99tm_n, 
                                                      type_Calle, 
                                                      source_Calle,
                                                      relevant_Calle, 
                                                      fishway_SMHI_CT,
                                                      fishway_BIOTOP_CT,
                                                      fishway_AIV_CT,
                                                      fishway_overall_CT,
                                                      fishwaytype_SMHI_CT,
                                                      fishwaytype_BIOTOP_CT,
                                                      fishwaytype_AIV_CT,
                                                      fishwaytype_merged_CT,
                                                      RSTID,
                                                      RSTID_NED,
                                                      overallID_Calle,
                                                      dist_to_network,
                                                      Per_Along,
                                                      Strahler)

merged_barriers$relevant_Calle[which(is.na(merged_barriers$relevant_Calle))] <- 1
merged_barriers$relevant_Calle[which(merged_barriers$fishwaytype_merged_CT == "Utrivning")] <- 0

table(merged_barriers$fishwaytype_merged_CT, useNA="always")
table(merged_barriers$relevant_Calle, useNA="always")
table(merged_barriers$type_Calle, useNA="always")

merged_barriers_clean <- merged_barriers %>% 
  dplyr::filter(!type_Calle %in% c("ålkista", "fiskgaller", "sjöutlopp"))
  

# Translate to English


unique(merged_barriers_clean$type_Calle)

merged_barriers_clean$type_CT <- "dam"
merged_barriers_clean$type_CT[which(merged_barriers_clean$type_Calle == "naturligt hinder")] <- "natural barrier"
merged_barriers_clean$type_CT[which(merged_barriers_clean$type_Calle == "vägpassage")] <- "road passage"
merged_barriers_clean$type_CT[which(merged_barriers_clean$type_Calle == "trumma")] <- "culvert"
merged_barriers_clean$type_CT[which(merged_barriers_clean$type_Calle == "övrigt hinder")] <- "other"

unique(merged_barriers_clean$type_CT)
table(merged_barriers_clean$fishwaytype_merged_CT, useNA = "always")

write.table(merged_barriers_clean, file = "C:/data/merged_barriers20241215_20250123.csv", sep = ";", dec = ",", row.names = F, fileEncoding = "Latin1")

#SMHI_BIOTOP_merged_snapped_with_AIV[which(SMHI_BIOTOP_merged_snapped_with_AIV$overallID_Calle == 21014),]

