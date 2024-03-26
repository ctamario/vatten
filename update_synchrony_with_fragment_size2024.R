


rm(list=ls())


library(pacman)

p_load(sf, dplyr, stringr, leaflet, geos)


getwd()


df_sf <- read_sf("data/vd_l_2016_3.shp")


df_sf_wgs84 <- st_transform(df_sf, crs = st_crs(4326))

df_sf_wgs84$SWE <- as.numeric(str_detect(df_sf_wgs84$DISTRICT, "^SE[0-9]$"))+as.numeric(str_detect(df_sf_wgs84$DISTRICT, "SE1TO"))


df_sf_wgs84


### Import disty-data

df_synch <- read.csv("C:/jobb/projects/TEMPTEMPTEMP/all_data_synchrony.csv", sep = ";")

# Need coordinates in either SWEREF99tm or WGS84 and connect them to df_synch

all_raw_data <- read.csv("C:/jobb/rawdata/rawdata_20220707.csv", sep = ";", dec = ",") # Load rawdata from SERS

COOR_KEY <- all_raw_data %>% group_by(XKOORLOK, YKOORLOK, ddlat, ddlong) %>% summarise(vdragnam = first(vdragnam)) #aggregate to make a key

head(COOR_KEY) # check so it works

df_synch <- df_synch %>% dplyr::left_join(COOR_KEY %>% 
                                            select(XKOORLOK, YKOORLOK, ddlat, ddlong), by = c("fromXKOORLOK" = "XKOORLOK",
                                                                                              "fromYKOORLOK" = "YKOORLOK")) #left join "real" coordinates based on XKOORLOK YKOORLOK



df_synch_sf <- st_as_sf(df_synch, coords = c("ddlong", "ddlat"), crs = 4326) # make the dataframe an sj object


###

leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data = df_synch_sf) 

### 

df_synch_joined <- st_join(df_synch_sf, df_sf_wgs84, join = st_nearest_feature, dist = 100) # join each synchrony site to an RSTID

###

synch_rstids <- df_synch_joined %>% dplyr::select(fromXKOORLOK, fromYKOORLOK, RSTID) %>% distinct()

### Here you can start to import code from the previous functions in order to calculate fragment sizes for all the sites .... 
### Remember to make sure how much "upstream" you should calculate. 
### It seems that the river polyline layer has been cut at every dam in a systematic way. WHAT A CONVENTIENT COINCIDENCE! 
### Make sure to use this in the proper way. 

### Also initiate dam layer:

dams_sf <- read_sf("data/DAMM_PROD_2013_3.shp")

dams_sf_wgs84 <- st_transform(dams_sf, crs = st_crs(4326))

dams_joined <- st_join(dams_sf_wgs84, df_sf_wgs84, join = st_nearest_feature, dist = 100)

dams_joined <- dams_joined %>% filter(STATUS != 2)


###

down_seg <- function(in_df, seg){
  down_seg <- in_df$RSTID_NED[which(in_df$RSTID == seg)]
  return(down_seg)
}


down_seg_until_dam <- function(in_df, seg){
  
  next_down <- down_seg(in_df, seg)
  list_down <- next_down
  i = 1
  while(i < 1000){
    next_down <- down_seg(in_df, next_down)
    list_down <- c(next_down, list_down)
    if(next_down %in% dams_joined$RSTID || in_df$LINJEKOD[which(in_df$RSTID == next_down)] == 26){
    #if(next_down %in% dams_joined$RSTID){  
      break
    }
    #print(paste0("next_down = ", next_down))
    i <- i + 1
  }
  return(list(list_down=list_down, furthest_down=list_down[1]))
}

###


###
seg_no <- 40

seg_no <- seg_no + 5

seg1 <- synch_rstids$RSTID[seg_no]

seg1 

down_seg_until_dam(df_sf_wgs84, seg1)

colorsss <- rainbow(length(down_seg_until_dam(df_sf_wgs84, seg1)$list_down))

leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data = synch_rstids[seg_no, ], group = "hej") %>%
  setView(
    lng = st_coordinates(synch_rstids[seg_no, ])[1],
    lat = st_coordinates(synch_rstids[seg_no, ])[2],
    zoom = 11
  ) %>%
  addPolylines(data = df_sf_wgs84[which(df_sf_wgs84$RSTID == seg1), ]) %>%
  addCircleMarkers(data = dams_joined, color = "red") %>%
  addPolylines(data = df_sf_wgs84[which(df_sf_wgs84$RSTID %in% down_seg_until_dam(df_sf_wgs84, seg1)$list_down), ], color = ~colorsss)



###

# Upstream function  

up_seg <- function(in_df, seg){
  up_seg <- in_df$RSTID[which(in_df$RSTID_NED == seg)]
  #print(up_seg)
  return(up_seg)
}

up_seg_all3 <- function(in_df, seg){
  next_up <- up_seg(in_df, seg)
  list_up <- next_up
  while(length(next_up) > 0) {
    new_up <- up_seg(in_df, next_up[1])
    new_up <- new_up[!new_up %in% list_up]  # Remove segments already in list_up
    #new_up <- sapply(new_up, function(x) !any(dams_related$RSTID == x))
    list_up <- c(list_up, new_up)
    new_up <- new_up[!new_up %in% dams_joined$RSTID]
    
    next_up <- c(next_up, new_up)
    next_up <- next_up[-1]
  }
  return(c(seg, list_up))
} # Det funkar!


### writing a function positions of points on origin segment

is_dam_present <- function(segment){
  if(segment %in% dams_joined$RSTID){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_dam_present(seg1)


is_site_below_dam <- function(segment){
  
  test_pt_origin <- synch_rstids[which(synch_rstids$RSTID == segment),]
  test_pt_dams <- dams_joined[which(dams_joined$RSTID == segment),]
  test_ln <- df_sf_wgs84[which(df_sf_wgs84$RSTID == segment),]
  
  origin_pos <- geos::geos_project_normalized(geos::as_geos_geometry(test_ln), geos::as_geos_geometry(test_pt_origin))
  dam_pos <- geos::geos_project_normalized(geos::as_geos_geometry(test_ln), geos::as_geos_geometry(test_pt_dams))
  
  if(FALSE %in% (origin_pos > dam_pos)){
    return(FALSE)
  } else {
    return(TRUE)
  }

}

is_site_below_dam(seg1)


### Here to create a new function that takes into account whether any potential dam is upstream or downstream origin point


seg1

up_seg_all3(df_sf_wgs84, seg1)


run_function <- function(segment){
  
  if(is_dam_present(segment) == T){
    print("dam is present")
    if(is_site_below_dam(segment) == T){
      print("site is below dam")
      furthest_down <- down_seg_until_dam(df_sf_wgs84, segment)$furthest_down
      out <- up_seg_all3(furthest_down)
    } else if(is_site_below_dam(segment) == F) {
      print("dam is below site")
      out <- up_seg_all3(df_sf_wgs84, segment)
    }
  } else {
    print("no dam is present in segment")
    furthest_down <- down_seg_until_dam(df_sf_wgs84, segment)$furthest_down
    out <- up_seg_all3(df_sf_wgs84, furthest_down)
  }
  return(out)
  
} 

test_run <- run_function(seg1)



###
###

leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data = synch_rstids[seg_no, ], group = "hej") %>%
  setView(
    lng = st_coordinates(synch_rstids[seg_no, ])[1],
    lat = st_coordinates(synch_rstids[seg_no, ])[2],
    zoom = 11
  ) %>%
  addPolylines(data = df_sf_wgs84[which(df_sf_wgs84$RSTID == seg1), ]) %>%
  addCircleMarkers(data = dams_joined, color = "red") %>%
  addPolylines(data = df_sf_wgs84[which(df_sf_wgs84$RSTID %in% test_run), ], color = ~colorsss)




# leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
#   addCircleMarkers(data=synch_rstids[seg_no,]) %>%
#   setView(lng=st_coordinates(synch_rstids[seg_no,])[1],
#           lat=st_coordinates(synch_rstids[seg_no,])[2], zoom=10) %>%
#   addPolylines(data=df_sf_wgs84[which(df_sf_wgs84$RSTID == seg1),]) %>%
#   addCircleMarkers(data=dams_joined, color="red")

REMOVE <- dams_joined[which(dams_joined$DAMMID == "{6BDEF4F1-54EA-45FE-BF42-6EA5087A7677}"),]

#######
# Benford's law



hist(as.numeric(df_synch$pythdist), breaks=2000)
hist(as.numeric(str_sub(df_synch$pythdist, 1, 1)))


hist(as.numeric(str_sub(abs(rnorm(50000,50000,50000)), 1, 1)))

###### Just the logic of the code

######

seg1 <- 63358561526532

point_on_seg <- function(segment){
  
  test_pt_origin <- synch_rstids[which(synch_rstids$RSTID == segment),]
  test_pt_dams <- dams_joined[which(dams_joined$RSTID == segment),]
  test_ln <- df_sf_wgs84[which(df_sf_wgs84$RSTID == segment),]
  
  origin_pos <- geos::geos_project_normalized(geos::as_geos_geometry(test_ln), geos::as_geos_geometry(test_pt_origin))
  dam_pos <- geos::geos_project_normalized(geos::as_geos_geometry(test_ln), geos::as_geos_geometry(test_pt_dams))
  
  return(list(list(test_pt_origin, test_pt_dams, test_ln), list(origin_pos, dam_pos)))
}

point_on_seg(63323851532981)




seg_info <- function(in_seg, in_data){
  out <- data.frame(RSTID = in_seg, seg_length = st_length(in_data[which(in_data$RSTID == in_seg),]))
  return(out)
}

seg_info <- function(in_seg, in_polylines, direction = "any"){
  if(direction == "any"){
    prop <- 1
  } else if(direction == "up"){
    prop <- 
  }
  out <- data.frame(RSTID = in_seg, seg_length = st_length(in_polylines[which(in_polylines$RSTID == in_seg),]))
  return(out)
}



seg_info(seg1, df_sf_wgs84)


###

find_fragment_size <- function(site_location, segment_name){
  
  #Is there a dam in the segment_name with the site_location?
  is_dam_present
    #If yes
      # Is the site most upstream?
        # calculate distance downstream to the dam_location
        # run the find_all_upstream
      # Is the site most downstream?
      # Is the site between two dams?
    #If no (else)
      # run find_furthest_downstream
      # run from furthest_downstream, run find_all_upstream
  
}




######

