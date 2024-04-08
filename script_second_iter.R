



### 

rm(list=ls())


library(pacman)

p_load(sf, dplyr, stringr, leaflet, geos)


getwd()


df_sf <- read_sf("data/vd_l_2016_3.shp")


df_sf_wgs84 <- st_transform(df_sf, crs = st_crs(4326))

df_sf_wgs84$SWE <- as.numeric(str_detect(df_sf_wgs84$DISTRICT, "^SE[0-9]$"))+as.numeric(str_detect(df_sf_wgs84$DISTRICT, "SE1TO"))

df_sf_wgs84

###

dams_sf <- read_sf("data/DAMM_PROD_2013_3.shp")

dams_sf_wgs84 <- st_transform(dams_sf, crs = st_crs(4326))

dams_joined <- st_join(dams_sf_wgs84, df_sf_wgs84, join = st_nearest_feature, dist = 100)

dams_joined <- dams_joined %>% filter(STATUS != 2)


### A very basic function of finding the position of a point on a line

# point_on_seg <- function(segment){
#   
#   test_pt_dams <- dams_joined[which(dams_joined$RSTID == segment),]
#   test_ln <- df_sf_wgs84[which(df_sf_wgs84$RSTID == segment),]
#   
#   #origin_pos <- geos::geos_project_normalized(geos::as_geos_geometry(test_ln), geos::as_geos_geometry(test_pt_origin))
#   dam_pos <- geos::geos_project_normalized(geos::as_geos_geometry(test_ln), geos::as_geos_geometry(test_pt_dams))
#   
#   return(dam_pos)
# }
# 
# point_on_seg(one_seg)

find_pos_on_segment <- function(in_point, in_segment, in_polylines){
  
  seg_sf <- in_polylines[which(in_polylines$RSTID == in_segment),]
  
  prop <- geos::geos_project_normalized(geos::as_geos_geometry(seg_sf), 
                                        geos::as_geos_geometry(in_point))
  return(prop)
}

find_pos_on_segment(one_dam, one_seg, df_sf_wgs84)

###

one_dam <- dams_joined[which(dams_joined$DAMMID == "{A27B6E35-860B-4B67-B4F5-79981E125B2E}"),]

###

one_seg <- sample(df_sf_wgs84$RSTID, 1)

#This segment has no dam.
#The above segment has no dam.
#The segment above that segment has a dam.
one_seg <- "63935341553915" 


# up_seg <- function(seg){
#   in_df <- df_sf_wgs84
#   up_seg <- in_df$RSTID[which(in_df$RSTID_NED == seg)]
#   len <- st_length(in_df %>% filter(RSTID==up_seg))
#   if (up_seg %in% dams_joined$RSTID){
#     print("dam in upstream segment!")
#     prop_out <- 1-point_on_seg(up_seg) # relative position of the dam on the segment, from upstream node
#   } else {
#     print("no dam in upstream segment!")
#     prop_out <- 1
#   }
#   out <- data.frame(RSTID = up_seg, prop = prop_out, line_length=len, actual_length = prop_out * len)
#   return(out)
# }

up_seg("64031991538464")

# 
# up_seg_all <- function(in_seg){
#   in_df <- df_sf_wgs84
#   
#   next_up <- up_seg(in_seg)
#   list_up <- next_up
#   
#   #is there a dam in next up?
#   if(next_up$RSTID %in% dams_joined$RSTID){
#     break
#   } else {
#     new_up <- up_seg(next_up$RSTID)
#   }
#   return(list_up)
# }

up_seg_all(one_seg)


up_seg <- function(seg){
  in_df <- df_sf_wgs84
  up_seg <- in_df$RSTID[which(in_df$RSTID_NED == seg)]
  return(up_seg)
}


up_seg(one_seg)

dam_on_seg <- function(in_seg){
  if(in_seg %in% dams_joined$RSTID){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

dam_on_seg("64017001539989")


leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  addPolylines(data = df_sf_wgs84 %>% dplyr::filter(RSTID == one_seg)) %>%
  #addPolylines(data = df_sf_wgs84 %>% dplyr::filter(RSTID == up_seg(df_sf_wgs84, one_seg)$RSTID), color="red") %>%
  addMarkers(data = one_dam)








