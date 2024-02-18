


library(pacman)

p_load(sf, dplyr, stringr, leaflet)


getwd()

### Handling the GIS data
df_sf <- read_sf("data/vd_l_2016_3.shp")

# reproject to WGS84 to make everything compatible?

df_sf_wgs84 <- st_transform(df_sf, crs = st_crs(4326))


df <- as.data.frame(df_sf)



## Just grabbing any segment and plotting where a fish can go...?

one_segment <- sample(df$RSTID, 1)

#one_segment <- "71552411724072"


up_seg <- function(in_df, seg){
  up_seg <- in_df$RSTID[which(in_df$RSTID_NED == seg)]
  #print(up_seg)
  return(up_seg)
}

down_seg <- function(in_df, seg){
  down_seg <- in_df$RSTID_NED[which(in_df$RSTID == seg)]
  return(down_seg)
}


up_seg(df, one_segment)
down_seg(df, one_segment)

one_segment_sf <- df_sf_wgs84[which(df$RSTID == one_segment),]

up_segment_sf <- df_sf_wgs84[which(df$RSTID %in% up_seg(df, one_segment)),]

down_segment_sf <- df_sf_wgs84[which(df$RSTID %in% down_seg(df, one_segment)),]


up_seg_all <- function(in_df, seg){
  next_up <- up_seg(in_df, seg)
  list_up <- next_up
  for(i in 1:length(next_up)){
    list_up <- c(list_up, up_seg(in_df, next_up[i]))
  }
  return(list_up)
}

up_seg_all2 <- function(in_df, seg){
  next_up <- up_seg(in_df, seg)
  list_up <- next_up
  while(length(next_up) > 0) {
    new_up <- up_seg(in_df, next_up[1])
    new_up <- new_up[!new_up %in% list_up]  # Remove segments already in list_up
    list_up <- c(list_up, new_up)
    next_up <- c(next_up, new_up)
    next_up <- next_up[-1]
  }
  return(list_up)
}


#up_seg_all2(df, one_segment)

up_all_segment_sf <- df_sf_wgs84[which(df$RSTID %in% up_seg_all2(df, one_segment)),] 

## leaflet map

leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  addPolylines(data=one_segment_sf, popup=~RW_PopNamn) %>%
  addPolylines(data=up_all_segment_sf, color="red", popup=~RW_PopNamn) %>%
  addPolylines(data=down_segment_sf, color="green", popup=~RW_PopNamn)

#### All above is working but now I need to make sure that it stop searching if it finds a dam...

## Först ladda in dammarna

#dams_sf <- read_sf("data/DAMM_PROD_2013_3.shp")

#dams_sf_wgs84 <- st_transform(dams_sf, crs = st_crs(4326))

#joined_data <- st_join(dams_sf_wgs84, df_sf_wgs84, join = st_nearest_feature, dist = 100)

dams_related <- joined_data[which(joined_data$RSTID %in% up_seg_all2(df, one_segment)),]

leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  addPolylines(data=one_segment_sf, popup=~RW_PopNamn) %>%
  addPolylines(data=up_all_segment_sf, color="red", popup=~RW_PopNamn) %>%
  addPolylines(data=down_segment_sf, color="green", popup=~RW_PopNamn) %>%
  addCircleMarkers(data=dams_related, color="white")

### Okej! Med hänsyn på dammar nu då...

up_seg_all3 <- function(in_df, seg){
  next_up <- up_seg(in_df, seg)
  list_up <- next_up
  while(length(next_up) > 0) {
    new_up <- up_seg(in_df, next_up[1])
    new_up <- new_up[!new_up %in% list_up]  # Remove segments already in list_up
    #new_up <- sapply(new_up, function(x) !any(dams_related$RSTID == x))
    list_up <- c(list_up, new_up)
    new_up <- new_up[!new_up %in% joined_data$RSTID]
    
    next_up <- c(next_up, new_up)
    next_up <- next_up[-1]
  }
  return(list_up)
} # Det funkar!

up_all_segment_sf2 <- df_sf_wgs84[which(df$RSTID %in% up_seg_all3(df, one_segment)),] 

leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  addPolylines(data=one_segment_sf, popup=~RW_PopNamn) %>%
  addPolylines(data=up_all_segment_sf2, color="red", popup=~RW_PopNamn) %>%
  addPolylines(data=down_segment_sf, color="green", popup=~RW_PopNamn) %>%
  addCircleMarkers(data=dams_related, color="white")

### MEN! Nu måste den bara leta sig nedåt till första dammen, innan den applicerar funktionen up_seg_all3

## down_seg då



down_seg <- function(in_df, seg){
  down_seg <- in_df$RSTID_NED[which(in_df$RSTID == seg)]
  return(down_seg)
}

down_seg_until_dam <- function(in_df, seg){
  
  next_down <- down_seg(in_df, seg)
  list_down <- next_down
  i = 1
  while(i < 10){
    next_down <- down_seg(in_df, next_down)
    list_down <- c(next_down, list_down)
    if(next_down %in% joined_data$RSTID || df$LINJEKOD[which(df$RSTID == next_down)] == 26){
      break
    }
    i <- i + 1
  }
  return(list(list_down=list_down, furthest_down=list_down[1]))
}



down_segment_sf2 <-  df_sf_wgs84[which(df$RSTID %in% down_seg_until_dam(df, one_segment)),]

furthest_down <- down_seg_until_dam(df, one_segment)$furthest_down

up_all_segment_sf3 <- df_sf_wgs84[which(df$RSTID %in% up_seg_all3(df, furthest_down)),] 

leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data=dams_sf_wgs84) %>%
  addPolylines(data=one_segment_sf, popup=~RW_PopNamn) %>%
  addPolylines(data=up_all_segment_sf3, color="red", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=down_segment_sf2, color="green", popup=~paste(RSTID, RW_PopNamn, sep="<br>"))

do_it_all <- function(in_df, seg){
  
  furthest_down <- down_seg_until_dam(df, one_segment)$furthest_down
  
  #up_all_segment_sf3 <- up_seg_all3(df, furthest_down)
  
  out <- df_sf_wgs84[which(df_sf_wgs84$RSTID %in% up_seg_all3(df, furthest_down)),]
  
  return(out)
}

test <- do_it_all(df, one_segment)

map <- leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data=dams_sf_wgs84) %>% #, clusterOptions = markerClusterOptions(minZoom = 10, maxZoom = 15)) %>%
  addPolylines(data=up_all_segment_sf3, color="red", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=down_segment_sf2, color="green", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=test, color="pink", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=one_segment_sf, popup=~RW_PopNamn)

map




