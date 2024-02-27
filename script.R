


library(pacman)

p_load(sf, dplyr, stringr, leaflet)


getwd()

### Handling the GIS data
df_sf <- read_sf("data/vd_l_2016_3.shp")

# reproject to WGS84 to make everything compatible?

df_sf_wgs84 <- st_transform(df_sf, crs = st_crs(4326))

df_sf_wgs84$SWE <- as.numeric(str_detect(df_sf_wgs84$DISTRICT, "^SE[0-9]$"))+as.numeric(str_detect(df_sf_wgs84$DISTRICT, "SE1TO"))

#table(str_detect(df_sf_wgs84$DISTRICT, "SE"))
#table(str_detect(df_sf_wgs84$DISTRICT, "^SE[0-9]$"))

df <- as.data.frame(df_sf)

## Just grabbing any segment and plotting where a fish can go...?

one_segment <- sample(df$RSTID, 1)

#one_segment <- "67233501418226"


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

#dams_related <- joined_data[which(joined_data$RSTID %in% up_seg_all2(df, one_segment)),]

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
  return(c(seg, list_up))
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
  while(i < 1000){
    next_down <- down_seg(in_df, next_down)
    list_down <- c(next_down, list_down)
    #if(next_down %in% joined_data$RSTID || df$LINJEKOD[which(df$RSTID == next_down)] == 26){
    if(next_down %in% joined_data$RSTID){  
      break
    }
    i <- i + 1
  }
  return(list(list_down=list_down, furthest_down=list_down[1]))
}


one_segment <- sample(df$RSTID, 1)

down_segment_sf2 <-  df_sf_wgs84[which(df$RSTID %in% down_seg_until_dam(df, one_segment)),]

furthest_down <- down_seg_until_dam(df, one_segment)$furthest_down

up_all_segment_sf3 <- df_sf_wgs84[which(df$RSTID %in% up_seg_all3(df, furthest_down)),] 

# leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
#   addCircleMarkers(data=dams_sf_wgs84) %>%
#   addPolylines(data=one_segment_sf, popup=~RW_PopNamn) %>%
#   addPolylines(data=up_all_segment_sf3, color="red", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
#   addPolylines(data=down_segment_sf2, color="green", popup=~paste(RSTID, RW_PopNamn, sep="<br>"))
# 

do_it_all <- function(in_df, seg){
  
  furthest_down <- down_seg_until_dam(df, one_segment)$furthest_down
  
  #up_all_segment_sf3 <- up_seg_all3(df, furthest_down)
  
  sel1 <- up_seg_all3(df, seg)
  sel2 <- up_seg_all3(df, furthest_down)
  sel3 <- unique(c(sel1, sel2))
  
  out <- df_sf_wgs84[which(df_sf_wgs84$RSTID %in% sel3),]
  
  return(out)
}

one_segment_sf <- df_sf_wgs84[which(df$RSTID == one_segment),]

test <- do_it_all(df, one_segment)

map <- leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  #addCircleMarkers(data=dams_sf_wgs84, popup=~paste(RSTID, sep="<br>")) %>% #, clusterOptions = markerClusterOptions(minZoom = 10, maxZoom = 15)) %>%
  addCircleMarkers(data = joined_data %>% filter(RSTID %in% up_seg_all3(df, furthest_down))) %>%
  #addCircleMarkers(data = joined_data, popup=~paste(RSTID, sep="<br>")) %>%
  addPolylines(data=up_all_segment_sf3, color="red", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=down_segment_sf2, color="green", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=test, color="pink", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=one_segment_sf, popup=~RW_PopNamn, color="purple")

map

map2 <- leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  #addCircleMarkers(data=dams_sf_wgs84, popup=~paste(RSTID, sep="<br>")) %>% #, clusterOptions = markerClusterOptions(minZoom = 10, maxZoom = 15)) %>%
  addCircleMarkers(data = joined_data %>% filter(RSTID %in% up_seg_all3(df, furthest_down))) %>%
  #addCircleMarkers(data = joined_data, popup=~paste(RSTID, sep="<br>")) %>%
  #addPolylines(data=up_all_segment_sf3, color="red", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  #addPolylines(data=down_segment_sf2, color="green", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=test, color="pink", popup=~paste(RSTID, RW_PopNamn, sep="<br>"))# %>%
  #addPolylines(data=one_segment_sf, popup=~RW_PopNamn)

map2


#### Ladda in kanadaröding-data

lake_trout <- read.csv(file="data/Kanadaröding 2021-02-21 till GIS.csv", sep=";")

coordinates <- lake_trout[, c("X", "Y")]
coordinates_df <- data.frame(coordinates)
spatial_object <- st_as_sf(coordinates_df, coords = c("X", "Y"), crs = 3847)
spatial_object2 <- st_transform(spatial_object, crs = st_crs(4326))
spatial_object2 <- cbind(spatial_object2, lake_trout)

df_lake_trout <- spatial_object2

map2 %>% addCircleMarkers(data=spatial_object2, fillColor="pink", color=0, fillOpacity = 1,  radius = 3)

df_lake_trout_joined <- st_join(df_lake_trout, df_sf_wgs84, join = st_nearest_feature, dist = 1500)


leaflet() %>% addProviderTiles("Esri.WorldImagery") %>% 
  addCircleMarkers(data=df_lake_trout_joined, fillColor="pink", color=0, fillOpacity = 1,  radius = 3) %>%
  addCircleMarkers(data=df_lake_trout_joined %>% filter(RSTID == df_lake_trout_joined$RSTID[1]), fillColor="pink", color="red", fillOpacity = 1,  radius = 10)


df_lake_trout_joined2 <- df_lake_trout_joined[which(df_lake_trout_joined$LINJEKOD < 26),]



ID <- 1

one_segment <- df_lake_trout_joined2$RSTID[ID]

down_segment_sf2 <-  df_sf_wgs84[which(df$RSTID %in% down_seg_until_dam(df, one_segment)),]

furthest_down <- down_seg_until_dam(df, one_segment)$furthest_down

up_all_segment_sf3 <- df_sf_wgs84[which(df$RSTID %in% up_seg_all3(df, furthest_down)),] 

test <- do_it_all(df, one_segment)

for(ID in 71:164){
  one_segment <- df_lake_trout_joined2$RSTID[ID]
  
  down_segment_sf2 <-  df_sf_wgs84[which(df$RSTID %in% down_seg_until_dam(df, one_segment)),]
  
  furthest_down <- down_seg_until_dam(df, one_segment)$furthest_down
  
  up_all_segment_sf3 <- df_sf_wgs84[which(df$RSTID %in% up_seg_all3(df, furthest_down)),] 
  
  test <- rbind(test, do_it_all(df, one_segment))
}

leaflet() %>% addProviderTiles("Esri.WorldImagery") %>% 
  #addCircleMarkers(data=df_lake_trout_joined2 %>% filter(RSTID == df_lake_trout_joined$RSTID[ID]), fillColor="pink", color="red", fillOpacity = 1,  radius = 10) %>%
  addPolylines(data=keep %>% filter(LINJEKOD != 26), color="pink", popup=~paste(RSTID, RW_PopNamn, sep="<br>"), weight=2) %>%
  addCircleMarkers(data = joined_data %>% filter(RSTID %in% test$RSTID), radius = 2, weight=2, fillOpacity = 1, fillColor="white") %>% 
  addCircleMarkers(data=df_lake_trout_joined2, fillColor="pink", color="red", fillOpacity = 1,  radius = 3)

keep <- test[!duplicated(test), ]

write_sf(obj=keep, dsn="data/kanadaroding.shp")


####
#### Testa hela Sveriges anadroma vattendrag???

nordhavet <- read_sf("data/nordhavet_mynning.shp")
nordhavet

up_seg_all4 <- function(in_df, seg, damLayer){
  next_up <- up_seg(in_df, seg)
  list_up <- next_up
  while(length(next_up) > 0) {
    new_up <- up_seg(in_df, next_up[1])
    new_up <- new_up[!new_up %in% list_up]  # Remove segments already in list_up
    #new_up <- sapply(new_up, function(x) !any(dams_related$RSTID == x))
    list_up <- c(list_up, new_up)
    new_up <- new_up[!new_up %in% damLayer$RSTID]
    
    next_up <- c(next_up, new_up)
    next_up <- next_up[-1]
  }
  return(c(seg, list_up))
} # Det funkar!

nordhavet2 <- st_join(nordhavet, df_sf_wgs84, join = st_nearest_feature, dist = 100)

one_segment_nordhavet <- nordhavet2$RSTID[1]

down_segment_sf2 <-  df_sf_wgs84[which(df$RSTID %in% down_seg_until_dam(df, one_segment_nordhavet)),]

furthest_down <- down_seg_until_dam(df, one_segment_nordhavet)$furthest_down

up_all_segment_sf3 <- df_sf_wgs84[which(df$RSTID %in% up_seg_all4(df, one_segment_nordhavet, joined_data)),] 

test <- do_it_all(df, one_segment_nordhavet)


leaflet() %>% addProviderTiles("Esri.WorldImagery") %>% 
  #addCircleMarkers(data=df_lake_trout_joined2 %>% filter(RSTID == df_lake_trout_joined$RSTID[ID]), fillColor="pink", color="red", fillOpacity = 1,  radius = 10) %>%
  addPolylines(data=up_all_segment_sf3 %>% filter(LINJEKOD < 26 & SWE > 0), color="white", popup=~paste(RSTID, RW_PopNamn, sep="<br>"), weight=2) %>%
  addCircleMarkers(data = joined_data %>% filter(RSTID %in% up_all_segment_sf3$RSTID) %>%
                     filter(RSTID != 67574770355019) %>% filter(RSTID != 67306520369160), popup=~paste(RSTID), radius = 2, weight=2, fillOpacity = 1, fillColor="white")# %>% 
  #addCircleMarkers(data=df_lake_trout_joined2, fillColor="pink", color="red", fillOpacity = 1,  radius = 3)


### Do it with two different dam layers!


# Clean dam layer:

dams_clean <- joined_data %>% 
  filter(STATUS != 2) %>% 
  filter(VHINDER != 2)

table(dams_clean$FISKVAG)

# Load in fishways: 

AIV <- read_sf(dsn="data/AtgarderIVatten.shp")

AIV_wgs84 <- st_transform(AIV, crs = st_crs(4326))

table(AIV$HuvAtgtyp)

AIV_clean <- AIV_wgs84 %>% filter(HuvAtgtyp == "Fiskvägar")  %>% filter(Nkoord > 6108110)

# Connect AIV to dam layer

dams_w_AIV <- st_join(dams_clean, AIV_clean, join = st_nearest_feature, dist = 10)

#dams_w_AIV <- st_nearest_feature(dams_clean, AIV_clean)

#dist = st_distance(dams_clean, AIV_clean[st_nearest_feature(dams_clean, AIV_clean),], by_element=TRUE)

#dams_w_AIV$dist <- dist

#dams_w_AIV %>% filter(dist < 100)

p_load(nngeo)

hm <- st_nn(dams_clean, AIV_clean, k = 1, returnDist = T)
hm2 <- cbind(dams_w_AIV, unlist(hm$dist))

hm3 <- hm2 %>% filter(unlist.hm.dist. < 100)

hm3

dams_w_FPS <- dams_clean %>% filter(!DAMMID %in% hm3$DAMMID)

####################


# Without fishways
conn_wo_FPS <- df_sf_wgs84[which(df$RSTID %in% up_seg_all4(df, one_segment_nordhavet, dams_clean)),] 

# With fishways
conn_w_FPS <- df_sf_wgs84[which(df$RSTID %in% up_seg_all4(df, one_segment_nordhavet, dams_w_FPS)),] 


leaflet() %>% addProviderTiles("Esri.WorldImagery") %>% 
  #addCircleMarkers(data=df_lake_trout_joined2 %>% filter(RSTID == df_lake_trout_joined$RSTID[ID]), fillColor="pink", color="red", fillOpacity = 1,  radius = 10) %>%
  addPolylines(data=conn_w_FPS %>% filter(LINJEKOD < 26 & SWE > 0), color="white", popup=~paste(RSTID, RW_PopNamn, sep="<br>"), weight=2) %>%
  addPolylines(data=conn_wo_FPS %>% filter(LINJEKOD < 26 & SWE > 0), color="red", popup=~paste(RSTID, RW_PopNamn, sep="<br>"), weight=2)
#  addCircleMarkers(data = joined_data %>% filter(RSTID %in% up_all_segment_sf3$RSTID) %>%
#                     filter(RSTID != 67574770355019) %>% filter(RSTID != 67306520369160), popup=~paste(RSTID), radius = 2, weight=2, fillOpacity = 1, fillColor="white")# %>% 
#addCircleMarkers(data=df_lake_trout_joined2, fillColor="pink", color="red", fillOpacity = 1,  radius = 3)


df_sf_wgs84$line.length <- st_length(df_sf_wgs84)
sum(df_sf_wgs84$line.length)

conn_wo_FPS_clean <- conn_wo_FPS %>% filter(LINJEKOD < 26 & SWE > 0)
conn_w_FPS_clean <- conn_w_FPS %>% filter(LINJEKOD < 26 & SWE > 0)

conn_wo_FPS_clean$line.length <- st_length(conn_wo_FPS_clean)
sum(conn_wo_FPS_clean$line.length)

conn_w_FPS_clean$line.length <- st_length(conn_w_FPS_clean)
sum(conn_w_FPS_clean$line.length)

sum(conn_wo_FPS_clean$line.length)/sum(df_sf_wgs84$line.length)
sum(conn_w_FPS_clean$line.length)/sum(df_sf_wgs84$line.length)
#Från 10.8% av Sveriges vattendrag till 11.7%,

sum(conn_w_FPS_clean$line.length)/sum(conn_wo_FPS_clean$line.length)
#En ökning av tillgängligt habitat på 8%



