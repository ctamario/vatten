


library(pacman)

p_load(sf, dplyr, stringr, leaflet, ggplot2)


getwd()

### Handling the GIS data
df_sf <- read_sf("data/vd_l_2016_3.shp")
df_sf <- read_sf("C:/Users/caio0001/Documents/test/vd_l_2016_3_RivEX.shp")

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

dams_sf <- read_sf("data/DAMM_PROD_2013_3.shp")

dams_sf_wgs84 <- st_transform(dams_sf, crs = st_crs(4326))

smhi_dams <- st_join(dams_sf_wgs84, df_sf_wgs84, join = st_nearest_feature)

(nearest = st_nearest_feature(dams_sf_wgs84,df_sf_wgs84))
(dist = st_distance(dams_sf_wgs84, df_sf_wgs84[nearest,], by_element=TRUE))


smhi_dams$dist <- dist

smhi_dams_only_near <- smhi_dams[which(as.numeric(smhi_dams$dist) < 100),]



#dams_related <- smhi_dams[which(smhi_dams$RSTID %in% up_seg_all2(df, one_segment)),]

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
    new_up <- new_up[!new_up %in% smhi_dams$RSTID]
    
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
    #if(next_down %in% smhi_dams$RSTID || df$LINJEKOD[which(df$RSTID == next_down)] == 26){
    if(next_down %in% smhi_dams$RSTID){  
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
  addCircleMarkers(data = smhi_dams %>% filter(RSTID %in% up_seg_all3(df, furthest_down))) %>%
  #addCircleMarkers(data = smhi_dams, popup=~paste(RSTID, sep="<br>")) %>%
  addPolylines(data=up_all_segment_sf3, color="red", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=down_segment_sf2, color="green", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=test, color="pink", popup=~paste(RSTID, RW_PopNamn, sep="<br>")) %>%
  addPolylines(data=one_segment_sf, popup=~RW_PopNamn, color="purple")

map

map2 <- leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  #addCircleMarkers(data=dams_sf_wgs84, popup=~paste(RSTID, sep="<br>")) %>% #, clusterOptions = markerClusterOptions(minZoom = 10, maxZoom = 15)) %>%
  addCircleMarkers(data = smhi_dams %>% filter(RSTID %in% up_seg_all3(df, furthest_down))) %>%
  #addCircleMarkers(data = smhi_dams, popup=~paste(RSTID, sep="<br>")) %>%
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
  addCircleMarkers(data = smhi_dams %>% filter(RSTID %in% test$RSTID), radius = 2, weight=2, fillOpacity = 1, fillColor="white") %>% 
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

#down_segment_sf2 <-  df_sf_wgs84[which(df$RSTID %in% down_seg_until_dam(df, one_segment_nordhavet)),]

#furthest_down <- down_seg_until_dam(df, one_segment_nordhavet)$furthest_down

up_all_segment_sf3 <- df_sf_wgs84[which(df$RSTID %in% up_seg_all4(df, one_segment_nordhavet, smhi_dams)),] 

test <- do_it_all(df, one_segment_nordhavet)


leaflet() %>% addProviderTiles("Esri.WorldImagery") %>% 
  #addCircleMarkers(data=df_lake_trout_joined2 %>% filter(RSTID == df_lake_trout_joined$RSTID[ID]), fillColor="pink", color="red", fillOpacity = 1,  radius = 10) %>%
  addPolylines(data=up_all_segment_sf3 %>% filter(LINJEKOD < 26 & SWE > 0), color="white", popup=~paste(RSTID, RW_PopNamn, sep="<br>"), weight=2) %>%
  addCircleMarkers(data = smhi_dams %>% filter(RSTID %in% up_all_segment_sf3$RSTID) %>%
                     filter(RSTID != 67574770355019) %>% filter(RSTID != 67306520369160), popup=~paste(RSTID), radius = 2, weight=2, fillOpacity = 1, fillColor="white")# %>% 
  #addCircleMarkers(data=df_lake_trout_joined2, fillColor="pink", color="red", fillOpacity = 1,  radius = 3)


segments_with_dams <- up_all_segment_sf3$RSTID[up_all_segment_sf3$RSTID %in% smhi_dams$RSTID]

my_dams <- smhi_dams[smhi_dams$RSTID %in% segments_with_dams,]

table(my_dams$FISKVAG)
############
############
############

# Starting to 

### Do it with two different dam layers!

vhinder <- read.csv("C:/Users/caio0001/Documents/gis/rawdata/Vandringshinder_csv.csv", sep=";", dec=",", fileEncoding = "Latin1")

vhinder <- vhinder[vhinder$Northing > 4000000,] # wrong location!
vhinder <- vhinder[vhinder$VandringshinderID != 85406,] # wrong location!


## Just some plotting of barrier types
table(vhinder$Vandringshindertyp)

vhinder %>% ggplot(aes(x=Vandringshindertyp)) + geom_bar()

vhinder_aggregated <- vhinder %>% 
  group_by(Vandringshindertyp) %>%
  summarise(count = n())

ggplot(vhinder_aggregated, aes(x = reorder(Vandringshindertyp, -count), y = count)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = count), vjust = -0.5, size = 3)+
  theme_classic()+
  labs(x="Vandringshindertyp")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

## Creating a sf layer of the whole thing

coordinates <- vhinder[, c("Easting", "Northing")]
coordinates_df <- data.frame(coordinates)
spatial_object <- st_as_sf(coordinates_df, coords = c("Easting", "Northing"), crs = 3006)
spatial_object2 <- st_transform(spatial_object, crs = st_crs(4326))
vhinder_sf <- cbind(spatial_object2, vhinder)

vhinder_sf_joined <- st_join(vhinder_sf, df_sf_wgs84, join = st_nearest_feature)

(nearest = st_nearest_feature(vhinder_sf,df_sf_wgs84))
(dist = st_distance(vhinder_sf, df_sf_wgs84[nearest,], by_element=TRUE))

vhinder_sf_joined$dist <- dist

vhinder_sf_joined_only_near <- vhinder_sf_joined[which(as.numeric(vhinder_sf_joined$dist) < 100),]

#vhinder_sf_joined_dams <- vhinder_sf_joined %>% filter(Vandringshindertyp == "damm")  #kanske remove


####
####
#### Master dam layer???

smhi_dams_not_in_vhinder <- st_join(smhi_dams_only_near, vhinder_sf_joined_only_near, left=T, join = st_is_within_distance, dist = 200)
smhi_dams_not_in_vhinder2 <- smhi_dams_not_in_vhinder %>% filter(is.na(VandringshinderID)) %>% dplyr::select(DAMMID, DNAMN, HARO, RSTID=RSTID.x, STATUS, FISKVAG, VHINDER)

smhi_dams_not_in_vhinder2 <- smhi_dams_not_in_vhinder2 %>% filter(STATUS != 2)

#write_sf(obj=smhi_dams_not_in_vhinder2, dsn="C:/temp/dams_not_in_vhinder_5.shp")

table(is.na(smhi_dams_not_in_vhinder2$VandringshinderID))
table(is.na(smhi_dams_not_in_vhinder2$DAMMID))

# Merge with vhinder?

dams_all <- bind_rows(vhinder_sf_joined_only_near, smhi_dams_not_in_vhinder2) 

dams_all$SOURCE <- "SMHI"
dams_all$SOURCE[which(is.na(dams_all$DAMMID))] <- "VHDB"

dams_all$Vandringshindertyp[which(is.na(dams_all$Vandringshindertyp))] <- "damm"

table(dams_all$Vandringshindertyp, dams_all$SOURCE, useNA = "always")


#write_sf(dams_test, "C:/temp/omfg2.shp")

####
####
# Do some filtering?

table(vhinder_sf_joined_all$För.Öring)
table(vhinder_sf_joined_all$Passerbarhet.för.Mört)

table(vhinder_sf_joined_all$För.Öring, vhinder_sf_joined_all$Passerbarhet.för.Mört)


vhinder_sf_joined_filtered <- vhinder_sf_joined_all[-which(vhinder_sf_joined_all$För.Öring == 0),]
vhinder_sf_joined_filtered <- vhinder_sf_joined_filtered %>% filter(VandringshinderID != 90397)

sum(table(vhinder_sf_joined_all$För.Öring))
table(vhinder_sf_joined_filtered$För.Öring)

# Tydligen finns det dammar i vandringshinderdatabasen utan någon metadata för om de utgör vandringshinder eller inte
# men där det finns info i SMHIs dammregister i "VHINDER"-kolumnen. 

DAMS_THAT_ARE_NOT_BARRIERS <- smhi_dams %>% filter(VHINDER == 2) %>% mutate(NOT_BARRIERS = 1) %>% select(NOT_BARRIERS)

dams_all2 <- dams_all %>% st_join(DAMS_THAT_ARE_NOT_BARRIERS, join = st_is_within_distance, dist = 3)

table(dams_all2$NOT_BARRIERS, useNA="always")

dams_all2 <- dams_all2 %>% filter(is.na(NOT_BARRIERS))

dams_all2$CalleID <- 1:nrow(dams_all2)

#### 

dams_w_FPS <- dams_all2

dams_w_FPS <- dams_w_FPS[-which(dams_w_FPS$Fiskväg == "SANT"),]
dams_w_FPS <- dams_w_FPS[-which(dams_w_FPS$FISKVAG == 1),]
dams_w_FPS <- dams_w_FPS[-which(dams_w_FPS$FISKVAG == 2),]
dams_w_FPS <- dams_w_FPS[-which(dams_w_FPS$FISKVAG == 4),]
dams_w_FPS <- dams_w_FPS[-which(dams_w_FPS$FISKVAG == 5),]
dams_w_FPS <- dams_w_FPS[-which(dams_w_FPS$FISKVAG == 6),]
dams_w_FPS <- dams_w_FPS[-which(dams_w_FPS$FISKVAG == 8),]

#test_fw <- dams_all2[which(dams_all2$FISKVAG == 8),]

### ÅIV ÅIV ÅIV
# Load in fishways: 

AIV <- read_sf(dsn="data/AtgarderIVatten.shp")

AIV_wgs84 <- st_transform(AIV, crs = st_crs(4326))

table(AIV$HuvAtgtyp)

AIV_clean <- AIV_wgs84 %>% filter(HuvAtgtyp == "Fiskvägar")  %>% filter(Nkoord > 6108110)




dams_w_FPS_plus_AIV <- st_join(dams_w_FPS, AIV_clean, 
                               join = st_is_within_distance, dist = 100)

dams_w_FPS_plus_AIV <- dams_w_FPS_plus_AIV[which(is.na(dams_w_FPS_plus_AIV$AtgID)),]


# remove_points <- function(solution_point_layer, points_to_Remove){
#   
#   test1 <- st_join(dams_all2, test_fw %>% dplyr::select(DAMMID, CalleID), 
#                    join = st_is_within_distance, dist = 100)
#   
# }



####

connectivity_vhinder <- df_sf_wgs84[which(df$RSTID %in% up_seg_all4(df, one_segment_nordhavet, dams_all2)),] 

connectivity_vhinder2 <- df_sf_wgs84[which(df$RSTID %in% up_seg_all4(df, one_segment_nordhavet, dams_w_FPS)),] 

connectivity_vhinder3 <- df_sf_wgs84[which(df$RSTID %in% up_seg_all4(df, one_segment_nordhavet, dams_w_FPS_plus_AIV)),] 

rstids <- data.frame(RSTID=c(connectivity_vhinder$RSTID, connectivity_vhinder3$RSTID)) %>% group_by(RSTID) %>% summarise(count = n())
old_rstids <- rstids %>% filter(count == 2)
new_rstids <- rstids %>% filter(count == 1)

leaflet() %>% addProviderTiles("Esri.WorldImagery") %>% 
  addPolylines(data=connectivity_vhinder3 %>% filter(RSTID %in% new_rstids$RSTID & LINJEKOD < 26 & SWE > 0), color="red", popup=~paste(RSTID, RW_PopNamn, sep="<br>"), weight=~log(Strahler+2), opacity=1) %>%
  addPolylines(data=connectivity_vhinder %>% filter(RSTID %in% old_rstids$RSTID & LINJEKOD < 26 & SWE > 0), color="white", popup=~paste(RSTID, RW_PopNamn, sep="<br>"), weight=~log(Strahler+2), opacity=1)# %>%
  #addPolygons(data=haro_wgs84_big, popup=~paste(HARO))
  ##addCircleMarkers(data = dams_all2 %>% filter(RSTID %in% connectivity_vhinder2$RSTID) %>%
  ##                   filter(RSTID != 67574770355019) %>% filter(RSTID != 67306520369160), popup=~paste(RSTID, 
  ##                                                                                                     VandringshinderID, 
  ##                                                                                                     Vandringshindertyp, sep="<br>"), radius = 2, weight=2, fillOpacity = 1, fillColor="white")

df_sf_wgs84$length <- st_length(df_sf_wgs84)
connectivity_vhinder$length <- st_length(connectivity_vhinder)
connectivity_vhinder3$length <- st_length(connectivity_vhinder3)

all_rivers <- df_sf_wgs84 %>% filter(LINJEKOD < 26 & SWE > 0) #%>% filter(LINJEKOD != 21) %>% filter(LINJEKOD != 22)

all_rivers$HARO <- floor(as.numeric(str_extract(all_rivers$RW_PopNamn, "\\d{4,6}"))/1000)
connectivity_vhinder$HARO <- floor(as.numeric(str_extract(connectivity_vhinder$RW_PopNamn, "\\d{4,6}"))/1000)
connectivity_vhinder3$HARO <- floor(as.numeric(str_extract(connectivity_vhinder3$RW_PopNamn, "\\d{4,6}"))/1000)

connectivity_vhinder_2 <- connectivity_vhinder %>% filter(LINJEKOD < 26 & SWE > 0) #%>% filter(LINJEKOD != 21) %>% filter(LINJEKOD != 22)
connectivity_vhinder3_2 <- connectivity_vhinder3 %>% filter(LINJEKOD < 26 & SWE > 0) #%>% filter(LINJEKOD != 21) %>% filter(LINJEKOD != 22)

river_per_haro <- as.data.frame(all_rivers) %>% group_by(HARO) %>% summarise(n = n(), sum_length = sum(length))
river_per_haro_available <- as.data.frame(connectivity_vhinder_2) %>% group_by(HARO) %>% summarise(n_avail = n(), avail_length = sum(length))
river_per_haro_available_FW <- as.data.frame(connectivity_vhinder3_2) %>% group_by(HARO) %>% summarise(n_avail_fw = n(), avail_length_fw = sum(length))

river_per_haro_both <- river_per_haro %>% left_join(river_per_haro_available, by="HARO")
river_per_haro_all_three <- river_per_haro_both %>% left_join(river_per_haro_available_FW, by="HARO")

river_per_haro_all_three$rel_length_fw <- river_per_haro_all_three$avail_length_fw / river_per_haro_all_three$sum_length

river_per_haro_all_three_large <- river_per_haro_all_three

#river_per_haro_all_three_large$HARO2 <- floor(as.numeric(river_per_haro_all_three_large$HARO)/1000)

#river_per_haro_all_three_large <- river_per_haro_all_three %>% filter(as.numeric(HARO) %% 10 == 0)

#river_per_haro_all_three_large %>% ggplot(aes(x=n, y=as.numeric(sum_length))) + geom_point()

summary(as.numeric(river_per_haro_all_three_large$rel_length_fw))

river_per_haro_all_three_large %>% ggplot(aes(x=as.numeric(sum_length), y=as.numeric(rel_length_fw))) + 
  geom_point() + 
  labs(x="Size of river catchment", y="Proportion that is accessible from sea")+
  #scale_x_continuous(trans = "log")+
  geom_smooth(
    method = 'glm', 
    formula = y ~ x, 
    method.args = list(family = gaussian(link = 'log'))
  )+
  theme_classic() +
  geom_text(label=river_per_haro_all_three_large$HARO, nudge_x=0.1, nudge_y=0.01, check_overlap=T)

#river_per_haro_both$rel_length <- river_per_haro_both$avail_length / river_per_haro_both$sum_length

sum(connectivity_vhinder_2$length) / sum(all_rivers$length)
sum(connectivity_vhinder3_2$length) / sum(all_rivers$length)

river_per_haro_both %>% filter(n>200) %>% filter(!is.na(HARO)) %>% ggplot(aes(x=as.numeric(rel_length))) + geom_histogram()

river_per_haro_all_three$rel_length_fw <- river_per_haro_all_three$avail_length_fw / river_per_haro_all_three$sum_length

#How much of all rivers is the available 28000 (Vindelälven)
test <- river_per_haro_all_three %>% filter(HARO == 28000 | HARO == 13000 | HARO == 4000 | HARO == 1000)
sum(test$avail_length_fw) / sum(river_per_haro_all_three$sum_length)

#
sum(river_per_haro_all_three$avail_length) / sum(river_per_haro_all_three$sum_length)
sum(river_per_haro_all_three$avail_length_fw) / sum(river_per_haro_all_three$sum_length)
sum(river_per_haro_all_three$avail_length_fw) / sum(river_per_haro_all_three$avail_length)

no_national <- river_per_haro_all_three %>% filter(HARO != 28000) %>%
  filter(HARO != 13000) %>%
  filter(HARO != 4000) %>%
  filter(HARO != 1000)


sum(no_national$avail_length_fw) / sum(river_per_haro_all_three$sum_length)
sum(river_per_haro_all_three$avail_length_fw) / sum(river_per_haro_all_three$sum_length)


leaflet() %>% addProviderTiles("Esri.WorldImagery") %>% 
  addPolylines(data=connectivity_vhinder3_2 %>% filter(LINJEKOD < 26 & SWE > 0) %>% filter(HARO != 28000 & HARO != 13000 & HARO != 4000 & HARO != 1000), color="red", popup=~paste(RSTID, RW_PopNamn, sep="<br>"), weight=2)

segments_with_dams <- connectivity_vhinder$RSTID[connectivity_vhinder$RSTID %in% dams_all2$RSTID]

###

bla <- AIV %>% dplyr::filter(HuvAtgtyp == "Fiskvägar")

bla$Atgtyper

####

# I wanna extract just the most downstream dam in each segment... 
# This means I have to find a function that returns the position of
# the dam on each segment... I have that function, let me fetch it...

point_on_seg <- function(segment, in_dams){
  
  test_pt_dams <- in_dams[which(in_dams$RSTID == segment),]
  test_ln <- df_sf_wgs84[which(df_sf_wgs84$RSTID == segment),]
  
  out_id <- test_pt_dams$VandringshinderID
  
  dam_pos <- geos::geos_project_normalized(geos::as_geos_geometry(test_ln), geos::as_geos_geometry(test_pt_dams))
  
  return(data.frame(segment, dam_pos, VandringshinderID=out_id))
}



point_on_seg(segment = "72903430806073", 
             in_dams = vhinder_sf_joined_dams)



point_on_seg(segment = "73905630850526", 
             in_dams = vhinder_sf_joined_dams)


####

first_encountered_dams <- smhi_dams %>% filter(RSTID %in% up_all_segment_sf3$RSTID)

####

for(i in 1:10){
  first_encountered_dams
}


# Clean dam layer:

dams_clean <- smhi_dams %>% 
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
#  addCircleMarkers(data = smhi_dams %>% filter(RSTID %in% up_all_segment_sf3$RSTID) %>%
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



