





library(pacman)

p_load(tidyr, dplyr, readxl, ggplot2, gridExtra, stringr, ggpubr)


setwd("C:/Users/caio0001/Documents/ArcGIS/Projects/Sveriges_vatten/output_with_Marc")

#node_altitude_key <- read_excel("nodes_with_altitude.xlsx")
node_altitude_key <- read.csv("Extracted_Z_values_2.csv", sep = ",", dec = ".")

head(node_altitude_key)

table(node_altitude_key$Z_value)

node_altitude_key$Z_value[which(node_altitude_key$Z_value == -9999)] <- NA


river_key <- read.csv("vd_l_riverlines.csv", sep=";", dec=",") 


river_key_Strahler <- read.csv("vd_l_riverlines_new_Strahler_order.csv", sep=";", dec=",") 


river_key2 <- river_key %>% dplyr::select(-c("Strahler_Stream_Order", "Strahler_Segment")) %>%
  left_join(river_key_Strahler %>% dplyr::select(c("RSTID", "Strahler_Stream_Order", "Strahler_Segment")))

river_key <- river_key2

river_key <- river_key[!is.na(river_key$Strahler_Stream_Order),]

head(river_key2)

table(river_key$Strahler_Stream_Order, useNA = "always")

river_key[1,]$Fnode

river_key[1,]$Tnode

river_key$altitude_change <- NA

for(i in 1:nrow(river_key)){
 river_key$altitude_change[i] <- node_altitude_key$Z_value[which(node_altitude_key$Node_ID == river_key[i,]$Fnode)] - node_altitude_key$Z_value[which(node_altitude_key$Node_ID == river_key[i,]$Tnode)]
}

river_key$altitude_change2 <- river_key$altitude_change
river_key$altitude_change2[river_key$altitude_change < 0] <- 0


river_key$slope <- river_key$altitude_change2 / river_key$Shape_Length


river_key %>% ggplot(aes(x=altitude_change, y=slope)) + geom_point() + facet_wrap(~factor(Strahler_Stream_Order))

p1 <- river_key %>% ggplot(aes(x = factor(Strahler_Stream_Order), y = altitude_change2)) + geom_boxplot(coef = Inf) + coord_cartesian(ylim=c(-50,100))
p2 <- river_key %>% ggplot(aes(x = factor(Strahler_Stream_Order), y = slope)) + geom_boxplot(coef = Inf) + coord_cartesian(ylim=c(-0.05,0.05))
p3 <- river_key %>% ggplot(aes(x = factor(Strahler_Stream_Order), y = slope)) + geom_boxplot()

grid.arrange(p1, p2, p3, ncol = 3)

river_key$HARO_string_length <- str_length(str_extract(river_key$RW_PopNamn, "^[0-9]+"))

#is.na(river_key$HARO_string_length)
river_key_clean <- river_key[river_key$HARO_string_length > 1,]
river_key_clean <- river_key_clean[!is.na(river_key_clean$HARO_string_length),]

table(river_key_clean$HARO_string_length)

river_key_clean$HARO_string_length2 <- river_key_clean$HARO_string_length - 3 

table(river_key_clean$HARO_string_length2)

river_key_clean$HARO <- str_sub(river_key_clean$RW_PopNamn, start = 1, end = river_key_clean$HARO_string_length2)

table(river_key_clean$HARO)

####### calculate sum of altitude change in each main catchment

alt_change <- river_key_clean %>% group_by(HARO) %>% summarise(sum_alt_change = sum(altitude_change, na.rm = T))

alt_change

####
#### Leif Kuhlins fil
####

dam_key <- read.csv("dam_key_utf8.csv", sep = ";", dec = ",", fileEncoding = "UTF-8")

dam_key <- dam_key[!is.na(dam_key$Fnode),]

table(is.na(dam_key$Fallh_m))


dam_key$HARO_string_length <- str_length(str_extract(dam_key$RW_PopNamn, "^[0-9]+"))
dam_key$HARO_string_length2 <- dam_key$HARO_string_length - 3 
dam_key$HARO <- str_sub(dam_key$RW_PopNamn, start = 1, end = dam_key$HARO_string_length2)


dammed_height <- dam_key %>% group_by(HARO, Strahler_Stream_Order) %>% summarise(dammed_height_kuhlin = sum(Fallh_m, na.rm = T))

####
#### SMHI dammdata
####

getwd()
#setwd("C:/Users/caio0001/Documents/ArcGIS/Projects/Sveriges_vatten/SMHI_Dams_RivEX")

SMHI_dams <- read.csv("C:/Users/caio0001/Documents/ArcGIS/Projects/Sveriges_vatten/SMHI_Dams_RivEX/SMHI_Dams_RivEX.csv", sep = ";", dec = ",", fileEncoding = "Latin1")
SMHI_dams %>% filter(DAMMHOJD > 0)
SMHI_clean <- SMHI_dams %>% group_by(NORTH, EAST) %>% slice_tail(n = 1)

SMHI_clean$DAMMHOJD[which(SMHI_clean$DAMMHOJD == 0)] <- NA
SMHI_clean <- SMHI_clean[!is.na(SMHI_clean$DAMMHOJD),]


SMHI_clean %>% ggplot(aes(x=Strahler.Stream.Order, group = Strahler.Stream.Order, y = DAMMHOJD)) + 
  geom_boxplot() + theme_classic() +
  scale_y_continuous(trans='log2')

SMHI_dam_summary <- SMHI_clean %>% filter(DAMMHOJD > 0) %>% filter(Strahler.Stream.Order > 0) %>% group_by(Strahler.Stream.Order) %>%
  summarize(height_mean = mean(DAMMHOJD, na.rm=T),
            height_sd = sd(DAMMHOJD, na.rm=T),
            height_10q = quantile(DAMMHOJD, 0.10, na.rm=T),
            height_90q = quantile(DAMMHOJD, 0.90, na.rm=T),
            n = n())

SMHI_dam_summary

### Connect the two databases and remove duplicates. 

SMHI_sf <- st_as_sf(SMHI_clean, coords = c("EAST", "NORTH"), crs = 3006)
dam_key_sf <- st_as_sf(dam_key, coords = c("Long","Lat"), crs = 4326) %>% st_transform(crs = 3006)

ggplot() +
  geom_sf(data = SMHI_sf, color = "red") +
  geom_sf(data = dam_key_sf, color = "blue")

SMHI_joined_by_dams <- st_join(SMHI_sf, dam_key_sf, join = st_is_within_distance, dist = 100)

SMHI_joined_by_dams <- SMHI_joined_by_dams[is.na(SMHI_joined_by_dams$RivID.y),]

SMHI_joined_by_dams$HARO_string_length <- str_length(str_extract(SMHI_joined_by_dams$HARO.x, "^[0-9]+"))
SMHI_joined_by_dams$HARO_string_length2 <- SMHI_joined_by_dams$HARO_string_length - 3
SMHI_joined_by_dams$HARO <- str_sub(SMHI_joined_by_dams$HARO.x, start = 1, end = SMHI_joined_by_dams$HARO_string_length2)

dammed_height_SMHI <- SMHI_joined_by_dams %>% as.data.frame() %>% group_by(HARO, Strahler.Stream.Order) %>% summarise(dammed_height_SMHI = sum(DAMMHOJD, na.rm = T))

names(dammed_height_SMHI)[2] <- "Strahler_Stream_Order"

dammed_height_both <- full_join(dammed_height_SMHI, dammed_height, by = c("HARO", "Strahler_Stream_Order"))

###

final <- alt_change %>% left_join(dammed_height_both, by = "HARO")

final$dammed_height_SMHI[is.na(final$dammed_height_SMHI)] <- 0
final$dammed_height_kuhlin[is.na(final$dammed_height_kuhlin)] <- 0

final$dammed_height_both <- final$dammed_height_SMHI + final$dammed_height_kuhlin

final$prop_dammed <- final$dammed_height_both / final$sum_alt_change

hist(final$prop_dammed, breaks = 100)


###

# alt_change_strahler_gt2 <- river_key_clean %>% dplyr::filter(Strahler_Stream_Order > 2) %>% group_by(HARO) %>% summarise(sum_alt_change = sum(altitude_change, na.rm = T),
#                                                                                                                           sum_river_length = sum(Shape_Length, na.rm = T))
# 
# final2 <- alt_change_strahler_gt2 %>% left_join(dammed_height, by = "HARO")
# 
# final2$dammed_height[is.na(final2$dammed_height)] <- 0
# 
# final2$prop_dammed <- final2$dammed_height / final2$sum_alt_change
# 
# hist(final2$prop_dammed, breaks = 100)
# 
# plot(data = final2, prop_dammed ~ sum_river_length)
# 
# ###
# 
final$sum_alt_change <- NULL

all_alt_change <- river_key_clean %>% group_by(Strahler_Stream_Order, HARO) %>% summarise(sum_alt_change = sum(altitude_change, na.rm = T),
                                                                                                                         sum_river_length = sum(Shape_Length, na.rm = T))

hej <- all_alt_change %>% left_join(final, by = c("Strahler_Stream_Order", "HARO"))

hej$dammed_height_both[is.na(hej$dammed_height_both)] <- 0

hej$dammed_height_kuhlin[which(hej$HARO == 40 & hej$Strahler_Stream_Order == 6)] <- hej$dammed_height_kuhlin[which(hej$HARO == 40 & hej$Strahler_Stream_Order == 6)] - 200
hej$dammed_height_kuhlin[which(hej$HARO == 70 & hej$Strahler_Stream_Order == 5)] <- hej$dammed_height_kuhlin[which(hej$HARO == 70 & hej$Strahler_Stream_Order == 5)] - 3.6
hej$dammed_height_kuhlin[which(hej$HARO == 108 & hej$Strahler_Stream_Order == 5)] <- hej$dammed_height_kuhlin[which(hej$HARO == 108 & hej$Strahler_Stream_Order == 5)] - 191
hej$dammed_height_kuhlin[which(hej$HARO == 28 & hej$Strahler_Stream_Order == 7)] <- 75


hej$dammed_height_both[which(hej$HARO == 40 & hej$Strahler_Stream_Order == 6)] <- hej$dammed_height_both[which(hej$HARO == 40 & hej$Strahler_Stream_Order == 6)] - 200
hej$dammed_height_both[which(hej$HARO == 70 & hej$Strahler_Stream_Order == 5)] <- hej$dammed_height_both[which(hej$HARO == 70 & hej$Strahler_Stream_Order == 5)] - 3.6
hej$dammed_height_both[which(hej$HARO == 108 & hej$Strahler_Stream_Order == 5)] <- hej$dammed_height_both[which(hej$HARO == 108 & hej$Strahler_Stream_Order == 5)] - 191
hej$dammed_height_both[which(hej$HARO == 28 & hej$Strahler_Stream_Order == 7)] <- 75

hej$prop_dammed_kuhlin <- hej$dammed_height_kuhlin / hej$sum_alt_change
hej$prop_dammed_SMHI <- hej$dammed_height_SMHI / hej$sum_alt_change
hej$prop_dammed_both <- hej$dammed_height_both / hej$sum_alt_change

hej$national_rivers <- 0

hej$national_rivers[which(hej$HARO == 28)] <- 1
hej$national_rivers[which(hej$HARO == 13)] <- 1
hej$national_rivers[which(hej$HARO == 4)] <- 1
hej$national_rivers[which(hej$HARO == 1)] <- 1


hej %>% ggplot(aes(x = factor(Strahler_Stream_Order), y = prop_dammed_both)) + 
  geom_boxplot(outlier.shape=NA) + 
  geom_line(aes(group = HARO), alpha = 0.32) +
  geom_jitter(width=0.05, aes(size = dammed_height_both)) +
  geom_abline(intercept = 1, slope = 0, linetype = 2)+
  labs(y = "Proportion of fall height dammed", x = "Strahler stream order")+
  theme_classic()

dev.copy2pdf(file="dammed height per Strahler2.pdf", height=5, width=8)

ggarrange(plot1, common.legend = T)

hej2 <- hej

hej2$prop_dammed_both[is.na(hej2$prop_dammed_both)] <- 0
hej2$prop_dammed_kuhlin[is.na(hej2$prop_dammed_kuhlin)] <- 0


p1 <- hej2 %>% ggplot(aes(x = factor(Strahler_Stream_Order), y = prop_dammed_kuhlin)) +
  geom_boxplot(outlier.shape = NA, color = "white", fill = "darkgrey") +
  geom_line(aes(group = HARO),
            alpha = 0.7,
            position = position_jitter(w = 0.02, h = 0.02), color = "white") +
  geom_jitter(width = 0.05, aes(size = dammed_height_kuhlin), color = "white") +
  geom_abline(intercept = 1,
              slope = 0,
              linetype = 2, color = "white") +
  labs(y = "Proportion of fall height dammed",
       x = "Strahler stream order",
       size = "Dammed height (m)",
       #title = "Kuhlin's database",
       tag = "A.)") +
  theme(legend.position = "top") +
  theme_black()

p2 <- hej2 %>% ggplot(aes(x = factor(Strahler_Stream_Order), y = prop_dammed_both)) +
  geom_boxplot(outlier.shape = NA, color = "white", fill = "darkgrey") +
  geom_line(aes(group = HARO),
            alpha = 0.7,
            position = position_jitter(w = 0.02, h = 0.02), color = "white") +
  geom_jitter(width = 0.05, aes(size = dammed_height_both), color = "white") +
  geom_abline(intercept = 1,
              slope = 0,
              linetype = 2, color = "white") +
  labs(y = "Proportion of fall height dammed",
       x = "Strahler stream order",
       size = "Dammed height (m)",
       title = "Kuhlin and SMHI",
       tag = "B.)") +
  theme(legend.position = "top") +
  theme_black()

ggarrange(p1, common.legend = T)

dev.copy2pdf(file="dammed height per Strahler_2024_11_12.pdf", height=5, width=10)
ggsave("C:/Users/caio0001/Documents/git/vatten/dammed_height_transparent.png", bg = "transparent", dpi = 500, width = 6, height = 5)


sum(hej2$dammed_height_kuhlin, na.rm = T)


### Checking if sum of gradient changes is the same as difference between top and bottom node.

fnode <- 1

find_tnode_from_fnode <- function(fnode, in_df){
  out <- in_df$Tnode[which(in_df$Fnode == fnode)]
  if(length(out) == 0){
    return(-9)
  } else {
    return(out)
  }
}

find_tnode_from_fnode(130876, in_df = river_key_clean)

find_node_sequence <- function(fnode, in_df){
  i <- 0
  
  tnode <- find_tnode_from_fnode(fnode, in_df = river_key_clean)
  
  # while(i <= 10){
  #   tnode <- c(tnode, find_tnode_from_fnode(tnode[length(tnode)], in_df = river_key_clean))
  #   i <- i + 1
  # }
  
  while(tnode[length(tnode)] != -9){
    tnode <- c(tnode, find_tnode_from_fnode(tnode[length(tnode)], in_df = river_key_clean))
    i <- i + 1
  }

  return(tnode)
}

node_seq <- find_node_sequence(11194, in_df = river_key_clean)

node_altitude_key$Z_value[which(node_altitude_key$Node_ID == 11194)]

node_seq <- node_seq[-length(node_seq)]

##
alt_temp <- NULL
for(i in 1:length(node_seq)){
  alt_temp[i] <- river_key_clean$altitude_change[river_key_clean$Fnode == node_seq[i]]
}

sum(alt_temp, na.rm=T)


test <- data.frame(hej = c(1,2,3,1,2,3,1,2,3), group=c("A", "B", "C","A", "B", "C","A", "B", "C"))

test %>% mutate(behavior = ifelse(group %in% c("A", "B"), "Restrained", "Other"))








