





library(pacman)

p_load(tidyr, dplyr, readxl, ggplot2, gridExtra, stringr)


setwd("C:/Users/caio0001/Documents/ArcGIS/Projects/Sveriges_vatten/output_with_Marc")

node_altitude_key <- read_excel("nodes_with_altitude.xlsx")

head(node_altitude_key)


river_key <- read.csv("vd_l_riverlines.csv", sep=";", dec=",") 

head(river_key)


river_key[1,]$Fnode

river_key[1,]$Tnode

river_key$altitude_change <- NULL

# for(i in 1:nrow(river_key)){
#   river_key$altitude_change[i] <- node_altitude_key$RASTERVALU[which(node_altitude_key$Node_ID == river_key[i,]$Fnode)] - node_altitude_key$RASTERVALU[which(node_altitude_key$Node_ID == river_key[i,]$Tnode)]
# }

river_key$slope <- river_key$altitude_change / river_key$Shape_Length


p1 <- river_key %>% ggplot(aes(x = factor(Strahler_Stream_Order), y = altitude_change)) + geom_boxplot(coef = Inf) + coord_cartesian(ylim=c(-50,100))
p2 <- river_key %>% ggplot(aes(x = factor(Strahler_Stream_Order), y = slope)) + geom_boxplot()

grid.arrange(p1, p2, ncol = 2)

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

dam_key <- read.csv("dam_key_utf8.csv", sep = ";", dec = ",", fileEncoding = "UTF-8")

dam_key <- dam_key[!is.na(dam_key$Fnode),]


dam_key$HARO_string_length <- str_length(str_extract(dam_key$RW_PopNamn, "^[0-9]+"))
dam_key$HARO_string_length2 <- dam_key$HARO_string_length - 3 
dam_key$HARO <- str_sub(dam_key$RW_PopNamn, start = 1, end = dam_key$HARO_string_length2)


dammed_height <- dam_key %>% group_by(HARO, Strahler_Stream_Order) %>% summarise(dammed_height = sum(Fallh_m, na.rm = T))

###

final <- alt_change %>% left_join(dammed_height, by = "HARO")

final$dammed_height[is.na(final$dammed_height)] <- 0

final$prop_dammed <- final$dammed_height / final$sum_alt_change

hist(final$prop_dammed, breaks = 100)


###

alt_change_strahler_gt2 <- river_key_clean %>% dplyr::filter(Strahler_Stream_Order > 2) %>% group_by(HARO) %>% summarise(sum_alt_change = sum(altitude_change, na.rm = T),
                                                                                                                         sum_river_length = sum(Shape_Length, na.rm = T))

final2 <- alt_change_strahler_gt2 %>% left_join(dammed_height, by = "HARO")

final2$dammed_height[is.na(final2$dammed_height)] <- 0

final2$prop_dammed <- final2$dammed_height / final2$sum_alt_change

hist(final2$prop_dammed, breaks = 100)

plot(data = final2, prop_dammed ~ sum_river_length)

###

alt_change_strahler_gt2 <- river_key_clean %>% group_by(Strahler_Stream_Order, HARO) %>% summarise(sum_alt_change = sum(altitude_change, na.rm = T),
                                                                                                                         sum_river_length = sum(Shape_Length, na.rm = T))


hej <- alt_change_strahler_gt2 %>% left_join(dammed_height, by = c("Strahler_Stream_Order", "HARO"))

hej$prop_dammed <- hej$dammed_height / hej$sum_alt_change

hej %>% ggplot(aes(x = factor(Strahler_Stream_Order), y = prop_dammed)) + geom_boxplot()


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

node_altitude_key$RASTERVALU[which(node_altitude_key$Node_ID == 11194)]

node_seq <- node_seq[-length(node_seq)]

##
alt_temp <- NULL
for(i in 1:length(node_seq)){
  alt_temp[i] <- river_key_clean$altitude_change[river_key_clean$Fnode == node_seq[i]]
}

sum(alt_temp, na.rm=T)


test <- data.frame(hej = c(1,2,3,1,2,3,1,2,3), group=c("A", "B", "C","A", "B", "C","A", "B", "C"))

test %>% mutate(behavior = ifelse(group %in% c("A", "B"), "Restrained", "Other"))








