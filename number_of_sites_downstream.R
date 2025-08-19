

number_of_barriers_downstream <- read.csv("//storage-ume.slu.se/home$/caio0001/Desktop/merged_barriers_BARRIERS_DOWNSTREAM_OUTTABLE.csv",
                                          sep = ";", dec = ",")

number_of_barriers_downstream$no <- ifelse(number_of_barriers_downstream$DSSite_ID == -1, 0, 1)

barriers_downstream_key <- number_of_barriers_downstream %>% group_by(SiteID) %>% summarise(No_barriers_downstream = sum(no))

getwd()

write.table(barriers_downstream_key, 
            file = "//storage-ume.slu.se/home$/caio0001/Desktop/barriers_downstream_key_2025_04_14.csv",
            sep = ";", dec = ",", row.names = F)


####
#### Find all subcatchments downstream (recursive function)
####


### For the Gyrodactylus salaris

all_daro <- read.csv("C:/data/Riskvatten2025/infolager/gyro/full_daro_list.csv")

gyro_daro <- read.csv("C:/data/Riskvatten2025/infolager/gyro/gyro_daro_list.csv")

find_ds_daro <- function(in_data, in_aro){
  check_aro <- in_aro
  aro_list <- check_aro
  while(in_data$UTOBJ[which(in_data$AROID == check_aro)] != "H"){
    aro_list <- c(aro_list, in_data$OMRID_NED[which(in_data$AROID == check_aro)])
    check_aro <- in_data$OMRID_NED[which(in_data$AROID == check_aro)]
  }
  return(aro_list)
}

gyro_daro$AROID[1]


gyro_daros <- find_ds_daro(all_daro, gyro_daro$AROID[1])

for(i in 2:nrow(gyro_daro)){
  gyro_daros <- c(gyro_daros, find_ds_daro(all_daro, gyro_daro$AROID[i]))
}

write.table(data.frame(AROID = unique(gyro_daros), gyro_possible = 1), file = "C:/data/Riskvatten2025/infolager/gyro/gyro_possible_daros.csv", sep = ";", row.names = F)

find_ds_daro(all_daro, "635195-131849")







