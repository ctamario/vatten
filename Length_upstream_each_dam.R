




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



how_much_length2 <- function(in_segment_list, in_blocking_barriers){
  
  riv_df <- rivers[which(rivers$RSTID %in% in_segment_list),]
  barr_df <- in_blocking_barriers
  
  riv_df$RSTID <- as.character(riv_df$RSTID)
  barr_df$RSTID <- as.character(barr_df$RSTID)
  
  riv_df$shape_length <- st_length(riv_df)
  
  riv_df <- as.data.frame(riv_df)
  barr_df <- as.data.frame(barr_df)
  
  riv_df$prop_length_counts <- 1
  barr_df$prop_length_counts <- (barr_df$Per_Along/100)
  
  result <- riv_df %>%
    left_join(barr_df %>% dplyr::select(RSTID, prop_length_counts), by = "RSTID", suffix = c("", ".key")) %>%
    mutate(prop_length_counts = if_else(!is.na(prop_length_counts.key), prop_length_counts.key, prop_length_counts)) %>%
    dplyr::select(-ends_with(".key"))
  
  return(result)
  
}


up_seg_all4 <- function(in_df, seg, in_barriers){
  next_up <- up_seg(in_df, seg)
  list_up <- next_up
  if(list_up %in% in_barriers$RSTID){
   return(c(seg, list_up))
  }
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

up_seg_all4_diag <- function(in_df, seg, in_barriers){
  next_up <- up_seg(in_df, seg)
  list_up <- next_up
  if(TRUE %in% (list_up %in% in_barriers$RSTID)){
    #return(c(seg, list_up))
    next_up <- next_up[!next_up %in% in_barriers$RSTID]
    #return(list_up)
  }
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


find_first_blocking_dams2 <- function(in_blocking_barriers){
  b <- in_blocking_barriers
  remove_these_barriers <- b %>% arrange(Per_Along) %>% group_by(RSTID) %>% slice_tail(n = 1)
  return(remove_these_barriers)
}


#up_seg_all3()




tabort <- blocking_set_of_dams %>% group_by(RSTID) %>% summarize(n = n())
tabort
rm(tabort)


#dams_on_segment_n <- merged_barriers %>% 

write.table(blocking_set_of_dams, file = "C:/temp/blocking_set_of_dams.csv", sep = ";", dec = ",", row.names = F)

##### 

blockset <- blocking_set_of_dams

blockset <- blockset %>% group_by(RSTID) %>% mutate(how_many_on_segment = n())

str(rivers$RSTID)

blockset$RSTID_chr <- as.character(blockset$RSTID)

blockset <- blockset %>% left_join(rivers %>% select(RSTID, shape_length_full), by = c("RSTID_chr" = "RSTID"))

blockset$RSTID_chr[1]
blockset$how_many_on_segment[1]

tabort <- blockset %>% filter(RSTID == '67194271590354')

9846
9250
290

upstream_from_dam <- function(in_dams, in_overallID){
  in_dams$RSTID_chr <- as.character(in_dams$RSTID)
  in_dams <- in_dams %>% left_join(rivers %>% select(RSTID, shape_length_full), by = c("RSTID_chr" = "RSTID"))
  
  origin_dam <- in_dams %>% dplyr::filter(overallID_Calle == in_overallID)
  
  RSTID_of_in_dam <- origin_dam$RSTID
  
  # have to check if the dam is alone on the segment
  dam_subset <- in_dams %>% dplyr::filter(RSTID %in% RSTID_of_in_dam)
  
  # Two different algorithms depending on if there are many dams on the same segment (alone = 0),
  # or if there is only one dam on the segment (alone = 1)
  # or if the dams is the most upstream dam on the segment (also alone = 1)
  if(nrow(dam_subset) == 1){
    alone <- 1
  } else {
    if(min(dam_subset$Per_Along) == origin_dam$Per_Along){
      alone <- 1
    } else {
      alone <- 0
    }
  }

  
  if(alone == 1){
    
    print("alone dam on segment, searching upstream")
    
    # length of segment up to upper border
    #out <- as.numeric(origin_dam$shape_length_full)*(origin_dam$Per_Along/100)
    
    first_fragment_length <- as.numeric(origin_dam$shape_length_full)*(origin_dam$Per_Along/100)
    
    # if it's alone, it should really search upstream
    #up_fragment_search <- up_seg_all3(rivers, RSTID_of_in_dam, in_dams)
    up_fragment_search <- rivers[which(rivers$RSTID %in% up_seg_all4_diag(rivers, RSTID_of_in_dam, in_dams)),]
    blocking_set_of_dams_new <- find_blocking_set_of_dams(in_dams, up_fragment_search)
    first_blocking_dams_new <- find_first_blocking_dams2(blocking_set_of_dams_new)
    
    up_fragment_search_without_first <- up_fragment_search[!(up_fragment_search$RSTID == RSTID_of_in_dam),]
    
    test123_new <- how_much_length(up_fragment_search_without_first$RSTID, first_blocking_dams_new)
    test123_new$real_length <- as.numeric(test123_new$shape_length)*test123_new$prop_length_counts
    
    out <- sum(test123_new$real_length)
    return(list(first_fragment_length, out, first_fragment_length+out, test123_new))
    #return(first_fragment_length+out)
    
  } else if(alone == 0){
    
    print("many dams on segment")
    
    # Find where on the segment the "origin dam" is.
    Per_Along_of_first_dam <- origin_dam$Per_Along
    
    # Filter to search only for dams upstream the origin dam
    dams_upstream_first_dam <- dam_subset %>% filter(Per_Along < Per_Along_of_first_dam)
    
    # Identify the next dam by  
    next_dam <- dams_upstream_first_dam %>% dplyr::filter(!overallID_Calle %in% in_overallID) %>% arrange(desc(Per_Along)) %>% slice(1)
    
    out <- as.numeric(origin_dam$shape_length_full)*(origin_dam$Per_Along/100)-as.numeric(next_dam$shape_length_full)*(next_dam$Per_Along/100)
    #return(dams_upstream_first_dam)
    return(out)
    
  } else {
    
    return(print("Edge case found"))
  }
  
}

##############
##############
##############


upstream_from_dam2 <- function(in_dams, in_overallID){
  in_dams$RSTID_chr <- as.character(in_dams$RSTID)
  in_dams <- in_dams %>% left_join(rivers %>% select(RSTID, shape_length_full), by = c("RSTID_chr" = "RSTID"))
  
  origin_dam <- in_dams %>% dplyr::filter(overallID_Calle == in_overallID)
  
  RSTID_of_in_dam <- origin_dam$RSTID
  
  # have to check if the dam is alone on the segment
  dam_subset <- in_dams %>% dplyr::filter(RSTID %in% RSTID_of_in_dam)
  
  # Two different algorithms depending on if there are many dams on the same segment (alone = 0),
  # or if there is only one dam on the segment (alone = 1)
  # or if the dams is the most upstream dam on the segment (also alone = 1)
  if(nrow(dam_subset) == 1){
    alone <- 1
  } else {
    if(min(dam_subset$Per_Along) == origin_dam$Per_Along){
      alone <- 1
    } else {
      alone <- 0
    }
  }
  
  
  if(alone == 1){
    
    print("alone dam on segment, searching upstream")
    
    # length of segment up to upper border
    #out <- as.numeric(origin_dam$shape_length_full)*(origin_dam$Per_Along/100)
    
    first_fragment_length <- as.numeric(origin_dam$shape_length_full)*(origin_dam$Per_Along/100)
    
    # if it's alone, it should really search upstream
    #up_fragment_search <- up_seg_all3(rivers, RSTID_of_in_dam, in_dams)
    up_fragment_search <- rivers[which(rivers$RSTID %in% up_seg_all4_diag(rivers, RSTID_of_in_dam, in_dams)),]
    blocking_set_of_dams_new <- find_blocking_set_of_dams(in_dams, up_fragment_search)
    first_blocking_dams_new <- find_first_blocking_dams2(blocking_set_of_dams_new)
    
    up_fragment_search_without_first <- up_fragment_search[!(up_fragment_search$RSTID == RSTID_of_in_dam),]
    
    if(nrow(up_fragment_search_without_first) == 0){
      return(as.numeric(first_fragment_length))
    }
    
    test123_new <- how_much_length(up_fragment_search_without_first$RSTID, first_blocking_dams_new)
    test123_new$real_length <- as.numeric(test123_new$shape_length)*test123_new$prop_length_counts
    
    out <- sum(test123_new$real_length)
    #return(list(first_fragment_length, out, first_fragment_length+out, test123_new))
    return(as.numeric(first_fragment_length+out))
    
  } else if(alone == 0){
    
    print("many dams on segment")
    
    # Find where on the segment the "origin dam" is.
    Per_Along_of_first_dam <- origin_dam$Per_Along
    
    # Filter to search only for dams upstream the origin dam
    dams_upstream_first_dam <- dam_subset %>% filter(Per_Along < Per_Along_of_first_dam)
    
    # Identify the next dam by  
    next_dam <- dams_upstream_first_dam %>% dplyr::filter(!overallID_Calle %in% in_overallID) %>% arrange(desc(Per_Along)) %>% slice(1)
    
    out <- as.numeric(origin_dam$shape_length_full)*(origin_dam$Per_Along/100)-as.numeric(next_dam$shape_length_full)*(next_dam$Per_Along/100)
    #return(dams_upstream_first_dam)
    return(as.numeric(out))
    
  } else {
    
    return(print("Edge case found"))
  }
  
}


hej <- upstream_from_dam(in_dams = merged_barriers, in_overallID = 26472)

upstream_from_dam2(in_dams = merged_barriers, in_overallID = 24975)

Calle_IDs <- unique(merged_barriers$overallID_Calle)

# Find cases with NA in Per_Along
which(merged_barriers$RSTID == merged_barriers$RSTID[which(is.na(merged_barriers$Per_Along))])

# There were three cases with NA in Per_Along. I looked them up and added them manually.
merged_barriers$Per_Along[which(merged_barriers$overallID_Calle == 10674)] <- 10
merged_barriers$Per_Along[which(merged_barriers$overallID_Calle == 21415)] <- 80
merged_barriers$Per_Along[which(merged_barriers$overallID_Calle == 21416)] <- 20

merged_barriers$upstream_length[1] <- upstream_from_dam2(in_dams = merged_barriers, in_overallID = Calle_IDs[1])

### Kör 3001 till 4000 igen
for(i in 17023:nrow(merged_barriers)){
  merged_barriers$upstream_length[i] <- upstream_from_dam2(in_dams = merged_barriers, in_overallID = Calle_IDs[i])
}

merged_barriers$upstream_length <- unlist(merged_barriers$upstream_length)





#up_seg_all3(rivers, 62117500467329, merged_barriers)

plot(log(merged_barriers$upstream_length))




write.table(merged_barriers, file="//storage-ume.slu.se/home$/caio0001/Desktop/merged_barriers_w_upstream_length.csv", sep = ";", dec = ",", row.names=F)

str(merged_barriers)
