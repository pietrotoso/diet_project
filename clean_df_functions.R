library(philentropy)

prepare_df_question2 <- function(df,item_group,element_code){
  group_element_df <- df |> filter(`Item Group`==item_group, `Element Code`==element_code)
  with_percentages <- group_element_df |> group_by(`Area Code`,Area,Year) |> mutate(Year_tot_value=sum(Value),.before=Flag) |> ungroup() |> mutate(percentage_year=Value/Year_tot_value, .before=Flag)
  
  return(with_percentages)
}

#param lower_quantile and upper_quantile between 0 and 1
# distance_q_2 <- function(df,d_method,lower_quantile=0.25,upper_quantile=0.75){
#   if (d_method=="bray-curtis") {d_method='manhattan'}
#   mat_summary <- matrix(nrow=nrow(distinct(df,Year)),ncol=4,dimnames=list(pull(distinct(df,Year),Year),c(paste(toString(lower_quantile*100),"_quantile",sep=''),"median",paste(toString(upper_quantile*100),"_quantile",sep=''),"average")))
#   i=1
#   for (x in (min(df$Year):max(df$Year))){
#     df_year <- df |> filter(Year==x)
# 
#     values <- df_year |> arrange(`Area Code`, `Item Code`) |> pull(percentage_year)
#     val_matrix <- matrix(values,nrow=nrow(distinct(df_year,`Area Code`)), byrow=TRUE)
#     row.names(val_matrix) <- distinct(df_year,Area, `Area Code`) |> arrange(`Area Code`) |> pull(Area)
#     distances <- distance(val_matrix, method=d_method,use.row.names = TRUE)
#     distance_vector <- distances[upper.tri(distances)]
#     #calculate median and percentiles
# 
#     quantiles <- quantile(distance_vector,c(lower_quantile,0.5,upper_quantile))
#     quantiles_and_mean <- c(quantiles,mean(distance_vector))
#     #fill matrix
#     mat_summary[i,] <- quantiles_and_mean
#     i=i+1
#   }
#   
#   return(as_tibble(mat_summary))
#   
# 
# }


#funzione per un anno
statistics_year <- function(df_year,lower_quantile=0.25,upper_quantile=0.75,dist_method){
  df_wide <- df_year |> select(`Area Code`,Area,`Item Code`,percentage_year) |> pivot_wider(names_from = `Item Code`,values_from = percentage_year)
  only_percentages <-df_wide |> select(-`Area Code`,-Area)
  #pairwise distance
  distances <- distance(only_percentages,method=dist_method)
  distance_vector <- distances[upper.tri(distances)]
  mean_pairwise <- mean(distance_vector)
  #centroid distance
  centroid <- only_percentages |>  as.matrix() |> colMeans()
  d_centroid <- dist_one_many(centroid,as.matrix(only_percentages),method=dist_method)
  quantiles <- quantile(d_centroid,c(lower_quantile,0.5,upper_quantile))
  mean_d_centr <- mean(d_centroid)
  #ratio
  ratio <- mean_pairwise/mean_d_centr
  #create statistics vector
  statistics <- c(quantiles, mean_d_centr,mean_pairwise)
  #normalize to have maximum 1
  if (dist_method %in% c('manhattan',"hellinger")){
    statistics <- statistics/2
  } else if(dist_method=='euclidean') {statistics <- statistics/sqrt(2)}
  #add ratio
  statistics <- c(statistics,ratio)
  names(statistics) <- c('low_quantile','median','up_quantile','average','mean_pairwise_distance','ratio_mean')
  
  return(statistics)
}

centroid_table <- function(df_year) {
  only_percentages <-df_year |> select(-Area)
  centroid <- only_percentages |>  as.matrix() |> colMeans()
  return(centroid)
  
}
compare_distances <- function(eucl,bray,hell){
  bray <- bray |>  mutate(dist_type=rep("Bray_Curtis",nrow(bray)),year=c(1961:2013),scaled=median/median[1])
  eucl <- eucl |> mutate(dist_type=rep("Euclidean",nrow(eucl)),year=c(1961:2013),scaled=median/median[1])
  hell <- hell |>  mutate(dist_type=rep("Hellinger",nrow(hell)),year=c(1961:2013),scaled=median/median[1])
  complete <- rbind(bray,eucl,hell)
  return(complete)
  
}



data_frame_for_itemgroup <- function(df,df_item_groups,item_group) {
  #merge df with item_groups
  df_with_groups <- df |> left_join(df_item_groups)
  
  #remove lines thet refer to macrogroups
  df_with_groups <- df_with_groups |> filter(!(is.na(`Item Group`)))
  
  #filter based on desire item_group
  df_with_groups <- df_with_groups |> filter(`Item Group`==item_group)
  
  return(df_with_groups)
}


