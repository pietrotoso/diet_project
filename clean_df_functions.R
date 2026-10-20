library(philentropy)
library(slider)

#item_group: "all" for the whole diet, otherwise the name of the group to investigate
#element_code: 645 for kg/capita/year, 664 for kcal/capita/day
#dist_method: "bray-curtis", "euclidean", "hellinger" or "all"
#pairwise: TRUE to include pairwise distance calculation, FALSE otherwise
full_process <- function(df,item_group,element_code,dist_method,pairwise){
  df <- prepare_df_question2(df,item_group,element_code)
  df <- statistics_final(df,dist_method,pairwise)
  return(df)
}


prepare_df_question2 <- function(df,item_group,element_code){
  if (item_group!= "all") {group_element_df <- df |> filter(`Item Group`==item_group, `Element Code`==element_code)}
  else {group_element_df <- df |> filter(`Element Code`==element_code)}
  with_percentages <- group_element_df |> group_by(`Area Code`,Area,Year) |> mutate(Year_tot_value=sum(Value),.before=Flag) |> ungroup() |> mutate(percentage_year=Value/Year_tot_value, .before=Flag)
  
  return(with_percentages)
}


statistics_final <- function(df,dist_method,pairwise){
  if (dist_method=="all") {
    distances <- df|> group_by(Year) |> group_modify(~{
      eucl <- statistics_year(.x,dist_method="euclidean",pairwise)
      bray <- statistics_year(.x,dist_method="bray-curtis",pairwise)
      hell <- statistics_year(.x,dist_method="hellinger",pairwise)
      risultati <- rbind(eucl, bray, hell) |>
        as.data.frame()
      
      risultati$method <- c("euclidean", "bray-curtis", "hellinger")
      
      tibble::as_tibble(risultati)
    }) |> ungroup()
    
  }
  else {
    distances <- df |> group_by(Year) |> group_modify(~{
      stats <- statistics_year(.x,dist_method,pairwise)
      risultati <- as.data.frame(t(stats))
      
      risultati$method <- dist_method
      
      tibble::as_tibble(risultati)
    }) |> ungroup()
  }
  #adding normalized median/average column to compare curves and rate of decrease with relative moving average
  distances<- distances|>
    group_by(method) |> 
    mutate(median_scaled=median/median[1],average_scaled=average/average[1],rate_average = (average - lag(average))/lag(average),moving_rate_average=slide_dbl(rate_average,mean,.before=4,.complete=TRUE)) |> ungroup()
  return(distances)
}

#funzione per un anno
statistics_year <- function(df_year,dist_method, pairwise,lower_quantile=0.25,upper_quantile=0.75){
  df_wide <- df_year |> select(`Area Code`,Area,`Item Code`,percentage_year) |> pivot_wider(names_from = `Item Code`,values_from = percentage_year)
  only_percentages <-df_wide |> select(-`Area Code`,-Area)
  #centroid distance
  centroid <- only_percentages |>  as.matrix() |> colMeans()
  if (dist_method=="bray-curtis") {
    #divide by 2 to obtain bray-curtis from manhattan
    d_centroid <- dist_one_many(centroid,as.matrix(only_percentages),method="manhattan")/2
  }
  else {
    d_centroid <- dist_one_many(centroid,as.matrix(only_percentages),method=dist_method)
  }
  quantiles <- quantile(d_centroid,c(lower_quantile,0.5,upper_quantile))
  mean_d_centr <- mean(d_centroid)
  if (pairwise==TRUE){  
    if (dist_method=="bray-curtis") {
      #divide by 2 to obtain bray-curtis from manhattan
      distances <- distance(only_percentages,method="manhattan")/2
    }
    else {
      distances <- distance(only_percentages,method=dist_method)
    }
    distance_vector <- distances[upper.tri(distances)]
    mean_pairwise <- mean(distance_vector)
    #ratio
    ratio <- mean_pairwise/mean_d_centr
    #create statistics vector
    statistics <- c(quantiles, mean_d_centr,mean_pairwise,ratio)
    names(statistics) <- c('low_quantile','median','up_quantile','average','mean_pairwise_distance','ratio_mean')
  }
  else {
    statistics <- c(quantiles, mean_d_centr)
    names(statistics) <- c('low_quantile','median','up_quantile','average')
  }
  
  return(statistics)
}


centroid_table <- function(df_year) {
  only_percentages <-df_year |> select(-Area)
  centroid <- only_percentages |>  as.matrix() |> colMeans()
  return(centroid)
  
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


