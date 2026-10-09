data_frame_for_itemgroup <- function(df,df_item_groups,item_group) {
  #merge df with item_groups
  df_with_groups <- df |> left_join(df_item_groups)
  
  #remove lines thet refer to macrogroups
  df_with_groups <- df_with_groups |> filter(!(is.na(`Item Group`)))
  
  #filter based on desire item_group
  df_with_groups <- df_with_groups |> filter(`Item Group`==item_group)
  
  return(df_with_groups)
}

