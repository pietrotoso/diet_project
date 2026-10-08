libray(tidyverse)

#Load raw data
all_data <- read_csv(file = 'data/raw/FoodBalanceSheetsHistoric_E_All_Data_(Normalized)/FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv', locale=locale(encoding = 'latin1'))

#Load area codes
Area_codes <- read_csv(file = 'data/raw/FoodBalanceSheetsHistoric_E_All_Data_(Normalized)/FoodBalanceSheetsHistoric_E_AreaCodes.csv')

#Load item groups
Item_groups <- read_csv('data/item_groups.csv')

Macro_groups <- Item_groups |> 
                        filter(!(`Item Group` %in% c('Grand Total','Vegetal Products','Animal Products'))) |> 
                        select(`Item Code`,`Item Group`)

#select countries to remove
Area_to_remove <- all_data |> distinct(Area,Year) |> count(Area) |> filter(n!=53) |> select(Area)

#keep desired columns
filtrato <- all_data |> filter(Area %in% c('Italy','Belize','Kenya','Philippines')) |> 
  filter(Element == "Food supply quantity (kg/capita/yr)")


#ricorda di completare dopo aver rimosso paesi strani e eventuali na

