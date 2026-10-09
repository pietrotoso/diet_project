library(tidyverse)

#Load raw data
all_data <- read_csv(file = 'data/raw/FoodBalanceSheetsHistoric_E_All_Data_(Normalized)/FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv', locale=locale(encoding = 'latin1'))

## PER DOMANDE 1,2,3

#keep only single countries with relative country groups
Area_codes <- read_csv(file = 'data/raw/FoodBalanceSheetsHistoric_E_All_Data_(Normalized)/country_with_groups.csv')
Single_countries <- Area_codes |> distinct(Country, `Country Code`) |> pull(`Country Code`)
df_only_single_countries <- all_data |> filter(`Area Code`%in% Single_countries)

#select countries to remove
Countries_to_remove <- df_only_single_countries |> distinct(`Area Code`,Year) |> count(`Area Code`) |> filter(n!=53) |> pull(`Area Code`)

#keep only countries with data for all the years
df_fullyears_countries <- df_only_single_countries |> filter(!(`Area Code` %in% Countries_to_remove))


# DOMANDA 1
# keep only food supply in calories and kilograms
df_calories_kilograms <- df_fullyears_countries |> filter(`Element Code` %in% c(664,645))

#Load item groups
Item_groups <- read_csv('data/item_groups.csv')

Macro_groups <- Item_groups |> 
                        filter(!(`Item Group` %in% c('Grand Total','Vegetal Products','Animal Products'))) |> 
                        select(`Item Code`,`Item Group`)
#keep only single items
df_single_items <- df_calories_kilograms |> left_join(Macro_groups) |> filter(!is.na(`Item Group`))

#remove miscellaneous item
df_single_items <- df_single_items |> filter(`Item Code`!=2899)

#make it rectangular. set to 0 missing values of Value
df_question1 <- df_single_items|> complete(nesting(Area,`Area Code`,`Area Code (M49)`),nesting(`Item Code`,Item,`Item Group`,`Item Code (FBS)`),nesting(Year,`Year Code`),nesting(Element,`Element Code`,Unit),fill=list(Value=0))



#filtrato <- all_data |> filter(Area %in% c('Italy','Belize','Kenya','Philippines')) |> 




#ricorda di completare dopo aver rimosso paesi strani e eventuali na

