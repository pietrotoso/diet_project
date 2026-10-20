library(tidyverse)
source("clean_df_functions.R")
#Load raw data
all_data <- read_csv(file = 'data/raw/FoodBalanceSheetsHistoric_E_All_Data_(Normalized)/FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv', locale=locale(encoding = 'latin1'))

## PER DOMANDE 1,2,3

#keep only single countries
Area_codes <- read_csv(file = 'data/raw/FoodBalanceSheetsHistoric_E_All_Data_(Normalized)/country_with_groups.csv')
Single_countries <- Area_codes |> distinct(Country, `Country Code`) |> pull(`Country Code`)
df_only_single_countries <- all_data |> filter(`Area Code`%in% Single_countries)

rm(all_data)
gc()

#Add continent and area in the tibble
continents <- c("Africa","Europe","Americas","Asia","Oceania")
df_continents <- Area_codes |> filter(`Country Group` %in% continents) |> rename(`Area Code`=`Country Code`,Continent=`Country Group`,Continent_code=`Country Group Code`) |> select(Continent,Continent_code, `Area Code`)
df_areas <- Area_codes |> filter(!(`Country Group` %in% continents) & `Country Group Code`<5700 & `Country Group Code` != 5000) |> rename(`Area Code`=`Country Code`, Macroarea=`Country Group`,Macroarea_code=`Country Group Code`) |> select(Macroarea,Macroarea_code,`Area Code`)
with_cont <- left_join(df_only_single_countries,df_continents)
rm(df_only_single_countries, df_continents)
gc()
with_macro <- left_join(with_cont,df_areas)
rm(with_cont,df_areas)
gc()



#select countries that don't have all of the years to remove
Countries_to_remove <- with_macro |> distinct(`Area Code`,Year) |> count(`Area Code`) |> filter(n!=53) |> pull(`Area Code`)

#keep only countries with data for all the years
df_fullyears_countries <- with_macro |> filter(!(`Area Code` %in% Countries_to_remove))

# keep only food supply in calories and kilograms
df_calories_kilograms <- df_fullyears_countries |> filter(`Element Code` %in% c(664,645))
#remove anomalous negatives (Japan oats)
df_calories_kilograms$Value <- pmax(df_calories_kilograms$Value,rep(0,length(df_calories_kilograms$Value)))


# QUESTION 1

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

##QUESTION 2

#select the macro item group to investigate and the element(645 for kg/capita/year, 664 for kcal/capita/day)
q2_statistics <- full_process(df_question1,'Cereals - Excluding Beer',645,"all",TRUE)

#calcola centroide anno per anno
tabella_centroidi <- df_question2 |> group_by(Year) |> select(Area,percentage_year,`Item Code`) |> pivot_wider(names_from = `Item Code`,values_from = percentage_year) |> group_modify(~centroid_table(.x) |> as.list() |> tibble::as_tibble())

#con_quote |> filter(`Area Code` %in% c(2,41,68,55,100,121)) |> ggplot(aes(Year,percentage_year,color=Area)) + geom_line() + facet_wrap(~Item,scales='free_y') + scale_color_brewer(palette = "Set1")

##QUESTION 3



#filtrato <- all_data |> filter(Area %in% c('Italy','Belize','Kenya','Philippines')) |> 




#ricorda di completare dopo aver rimosso paesi strani e eventuali na

