library(tidyverse)
source("clean_df_functions.R")
#Load raw data
all_data <- read_csv(file = 'data/raw/FoodBalanceSheetsHistoric_E_All_Data_(Normalized)/FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv', locale=locale(encoding = 'latin1'))

## PER DOMANDE 1,2,3

#keep only single countries with relative country groups
Area_codes <- read_csv(file = 'data/raw/FoodBalanceSheetsHistoric_E_All_Data_(Normalized)/country_with_groups.csv')
Single_countries <- Area_codes |> distinct(Country, `Country Code`) |> pull(`Country Code`)
df_only_single_countries <- all_data |> filter(`Area Code`%in% Single_countries)

rm(all_data)
gc()

#add continent and area
continents <- c("Africa","Europe","Americas","Asia","Oceania")
df_continents <- Area_codes |> filter(`Country Group` %in% continents) |> rename(`Area Code`=`Country Code`,Continent=`Country Group`,Continent_code=`Country Group Code`) |> select(Continent,Continent_code, `Area Code`)
df_areas <- Area_codes |> filter(!(`Country Group` %in% continents) & `Country Group Code`<5700 & `Country Group Code` != 5000) |> rename(`Area Code`=`Country Code`, Macroarea=`Country Group`,Macroarea_code=`Country Group Code`) |> select(Macroarea,Macroarea_code,`Area Code`)
with_cont <- left_join(df_only_single_countries,df_continents)
rm(df_only_single_countries)
gc()
with_macro <- left_join(with_cont,df_areas)

#select countries to remove
Countries_to_remove <- with_macro |> distinct(`Area Code`,Year) |> count(`Area Code`) |> filter(n!=53) |> pull(`Area Code`)

#keep only countries with data for all the years
df_fullyears_countries <- with_macro |> filter(!(`Area Code` %in% Countries_to_remove))


# QUESTION 1
# keep only food supply in calories and kilograms
df_calories_kilograms <- df_fullyears_countries |> filter(`Element Code` %in% c(664,645))
#togli gli zeri del giappone
df_calories_kilograms$Value <- pmax(df_calories_kilograms$Value,rep(0,length(df_calories_kilograms$Value)))

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
df_question2 <- prepare_df_question2(df_question1, 'Cereals - Excluding Beer',645)
#calculate distances statistics with three different methods
distances <- df_question2 |> group_by(Year) |> group_modify(~{
  eucl <- statistics_year(.x,dist_method="euclidean")
  bray <- statistics_year(.x,dist_method="manhattan")
  hell <- statistics_year(.x,dist_method="hellinger")
  risultati <- rbind(eucl, bray, hell) |>
    as.data.frame()
  
  risultati$method <- c("euclidean", "manhattan", "hellinger")
  
  tibble::as_tibble(risultati)
}) |> ungroup()

#adding normalized median/average column to compare curves
distances <- distances |> group_by(method) |> mutate(median_scaled=median/median[1],average_scaled=average/average[1]) 
#calculate distance growth rate per year
with_rates <- distances|>  mutate(rate_median = (median - lag(median))/lag(median),rate_average = (average - lag(average))/lag(average))
#calculate moving average of median/average and their rates
with_moving_avg <- with_rates |> mutate(moving_median=slide_dbl(median,mean,.before=4,.complete=TRUE),moving_average=slide_dbl(average,mean,.before=4,.complete=TRUE),moving_rate_median=slide_dbl(rate_median,mean,.before=4,.complete=TRUE), moving_rate_average=slide_dbl(rate_average,mean,.before=4,.complete=TRUE))


#ggplot(conf,aes(year,median,color=dist_type)) + geom_line()+geom_ribbon(aes(ymin=`40_quantile`,ymax = `60_quantile`,fill=dist_type),linetype='dashed',linewidth = 0.25, alpha=0.05) +labs(y='average_distanve')
#con_quote |> filter(`Area Code` %in% c(2,41,68,55,100,121)) |> ggplot(aes(Year,percentage_year,color=Area)) + geom_line() + facet_wrap(~Item,scales='free_y') + scale_color_brewer(palette = "Set1")

##QUESTION 3



#filtrato <- all_data |> filter(Area %in% c('Italy','Belize','Kenya','Philippines')) |> 




#ricorda di completare dopo aver rimosso paesi strani e eventuali na

