## Consumption by Food Group

# libraries ---------------------------------------------------------------
library(here)
#library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(reshape2)
library(ggplot2)
library(stringr)
library(conflicted)
library(writexl)
library(openxlsx)
library(stats)
library(zoo)
library(cluster) 
library(factoextra)

conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)
here()


# mapping_alpha3_Country <- read_excel(here("data","mapping_alpha3_Country.xlsx"))
# mapping_country_FAO_FABLE <- read_excel(here("data", "mapping_country_FAO_FABLE.xlsx"))
# 
# FAOSTAT_FoodSupply_Other <- read_csv(here("data", "240523_FAOSTAT_FoodSupply_Other.csv")) %>% 
#   select(Area, Item, Year, Value) %>% 
#   left_join(mapping_country_FAO_FABLE, by = c("Area" = "Country_FAO")) %>% 
#   left_join(mapping_alpha3_Country, by = c("Country_FABLE" = "Country")) %>% 
#   mutate(ALPHA3 = ifelse(Country_FABLE == "GRC", "GRC", 
#                          ifelse(Country_FABLE == "NPL", "NPL",
#                          ALPHA3))) %>% 
# drop_na() %>% 
#   select(-Country_FABLE)
# 
# FAOSTAT_FoodSupply_Other_wide <- FAOSTAT_FoodSupply_Other %>%
#   pivot_wider(names_from = Item, values_from = Value)
# 
# 
# 
# database <- FAOSTAT_FoodSupply_Other_wide %>% 
#   group_by(Year, ALPHA3) %>% 
#   mutate(pop_tot = sum(Population, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   group_by(Year) %>% 
#   mutate(offals = Population / pop_tot * `Offals, Edible`) %>% 
#   mutate(fish = Population / pop_tot * `Fish, Seafood`) %>% 
#   group_by(Year, ALPHA3) %>% 
#   mutate(offals_tot = sum(offals)) %>% 
#   mutate(fish_total = sum(fish)) %>% 
#   ungroup() %>% 
#   select(ALPHA3, Year, offals_tot, fish_total) %>% 
#   unique() %>% 
#   mutate(OTHER =offals_tot+fish_total)
# 
# write.xlsx(database, file = here("data", paste0(gsub("-", "",Sys.Date()), "_","OTHER.xlsx")))


OTHER <- read_excel(here("data", "20240612_OTHER.xlsx"))


#Data -------------------------------------------------------------------
# missfood_regions <- read_excel(here("data", "240612_MissingFood.xlsx")) %>% 
#   filter(CountryName %in% c("R_ASP", "R_CSA", "R_OEU", "R_NEU", "R_SSA", "R_NMC"))
# 
# missfood_regions_long <- missfood_regions %>%
#   pivot_longer(cols = starts_with("20"),  # Select columns starting with "20" (assuming they are the year columns)
#                names_to = "Year",  # New column name for years
#                values_to = "kcalfeas") %>% 
#   select(-type)
# 
# write.xlsx(missfood_regions_long, file = here("data", paste0(gsub("-", "",Sys.Date()), "_","missfood_regions_long.xlsx")))

missfood_regions <- read_excel(here("data","20240612_missfood_regions_long.xlsx"))


# scenathon<- read_csv(here("data", "240523_FullProductDataBase.csv")) %>%
#   rename(alpha3 = country, Pathway = pathway, Year = year, Product = product) %>%
#   mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>%
#   filter(iteration == "5") %>%
#   filter(!Year %in% c("2000", "2005", "2010", "2015")) %>%
# select(alpha3,Pathway, Year, Product, kcalfeasprod)
# 
# mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>%
#   rename(Product = PRODUCT)
# 
# consumption <- scenathon %>%
#   left_join(mapping, by ="Product") %>%
#   group_by(Pathway, alpha3, Year, PROD_GROUP) %>%
#   mutate(kcalfeasprod_productgroup = sum(kcalfeasprod)) %>%
#   ungroup()  %>%
#   group_by(Pathway, alpha3, Year) %>%
#   mutate(total_kcal = sum(kcalfeasprod)) %>%
#   ungroup() %>%
#   select(-Product, -kcalfeasprod) %>%
#   filter(!PROD_GROUP %in% c("FIBERINDUS", "OLSCAKE")) %>%
#   # filter(alpha3 == country) %>%
#   unique %>%
#   drop_na() %>%
#   mutate(kcalfeasprod_productgroup = ifelse(kcalfeasprod_productgroup < 0, 0, kcalfeasprod_productgroup))

consumption <- read_excel(here("data", "20240612_consumption.xlsx")) %>% 
  select(-total_kcal)%>% 
  rbind(missfood_regions) %>% 
  rbind(OTHER) %>% 
  group_by(Pathway, alpha3, Year) %>% 
  mutate(total_kcal = sum(kcalfeasprod_productgroup)) %>% 
  ungroup()
  




### MDER
# MDER <- read.csv(here("data", "240523_FullDataBase.csv")) %>% 
#   filter(tradeajustment == "Yes") %>% 
#   select(country, year, kcal_mder) %>% 
#   rename(alpha3 = country) 
# 
# consumption_final <- consumption %>% 
#   left_join(MDER)



# 
# ### Handling Missing Food groups (not done for regions)
# FOOD_missing <- read_excel(here("data","Figure 6", "MissingFoodProducts.xlsx"), sheet = "figure6") %>%
#   mutate(PROD_GROUP = ifelse(
#     Item %in% c("Wine", "Beer", "Beverages, Alcoholic"), "ALCOHOL","ANIMFAT" )) %>%
#   group_by(Country, PROD_GROUP) %>%
#   mutate(kcalfeasprod = sum(Value)) %>%
#   select(-Value, -Item) %>%
#   unique()
# 
# country_abbreviations <- c(
#   "Argentina" = "ARG",
#   "Australia" = "AUS",
#   "Brazil" = "BRA",
#   "Canada" = "CAN",
#   "China" = "CHN",
#   "Colombia" = "COL",
#   "Ethiopia" = "ETH",
#   "Finland" = "FIN",
#   "Germany" = "DEU",
#   "India" = "IND",
#   "Indonesia" = "IDN",
#   "Mexico" = "MEX",
#   "Norway" = "NOR",
#   "Nepal" = "NPL",
#   "Türkiye" = "TUR",
#   "Denmark" = "DNK",
#   "Greece" = "GRC",
#   "Russian Federation" = "RUS",
#   "Rwanda" = "RWA",
#   "Sweden" = "SWE",
#   "United Kingdom of Great Britain and Northern Ireland" = "GBR",
#   "United States of America" = "USA"
# )
# 
# FOOD_missing <- FOOD_missing %>%
#   mutate(Country = case_when(
#     Country %in% names(country_abbreviations) ~ country_abbreviations[Country],
#     TRUE ~ as.character(Country)
#   )) %>%
#   select(-Year) %>%
#   rename(alpha3 = Country) %>%
#   filter(alpha3 %in% c("ARG","AUS","BRA","CAN","CHN", "COL","DEU","ETH","FIN","GBR", "IDN", "IND","MEX","NOR", "RUS", "RWA","SWE",  "USA",
#                           "DNK","GRC","TUR", "NPL",
#                           "R_ASP", "R_CSA", "R_NMC", "R_OEU", "R_NEU", "R_SSA"))
# 
# 
# 
# 
# 
# prod_groups <- unique(FOOD_missing$PROD_GROUP)
# years <- seq(2020, 2050, by = 5)
# 
# expanded_data <- data.frame()
# 
# for (prod_group in prod_groups) {
#   temp_data <- data.frame(
#     PROD_GROUP = prod_group,
#     Year = rep(years, each = length(which(FOOD_missing$PROD_GROUP == prod_group)))
#   )
#   expanded_data <- rbind(expanded_data, temp_data)
# }
# 
# FOOD_missing_final <- merge(FOOD_missing, expanded_data, by = "PROD_GROUP") %>%
#   unique() %>% 
#   rename(kcalfeasprod_productgroup = kcalfeasprod)
# 
# write.xlsx(FOOD_missing_final, file = here("data", paste0(gsub("-", "",Sys.Date()), "_","FOOD_missing_final.xlsx")))
# write.xlsx(consumption, file = here("data", paste0(gsub("-", "",Sys.Date()), "_","consumption.xlsx")))




#Plot ---------------------------------------------------------

consumption$Pathway <- factor(consumption$Pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))


product_colors <- c(
  "BEVSPICES" = "darkgrey",    
  "CEREALS" = "#FFD700",      
  "EGGS" = "#00008B",         
  "FRUVEG" = "#8A2BE2",        
  "MILK" = "#006400",         
  "NUTS" = "#8FBC8F",          
  "OLSOIL" = "steelblue",        
  "PORK" = "#DC143C",         
  "POULTRY" = "#4B0082",
  "PULSES" = "#F17CB0",
  "REDMEAT" = "#B22222",       
  "ROOTS" = "#DAA520",        
  "SUGAR" = "#FF4500",
  "ALCOHOL" = "#1E90FF",
  "ANIMFAT" = "#A0522D",
  "OTHER"="#00CED1"
)

product_labels <- c(
  "BEVSPICES" = "Beverages and Spices",
  "CEREALS" = "Cereals",
  "EGGS" = "Eggs",
  "FRUVEG" = "Fruits and Vegetables",
  "MILK" ="Milk",
  "NUTS" = "Nuts",
  "OLSOIL" = "Oilseeds and Veg. Oils",
  "PORK" = "Pork",
  "POULTRY" = "Poultry",
  "PULSES" = "Pulses",
  "REDMEAT" = "Beef, Goat and Lamb",
  "ROOTS" = "Roots and Tubers",
  "SUGAR" ="Sugar and Sugar Crops",
  "ALCOHOL" = "Alcohol",
  "ANIMFAT" = "Animal Fat",
  "OTHER" = "Other"
)

#List countries
countries <- c(
   "ARG",
  "AUS",
  "BRA"
  , "CAN", "CHN", "COL","DEU",
  "ETH"
  ,"FIN","GBR", "IDN", "IND",
  "MEX"
  ,"NOR", "RUS", "RWA","SWE",  "USA",
  "DNK",
  "GRC","TUR", "NPL",
  "R_ASP", "R_CSA", "R_NMC", "R_OEU", "R_NEU", "R_SSA"
)



figure_directory <- here("output", "figures", "fig4_diet", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

# Loop for each country

for (country in countries) {
  
  country_data <- subset(consumption, alpha3 == country)
  
  # Create plot for the specific country
  p_consumption <- ggplot(country_data, aes(x = as.factor(Year))) +
    geom_bar(aes(y = kcalfeasprod_productgroup, fill = PROD_GROUP), stat = "identity", position = "stack", width = 0.6) +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(x="",
      y = "kcal/cap/day",
      fill = ""
    ) +
    scale_y_continuous(breaks = seq(0, max(country_data$kcalfeasprod_productgroup + 3000), 250)) +
    facet_grid(. ~ Pathway, scales = "free_y",
               labeller = labeller(Pathway = c(
                 "CurrentTrends" = "Current Trends",
                 "NationalCommitments" = "National Commitments",
                 "GlobalSustainability" = "Global Sustainability"
               ))) +
    scale_fill_manual(values = product_colors, labels = product_labels) +  
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 54, face = "bold"),
      legend.title = element_text(family = "sans", color = "black", size = 54),
      legend.text = element_text(family = "sans", size = 42),
      axis.title.x = element_text(color = "black", size = 38),
      axis.text.x = element_text(color = "black", size = 32),
      axis.title.y = element_text(color = "black", size = 38),
      axis.text.y = element_text(color = "black", size = 32),
      legend.position = "bottom",
      legend.spacing.x = unit(3, "cm"),
      panel.spacing = unit(2, "cm")
    )  + guides(
      fill = guide_legend(override.aes = list(size = 10), keyheight = unit(2, "cm"), keywidth = unit(2, "cm")),
      color = guide_legend(override.aes = list(size = 10))
    )
  
  # Save the plot as a TIFF file
filename <- paste0(gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".png")
png(
  filename = here(figure_directory, filename),
  units = "in", height = 16, width = 32, res = 300)
  print(p_consumption)
dev.off()

}

