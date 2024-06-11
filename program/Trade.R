## TRADE ### 

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
library(RColorBrewer)

conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)
here()


#Data -------------------------------------------------------------------
# scenathon<- read_csv(here("data", "240523_FullProductDataBase.csv")) %>% 
#   rename(alpha3 = country, Pathway = pathway, Year = year, Product = product) %>% 
#   mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>% 
#   filter(iteration == "5") %>% 
#   filter(!Year %in% c("2000", "2005", "2010", "2015")) %>% 
#   select(alpha3,Pathway, Year, Product, kcalfeasprod, import_quantity, export_quantity) %>% 
#   mutate(import_quantity = import_quantity/1000) %>% 
#   mutate(export_quantity = export_quantity/1000)



df <- read.csv(here("data", "240523_FullDataBase.csv")) %>% 
  dplyr::filter(tradeajustment == "Yes")
product <- read.csv(here("data",  "240523_FullProductDataBase.csv")) %>% 
  dplyr::filter(tradeadjustment == "Yes")
mapping_F6 <- read_excel(here("data",  "DataForFoodFigures.xlsx"), 
                         sheet = "prod groups map")

db_scenarios <- read.csv(here("data", "240523_scenarios.csv")) %>%
  select(pathways, country, afforestation,agricultural_land_expansion)%>% 
  rename(ALPHA3 = country,
         Pathway = pathways) %>% 
  mutate(Pathway = recode(Pathway, "CurrentTrends" = "CurrentTrend")) %>% 
  unique()

fao_prod <- read.csv(here("data", "FAO_FoodBalance.csv"))


fao_prod$Item <- ifelse(fao_prod$Item == "Rice and products", "Rice (Milled Equivalent)", 
                        ifelse(fao_prod$Item == "Groundnuts", "Groundnuts (in Shell Eq)",
                               ifelse(fao_prod$Item == "Vegetables, other", "Vegetables, Other",
                                      ifelse(fao_prod$Item == "Fruits, other", "Fruits, Other",
                                             ifelse(fao_prod$Item == "Freshwater Fish", "Fish Seafood + (Total)",
                                                    ifelse(fao_prod$Item == "Meat, Other", "Meat Other",
                                                           ifelse(fao_prod$Item == "Offals, Edible", "Offals Edible",
                                                                  fao_prod$Item)))))))



# mapping_animalprod <- read.csv(here("data", "FAOSTAT_mapping_animalprod.csv"))

# fao_prod <- fao_prod %>% 
#   inner_join(mapping_animalprod, by = "Item")

mapping <- read_excel(here("data",  "mapping_GAMS_FAO_products.xlsx"))
mapping[which(mapping$FAO == "Groundnuts (Shelled Eq)"), "FAO"] <- "Groundnuts"

mapping_country <- read_excel(here("data", "mapping_country_FAO_FABLE.xlsx")) %>% 
  mutate(iso3c = countrycode::countrycode(sourcevar = Country_FAO, origin = "country.name", destination = "iso3c"))
mapping_ALPHA3 <- read_excel(here("data",  "mapping_alpha3_Country.xlsx")) %>% 
  mutate(ALPHA3 = gsub("R_", "", ALPHA3))


fao_prod$Area <- as.character(fao_prod$Area)
fao_prod[which(fao_prod$Area == "United Kingdom of Great Britain and Northern Ireland"), "Area"] <- "United Kingdom"


product_dt <- left_join(product,
                        # rename(alpha3 = country) %>% 
                        # mutate(alpha3 = as.character(alpha3)) %>% 
                        # select(#-id, 
                        #   -scenathon_id), 
                        df,
                        # rename(alpha3 = country) %>% 
                        # select(#-id, 
                        #   -scenathon_id),
                        by = c("country", "pathway", "year")) %>% 
  rename(ALPHA3 = country) %>% 
  mutate(pathway = recode(pathway, "CurrentTrends" = "CurrentTrend")) %>% 
  left_join(mapping_ALPHA3) %>% 
  mutate(ALPHA3 = gsub("R_", "", ALPHA3)) %>% 
  unique()


##Compute kcal/kg data from FAO 2020
df_fao <- fao_prod %>%
  left_join(mapping %>% select(FPRODUCT, FAO) %>% unique(), by = c("Item" = "FAO")) %>%
  mutate(iso3c = countrycode::countrycode(sourcevar = Area, origin = "country.name", destination = "iso3c")) %>%
  left_join(mapping_country, by = c("iso3c")) %>%
  left_join(mapping_ALPHA3, by = c("Country_FABLE" = "Country")) %>%
  mutate(ALPHA3 = ifelse(Country_FABLE == "NPL", "NPL", 
                         ifelse(Country_FABLE == "GRC", "GRC", ALPHA3))) %>% 
  select(ALPHA3, FPRODUCT, Element, Value) 

df <- df %>% 
  rename(ALPHA3 = country) %>% 
  rename(Pathway = pathway)

product <- product %>% 
  rename(ALPHA3 = country)


#Computing Kcal content per Kg
df_fao <- (df_fao %>%
             group_by(ALPHA3, FPRODUCT, Element) %>% 
             dplyr::summarise_at(vars(Value),
                                 sum,
                                 na.rm = T) %>% 
             pivot_wider(names_from = Element,
                         values_from = Value) %>% 
             dplyr::rename(kg = "Food supply quantity (kg/capita/yr)",
                           kcal = "Food supply (kcal/capita/day)") %>% 
             mutate(kcal.kg = ifelse(kg!=0,
                                     kcal/kg,
                                     NA)) %>% 
             # mutate(ALPHA3 = ifelse(ALPHA3 == "NMC", "RMECAS", ALPHA3)) %>% 
             data.frame()) 

product_df <- left_join(product_dt, df_fao, by = c("ALPHA3" = "ALPHA3", "product" = "FPRODUCT")) %>%
  rename(Pathway = pathway)




#Computing imports and exports quantity relative changes 2020-2050, using Kcal per Kg to aggregate all products
scenathon <- product_df %>% 
  mutate(Pathway = ifelse(Pathway == "CurrentTrend", "CurrentTrends", Pathway)) %>%
  mutate(export_quantity = export_quantity * kcal.kg) %>% 
  mutate(import_quantity = import_quantity * kcal.kg) %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "ASP", "R_ASP",
                         ifelse(ALPHA3 == "CSA", "R_CSA",
                                ifelse(ALPHA3 == "NEU", "R_NEU",
                                       ifelse(ALPHA3 == "OEU", "R_OEU",
                                              ifelse(ALPHA3 == "SSA", "R_SSA", 
                                                     ifelse(ALPHA3 == "NMC", "R_NMC", ALPHA3))))))) %>% 
  rename(alpha3 = ALPHA3, Year = year, Product = product) %>% 
  mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(!Year %in% c("2000", "2005", "2010", "2015")) %>% 
  select(alpha3,Pathway, Year, Product, kcalfeasprod, import_quantity, export_quantity) %>% 
  mutate(import_quantity = import_quantity/1000) %>%
  mutate(export_quantity = export_quantity/1000)









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



## #Export ###

get_top_products <- function(country_data) {
  # Determine the number of top products based on the country
  top_n <- case_when(
    unique(country_data$alpha3) == "NOR" ~ 3,
    unique(country_data$alpha3) == "GBR" ~ 4,
    TRUE ~ 5
  )
  
  # Calculate top products by export quantity
  top_products <- country_data %>%
    group_by(Product) %>%
    summarise(total_export_quantity = sum(export_quantity, na.rm = TRUE)) %>%
    arrange(desc(total_export_quantity)) %>%
    slice_head(n = top_n) %>%
    pull(Product)
  
  # Filter data for top products
  country_data %>%
    filter(Product %in% top_products)
}

# Apply the function for each country
top_exports <- lapply(split(scenathon, scenathon$alpha3), get_top_products) %>%
  bind_rows()

# Group by Pathway and Year
top_exports <- top_exports %>%
  group_by(alpha3, Pathway, Year) %>%
  arrange(desc(export_quantity)) %>%
  ungroup()


figure_directory <- here("output", "figures", "Export", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

#Plot Pathway ---------------------------------------------------------------
top_exports$Pathway <- factor(top_exports$Pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

unique_products <- unique(top_exports$Product)

export_products <- c(
  "palmkernelcake" = "Palm kernel cake",
  "soyabean" = "Soyabean",
  "corn" = "Corn",
  "wheat" = "Wheat",
  "soycake" = "Soy cake",
  "palm_oil" = "Palm oil",
  "sugarraw" = "Sugar raw",
  "rice" = "Rice",
  "cassava" = "Cassava",
  "banana" = "Banana",
  "rapeseed" = "Rapeseed",
  "soyoil" = "Soy oil",
  "barley" = "Barley",
  "orange" = "Orange",
  "nuts" = "Nuts",
  "sunfloil" = "Sunflower oil",
  "fruit_other" = "Other fruits",
  "vegetable_other" = "Other vegetables",
  "tomato" = "Tomato",
  "rubber" = "Rubber",
  "sorghum" = "Sorghum",
  "palmkerneloil" = "Palm kernel oil",
  "peas" = "Peas",
  "rapecake" = "Rape cake",
  "apple" = "Apple",
  "pork" = "Pork",
  "cocoa" = "Cocoa",
  "potato" = "Potato",
  "pinapple" = "Pineapple",
  "groundnutcake" = "Groundnut cake",
  "coconut" = "Coconut",
  "pulses_other" = "Other pulses",
  "milk" = "Milk",
  "sunflower" = "Sunflower",
  "date" = "Date",
  "grape" = "Grape",
  "onion" = "Onion",
  "coffee" = "Coffee",
  "lemon" = "Lemon",
  "sesame" = "Sesame",
  "spices_other" = "Other spices",
  "other_oil" = "Other oil",
  "oats" = "Oats",
  "oilseed_other" = "Other oilseeds",
  "rapeoil" = "Rapeseed oil",
  "oliveoil" = "Olive oil",
  "olive" = "Olive",
  "beans" = "Beans",
  "plantain" = "Plantain",
  "rye" = "Rye",
  "tea" = "Tea",
  "mutton_goat" = "Mutton/goat",
  "lentil" = "Lentil",
  "other_olscake" = "Other oilseed cakes",
  "cereal_other" = "Other cereals",
  "eggs" = "Eggs",
  "pepper" = "Pepper"
)



# Create plot for each country and save as TIFF
for (country in countries) {
  
  # Subset data for the specific country
  country_data <- subset(top_exports, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway <- ggplot(country_data, aes(x = Year, y = export_quantity, color = Product)) +
    geom_point(size = 2) +  # Use points to show each year
    geom_line(size = 1.2, aes(group = Product)) +
    facet_wrap(~ Pathway, scales = "free_y",
               labeller = labeller(Pathway = c(
                 "CurrentTrends" = "Current Trend Pathway",
                 "NationalCommitments" = "National Commitments Pathway",
                 "GlobalSustainability" = "Global Sustainability Pathway"
               ))) +
    labs(
      x = "",
      y = "billion kcal",
      color = ""
    ) +
    scale_color_manual(values = brewer.pal(n = length(unique(country_data$Product)), name = "Set1"), 
                       labels = export_products) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 24, face = "bold"),
      legend.title = element_text(family = "sans", color = "black", size = 18),
      legend.text = element_text(family = "sans", size = 18),
      axis.title.x = element_text(color = "black", size = 18),
      axis.title.y = element_text(color = "black", size = 18),
      legend.position = "bottom",
      panel.spacing = unit(2, "cm")
    )
  
  # Save the plot as a TIFF file
  filename <- paste0(gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".tiff")
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 7, width = 24, res = 300
  )
  print(p_pathway)
  dev.off()
}








### Import ###




# Function to get top 5 products by import_quantity for each country
get_top_5_products <- function(country_data) {
  top_products <- country_data %>%
    group_by(Product) %>%
    summarise(total_import_quantity = sum(import_quantity, na.rm = TRUE)) %>%
    arrange(desc(total_import_quantity)) %>%
    slice_head(n = 5) %>%
    pull(Product)
  
  country_data %>%
    filter(Product %in% top_products)
}

# Apply the function for each country and then group by Pathway and Year
top_imports <- scenathon %>%
  group_by(alpha3) %>%
  group_modify(~get_top_5_products(.x)) %>%
  ungroup() %>%
  group_by(alpha3, Pathway, Year) %>%
  arrange(desc(import_quantity)) %>%
  ungroup()





figure_directory <- here("output", "figures", "Import", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

#Plot Pathway ---------------------------------------------------------------
top_imports$Pathway <- factor(top_imports$Pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

unique_products <- unique(top_imports$Product)

import_products <- c(
  "soyabean" = "Soyabean",
  "soycake" = "Soy cake",
  "wheat" = "Wheat",
  "corn" = "Corn",
  "milk" = "Milk",
  "cassava" = "Cassava",
  "barley" = "Barley",
  "apple" = "Apple",
  "rice" = "Rice",
  "nuts" = "Nuts",
  "vegetable_other" = "Other vegetables",
  "sugarraw" = "Sugar raw",
  "orange" = "Orange",
  "sunflcake" = "Sunflower cake",
  "lemon" = "Lemon",
  "palm_oil" = "Palm oil",
  "other_oil" = "Other oil",
  "rapeseed" = "Rapeseed",
  "soyoil" = "Soy oil",
  "tomato" = "Tomato",
  "sunfloil" = "Sunflower oil",
  "sunflower" = "Sunflower",
  "fruit_other" = "Other fruits",
  "cereal_other" = "Other cereals",
  "pulses_other" = "Other pulses",
  "banana" = "Banana",
  "potato" = "Potato",
  "cottcake" = "Cottonseed cake",
  "rapeoil" = "Rapeseed oil",
  "pork" = "Pork",
  "coffee" = "Coffee",
  "cocoa" = "Cocoa"
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


# Create plot for each country and save as TIFF
for (country in countries) {
  
  # Subset data for the specific country
  country_data <- subset(top_imports, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway <- ggplot(country_data, aes(x = Year, y = import_quantity, color = Product)) +
    geom_point(size = 2) +  # Use points to show each year
    geom_line(size = 1.2, aes(group = Product))+
    facet_wrap(~ Pathway, scales = "free_y",
               labeller = labeller(Pathway = c(
                 "CurrentTrends" = "Current Trend Pathway",
                 "NationalCommitments" = "National Commitments Pathway",
                 "GlobalSustainability" = "Global Sustainability Pathway"
               ))) +
    labs(
      x = "",
      y = "billion kcal",
      color = ""
    ) +
    scale_color_manual(values = brewer.pal(n = length(unique(country_data$Product)), name = "Set1"), 
                       labels = import_products) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 24, face = "bold"),
      legend.title = element_text(family = "sans", color = "black", size = 18),
      legend.text = element_text(family = "sans", size = 18),
      axis.title.x = element_text(color = "black", size = 18),
      axis.title.y = element_text(color = "black", size = 18),
      legend.position = "bottom",
      panel.spacing = unit(2, "cm")
    )
  
  # Save the plot as a TIFF file
  filename <- paste0(gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".tiff")
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 7, width = 24, res = 300
  )
  print(p_pathway)
  dev.off()
}




