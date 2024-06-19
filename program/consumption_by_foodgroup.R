# Consumption by Food Group

# libraries --------------------------------------------------------------------
library(here)
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

# 1) We are going to determinate the Missing Food groups for countries, and then for regions. 
# 2) The OTHER category will be set, 
# 3) Teff data for Ethiopia will be included in the Scenathon database
# 4) Merging all the previous dataset, we will come up with the complete data
# 5) Plotting the data

#Missing Food (Countries)-----------------------------------------------------
missing_food <- read_excel(here("data", "240612_MissingFood.xlsx")) %>% 
  select(-"2000", -"2005", -"2010", -"2015")

missing_food_long <- missing_food %>%
  pivot_longer(cols = "2020",
               names_to = "Year",
               values_to = "kcalfeas") %>%
  mutate(Year = as.numeric(Year)) %>% 
  select(-type)


# Create a series of new years
new_years <- c(2025, 2030, 2035, 2040, 2045, 2050)

# Create a new data frame by replicating the existing data for each new year
new_data <- missing_food_long %>%
  slice(rep(1:n(), each = length(new_years))) %>%
  mutate(Year = rep(new_years, times = nrow(missing_food_long)))


combined_data_years <- bind_rows(missing_food_long, new_data)

# Create the Pathway column values
pathways <- c("CurrentTrends", "NationalCommitments", "GlobalSustainability")


# Replicate the data for each Pathway
missing_food_long <- combined_data_years %>%
  slice(rep(1:n(), each = length(pathways))) %>%
  mutate(Pathway = rep(pathways, times = nrow(combined_data_years)))

#Renaming Russia
missing_food_long <- missing_food_long %>%
  mutate(CountryName = ifelse(CountryName == "RussianFed", "Russia", CountryName))  


mapping_prod<- read_excel(here("data", "mapping_product_group.xlsx")) %>%
  rename(Product = PRODUCT)

missing_food_long <- missing_food_long %>% 
  mutate(Product = ifelse(Product == "Beverages, Alcoholic", "BevAlcoholic", Product),
         Product = ifelse(Product == "Beverages fermented", "BevFermented", Product),
         Product = ifelse(Product == "Butter, Ghee", "ButterGhee", Product),
         Product = ifelse(Product == "Fats, Animals, Raw", "RawAnimalFat", Product)) %>% 
  left_join(mapping_prod) %>% 
  unique()


mapping_alpha3_Country <- read_excel(here("data" ,"mapping_alpha3_Country.xlsx")) %>% 
  rename(CountryName = Country)

missing_food_long <- missing_food_long %>% 
  left_join(mapping_alpha3_Country, relationship = "many-to-many") %>% 
  ungroup() %>% 
  select(-CountryName) %>% 
  drop_na() %>% 
  rename(alpha3 = ALPHA3, kcalfeasprod = kcalfeas) %>% 
  filter(!str_detect(alpha3, "^R_")) %>% 
  unique()

#Missing Food (Regions) --------------------------------------------------------
missfood_regions <- read_excel(here("data", "240612_MissingFood.xlsx")) %>%
  filter(CountryName %in% c("R_ASP", "R_CSA", "R_OEU", "R_NEU", "R_SSA", "R_NMC")) %>% 
  select(-"2000", -"2005", -"2010", -"2015")

missfood_regions_long <- missfood_regions %>%
  pivot_longer(cols = "2020",
               names_to = "Year",
               values_to = "kcalfeas") %>%
  mutate(Year = as.numeric(Year)) %>% 
  select(-type)

# Create a series of new years
new_years <- c(2025, 2030, 2035, 2040, 2045, 2050)

# Create a new data frame by replicating the existing data for each new year
new_data <- missfood_regions_long %>%
  slice(rep(1:n(), each = length(new_years))) %>%
  mutate(Year = rep(new_years, times = nrow(missfood_regions_long)))

combined_data_years <- bind_rows(missfood_regions_long, new_data)

# Create the Pathway column values
pathways <- c("CurrentTrends", "NationalCommitments", "GlobalSustainability")

# Replicate the data for each Pathway
missfood_regions_long <- combined_data_years %>%
  slice(rep(1:n(), each = length(pathways))) %>%
  mutate(Pathway = rep(pathways, times = nrow(combined_data_years)))

mapping_prod<- read_excel(here("data", "mapping_product_group.xlsx")) %>%
  rename(Product = PRODUCT)

missfood_regions_long_final <- missfood_regions_long %>% 
  mutate(Product = ifelse(Product == "Beverages, Alcoholic", "BevAlcoholic", Product),
         Product = ifelse(Product == "Beverages fermented", "BevFermented", Product),
         Product = ifelse(Product == "Butter, Ghee", "ButterGhee", Product),
         Product = ifelse(Product == "Fats, Animals, Raw", "RawAnimalFat", Product)) %>% 
  left_join(mapping_prod) %>% 
  drop_na() %>% 
  group_by(CountryName, Year, Pathway, PROD_GROUP) %>% 
  mutate(kcalfeasprod_productgroup = sum(kcalfeas)) %>% 
  ungroup() %>% 
  select(-kcalfeas, -Product) %>% 
  rename(alpha3 = CountryName) %>% 
  unique()

# Computing OTHER category -----------------------------------------------------
mapping_alpha3_Country <- read_excel(here("data","mapping_alpha3_Country.xlsx"))
mapping_country_FAO_FABLE <- read_excel(here("data", "mapping_country_FAO_FABLE.xlsx"))

FAOSTAT_FoodSupply_Other <- read_csv(here("data", "240523_FAOSTAT_FoodSupply_Other.csv")) %>%
  select(Area, Item, Year, Value) %>%
  left_join(mapping_country_FAO_FABLE, by = c("Area" = "Country_FAO")) %>%
  left_join(mapping_alpha3_Country, by = c("Country_FABLE" = "Country")) %>%
  mutate(ALPHA3 = ifelse(Country_FABLE == "GRC", "GRC",
                         ifelse(Country_FABLE == "NPL", "NPL",
                                ALPHA3))) %>%
  drop_na() %>%
  select(-Country_FABLE)

FAOSTAT_FoodSupply_Other_wide <- FAOSTAT_FoodSupply_Other %>%
  pivot_wider(names_from = Item, values_from = Value)

Other <- FAOSTAT_FoodSupply_Other_wide %>%
  filter(Year == "2020") %>% 
  group_by(Year, ALPHA3) %>%
  mutate(pop_tot = sum(Population, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(offals = Population / pop_tot * `Offals, Edible`) %>%
  mutate(fish = Population / pop_tot * `Fish, Seafood`) %>%
  group_by(Year, ALPHA3) %>%
  mutate(offals_tot = sum(offals)) %>%
  mutate(fish_total = sum(fish)) %>%
  ungroup() %>%
  select(ALPHA3, Year, offals_tot, fish_total) %>%
  unique() %>%
  mutate(kcalfeasprod_productgroup = offals_tot + fish_total)

# Create a series of new years
new_years <- c(2025, 2030, 2035, 2040, 2045, 2050)

# Create a new data frame by replicating the existing data for each new year
new_data <- Other %>%
  slice(rep(1:n(), each = length(new_years))) %>%
  mutate(Year = rep(new_years, times = nrow(Other)))

combined_data_years <- bind_rows(Other, new_data)

# Create the Pathway column values
pathways <- c("CurrentTrends", "NationalCommitments", "GlobalSustainability")

# Replicate the data for each Pathway
Other <- combined_data_years %>%
  slice(rep(1:n(), each = length(pathways))) %>%
  mutate(Pathway = rep(pathways, times = nrow(combined_data_years)))

# Cleaning
OTHER <- Other %>% 
  mutate(PROD_GROUP = "OTHER") %>% 
  select(-offals_tot, -fish_total) %>% 
  rename(alpha3 = ALPHA3)

# OTHER <- read_excel(here("data", "20240612_OTHER.xlsx"))



#Teff data for Ethiopia --------------------------------------------------
teff_kcal <- read_excel(here("data", "240517_teff_kcal.xlsx"))





#Complete data----------------------------------------------------------------
scenathon<- read_csv(here("data", "240523_FullProductDataBase.csv")) %>%
  rename(alpha3 = country, Pathway = pathway, Year = year, Product = product) %>%
  mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>%
  filter(iteration == "5") %>%
  filter(!Year %in% c("2000", "2005", "2010", "2015")) %>%
select(alpha3,Pathway, Year, Product, kcalfeasprod)

consumption <- scenathon %>%
  left_join(mapping_prod, by ="Product") %>%
  rbind(missing_food_long) %>% 
  group_by(Pathway, alpha3, Year, PROD_GROUP) %>%
  mutate(kcalfeasprod_productgroup = sum(kcalfeasprod)) %>%
  ungroup()  %>%
  group_by(Pathway, alpha3, Year) %>%
  mutate(total_kcal = sum(kcalfeasprod)) %>%
  ungroup() %>%
  select(-Product, -kcalfeasprod) %>%
  filter(!PROD_GROUP %in% c("FIBERINDUS", "OLSCAKE")) %>%
  unique %>%
  drop_na() %>% 
  # to correct for some products that assume negative values (Pork in DNK)
  mutate(kcalfeasprod_productgroup = ifelse(kcalfeasprod_productgroup < 0, 0, kcalfeasprod_productgroup)) 
  
consumption <- consumption %>% 
  select(-total_kcal) %>%
  rbind(missfood_regions_long_final) %>% 
  rbind(OTHER) %>% 
  rbind(teff_kcal) %>% 
  group_by(Pathway, alpha3, Year, PROD_GROUP) %>% 
  mutate(kcalfeasprod_productgroup = sum(kcalfeasprod_productgroup)) %>% 
  ungroup() %>% 
  unique() %>% 
  group_by(Pathway, alpha3, Year) %>% 
  mutate(total_kcal = sum(kcalfeasprod_productgroup)) %>% 
  ungroup()



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
   "ARG", "AUS", "BRA", "CAN", "CHN", "COL", "DEU",
  "ETH", "FIN", "GBR", "IDN", "IND","MEX", "NOR",
  "RUS", "RWA", "SWE", "USA", "DNK", "GRC", "TUR", "NPL",
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
  
  # Save the plot as a PNG file
filename <- paste0("Fig4_", gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".png")

png(
  filename = here(figure_directory, filename),
  units = "in", height = 16, width = 32, res = 300)
  print(p_consumption)
dev.off()

}

