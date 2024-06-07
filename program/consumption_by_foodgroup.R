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


#Data -------------------------------------------------------------------
scenathon<- read_csv(here("data", "240523_FullProductDataBase.csv")) %>% 
  rename(alpha3 = country, Pathway = pathway, Year = year, Product = product) %>% 
  mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(iteration == "5") %>% 
  filter(Year %in% c("2020", "2030", "2040", "2050")) %>%
  select(alpha3,Pathway, Year, Product, kcalfeasprod)

mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

consumption <- scenathon %>% 
  left_join(mapping, by ="Product") %>% 
  group_by(Pathway, alpha3, Year, PROD_GROUP) %>% 
  mutate(kcalfeasprod_productgroup = sum(kcalfeasprod)) %>% 
  ungroup()  %>% 
  group_by(Pathway, alpha3, Year) %>% 
  mutate(total_kcal = sum(kcalfeasprod)) %>% 
  ungroup() %>%
  select(-Product, -kcalfeasprod) %>%
  # filter(alpha3 == country) %>%
  unique %>% 
  drop_na()


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
# FOOD_missing <- merge(FOOD_missing, expanded_data, by = "PROD_GROUP") %>% 
#   unique()
# 
# 
# 
# consumption_final <- consumption %>% 
#   rbind(FOOD_missing)






#Plot ---------------------------------------------------------

consumption$Pathway <- factor(consumption$Pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))


product_colors <- c(
  "BEVSPICES" = "#8B0000",    
  "CEREALS" = "#FFD700",      
  "EGGS" = "#00008B",         
  "FIBERINDUS" = "#8B4513",   
  "FRUVEG" = "#8A2BE2",        
  "MILK" = "#006400",         
  "NUTS" = "#8FBC8F",          
  "OLSCAKE" = "#A52A2A",      
  "OLSOIL" = "steelblue",        
  "PORK" = "#DC143C",         
  "POULTRY" = "#4B0082",
  "PULSES" = "#F17CB0",
  "REDMEAT" = "#B22222",       
  "ROOTS" = "#DAA520",        
  "SUGAR" = "#FF4500"       
)

product_labels <- c(
  "BEVSPICES" = "Beverages and Spices",
  "CEREALS" = "Cereals",
  "EGGS" = "Eggs",
  "FIBERINDUS" ="Fiber and Industrial Crops",
  "FRUVEG" = "Fruits & Vegetables",
  "MILK" ="Milk",
  "NUTS" = "Nuts",
  "OLSCAKE" = "Oil Cakes",
  "OLSOIL" = "Oilseeds and veg. oils",
  "PORK" = "Pork",
  "POULTRY" = "Poultry",
  "PULSES" = "Pulses",
  "REDMEAT" = "Beef, Goat & Lambs",
  "ROOTS" = "Roots and Tubers",
  "SUGAR" ="Sugar and Sugar Crops"
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



figure_directory <- here("output", "figures", "consum_foodgroup", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)
# Loop for each country

for (country in countries) {
  
  country_data <- subset(consumption, alpha3 == country)
  
  # Create plot for the specific country
  p_consumption <- ggplot(country_data, aes(x = as.factor(Year))) +
    geom_bar(aes(y = kcalfeasprod_productgroup, fill = PROD_GROUP), stat = "identity", position = "stack", width = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(x="",
      y = "Consumption (kcal/cap/day)",
      fill = ""
    ) +
    scale_y_continuous(breaks = seq(0, max(country_data$kcalfeasprod_productgroup + 2000), 250)) +
    facet_grid(. ~ Pathway, scales = "free_y",
               labeller = labeller(Pathway = c(
                 "CurrentTrends" = "Current Trend Pathway",
                 "NationalCommitments" = "National Commitments Pathway",
                 "GlobalSustainability" = "Global Sustainability Pathway"
               ))) +
    scale_fill_manual(values = product_colors, labels = product_labels) +  
    theme_minimal() +
    theme(
      text = element_text(family = "sans", "black", size = 18, face = "bold"),
      # legend.title = element_text(family = "sans", color = "steelblue", size = 16, face = "bold"),
      legend.text = element_text(family = "sans", size = 18),
      # plot.title = element_text(color = "steelblue", size = 16, face = "bold"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(color = "black", size = 18),
      legend.position = "bottom",
      panel.spacing = unit(2, "cm")
    ) +

  guides(fill = guide_legend(nrow = 3))
  
  # Save the plot as a TIFF file
filename <- paste0(gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".tiff")
tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 7, width = 24, res = 300)
print(p_consumption)
dev.off()

}
