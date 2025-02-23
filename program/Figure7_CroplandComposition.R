# Figure 11: Evolution of the cropland composition 2000-2050
# Author: Clara Douzal (SDSN)
# Last update: 20240529

# libraries ---------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(ggplot2)
library(conflicted)


conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)
here()


#Data -------------------------------------------------------------------
scenathon<- read.csv(here("data", "240523_FullProductDataBase_expost.csv"), sep = "") %>% 
  filter(tradeadjustment == "Yes") %>% 
  mutate( pathway_id = factor(pathway_id, levels = c("CT", "NC", "GS"))) %>% 
  select(pathway_id, country, product, year, feasplantarea)

#List countries
countries <- c(
  "ARG", "AUS", "BRA", "CAN", "CHN", "COL","DEU",
  "ETH","FIN","GBR", "IDN", "IND",
  "MEX", "NOR", "RUS", "RWA", "SWE","USA",
  "DNK", "GRC","TUR", "NPL",
  "R_ASP", "R_CSA", "R_NMC", "R_OEU", "R_NEU", "R_SSA"
)


## #Main crops ###
  
get_top_products <- function(country_data) {
  # Calculate top products by planted area 
top_crops <- country_data %>%
  group_by(product) %>%
  summarise(total_planted = sum(feasplantarea, na.rm = TRUE)) %>%
  arrange(desc(total_planted)) %>%
  slice_head(n = 5) %>%
  pull(product)

# Filter data for top products
country_data %>%
  filter(product %in% top_crops)
}
# Apply the function for each country
top_planted <- lapply(split(scenathon, scenathon$country), get_top_products) %>%
  bind_rows()

# Group by Pathway and Year
top_planted <- top_planted %>%
  group_by(country, pathway_id, year) %>%
  arrange(desc(feasplantarea)) %>%
  ungroup()

rest_planted <- scenathon %>% 
  setdiff(top_planted) %>% 
  mutate(product = "Rest of cropland") %>% 
  reframe(
    feasplantarea = sum(feasplantarea, na.rm = T),
    .by = c(pathway_id, country, product, year)
  ) 
  

df_planted <- top_planted %>% 
  rbind(rest_planted)  


figure_directory <- here("output", "figures", "Figure7_CroplandComposition", format(Sys.Date(),format = "%y%m%d"))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

## labels

crops_labels <- c(
  "palmkernelcake" = "Palm kernel cake",
  "soyabean" = "Soyabean",
  "corn" = "Corn",
  "wheat" = "Wheat",
  "soycake" = "Soy cake",
  "palm_oil" = "Palm oil",
  "oilpalmfruit" = "Oil palm fruit",
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
  "millet" = "Millet",
  "peas" = "Peas",
  "rapecake" = "Rape cake",
  "apple" = "Apple",
  "pork" = "Pork",
  "cocoa" = "Cocoa",
  "cotton" = "Cotton",
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
  "sweet_potato" = "Sweet Potato",
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
  "pepper" = "Pepper",
  "haysilage" = "Haysilage",
  "sugarcane" = "Sugarcane"
)


for (cur_country in countries) {
  
  country_data <- df_planted %>% 
    filter(country == cur_country) 
  
  # Create plot for the specific country
  p_crop <- ggplot(country_data %>% 
                     mutate(product = factor(product, 
                                             levels = c(setdiff(unique(country_data$product), "Rest of cropland"), 
                                                        "Rest of cropland")))) +
    geom_area(aes(x = year, y = feasplantarea/1000, fill = product)) +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(x="",
         y = "Mha",
         fill = ""
    ) +
    scale_x_continuous(breaks = c(2000, 2020, 2050)) +
    facet_grid(. ~ pathway_id, scales = "free_y",
               labeller = labeller(pathway_id = c(
                 "CT" = "Current Trends",
                 "NC" = "National Commitments",
                 "GS" = "Global Sustainability"
               ))
    ) +
    scale_fill_manual(values = c("orange", "yellow", "brown", "green", "maroon", "grey"),
                      labels = crops_labels) +  
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 58, face = "bold"),
      legend.text = element_text(family = "sans", size = 44, margin = margin(r = 1, unit = 'cm')),
      axis.text.x = element_text(color = "black", size = 44),
      axis.text.y = element_text(color = "black", size = 44),
      axis.title.x = element_text(color = "black", size = 44),
      axis.title.y = element_text(color = "black", size = 44),
      legend.position = "bottom",
      panel.spacing = unit(2, "cm"),
      plot.margin = margin(0,0.5,0,0, "cm")
    ) +guides(
      fill = guide_legend(override.aes = list(size = 10), keyheight = unit(2, "cm"), keywidth = unit(2, "cm")),
      color = guide_legend(override.aes = list(size = 10))
    )
  
  
  # Save the plot as a png file
  filename <- paste0("Fig7_", format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", cur_country), ".png")
  png(
    filename = here(figure_directory, filename),
    units = "in", height = 16, width = 32, res = 300)
  
    print(p_crop)
  dev.off()
  
}
