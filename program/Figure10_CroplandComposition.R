## cropland composition

# libraries ---------------------------------------------------------------
library(here)
#library(plyr)
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
  #filter(year %in% c("2030", "2050")) %>% 
  mutate( pathway_id = factor(pathway_id, levels = c("CT", "NC", "GS"))) %>% 
  select(pathway_id, country, product, year, feasplantarea)

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
  #group_by(country, pathway_id, year) %>% 
  reframe(
    feasplantarea = sum(feasplantarea, na.rm = T),
    .by = c(pathway_id, country, product, year)
  ) 
  

df_planted <- top_planted %>% 
  rbind(rest_planted) %>% 
  filter(year %in% c(2030, 2050))

df_planted2 <- top_planted %>% 
  rbind(rest_planted) %>% 
  filter(year > 2015) 

figure_directory <- here("output", "figures", "Figure10_CroplandComposition")
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

# Loop for each country

for (cur_country in countries) {
  
  country_data <- df_planted %>% 
    filter(country == cur_country)
  
  # Create plot for the specific country
  p_crop <- ggplot(country_data, aes(x = as.factor(pathway_id))) +
    geom_bar(aes(y = feasplantarea, fill = product), stat = "identity", position = "stack", width = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(x="",
         y = "Cropland composition (1000 ha)",
         fill = ""
    ) +
    #scale_y_continuous(breaks = seq(0, max(country_data$kcalfeasprod_productgroup + 2000), 250)) +
    facet_grid(. ~ year, scales = "free_y"#,
               # labeller = labeller(pathway = c(
               #   "CurrentTrends" = "Current Trend",
               #   "NationalCommitments" = "National Commitments",
               #   "GlobalSustainability" = "Global Sustainability"
               #))
) +
    #scale_fill_manual(values = product_colors, labels = product_labels) +  
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
  # filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", country), ".tiff")
  # tiff(
  #   filename = here(figure_directory, filename),
  #   units = "in", height = 7, width = 24, res = 300)
  print(p_crop)
  #dev.off()
  
}



##### other figure proposition

for (cur_country in countries) {
  
  country_data <- df_planted2 %>% 
    filter(country == cur_country) 
  
  # Create plot for the specific country
  p_crop <- ggplot(country_data %>% 
                     mutate(product = factor(product, 
                                             levels = c(setdiff(unique(country_data$product), "Rest of cropland"), 
                                                        "Rest of cropland")))) +
    geom_area(aes(x = year, y = feasplantarea/1000, fill = product)) +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(x="",
         y = "Cropland composition (Mha)",
         fill = ""
    ) +
    scale_x_continuous(breaks = c(2020, 2050)) +
    #scale_fill_manual()
    facet_grid(. ~ pathway_id, scales = "free_y",
               labeller = labeller(pathway_id = c(
                 "CT" = "Current\nTrend",
                 "NC" = "National\nCommitments",
                 "GS" = "Global\nSustainability"
               ))
    ) +
    scale_fill_manual(values = c("orange", "yellow", "brown", "green", "maroon", "grey")) +  
    theme_minimal() +
    theme(
      text = element_text(family = "sans", "black", size = 18, face = "bold"),
      # legend.title = element_text(family = "sans", color = "steelblue", size = 16, face = "bold"),
      legend.text = element_text(family = "sans", size = 18),
      # plot.title = element_text(color = "steelblue", size = 16, face = "bold"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(color = "black", size = 18),
      legend.position = "bottom",
      panel.spacing = unit(1, "cm"),
      plot.margin = margin(0,0.5,0,0, "cm")
    ) +
    
    guides(fill = guide_legend(nrow = 2))
  
  # Save the plot as a TIFF file
  filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", cur_country), ".tiff")
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 4.75, width = 6.2, res = 300)
  print(p_crop)
  dev.off()
  
}
