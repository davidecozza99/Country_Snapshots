# Figure 11: Nitrogen application
# Author: Clara Douzal (SDSN)
# Last update: 20240529

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
scenathon<- read.csv(here("data", "240523_FullDataBase_expost.csv"), sep = "") %>% 
  filter(tradeadjustment == "Yes") %>% 
  filter(year %in% c("2030", "2050")) %>% 
  mutate(pathway_id = factor(pathway_id, levels = c("CT", "NC", "GS"))) %>% 
  select(pathway_id, country, year, calcn_synth, calcn_agsoils, calcn_leftpasture) %>% 
  rename(`Org. N applied to cropland` = calcn_agsoils,
         `N left on pasture` = calcn_leftpasture,
         `Synth. N applied to cropland` = calcn_synth) %>% 
  pivot_longer(cols = c(`Org. N applied to cropland`,
                        `N left on pasture`,
                        `Synth. N applied to cropland`), 
               names_to = "type", values_to = "value") %>% 
  mutate(type = factor(type, levels = c("N left on pasture", 
                                        "Org. N applied to cropland", 
                                        "Synth. N applied to cropland")))

#List countries
countries <- c(
  "ARG", "AUS", "BRA", "CAN", "CHN", "COL","DEU",
  "ETH" ,"FIN","GBR", "IDN", "IND", "MEX",
  "NOR", "RUS", "RWA","SWE",  "USA", "DNK",
  "GRC","TUR", "NPL", "R_ASP", "R_CSA", "R_NMC", "R_OEU", "R_NEU", "R_SSA"
)



figure_directory <- here("output", "figures", "Fig11_NApplication")
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

# Loop for each country

for (cur_country in countries) {
  
  country_data <- subset(scenathon, country == cur_country)
  print(summary(country_data))
  # Create plot for the specific country
  p_N <- ggplot(country_data, aes(x = as.factor(year))) +
    geom_bar(aes(y = value/1000, fill = type), stat = "identity", position = "stack", width = 0.6) +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(x="",
         y = "1000 t N",
         fill = ""
    ) +
    facet_grid(. ~ pathway_id, scales = "free_y",
               labeller = labeller(pathway_id = c(
                 "CT" = "Current Trends",
                 "NC" = "National Commitments",
                 "GS" = "Global Sustainability"
               )))+
    scale_fill_manual(values = c(`N left on pasture` = "#8B4513", 
                                 `Org. N applied to cropland` = "#FFD700", 
                                 `Synth. N applied to cropland` = "#1E90FF")) +  
    theme_minimal() +
    theme(
      text = element_text(family = "sans", "black", size = 16, face = "bold"),
      legend.text = element_text(family = "sans", size = 14),
      axis.title.x = element_blank(),
      axis.title.y = element_text(color = "black", size = 14),
      legend.position = "bottom",
      panel.spacing = unit(0.05, "cm")
    ) +
    
    guides(fill = guide_legend(nrow = 3))
  
  # Save the plot as a TIFF file
  filename <- paste0("Fig11_",format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", cur_country), ".png")
  png(
    filename = here(figure_directory, filename),
    units = "cm", height = 15.8, width = 17.4, res = 300)
  print(p_N)
  dev.off()
  
}
