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
scenathon<- read.csv(here("data", "240523_FullDataBase_expost.csv"), sep = "") %>% 
  filter(tradeadjustment == "Yes") %>% 
  filter(year %in% c("2030", "2050")) %>% 
  mutate( pathway_id = factor(pathway_id, levels = c("CT", "NC", "GS"))) %>% 
  select(pathway_id, country, year, calcn_synth, calcn_agsoils, calcn_leftpasture) %>% 
  rename(`Application of organic fertilizers` = calcn_agsoils,
         `Manure left on pastures` = calcn_leftpasture,
         `Application of synthetic fertilizers` = calcn_synth) %>% 
  pivot_longer(cols = c(`Application of organic fertilizers`,
                        `Manure left on pastures`,
                        `Application of synthetic fertilizers`), 
               names_to = "type", values_to = "value")

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



figure_directory <- here("output", "figures", "Figure8_NApplication")
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

# Loop for each country

for (country in countries) {
  
  country_data <- subset(scenathon, country == country)
  
  # Create plot for the specific country
  p_N <- ggplot(country_data, aes(x = as.factor(pathway_id))) +
    geom_bar(aes(y = value/1000, fill = type), stat = "identity", position = "stack", width = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(x="",
         y = "N Application (1000 t N)",
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
  print(p_N)
  #dev.off()
  
}
