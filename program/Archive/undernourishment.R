### Undernourishment ###

# Libraries ---------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(conflicted)
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(reshape2)
library(gridExtra)


conflict_prefer("filter", "dplyr")

here()

#Data extraction ---------------------------------------------------------------
scenathon<- read.csv(here("data", "240523_FullDataBase_expost.csv"), sep = "") %>% 
  filter(tradeadjustment == "Yes") %>%
  rename(alpha3 = country, Year = year, Pathway = pathway) %>% 
  select(alpha3, Pathway, Year, pou_computed) %>% 
  filter(Year %in% c("2030", "2050")) 
  
scenathon <- read_table(here("data", "240523_FullDataBase_expost.csv"))



scenathon$Pathway <- factor(scenathon$Pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))




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



figure_directory <- here("output", "figures", "fig3", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)




for (country in countries) {
  
  country_data <- subset(scenathon, alpha3 == country)
  
  # Create plot for the specific country
  p_consumption <- ggplot(country_data, aes(x = as.factor(Year))) +
    geom_point(aes(y = kcal_pou)) +
    labs(x="",
         y = "Consumption (kcal/cap/day)",
         fill = ""
    ) +
    facet_grid(. ~ Pathway, scales = "free_y",
               labeller = labeller(Pathway = c(
                 "CurrentTrends" = "Current Trends",
                 "NationalCommitments" = "National Commitments",
                 "GlobalSustainability" = "Global Sustainability"
               ))) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", "black", size = 10, face = "bold"),
      # legend.title = element_text(family = "sans", color = "steelblue", size = 16, face = "bold"),
      legend.text = element_text(family = "sans", size = 8),
      # plot.title = element_text(color = "steelblue", size = 16, face = "bold"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(color = "black", size = 8),
      legend.position = "bottom",
      panel.spacing = unit(0.05, "cm")
    ) 
  # Save the plot as a TIFF file
  filename <- paste0(gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".tiff")
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 5, width = 5.2, res = 300)
  print(p_consumption)
  dev.off()
  
}





