### LNPP ###


# libraries ---------------------------------------------------------------
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
library(scales)

conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)
here()


#Data -------------------------------------------------------------------
scenathon<- read.csv(here("data", "240523_FullDataBase_expost.csv"), sep = "") %>% 
  rename(alpha3 = country, Pathway = pathway, Year = year) %>% 
  filter(tradeadjustment == "Yes") %>%
  filter(Year %in% c("2030", "2050")) %>%
  select(alpha3,Pathway, Year, lnppmatureforest_expost, lnppmatureotherland_expost,	lnppnewforest, lnppnewotherland) 
  

scenathon_long <- scenathon %>%
  pivot_longer(cols = c(lnppmatureforest_expost, lnppmatureotherland_expost,	lnppnewforest, lnppnewotherland), names_to = "LandType", values_to = "Value") %>%
  ungroup() %>% 
  mutate(Value = Value/1000)


# Aesthetics ------------------------------------------------------------------
lnpp_colors <- c(
  "lnppmatureforest_expost" = "#006400",
  "lnppmatureotherland_expost" = "#FFD700",
  "lnppnewforest" = "#9ACD32",
  "lnppnewotherland" = "#FF4500"
)

lnpp_labels <- c(
  "lnppmatureforest_expost" = "LNPP in mature forests",
  "lnppmatureotherland_expost" = "LNPP in mature other \nnatural lands",
  "lnppnewforest" = "LNPP in new forest",
  "lnppnewotherland" = "LNPP in new other \nnatural land"
)


#Plot Pathway ---------------------------------------------------------------
scenathon_long$Pathway <- factor(scenathon_long$Pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

# List countries
countries <- c(
  "ARG", "AUS", "BRA", "CAN", "CHN", "COL", "DEU", "ETH",
  "FIN", "GBR", "IDN", "IND", "MEX", "NOR", "RUS", "RWA",
  "SWE", "USA", "DNK", "GRC", "TUR", "NPL",
  "R_ASP", "R_CSA", "R_NMC", "R_OEU", "R_NEU", "R_SSA"
)


figure_directory <- here("output", "figures", "Figure10_LNPP", format(Sys.Date(),format = "%y%m%d"))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

# Create plot for each country
plot_list <- list()


for (country in countries) {
  # Subset data for the specific country
  country_data <- subset(scenathon_long, alpha3 == country)

  # Create ggplot for the specific country
  p_pathway <- ggplot(country_data, aes(x = as.factor(Year), y = Value, fill = LandType)) +
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(
      x = "",
      y = "Mha", fill = ""
    ) +
    facet_grid(. ~ Pathway, scales = "free_y",
               labeller = labeller(Pathway = c(
                 "CurrentTrends" = "Current Trends",
                 "NationalCommitments" = "National Commitments",
                 "GlobalSustainability" = "Global Sustainability"
               ))) +
    scale_fill_manual(values = lnpp_colors, labels = lnpp_labels) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "sans", color = "black", size = 16),
      legend.text = element_text(family = "sans", size = 14),
      axis.title.x = element_text(color = "black", size = 14),
      axis.title.y = element_text(color = "black", size = 14),
      legend.position = "bottom",
      panel.spacing = unit(0.05, "cm")
    )  + guides(
      fill = guide_legend(override.aes = list(size = 5), keyheight = unit(1, "cm"), keywidth = unit(1, "cm"),  nrow = 2),
      color = guide_legend(override.aes = list(size = 5))
    )
  

  # Save the plot as a TIFF file
  filename <- paste0("Fig10_", format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", country), ".png")
  png(
    filename = here(figure_directory, filename),
    units = "in", height = 5, width = 7, res = 300)

  print(p_pathway)
  dev.off()
  }
