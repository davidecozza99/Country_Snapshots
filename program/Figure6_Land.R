##----------------------------------------------------------------------------
# LAND COMPOSITION EVOLUTION
# ----------------------------------------------------------------------------
# Author: Davide Cozza (SDSN)

# Steps: 
# 1) Getting and preparing data 
# 2) Plotting data


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



# ----------------------------------------------------------------------------
# 1 DATABASE  ----------------------------------------------------------------
# ----------------------------------------------------------------------------

scenathon <- read_csv(here("data", "240523_FullDataBase.csv")) %>% 
  rename(alpha3 = country, Pathway = pathway, Year = year) %>% 
  mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  #tradeadjustment set to Yes
  filter(iteration == "5") %>% 
  #selecting interesting variables
  select(alpha3, Pathway, Year, calccropland, calcpasture, calcforest, calcnewforest, calcotherland, calcurban, newotherland, totalland, protectedareasforest, protectedareasother, protectedareasothernat) %>%
  # Creating PA variable
  mutate(PA =protectedareasforest + protectedareasother + protectedareasothernat) %>% 
  mutate(across(c(calccropland, calcpasture, calcforest, calcnewforest, calcotherland, calcurban, newotherland, totalland, PA), ~ . / 1000)) %>% 
  select(-protectedareasforest,- protectedareasother, -protectedareasothernat) %>% 
  mutate(calccropland = (ifelse(alpha3 == "SWE",
                                calccropland + newotherland,
                                calccropland))) %>% 
  mutate(newotherland = (ifelse(alpha3 == "SWE",
                                newotherland - newotherland,
                                newotherland)))


# Changing structure of the data
scenathon_long <- scenathon %>%
  pivot_longer(cols = c(calccropland,calcpasture,calcforest, calcnewforest, calcotherland,calcurban,newotherland), names_to = "LandType", values_to = "Value") %>%
  group_by(alpha3, Pathway, Year) %>% 
  mutate(Percentage = round(Value / sum(Value) * 100, 1)) %>%
  ungroup()



# ----------------------------------------------------------------------------
# 2 PLOT  --------------------------------------------------------------------
# ----------------------------------------------------------------------------

#Order of pathways
scenathon_long$Pathway <- factor(scenathon_long$Pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

#Order of Land type
scenathon_long$LandType <- factor(scenathon_long$LandType, levels = c(
  "calccropland", "calcpasture", "calcforest", "calcotherland", "calcurban", "newotherland", "calcnewforest"
))

# List countries
countries <- c(
  "ARG", "AUS", "BRA", "CAN", "CHN", "COL", "DEU", "ETH",
  "FIN", "GBR", "IDN", "IND", "MEX", "NOR", "RUS", "RWA",
  "SWE", "USA", "DNK", "GRC", "TUR", "NPL",
  "R_ASP", "R_CSA", "R_NMC", "R_OEU", "R_NEU", "R_SSA"
)


# Aesthetics
land_colors <- c(
  "calccropland" = "#B8860B",     
  "calcpasture" = "#FF4500",      
  "calcforest" = "#006400",       
  "calcnewforest" = "#9ACD32",    
  "calcotherland" = "#FFD700",    
  "calcurban" = "pink",        
  "newotherland" = "#76c4c4",     
  "totalland" = "#2E8B57"         
)

land_labels <- c(
  "calccropland" = "Cropland",
  "calcpasture" = "Pasture",
  "calcforest" = "Forest",
  "calcnewforest" = "New Forest",
  "calcotherland" = "Other Land",
  "calcurban" = "Urban",
  "newotherland" = "New Other Land",
  "totalland" = "Total Land"
  )

#Setting directory
figure_directory <- here("output", "figures", "fig6_land", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)


percent_labels <- function(x) {
  paste0(formatC(x, format = "f", digits = 0), " %")
}


# Create plot for each country
plot_list <- list()


for (country in countries) {
  # Subset data for the specific country
  country_data <- subset(scenathon_long, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway <- ggplot(country_data, aes(x = Year, y = Value, fill = LandType)) +
    geom_area(position = "stack") +
    geom_hline(yintercept = 0, linetype = "solid") +
    geom_line(aes(y = PA, color = "Protected Areas"), linetype = "dashed", size = 2) + 
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
    scale_fill_manual(values = land_colors, labels = land_labels) +
    scale_color_manual(values = c("Protected Areas" = "black")) + 
    scale_y_continuous(
      sec.axis = sec_axis(~ . / country_data$totalland * 100, name = "% total land")
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 58, face = "bold"),
      legend.title = element_blank(), 
      legend.text = element_text(family = "sans", size = 44, margin = margin(r = 1, unit = 'cm')),
      axis.title.x = element_text(color = "black", size = 44),
      axis.text.x = element_text(color = "black", size = 44),
      axis.title.y = element_text(color = "black", size = 44),
      axis.title.y.right = element_text(color = "black", size = 44, angle = 90),
      axis.text.y = element_text(color = "black", size = 44),
      legend.position = "bottom",
      legend.spacing.x = unit(3, "cm"),
      panel.spacing = unit(2, "cm"),
      axis.text.x.right = element_text(color = "black", size = 44),
      axis.line.y.left = element_line(color = "black"),
      axis.line.y.right  = element_line(color = "black"), 
      axis.ticks.y.right = element_line(color = "black"), size = 30,
      axis.ticks.y.left = element_line(color = "black"), size = 30
    ) +  guides(
      fill = guide_legend(override.aes = list(size = 10), keyheight = unit(2, "cm"), keywidth = unit(2, "cm")),
      color = guide_legend(override.aes = list(size = 10))
    )
  
  # Save the plot as a PNG file
  filename <- paste0("Fig6_", gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".png")
  png(
    filename = here(figure_directory, filename),
    units = "in", height = 16, width = 32, res = 300
  )
  print(p_pathway)
  dev.off()
}

