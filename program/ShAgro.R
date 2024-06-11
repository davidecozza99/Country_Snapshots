## Share of cropland under agroecological practises

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
scenathon <- read_csv(here("data", "240523_FullDataBase.csv")) %>% 
  rename(alpha3 = country, Pathway = pathway, Year = year) %>% 
  mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(iteration == "5") %>% 
  filter(!Year %in% c("2000", "2005", "2010", "2015")) %>%
select(alpha3,Pathway, Year, agroecosh) 


# 
# scenathon_long <- scenathon %>%
#   pivot_longer(cols = c(agroecosh), names_to = "LandType", values_to = "Value") %>%
#   ungroup()





#Plot Pathway ---------------------------------------------------------------
scenathon$Pathway <- factor(scenathon$Pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

# List countries
countries <- c(
  "ARG", "AUS", "BRA", "CAN", "CHN", "COL", "DEU", "ETH",
  "FIN", "GBR", "IDN", "IND", "MEX", "NOR", "RUS", "RWA",
  "SWE", "USA", "DNK", "GRC", "TUR", "NPL",
  "R_ASP", "R_CSA", "R_NMC", "R_OEU", "R_NEU", "R_SSA"
)


figure_directory <- here("output", "figures", "fig6", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

# Create plot for each country
plot_list <- list()

pathway_colors <- c("CurrentTrends" = "#76c4c4", "NationalCommitments" = "#B22222", "GlobalSustainability" = "#FFCB52")

# Get unique countries
countries <- unique(scenathon$alpha3)

# Loop over countries
for (country in countries) {
  # Subset data for the specific country
  country_data <- subset(scenathon, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway <- ggplot(country_data, aes(x = Year, y = agroecosh, color = Pathway)) +
    geom_line() +
    labs(
      x = "Year",
      y = "Agroecosh",
      color = "Pathway",
      title = paste("Agroecosh by Year and Pathway -", country)
    ) +
    scale_color_manual(values = pathway_colors) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 10, face = "bold"),
      legend.title = element_text(family = "sans", color = "black", size = 8),
      legend.text = element_text(family = "sans", size = 8),
      axis.title.x = element_text(color = "black", size = 8),
      axis.title.y = element_text(color = "black", size = 8),
      legend.position = "bottom",
      panel.spacing = unit(0.05, "cm")
    )
  
  # Save the plot as a TIFF file
  filename <- paste0(gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".tiff")
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 5, width = 5.2, res = 300
  )
  print(p_pathway)
  dev.off()
}
















# 
# 
# for (country in countries) {
#   # Subset data for the specific country
#   country_data <- subset(scenathon_long, alpha3 == country)
#   
#   # Create ggplot for the specific country
#   p_pathway <- ggplot(country_data, aes(x = as.factor(Year), y = Value, fill = LandType)) +
#     geom_bar(stat = "identity", position = "stack") +
#     geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 5, color = "ivory") +
#     geom_hline(yintercept = 0, linetype = "solid") +
#     labs(
#       x = "",
#       y = "Mha", fill = ""
#     ) +
#     facet_grid(. ~ Pathway, scales = "free_y",
#                labeller = labeller(Pathway = c(
#                  "CurrentTrends" = "Current Trends",
#                  "NationalCommitments" = "National Commitments",
#                  "GlobalSustainability" = "Global Sustainability"
#                ))) +
#     scale_fill_manual(values = c("nonagroecland" = "#9ACD32", "agroecland" = "#006400"),
#                       labels = c("Cropland under conventional practices", "Cropland under agroecological practices")) +
#     theme_minimal() +
#     theme(
#       text = element_text(family = "sans", color = "black", size = 10, face = "bold"),
#       legend.title = element_text(family = "sans", color = "black", size = 8),
#       legend.text = element_text(family = "sans", size = 8),
#       axis.title.x = element_text(color = "black", size = 8),
#       axis.title.y = element_text(color = "black", size = 8),
#       legend.position = "bottom",
#       panel.spacing = unit(0.05, "cm")
#     )
#   
#   # Save the plot as a TIFF file
#   filename <- paste0(gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".tiff")
#   tiff(
#     filename = here(figure_directory, filename),
#     units = "in", height = 5, width = 5.2, res = 300)
# 
#   print(p_pathway)
#   dev.off()
#   
#   }
# 
# 
# p_pathway
# 
# 
# 
# 
