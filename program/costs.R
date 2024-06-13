## Costs

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
product <- read.csv(here("data",  "240523_FullProductDataBase.csv")) %>% 
  rename(Pathway = pathway) %>% 
  filter(tradeadjustment == "Yes") %>% 
  filter(!year %in% c("2000", "2005", "2010", "2015", "2020", "2025", "2035", "2040", "2045")) %>%
  select(country, Pathway, year, workersfte, fertilizercost, labourcost, machineryrunningcost,dieselcost, pesticidecost) %>% 
  group_by(country, Pathway, year)  %>% 
  summarise(
    workersfte = sum(workersfte, na.rm = T),
    fertilizercost = sum(fertilizercost, na.rm = T)/1000,
    labourcost = sum(labourcost, na.rm = T)/1000,
    machineryrunningcost = sum(machineryrunningcost, na.rm = T)/1000,
    dieselcost = sum(dieselcost, na.rm = T)/1000,
    pesticidecost = sum(pesticidecost, na.rm = T)/1000
  ) %>% 
  group_by(country, Pathway, year)  %>% 
  mutate(
  fertilizercost = if_else(country == "CHN", fertilizercost/1000, fertilizercost ),
  labourcost = if_else(country == "CHN", labourcost/1000, labourcost),
  machineryrunningcost = if_else(country == "CHN", machineryrunningcost/1000, machineryrunningcost),
  dieselcost = if_else(country == "CHN", dieselcost/1000, dieselcost),
  pesticidecost = if_else(country == "CHN", pesticidecost/1000, pesticidecost)
)


scenathon_long <- product %>%
  pivot_longer(cols = c(fertilizercost, labourcost, machineryrunningcost,dieselcost, pesticidecost), names_to = "CostType", values_to = "Value") %>%
  group_by(country, Pathway, year) %>% 
  mutate(Percentage = round(Value / sum(Value) * 100, 1)) %>%
  ungroup()






# Define the colors
costs_colors <- c(
  "workersfte" = "#FFD700", 
  "fertilizercost" = "#FF4500", 
  "labourcost" = "#1E90FF", 
  "machineryrunningcost" = "#32CD32", 
  "dieselcost" = "brown", 
  "pesticidecost" = "#8A2BE2"
)

# Define the labels
costs_labels <- c(
  "workersfte" = "Full-Time Equivalent Workers",
  "fertilizercost" = "Fertilizer",
  "labourcost" = "Labour",
  "machineryrunningcost" = "Machinery Running",
  "dieselcost" = "Diesel",
  "pesticidecost" = "Pesticide"
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


figure_directory <- here("output", "figures", "fig12_costs", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

# Create plot for each country
plot_list <- list()

for (curr in countries) {
  # Subset data for the specific country
  country_data <- subset(scenathon_long, curr == country)
  
  y_axis_label <- ifelse(curr == "CHN", "billion USD", "million USD")
  
  
  # Create ggplot for the specific country
  p_pathway <- ggplot(country_data, aes(x = as.factor(year), y = Value, fill = CostType)) +
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    geom_hline(yintercept = 0, linetype = "solid") +
    geom_point(aes(x = as.factor(year), y = workersfte / max(workersfte) * max(Value), group = 1, shape = "Workers FTE"), color = "black", size = 3) +
    scale_y_continuous(sec.axis = sec_axis(~ . * max(country_data$workersfte) / max(country_data$Value), name = "workers FTE")) +
    labs(
      x = "",
      y = y_axis_label, fill = ""
    ) +
    facet_grid(. ~ Pathway, scales = "free_y",
               labeller = labeller(Pathway = c(
                 "CurrentTrends" = "Current Trends",
                 "NationalCommitments" = "National Commitments",
                 "GlobalSustainability" = "Global Sustainability"
               ))) +
    scale_fill_manual(values = costs_colors, labels = costs_labels) +
    scale_shape_manual(name = "", values = c("Workers FTE" = 18), labels = c("Workers FTE")) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 14, face = "bold"),
      # legend.title = element_text(family = "sans", color = "black", size = 16),
      legend.text = element_text(family = "sans", size = 13),
      # axis.title.x = element_text(color = "black", size = 14),
      axis.title.y = element_text(color = "black", size = 13),
      axis.title.y.right = element_text(color = "black", size = 13, angle = 90),
      legend.position = "bottom",
      panel.spacing = unit(0.05, "cm")
    ) + 
    guides(
      fill = guide_legend(override.aes = list(shape = NA), byrow = TRUE, nrow = 2),
      shape = guide_legend(override.aes = list(color = "black", size = 3))
    )
  
  # Save the plot as a TIFF file
  filename <- paste0("Fig12_", gsub("-", "", Sys.Date()), "_", gsub(" ", "_", curr), ".png")
  png(
    filename = here(figure_directory, filename),
    units = "cm", height = 15.8, width = 17.4, res = 300)
  print(p_pathway)
  dev.off()
  
}

