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
scenathon <- read_csv(here("data", "240523_FullDataBase.csv")) %>% 
  rename(alpha3 = country, Pathway = pathway, Year = year) %>% 
  mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(iteration == "5") %>% 
  filter(!Year %in% c("2000", "2005", "2010", "2015", "2025", "2035", "2045")) %>%
  select(alpha3,Pathway, Year, agroecosh, calccropland) %>% 
  mutate(agroecland = agroecosh*calccropland/1000 ) %>% 
  mutate(nonagroecland = (1-agroecosh)*calccropland/1000) %>% 
  filter(!Year %in% c("2000", "2005", "2010", "2015")) 




product <- read.csv(here("data",  "240523_FullProductDataBase.csv")) %>% 
  rename(Pathway = pathway) %>% 
  filter(tradeadjustment == "Yes") %>% 
  filter(!year %in% c("2000", "2005", "2010", "2015", "2025", "2035", "2045")) %>%
  select(country, Pathway, year, workersfte, fertilizercost, labourcost, machineryrunningcost,dieselcost, pesticidecost) %>% 
  group_by(country, Pathway, year)  %>% 
  summarise(
    workersfte = sum(workersfte, na.rm = T),
    fertilizercost = sum(fertilizercost, na.rm = T),
    labourcost = sum(labourcost, na.rm = T),
    machineryrunningcost = sum(machineryrunningcost, na.rm = T),
    dieselcost = sum(dieselcost, na.rm = T),
    pesticidecost = sum(pesticidecost, na.rm = T)
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
  "dieselcost" = "#8A2BE2", 
  "pesticidecost" = "#FF69B4"
)

# Define the labels
costs_labels <- c(
  "workersfte" = "Full-Time Equivalent Workers",
  "fertilizercost" = "Fertilizer Cost",
  "labourcost" = "Labour Cost",
  "machineryrunningcost" = "Machinery Running Cost",
  "dieselcost" = "Diesel Cost",
  "pesticidecost" = "Pesticide Cost"
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


figure_directory <- here("output", "figures", "costs", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

# Create plot for each country
plot_list <- list()

for (country in countries) {
  # Subset data for the specific country
  country_data <- subset(scenathon_long, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway <- ggplot(country_data, aes(x = as.factor(Year), y = Value, fill = LandType)) +
    geom_bar(stat = "identity", position = "stack") +
    # geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 5, color = "ivory") +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(
      x = "",
      y = "1000 USD", fill = ""
    ) +
    facet_grid(. ~ Pathway, scales = "free_y",
               labeller = labeller(Pathway = c(
                 "CurrentTrends" = "Current Trend Pathway",
                 "NationalCommitments" = "National Commitments Pathway",
                 "GlobalSustainability" = "Global Sustainability Pathway"
               ))) +
    scale_fill_manual(values = costs_colors, labels = costs_labels) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 24, face = "bold"),
      legend.title = element_text(family = "sans", color = "black", size = 18),
      legend.text = element_text(family = "sans", size = 18),
      axis.title.x = element_text(color = "black", size = 18),
      axis.title.y = element_text(color = "black", size = 18),
      legend.position = "bottom",
      panel.spacing = unit(2, "cm")
    )
  
  # Save the plot as a TIFF file
  filename <- paste0(gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".tiff")
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 10, width = 20, res = 300
  )
  print(p_pathway)
  dev.off()
  
}
