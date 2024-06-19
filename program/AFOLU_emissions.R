##----------------------------------------------------------------------------
# AFOLU Emission evolution by gas and source 
# ----------------------------------------------------------------------------
# Author: Davide Cozza (SDSN)

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


# Steps: 
# 1) Getting and preparing data 
# 2) Plotting data



# ----------------------------------------------------------------------------
# 1 DATABASE  ----------------------------------------------------------------
# ----------------------------------------------------------------------------

scenathon <- read_csv(here("data", "240523_FullDataBase.csv")) %>% 
  rename(alpha3 = country, Year = year) %>% 
  mutate(pathway = recode(pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(tradeajustment == "Yes") %>%
  filter(!Year %in% c("2000", "2005")) %>%
  rename(calcpeatco2 = calcsequestco3) %>% 
  select(alpha3, pathway, Year, calccropn2o, calccropch4, calccropco2, calcliven2o, calclivech4, 
         ghgbiofuels,
         calcdeforco2, calcotherlucco2, 
         calcpeatco2,
         calcsequestaband, calcsequestaffor) %>% 
  group_by(Year, pathway, alpha3) %>% 
  #aggregating emissions
  mutate(N2O_crop = sum(calccropn2o)) %>% 
  mutate(CH4_crop = sum(calccropch4)) %>% 
  mutate(CO2_crop = sum(calccropco2)) %>% 
  mutate(N2O_live = sum(calcliven2o)) %>% 
  mutate(CH4_live = sum(calclivech4)) %>% 
  mutate(CO2_def = sum(calcdeforco2)) %>% 
  mutate(CO2_other = sum(calcotherlucco2)) %>%
  mutate(calcpeatco2 = as.numeric(calcpeatco2)) %>%
  mutate(CO2_peat = sum(calcpeatco2, na.rm = TRUE)) %>% 
  mutate(CO2_aband = sum(calcsequestaband)) %>% 
  mutate(CO2_affor = sum(calcsequestaffor)) %>% 
  mutate(CO2_bio = sum(ghgbiofuels)) %>% 
  select(-alpha3, -calccropn2o, -calccropch4, -calccropco2, -calcliven2o, -calclivech4, -calcdeforco2, -calcotherlucco2, - ghgbiofuels, 
         -calcpeatco2,
         -calcsequestaband, -calcsequestaffor) %>% 
  distinct() 


# Changing structure of the database
df_long <- melt(scenathon, id.vars = c("Year", "pathway", "alpha3"), variable.name = "Gas", value.name = "Emission") 
df_long$Gas <- as.character(df_long$Gas)
df_long <- df_long %>% 
  mutate(Gas_type = case_when(
    startsWith(Gas, "CO2") ~ "CO2",
    startsWith(Gas, "CH4") ~ "CH4",
    startsWith(Gas, "N2O") ~ "N2O",
    TRUE ~ NA_character_
  )) 




# ----------------------------------------------------------------------------
# 2 PLOT  --------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Aesthetics
gas_colors <- c("CH4_live" = "#943F3F", "N2O_live" = "#FF0000",
                "CH4_crop" = "#D9AB2B", "N2O_crop" = "#FFCB52",
                "CO2_crop" = "#D5E59E", "CO2_def" = "#0CA48D",
                "CO2_other" ="green","CO2_peat" = "#8A2BE2",
                "CO2_aband" = "#66BB6A",
                "CO2_affor" = "#1B5E20",
                "CO2_bio"= "grey")

gas_labels <- c(
  "N2O_live" = "N2O Livestock", "CH4_live" = "CH4 Livestock",
  "N2O_crop" = "N2O Crop",  "CH4_crop" = "CH4 Crop", "CO2_crop" = "CO2 from Farm",
  "CO2_def" = "CO2 Deforestation", "CO2_other" ="CO2 Other Land Use",
  "CO2_aband" = "CO2 Abandoned Agr. Land", "CO2_affor" = "CO2 Afforestation", 
  "CO2_peat" = "CO2 Peatland", "CO2_bio" ="Savings from Biofuels")


df_long$Gas <- factor(df_long$Gas, levels = c(
  "N2O_live", "CH4_live", 
  "N2O_crop","CH4_crop", "CO2_crop",
  "CO2_def", 
  "CO2_other", 
  "CO2_aband",   
  "CO2_affor","CO2_peat",
  "CO2_bio"   
))


#Order of pathways
unique_pathways <- unique(df_long$pathway)
df_long$pathway <- factor(df_long$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))


#Setting directory
figure_directory <- here("output", "figures", "fig8_ghg", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)


#List countries
countries <- c(
  "ARG", "AUS", "BRA", "CAN", "CHN", "COL", "DEU",
  "ETH", "FIN", "GBR", "IDN", "IND","MEX", "NOR",
  "RUS", "RWA", "SWE", "USA", "DNK", "GRC", "TUR", "NPL",
  "R_ASP", "R_CSA", "R_NMC", "R_OEU", "R_NEU", "R_SSA"
)


# Loop through each country
for (country in countries) {
  
  # Subset data for the current country
  country_data <- df_long %>% filter(alpha3 == country)
  
  # Create plot for the specific country
  p <- ggplot(country_data, aes(x = Year, y = Emission, fill = Gas)) +
    geom_area() +
    labs(
      x = "", y = "MtCO2e", fill = "") +
    scale_fill_manual(values = gas_colors, labels = gas_labels, breaks = names(gas_labels)) +
    # scale_y_continuous(breaks = seq(-10, 20, by = 5)) +
    facet_grid(. ~ pathway, scales = "free_y",
               labeller = labeller(pathway = c("CurrentTrends" = "Current Trends",
                                               "NationalCommitments" = "National Commitments",
                                               "GlobalSustainability" = "Global Sustainability"))) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 58, face = "bold"),
      legend.text = element_text(family = "sans", size = 42, margin = margin(r = 1.2, unit = 'cm')),
      axis.title.x = element_text(color = "black", size = 38),
      axis.text.x = element_text(color = "black", size = 32),
      axis.title.y = element_text(color = "black", size = 38),
      axis.text.y = element_text(color = "black", size = 32),
      legend.position = "bottom",
      panel.spacing = unit(2, "cm")
    )  +
  guides(
    fill = guide_legend(override.aes = list(size = 10), keyheight = unit(2, "cm"), keywidth = unit(2, "cm")),
    color = guide_legend(override.aes = list(size = 10))
  ) +
    theme(legend.spacing.x = unit(1,'cm'))
  
  # Save the plot as a PNG file
  filename <- paste0("Fig8_", gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".png")
  png(
    filename = here(figure_directory, filename),
    units = "in", height = 16, width = 32, res = 300
  )
  print(p)
  dev.off()
  
}


