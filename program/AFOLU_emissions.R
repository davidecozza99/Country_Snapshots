### AFOLU Emissions by Gas type ###

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

scenathon <- read_csv(here("data", "240523_FullDataBase.csv")) %>% 
  rename(alpha3 = country, Year = year) %>% 
  mutate(pathway = recode(pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(tradeajustment == "Yes") %>%
  # filter(!Year %in% c("2000", "2005", "2010")) %>% 
  rename(calcpeatco2 = calcsequestco3) %>% 
  select(alpha3, pathway, Year, calccropn2o, calccropch4, calccropco2, calcliven2o, calclivech4, 
         ghgbiofuels,
         calcdeforco2, calcotherlucco2, 
         calcpeatco2,
         calcsequestaband, calcsequestaffor) %>% 
  # filter(Year %in% c(2015, 2020,2025,2030)) %>% 
  # select(alpha3, pathway, Year,calcdeforco2) %>% 
  # filter(alpha3 != "BRA") %>% 
  group_by(Year, pathway, alpha3) %>% 
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


#Data manipulation -------------------------------------------------------------
df_long <- melt(scenathon, id.vars = c("Year", "pathway", "alpha3"), variable.name = "Gas", value.name = "Emission") 
df_long$Gas <- as.character(df_long$Gas)
df_long <- df_long %>% 
  mutate(Gas_type = case_when(
    startsWith(Gas, "CO2") ~ "CO2",
    startsWith(Gas, "CH4") ~ "CH4",
    startsWith(Gas, "N2O") ~ "N2O",
    TRUE ~ NA_character_
  )) 


# df_long2 <- melt(scenathon, id.vars = c("Year", "pathway"), variable.name = "Gas", value.name = "Emission") 
# df_long2$Gas <- as.character(df_long2$Gas)


# df_long2 <- df_long2 %>% 
#   mutate(Gas_type = case_when(
#     startsWith(Gas, "CO2") ~ "CO2",
#     startsWith(Gas, "CH4") ~ "CH4",
#     startsWith(Gas, "N2O") ~ "N2O",
#     TRUE ~ NA_character_
#   )) %>% 
#   group_by(Year, pathway, Gas_type) %>% 
#   summarize(Emission_sum = sum(Emission, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = Gas_type, values_from = Emission_sum)
# 
# df_long_final <- left_join(df_long, df_long2) %>% 
#   select(-Gas_type)


#Preparing aesthetic for the plot ----------------------------------------------

gas_colors <- c("CH4_live" = "#943F3F", "N2O_live" = "#FF0000",
                "CH4_crop" = "#D9AB2B", "N2O_crop" = "#FFCB52",
                "CO2_crop" = "#D5E59E", "CO2_def" = "#0CA48D",
                "CO2_other" ="green","CO2_peat" = "#8A2BE2",
                "CO2_aband" = "#66BB6A",
                "CO2_affor" = "#1B5E20",
                "CO2_bio"= "grey"
)


gas_labels <- c(
  "N2O_live" = "N2O Livestock", "CH4_live" = "CH4 Livestock",
  "N2O_crop" = "N2O Crop",  "CH4_crop" = "CH4 Crop", "CO2_crop" = "CO2 on farm",
  "CO2_def" = "CO2 Deforestation", "CO2_other" ="CO2 Other Land Use",
  "CO2_aband" = "CO2 Abandoned Agr. Land", "CO2_affor" = "CO2 Afforestation", 
  "CO2_peat" = "CO2 Peatland", "CO2_bio" ="Savings from Biofuels")

# 
# gas_colors <- c(
#   "N2O_crop" = "steelblue",
#   "N2O_live" = "lightblue",
#   "CH4_crop" = "#7B1FA2",
#   "CH4_live" = "#BA68C8",
#   "CO2_crop" = "#66BB6A",
#   "CO2_def" = "#4CAF50",
#   "CO2_other" = "#388E3C",
#   "CO2_peat" = "#2E7D32",
#   "CO2_aband" = "#1B5E20",
#   "CO2_affor" = "#004D40",
#   "CO2_bio"= "#003300"
# )
# 
# 
# gas_labels <- c(
#            "N2O_crop" = "N2O from Crop", "N2O_live" = "N2O from Livestock \n",
#            "CH4_crop" = "CH4 from Crop", "CH4_live" = "CH4 from Livestock \n",
#            "CO2_crop" = "CO2 from Crop",  "CO2_def" = "CO2 from Deforestation", 
#            "CO2_other" ="CO2 from Other Land Use", "CO2_peat" = "CO2 from Peatland",
#            "CO2_aband" = "CO2 from Abandoned Agr. Land", "CO2_affor" = "CO2 from Afforestation", 
#            "CO2_bio" ="CO2 savings from Biofuels"
#   )

df_long$Gas <- factor(df_long$Gas, levels = c(
  "N2O_live", "CH4_live", 
  "N2O_crop","CH4_crop", "CO2_crop",
  "CO2_def", 
  "CO2_other", 
  "CO2_aband",   
  "CO2_affor","CO2_peat",
  "CO2_bio"   
))


#Plot --------------------------------------------------------------------------

unique_pathways <- unique(df_long$pathway)
df_long$pathway <- factor(df_long$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))


# df_long_land <- df_long %>% 
#   filter((Gas %in% c("CO2_def", "CO2_other", "CO2_peat", "CO2_aband", "CO2_affor", "CO2_bio")))
# 
# df_long_agri <- df_long %>% 
#   filter((Gas %in% c("N2O_crop", "N2O_live","CH4_crop", "CH4_live","CO2_crop")))
figure_directory <- here("output", "figures", "fig5", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

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


# Loop through each country
for (country in countries) {
  
  # Subset data for the current country
  country_data <- df_long %>% filter(alpha3 == country)
  
  # Create plot for the specific country
  p <- ggplot(country_data, aes(x = Year, y = Emission, fill = Gas)) +
    geom_area() +
    labs(
      x = "", y = "MtCO2e", fill = "")+
    scale_fill_manual(values = gas_colors, labels = gas_labels, breaks = names(gas_labels)) +
    # scale_y_continuous(breaks = seq(-10, 20, by = 5)) +
    facet_grid(. ~ pathway, scales = "free_y",
               labeller = labeller(pathway = c("CurrentTrends" = "Current Trends",
                                               "NationalCommitments" = "National Commitments",
                                               "GlobalSustainability" = "Global Sustainability"))) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "black", size = 24, face = "bold"),
      legend.title = element_text(family = "sans", color = "black", size = 18),
      legend.text = element_text(family = "sans", size = 18),
      axis.title.x = element_text(color = "black", size = 18),
      axis.title.y = element_text(color = "black", size = 18),
      legend.position = "bottom",
      panel.spacing = unit(2, "cm")
    )  +
    guides(fill = guide_legend(nrow = 2))
  
  # Save the plot as a TIFF file
  filename <- paste0(gsub("-", "", Sys.Date()), "_", gsub(" ", "_", country), ".tiff")
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 10, width = 20, res = 300
  )
  print(p)
  dev.off()
  

}


