## Prevalence of undernourishment

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
  filter(year %in% c("2030")) %>% 
  mutate( pathway_id = factor(pathway_id, levels = c("CT", "NC", "GS"))) %>% 
  select(pathway_id, country, year, pou_computed) 
  

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



figure_directory <- here("output", "figures", "Figure6_PoU")
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)

myLinetype <- c("dotted", "solid")
names(myLinetype) <- c("2.5%", "5%")


# Loop for each country

for (cur_country in countries) {
  
  country_data <- subset(scenathon, country == cur_country) %>% 
    mutate(pou_computed = ifelse(is.na(pou_computed), 2.49, pou_computed)) %>% 
    mutate(pou_display = ifelse(is.na(pou_computed), "<2.5", pou_computed))
  
  CT_kcal_tot <- country_data %>% filter(pathway_id == "CT") %>% pull(pou_display) %>% unique() %>% round()
  NC_kcal_tot <- country_data %>% filter(pathway_id == "NC") %>% pull(pou_display) %>% unique() %>% round()
  GS_kcal_tot <- country_data %>% filter(pathway_id == "GS") %>% pull(pou_display) %>% unique() %>% round()
  
  
  cat.labs <- c(CT = paste0("Current Trends\n2030\n", CT_kcal_tot, " %"),
                NC = paste0("National Commitments\n2030\n", NC_kcal_tot, " %"),
                GS = paste0("Global Sustainability\n2030\n", GS_kcal_tot, " %"))
  
  # Create plot for the specific country
  p_POU <- ggplot(data = country_data) +
    geom_col(aes(x = 1, y = pou_computed, fill = "Prevalence of \nUndernourishment"),
             position = "identity",
             width = 1
             # ,
             # show.legend = F
             ) +
    geom_col(aes(x = 1, y = rep(2.5, 3), fill = "<2.5%"),
             position = "identity",
             width = 1
             # ,
             # show.legend = F
             ) +
      scale_fill_manual(values = c("Prevalence of \nUndernourishment" = "orange", "<2.5%" = "grey"), name = NULL)+
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(limits = c(0, max(1.25*max(country_data$pou_computed), 10)))+
    coord_polar() +
    facet_grid(pathway_id ~ ., 
               switch = "y",
               labeller = labeller(pathway_id = cat.labs)
               ) +
    geom_hline(aes(yintercept = 2.5,
                     linetype = "2.5%")) +
    geom_hline(aes(yintercept = 5,
                     linetype = "5%")) +
    scale_linetype_manual(values = myLinetype, name = NULL) +
    #guides(colour = guide_legend(override.aes = list(linetype = 0, shape=''))) +
    # guides(fill=guide_legend(nrow=5),
    #        colour=guide_legend(nrow = 2),
    #        linetype = guide_legend(nrow = 2)) +
    theme_minimal() +
    labs(x = NULL, 
         y = NULL) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.caption = element_text(hjust = 0.15, 
                                      color = "#666666", face= "sans"),
          #panel.spacing = unit(-2.5, "lines"),
          strip.text = element_text(size = 15, family = "sans"),
          strip.text.y.left = element_text(angle = 0),
          strip.placement = "outside"
          # ,
          # plot.margin = margin(t = -10, b= -20)
          )
  
  # Save the plot as a TIFF file
  filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", cur_country), ".tiff")
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 5, width = 5.5, res = 300)
  print(p_POU)
  dev.off()
  
}
