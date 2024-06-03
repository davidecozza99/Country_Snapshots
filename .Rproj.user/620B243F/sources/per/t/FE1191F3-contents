
# Figure 3: Historical values from the UNFCCC gas inventory
# Author: Clara Douzal (SDSN), updated by Davide Cozza (SDSN)
# Last update: 20240529

###### NEXT STEPS
# IMPROVE FIGURE 
# CHECK 

libraries <- c("tidyr", "dplyr", "ggplot2", "reshape2", "RColorBrewer", 
               "conflicted", "cowplot", "patchwork", "egg", "ggforce", "readxl", 
               "grid", "scales", "wesanderson", "tidyverse", "latex2exp", 
               "stringr", "shadowtext", "here", "webr")
lapply(libraries, library, character.only = TRUE)

require(moonBook)
require(webr)

conflict_prefer("arrange", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)
here()
conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")


here()

# Data --------------------------------------------------------------------
data <- read.csv(here("data", "240523_FullDataBase.csv")) %>% 
  filter(tradeajustment == "Yes")

product <- read.csv(here("data",  "240523_FullProductDataBase.csv")) %>% 
  rename(Pathway = pathway) %>% 
  filter(tradeadjustment == "Yes")


#UNFCCC data for figure 3
data_F3 <- as.data.frame(read_csv(here("data","Figure 3", "20230720_HomePageISO.csv"))) 
data_F3$Category <- factor(data_F3$Category, levels = c("AFOLU", "Waste", "Energy", "IPPU", "Other"))

# 
# country_abbreviations <- c(
#   "Argentina" = "ARG",
#   "Australia" = "AUS",
#   "Brazil" = "BRA",
#   "Canada" = "CAN",
#   "China" = "CHN",
#   "Colombia" = "COL",
#   "Ethiopia" = "ETH",
#   "Finland" = "FIN",
#   "Germany" = "DEU",
#   "India" = "IND",
#   "Indonesia" = "IDN",
#   "Mexico" = "MEX",
#   "Norway" = "NOR",
#   "Nepal" = "NPL",
#   "Türkiye" = "TUR",
#   "Denmark" = "DNK",
#   "Greece" = "GRC",
#   "Russia" = "RUS",
#   "Rwanda" = "RWA",
#   "Sweden" = "SWE",
#   "UK" = "GBR",
#   "United States of America" = "USA"
# )
# 
# data_F3 <- data_F3 %>%
#   mutate(Country = case_when(
#     Country %in% names(country_abbreviations) ~ country_abbreviations[Country],
#     TRUE ~ as.character(Country)
#   )) 




# Define the output directory
figure_directory <- here("output", "figures", "figure3", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
print(figure_directory)




# if(Country == "Colombia"){
#   data_F3 <- as.data.frame(read_excel(here("data","Figure 3", "Historical_data_COL.xlsx"), 
#                                       sheet = "Feuil1"))
#   data_F3$Category <- factor(data_F3$Category, levels = c("AFOLU", "Waste", "Energy", "IPPU", "Other"))
#   
# }


# Colour palettes ---------------------------------------------------------
myColors_GHG_AFOLU <- c("AFOLU" = "#8F4B07",
                        "Waste" = "#8C8C80",
                        "Energy" = "#DBDBCA",
                        "IPPU"= "#DFDE7F",
                        "Other" = "#615D42")

myColors_AFOLU <- colorRampPalette(brewer.pal(8, "Set1"))(nlevels(as.factor(data_F3[which(data_F3$Category == "AFOLU"), "Sub.Category"])))
names(myColors_AFOLU) <- levels(as.factor(data_F3[which(data_F3$Category == "AFOLU"), "Sub.Category"]))




myColorsCO2AFOLU <- c("#B84D50", wes_palette("Darjeeling1", n = 4)[c(2,4)], "#FFC748","#91341d", "#145a32", "#7d7979") 

names(myColorsCO2AFOLU) <- c("CalcLiveAllCO2e", "CalcDeforCO2", 
                             "CalcOtherLUCCO2","CalcCropAllCO2e", "CalcSequestCO2",
                             "CalcPeatCO2","GHGbiofuels")

countries <- c(
  #  "ARG",
  # "AUS"
  # ,
  # "BRA"
  # , "CAN", "CHN", "COL","DEU",
  # "ETH"
  # ,"FIN","GBR", "IDN", "IND",
  "MEX"
  # ,"NOR", "RUS", "RWA","SWE",  "USA",
  # "DNK",
  # "GRC","TUR", "NPL"
)

# mex, bra and eth to do separetely from the others

ALPHA3 <- "MEX"
# Loop over each country
for (ALPHA3 in countries) {
  
  
  
  data_country <- droplevels(data[which(data$country == ALPHA3),])
  product_country <- droplevels(product[which(product$country == ALPHA3),])
  
  # if(country != "Colombia"){
  data_F3_country <- droplevels(data_F3[which(data_F3$Country == ALPHA3),])
  # }else{
  # data_F3 <- droplevels(data_F3)}
  
  
  
  my_data <- data_F3_country
  
  
  
  
  my_data_full <- my_data
  if( length(which(my_data[,"Mt.CO2.equivalent"]== 0)>0)){
    my_data_full <- my_data <- droplevels(my_data[-which(my_data[,"Mt.CO2.equivalent"]== 0),])
  }
  #create dataframe for negative values
  if(length(which(my_data[,"Mt.CO2.equivalent"]<0))>0){
    my_data_neg <- my_data[which(my_data[,"Mt.CO2.equivalent"]<0),]
    my_data <- my_data[-which(my_data[,"Mt.CO2.equivalent"]<0),]
    my_data_neg$Amount <- -my_data_neg[,"Mt.CO2.equivalent"] / sum(my_data[,"Mt.CO2.equivalent"])#to have right unit"
    
  }
  
  #Data for the donut chart is percenatges
  my_data$Amount <- my_data[,"Mt.CO2.equivalent"] / sum(my_data[,"Mt.CO2.equivalent"])
  my_data <- my_data[order(my_data$Category),]
  
  #used to put the middle of the AFOLU segment at cos(x) = 1 and sin(x) = 0
  temp <- sum(my_data$Amount[which(my_data$Category == "AFOLU")])/2
  
  #Data used for the donut chart
  my_data_aug <- my_data %>%
    group_by(Category) %>% 
    summarise(Amount = sum(Amount)) %>%
    mutate(arc_start = cumsum(lag(Amount, default = 0)) * 2*pi + pi/2 -temp*2*pi, 
           arc_end   = cumsum(Amount) * 2*pi + pi/2 - temp*2*pi,
           x_pos = 0 + cos(arc_start - pi/2),
           y_pos = 1 - sin(arc_start - pi/2),
           middle = 0.5 * (arc_start + arc_end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  my_data_aug <- data.frame(my_data_aug)
  
  #data used for the emissions bar chart from AFOLU
  my_data_detail <- my_data %>%
    filter(Category == "AFOLU") %>%
    arrange(`Sub.Category`) %>%
    purrr::map_df(rev) %>%    # This reverses the order
    mutate(Amount_scaled = Amount / sum(Amount) * 2) %>% 
    data.frame()
  my_data_detail <- my_data_detail %>% 
    group_by(Sub.Category) %>% 
    mutate(Amount_scaled = sum(Amount_scaled, na.rm = T),
           Amount = sum(Amount, na.rm = T),
           Mt.CO2.equivalent = sum(Mt.CO2.equivalent, na.rm = T)) %>% 
    select(Category, Sub.Category, Mt.CO2.equivalent, Amount, Amount_scaled) %>% 
    unique() %>% 
    data.frame()
  
  
  if(exists("my_data_neg")){
    #data used for the removals bar chart from AFOLU
    my_data_detail_neg <- my_data_neg %>%
      filter(Category == "AFOLU") %>%
      arrange(`Sub.Category`) %>%
      purrr::map_df(rev) %>%    # This reverses the order
      mutate(Amount_scaled = Amount / sum(my_data$Amount[which(my_data$Category == "AFOLU")]) * 2) %>% 
      data.frame()
    
    my_data_detail_neg <- my_data_detail_neg %>% 
      group_by(Sub.Category) %>% 
      mutate(Amount_scaled = sum(Amount_scaled, na.rm = T),
             Amount = sum(Amount, na.rm = T),
             Mt.CO2.equivalent = sum(Mt.CO2.equivalent, na.rm = T)) %>% 
      select(Category, Sub.Category, Mt.CO2.equivalent, Amount, Amount_scaled) %>% 
      unique() %>% 
      data.frame()
  }
  
  #the lines going from extremities of the AFOLU segment in the donut to the extremities of the emissions from AFOLU bar chart
  my_data_lines <- my_data_aug %>%
    filter(Category == "AFOLU" | lag(Category == "AFOLU")) %>%
    slice(1, n())
  
  if(my_data_lines$x_pos[1]<0){
    my_data_lines$x_pos <- c(0,0)
    my_data_lines$y_pos <- c(2,0)
  }
  
  
  # Total Emissions displayed in the middle of the donut
  
  tot_Emission_All <- TeX(paste0(round(sum(my_data[,"Mt.CO2.equivalent"])), 'MtCO$_{',2,'}e'), output = "character")
  
  
  # Building the legend  -----------------------------------
  
  #keeping only the necessary categories for your country
  myColors_GHG_AFOLU <- myColors_GHG_AFOLU[my_data_aug$Category]
  
  p <- ggplot(my_data_aug)
  
  data_bar_emission <- my_data_detail %>% 
    mutate(percent = Amount_scaled/2) %>% 
    slice(which(percent>0.01)) %>% 
    droplevels() %>% 
    data.frame()
  
  
  text_donut <- c("AFOLU" = "white",
                  "Waste" = "black",
                  "Energy" = "black",
                  "IPPU"= "black",
                  "Other" = "black")
  
  # if(country != "Colombia"){#colour palettes for countries that are not Colombia
  
  myColors_AFOLU <- c("Enteric Fermentation" = "#D1492C",
                      "Drained organic soils (CO2)" = "blue",
                      "Manure Management" = "#E18248",
                      "Rice Cultivation" = "#F0BA63",
                      "Agricultural Soils" = "#C21111",
                      "Other (Agriculture)" = "#FFF27E",
                      "Grassland" = "#c6e065",
                      "CO2 Emissions and Removals from Soil"= "#76c4c4" ,
                      "Harvested Wood Products" = "#734339",
                      "Cropland" = "#96325f",
                      "Wetlands" = "#bf2cac",
                      "Forest and Grassland Conversion" = "#143b13",
                      "Settlements" = "#9ea39e",
                      "Forest Land" = "#147314",
                      "Changes in Forest and Other Woody Biomass Stocks" = "#d65a62",
                      "Abandonment of Managed Lands" = "#2563ba",
                      "Other (Forest & LUC)" = "#83a1cc",
                      "Land-Use Change and Forestry" = "#61543f")
  
  myColors_text <- c("Enteric Fermentation" = "white",
                     "Drained organic soils (CO2)" = "blue",
                     "Manure Management" = "black",
                     "Rice Cultivation" = "black",
                     "Agricultural Soils" = "white",
                     "Other (Agriculture)" = "black",
                     "Grassland" = "black",
                     "CO2 Emissions and Removals from Soil"= "black" ,
                     "Harvested Wood Products" = "white",
                     "Cropland" = "white",
                     "Wetlands" = "white",
                     "Forest and Grassland Conversion" = "white",
                     "Settlements" = "black",
                     "Forest Land" = "white",
                     "Changes in Forest and Other Woody Biomass Stocks" = "white",
                     "Abandonment of Managed Lands" = "white",
                     "Other (Forest & LUC)" = "black",
                     "Land-Use Change and Forestry" = "white")
  
  order_emissions = c("Agricultural Soils",
                      "Enteric Fermentation",
                      "Manure Management",
                      "Rice Cultivation",
                      "Other (Agriculture)",
                      "CO2 Emissions and Removals from Soil" ,
                      "Grassland",
                      "Harvested Wood Products",
                      "Cropland",
                      "Forest and Grassland Conversion",
                      "Land-Use Change and Forestry",
                      "Settlements",
                      "Wetlands",
                      "Other (Forest & LUC)")
  
  order_removals = c("Abandonment of Managed Lands", 
                     "Changes in Forest and Other Woody Biomass Stocks",
                     "CO2 Emissions and Removals from Soil" ,
                     "Forest Land",
                     "Grassland",
                     "Harvested Wood Products",
                     "Land-Use Change and Forestry",
                     "Other (Forest & LUC)")

  #only keep the necessary sub categories for your country
  myColors_AFOLU <- myColors_AFOLU[unique(my_data_full[which(my_data_full$Category == "AFOLU"),"Sub.Category"])]
  myColors_AFOLU_legend_emissions <- myColors_AFOLU[unique(data_bar_emission$Sub.Category)]
  
  data_bar_emission <- data_bar_emission %>% 
    mutate(Sub.Category = factor(Sub.Category, levels = order_emissions)) %>% 
    droplevels()
  
  p_bar_emissions <- p + 
    geom_col(data = data_bar_emission, aes(x = `Sub.Category`, y = `Mt.CO2.equivalent`, fill = `Sub.Category`)) + 
    scale_fill_manual(name = "Source of AFOLU \nEmissions",
                      values = myColors_AFOLU,
                      labels = str_wrap(levels(data_bar_emission$Sub.Category),22)) + 
    theme(plot.margin = unit(c(-10, 0, -10, 0), "cm"),
          legend.spacing.x = unit(0.2, 'cm'),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20))
  
  
  legend_emissions <- cowplot::get_legend(p_bar_emissions)
  
  if(exists("my_data_neg")){
    data_bar_emission_neg <- my_data_detail_neg %>% 
      mutate(percent = Amount_scaled/2) %>% 
      slice(which(percent>0.01)) %>% 
      droplevels() %>% 
      data.frame()

    myColors_AFOLU_legend_removals <- myColors_AFOLU[unique(data_bar_emission_neg$Sub.Category)]
    
    p_bar_removals <- p + geom_col(data = data_bar_emission_neg, aes(x = `Sub.Category`, y = `Mt.CO2.equivalent`, fill = `Sub.Category`)) + 
      scale_fill_manual(name = "Sink for AFOLU \nRemovals",
                        values = myColors_AFOLU,
                        labels = str_wrap(names(myColors_AFOLU_legend_removals)[order(names(myColors_AFOLU_legend_removals))],22)) + 
      theme(plot.margin = unit(c(-10, 0, -10, 0), "cm"),
            legend.spacing.x = unit(0.2, 'cm'),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 20))
    
    
    legend_removals <- cowplot::get_legend(p_bar_removals)
    
  }
  
  
  
  # Creation of plot to display ---------------------------------------------
  
  #If only emissions from AFOLU for your country
  
  p <- ggplot(my_data_aug)
  
  #Need to adjust the coordinates for the lines that link the donut to the bar chart depending on the country
  df_yend <- data.frame(Country = c("ARG","AUS","BRA","CAN","CHN","COL","ETH","FIN","DEU","IND",
                                    "IDN","MEX","NOR","NPL","TUR","DNK","GRC","RUS","RWA","SWE",
                                    "GBR","USA"),
                        lines_right_top = rep(1.75, 22),
                        lines_right_bottom = rep(0.2, 22))
  
  
  
  
  placeholder_top <- 0
  placeholder_bottom <- 0
  
  if(ALPHA3 %in% c("CAN","CHN","COL", 
                    "FIN", "DEU", "IDN", "NOR", 
                    "RUS", "RWA", "SWE", "GBR", "USA")){
    placeholder        <- (sum(my_data_detail_neg$Amount_scaled)- sum(my_data_detail$Amount_scaled))
    placeholder_top    <- (placeholder + sum(my_data_detail$Amount_scaled))/sum(my_data_detail_neg$Amount_scaled)
    placeholder_bottom <- (placeholder/sum(my_data_detail_neg$Amount_scaled)) 
  }
  
  df_yend <- df_yend %>% 
    #change the y coordinate for the top right end of the segment that links the donut with the bar chart
    mutate(lines_right_top = ifelse(Country == "UK",         ################ TO UNDERSTAND AND UPDATE #############
                                    1.75,
                                    ifelse(Country == "RWA",
                                           0.4 + placeholder_top,
                                           ifelse(Country == "FIN",
                                                  0.35 + placeholder_top,
                                                  ifelse(Country %in% c("SWE", "NOR", "RUS"),
                                                         0.2 + placeholder_top,
                                                         ifelse(Country == "USA",
                                                                0.5 + placeholder_top,
                                                                ifelse(Country == "Malaysia",
                                                                       0.05 + placeholder_top,
                                                                       ifelse(Country %in% c("ARG", "BRA", "ETH",
                                                                                             "IND", "MEX"),
                                                                              2,
                                                                              lines_right_top)))))))) %>% 
    #change the y coordinate for the bottom right end of the segment that links the donut with the bar chart
    mutate(lines_right_bottom = ifelse(Country %in% c("FIN", "NOR", 
                                                      "RUS", "RWA", "SWE", "USA"),
                                       placeholder_bottom,
                                       ifelse(Country %in% c("ARG", "BRA", "ETH",
                                                             "IND", "MEX"),
                                              0,
                                              lines_right_bottom)))
  #keep only the data for the country we are currently plotting
  df_yend <- df_yend %>% 
    slice(which(Country == ALPHA3))
  #Need to adjust the y coordinates of the text in the donut
  df_y_textdonut <- data.frame(Country = c("ARG","AUS","BRA","CAN","CHN","COL","ETH","FIN","DEU","IND",
                                           "IDN","MEX","NOR","NPL","TUR","DNK","GRC","RUS","RWA","SWE",
                                           "GBR","USA"),
                               AFOLU_t = rep(0, 22),
                               Waste_t = rep(0, 22),
                               Energy_t = rep(0, 22),
                               IPPU_t = rep(0, 22))
  
  df_y_textdonut <- df_y_textdonut %>% 
    mutate(AFOLU_t = ifelse(Country == "Malaysia",
                            -0.035,
                            AFOLU_t)) %>% 
    mutate(Waste_t = ifelse(Country %in% c("RUS", "DEU"),
                            -0.05,
                            ifelse(Country == "CHN",
                                   -0.085,
                                   ifelse(Country %in% c("RWA", "USA"),
                                          -0.1,
                                          Waste_t)))) %>% 
    mutate(IPPU_t = ifelse(Country %in% c("RWA", "USA"),
                           0.05,
                           IPPU_t))
  #keep only the data for the country we are currently plotting
  df_y_textdonut <- df_y_textdonut %>% 
    slice(which(Country == ALPHA3))
  
  #plot the donut of all sectors net emissions
  p_donut <- p + geom_arc_bar(aes(x0 = 0, y0 = 1,  
                                  r0 = 0.52, r  = 1,
                                  fill = Category,
                                  start = arc_start,
                                  end   = arc_end), color = NA, data = my_data_aug, show.legend = F) +
    scale_fill_manual(name = "Source of Emissions", 
                      values = myColors_GHG_AFOLU)+
    geom_text(aes(x = 0.75*sin(middle), 
                  y = 1 + 0.75*cos(middle) + c(df_y_textdonut$AFOLU_t, df_y_textdonut$Waste_t, df_y_textdonut$Energy_t, df_y_textdonut$IPPU_t),
                  label = paste0(Category, "\n", round(Amount* 100, 1), "%"),
                  color = Category),
              size = 6,
              show.legend = F)+
    scale_color_manual(values = text_donut)+
    annotate("segment", 
             x = my_data_lines[1:2, "x_pos"] +ifelse(ALPHA3 == "RWA", 0.25, 0),
             y = my_data_lines[1:2, "y_pos"],
             xend = 1.5,
             yend = c(df_yend$lines_right_top,
                      df_yend$lines_right_bottom)) + 
    annotate("text", x = 0, y = 1, label = tot_Emission_All, size = 8.5, parse = TRUE)+
    coord_equal() +
    theme(plot.margin = unit(c(-2, -2, -2, -2), "cm"))+
    theme_void()+
    theme(plot.background = element_rect(fill = "White"))
  
  my_data_detail$Sub.Category <- factor(my_data_detail$Sub.Category,
                                        levels = order_emissions)
  my_data_detail <- my_data_detail[rev(order(my_data_detail$Sub.Category)),]
  
  my_data_detail <- my_data_detail %>% 
    mutate(label = ifelse(Amount_scaled/sum(Amount_scaled)>0.13, 
                          TeX(paste0(round(Mt.CO2.equivalent,0), "MtCO$_{",2,"}e"), output = "character"), 
                          NA)) %>%
    data.frame()
  #plot the bar chart for emisions from AFOLU
  p_bar <- p + geom_tile(data = my_data_detail,
                         aes(x = 1, 
                             y = cumsum(Amount_scaled) - Amount_scaled/2,
                             height = Amount_scaled, fill = Sub.Category),
                         show.legend = F,
                         color = "white") +
    geom_text(data = my_data_detail,
              aes(x = 1, 
                  y = cumsum(Amount_scaled) - Amount_scaled/2,
                  label = label,
                  color = Sub.Category),
              parse = T,
              size = 6,
              show.legend = F)+
    scale_color_manual(values = myColors_text)+
    scale_fill_manual(name = "Source of AFOLU \nEmissions and \nRemovals",
                      values = myColors_AFOLU,
                      labels = str_wrap(names(myColors_AFOLU[(my_data_detail[,"Sub.Category"])[order(my_data_detail[,"Sub.Category"])]]),27))+
    ylim(0,max(sum(my_data_detail$Amount_scaled)))+
    coord_equal() +
    theme_void()+
    labs(title = TeX(paste0(round(sum(my_data_detail[,"Mt.CO2.equivalent"]), 0), "MtCO$_{",2,"}e")))+
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),  # Adjust margins as needed
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.background = element_rect(fill = "white", color = "white"))+           
    xlab("Emissions")+ 
    ylab(NULL)
  

  
  
  
  #If there are emissions & removals from AFOLU for your country
  if(exists("my_data_neg")){
    #plot the bar chart for emisions from AFOLU
    p_bar <- p + geom_tile(data = my_data_detail,
                           aes(x = 1,
                               y = cumsum(Amount_scaled) - Amount_scaled/2,
                               height = Amount_scaled, fill = Sub.Category),
                           show.legend = F) +
      geom_text(data = my_data_detail,
                aes(x = 1,
                    y = cumsum(Amount_scaled) - Amount_scaled/2,
                    label = label,
                    color = Sub.Category),
                parse = T,
                size = 6,
                show.legend = F)+
      scale_color_manual(values = myColors_text)+
      scale_fill_manual(values = myColors_AFOLU)+
      ylim(0,max(sum(my_data_detail_neg$Amount_scaled),
                 2))+
      coord_equal()+
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
            plot.background = element_rect(fill = "White"),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            plot.title = element_text(size = 20, hjust = 0.5),
            axis.title.x = element_text(size = 20),
            panel.background = element_blank()) +
      xlab("Emissions") +
      ylab(NULL) +
      labs(title = TeX(paste0(round(sum(my_data_detail[,"Mt.CO2.equivalent"]), 0), "MtCO$_{",2,"}e")))
    
    #add a label variable for sub categories that contributes enough to be readable
    my_data_detail_neg <- my_data_detail_neg %>%
      mutate(label = ifelse(Amount_scaled>0.20, TeX(paste0(round(Mt.CO2.equivalent,0), "MtCO$_{",2,"}e"), output = "character"), NA)) %>%
      data.frame()
    
    #nee to define the y axis range
    ylim_max <- ifelse(sum(my_data_detail_neg$Amount_scaled)>2,
                       sum(my_data_detail_neg$Amount_scaled),
                       2)
    #plot the bar chart for removals from AFOLU
    p_bar_neg <- p + geom_tile(data = my_data_detail_neg,
                               aes(x = 1,
                                   y = cumsum(Amount_scaled) - Amount_scaled/2,
                                   height = Amount_scaled, fill = Sub.Category),
                               show.legend = F) +
      geom_text(data = my_data_detail_neg,
                aes(x = 1,
                    y = cumsum(Amount_scaled) - Amount_scaled/2,
                    label = label,
                    color = Sub.Category),
                parse = T,
                size = 6,
                show.legend = F) +
      scale_color_manual(values = myColors_text)+
      scale_fill_manual(values = myColors_AFOLU)+
      ylim(0,max(sum(my_data_detail_neg$Amount_scaled),
                 2))+
      coord_equal()+
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
            plot.background = element_rect(fill = "White"),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            plot.title = element_text(size = 20, hjust = 0.5),
            axis.title.x = element_text(size = 20),
            panel.background = element_blank())+
      xlab("Removals")+
      labs(title = TeX(paste0("  ",round(sum(my_data_detail_neg[,"Mt.CO2.equivalent"]), 0), "MtCO$_{",2,"}e")))+
      ylab(NULL)
    
    #If your country has more removal than emissions
    if(sum(my_data_detail_neg$Amount_scaled)>2){
      #plot the bar chart for removals from AFOLU
      p_bar_neg <- p + geom_tile(data = my_data_detail_neg,
                                 aes(x = 1,
                                     y = cumsum(Amount_scaled) - Amount_scaled/2,
                                     height = Amount_scaled, fill = Sub.Category),
                                 show.legend = F) +
        geom_text(data = my_data_detail_neg,
                  aes(x = 1,
                      y = cumsum(Amount_scaled) - Amount_scaled/2,
                      label = label,
                      color = Sub.Category),
                  parse = T,
                  size = 6,
                  show.legend = F) +
        scale_color_manual(values = myColors_text)+
        scale_fill_manual(values = myColors_AFOLU)+
        ylim(0,sum(my_data_detail_neg$Amount_scaled))+
        theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
              plot.background = element_rect(fill = "White"),
              axis.ticks = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              plot.title = element_text(size = 20, hjust = 0.5),
              axis.title.x = element_text(size = 20),
              panel.background = element_blank())+
        xlab("Removals")+
        labs(title = TeX(paste0("  ",round(sum(my_data_detail_neg[,"Mt.CO2.equivalent"]), 0), "MtCO$_{",2,"}e")))+
        ylab(NULL)
      
      #Placeholder needed to center the emissions bar chart so that it looks nicer
      placeholder <- (sum(my_data_detail_neg$Amount_scaled)- sum(my_data_detail$Amount_scaled))/sum(my_data_detail$Amount_scaled) -0.2
      
      temp_levels <- levels(as.factor(my_data_detail$Sub.Category))
      myColors_AFOLU <- c(myColors_AFOLU, "Z" = "white")
      
      #temp dataframe to add later to the dataframe ploted
      vect_temp <- my_data_detail[1,] %>% 
        mutate(Category = "AFOLU",
               Sub.Category = "Z",
               Amount_scaled = placeholder)
      
      my_data_detail <- my_data_detail %>% 
        rbind(vect_temp) %>% 
        mutate(Sub.Category = factor(Sub.Category, levels = c(temp_levels)))  
        #reorder to have the SubCategory Z first  
        my_data_detail <- my_data_detail[c(which(my_data_detail$Sub.Category == "Z"), which(my_data_detail$Sub.Category != "Z")),]
      #add a label variable for sub categories that contributes enough to be readable
      my_data_detail <- my_data_detail %>%
        mutate(label = ifelse((Amount_scaled/sum(my_data_detail_neg$Amount_scaled) > 0.15), TeX(paste0(round(Mt.CO2.equivalent,0), "MtCO$_{",2,"}e"), output = "character"), NA)) %>%
        data.frame()
      
      #plot the bar chart for emissions from AFOLU
      p_bar <- p + geom_tile(data = my_data_detail,
                             aes(x = 1,
                                 y = cumsum(Amount_scaled) - Amount_scaled/2,
                                 height = Amount_scaled,
                                 fill = Sub.Category),
                             show.legend = F,
                             color = "white") +
        geom_text(data = my_data_detail,
                  aes(x = 1,
                      y = cumsum(Amount_scaled) - Amount_scaled/2,
                      label = label,
                      color = Sub.Category),
                  parse = T,
                  size = 6,
                  show.legend = F) +
        scale_color_manual(values = myColors_text)+
        scale_fill_manual(values = myColors_AFOLU)+
        ylim(0,sum(my_data_detail_neg$Amount_scaled))+
        theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
              plot.background = element_rect(fill = "White"),
              axis.ticks = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              plot.title = element_text(size = 20, hjust = 0.5),
              axis.title.x = element_text(size = 20),
              panel.background = element_blank())+
        xlab("Emissions")+
        labs(title = TeX(paste0(round(sum(my_data_detail[,"Mt.CO2.equivalent"]), 0), "MtCO$_{",2,"}e")))+
        ylab(NULL)
      
    }
    
    #merge the two bars plot together
    p_bars <- plot_grid(p_bar ,
                        p_bar_neg,
                        nrow = 1,
                        align = "hv",
                        axis = "bl")
    

  }
  #legend if only emissions from AFOLU
  legend <- legend_emissions
  
  if(exists("my_data_neg")){
    #legend if emissions and reomvals from AFOLU
    legend <- plot_grid(legend_emissions,
                        legend_removals,
                        align = "hv",
                        nrow = 2,
                        rel_heights = if(ALPHA3 != "Colombia") c(1.6, 1.4) else c(2, 1))
  }
  
  
  #Merge donut and bar plots together if only emissions from AFOLU
  p_figure <- ggarrange(p_donut + 
                          theme(plot.margin = margin(r = -15, l = -50),
                                plot.background = element_rect(fill = "White")),  
                        p_bar + 
                          theme(plot.margin = margin(l = 0, r = -40),
                                plot.background = element_rect(fill = "White")), nrow = 1, widths = c( 3, 1.1))
  
  #add the legend to the plot
  p_final <- plot_grid(grid.arrange(
    grobs = list(p_figure, legend), 
    widths = c(2,1.5,1.85),
    heights = c(1,8,1),
    layout_matrix = rbind(c(NA, NA, 2),
                          c(1, 1, 2),
                          c(NA, NA, 2))))
  width_plot = 7.35
  
  
  if(exists("my_data_neg")){
    #Merge donut and bar plots together if emissions and removals from AFOLU
    p_figure <- ggarrange(p_donut +
                            theme(plot.margin = margin(r = -15, l = -50),
                                  plot.background = element_rect(fill = "White")),
                          p_bars +
                            theme(plot.margin = margin(l = 0, r = -20),
                                  plot.background = element_rect(fill = "White")),
                          nrow = 1,
                          widths = c( 2.2, 1.8))
    
    
    #add the legend to the plot
    p_final <- plot_grid(grid.arrange(
      grobs = list(p_figure, legend),
      widths = c(2.40, 0.4, 1.2, 1.8),
      heights = c(1,8,1),
      layout_matrix = rbind(c(NA, NA, NA, 2),
                            c(1, 1, 1, 2),
                            c(NA, NA, NA, 2))))
    

    
  }

  
  # Save the plot as TIFF file
  filename <- paste0(gsub("-", "", Sys.Date()), "_", gsub(" ", "_", ALPHA3), ".tiff")
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 6, width = 16, res = 600
  )
  print(p_final)
  dev.off()
  
}




