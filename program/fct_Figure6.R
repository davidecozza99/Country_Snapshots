
#################################################################################
#                        TO FILL BY COUNTRY TEAM                                #
#################################################################################

# "Argentina", "Australia", "Brazil", "Canada",
# "China", "Colombia", "Ethiopia", "Finland",
# "Germany", "India", "Indonesia", "Malaysia",
# "Mexico", "Norway", "Russia", "Rwanda",
# "Sweden", "SouthAfrica", "UK", "USA"


libraries <- c("tidyr", "dplyr", "ggplot2", "reshape2", "RColorBrewer", 
               "conflicted", "cowplot", "patchwork", "egg", "readxl", 
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






# data --------------------------------------------------------------------
data <- read.csv(here("data", "240523_FullDataBase.csv")) 

if (!is.factor(data$pathway)) {
  data$pathway <- as.factor(data$pathway)
}


product <- read.csv(here("data",  "240523_FullProductDataBase.csv")) %>% 
  rename(Pathway = pathway)

FAO_F6 <- read_excel(here("data", "Figure 6", "FAO_2015.xlsx"))   ################## TO BE CHANGED WHEN HAVING THE NEW DATABASE

mapping_F6 <- read_excel(here("data", "DataForFoodFigures.xlsx"), 
                         sheet = "prod groups map") %>% 
  rename(product = PRODUCT)

product<- product %>% 
  inner_join (mapping_F6, by ="product") %>% 
  unique 


EAT_data <- read_excel(here("data", "DataForFoodFigures.xlsx"), ###################### ???????????????
                       sheet = "EAT-LANCET", range = "A3:J17") 

FOOD_missing <- read_excel(here("data","Figure 6", "MissingFoodProducts.xlsx"), sheet = "Figure6")



data <- droplevels(data[which(data$country == "AUS"),])
product <- droplevels(product[which(product$country == "AUS"),])

FAO_F6 <- droplevels(FAO_F6[which(FAO_F6$Country == "Australia"),])
FOOD_missing <- droplevels(FOOD_missing[which(FOOD_missing$Country == "Australia"),])





fct_Figure6 <- function(data, product, mapping_F6, EAT_data, FOOD_missing, outpath, FAO_F6, country){
  
  
  # EAT-Lancet country specific recommendation ------------------------------
  
  #recommended total daily kcal
  kcal_fixed <- sum(EAT_data$kcal)
  
  #empty database to fill 
  EAT_data_fill <- data.frame(matrix(NA, 
                                     nrow = nlevels(data$pathway)*nrow(EAT_data), 
                                     ncol = 5)) 
  colnames(EAT_data_fill) <- c("Pathway", "PROD_GROUP","kcal_adj", "minkcal_adj","maxkcal_adj")
  EAT_data_fill <- EAT_data_fill %>% 
    mutate(PROD_GROUP = rep(EAT_data$PROD_GROUP, 
                            nlevels(data$pathway)),
           Pathway = as.vector(t(replicate(nrow(EAT_data), 
                                           levels(data$pathway))))) %>% 
    slice(which(PROD_GROUP != "FISH")) %>% 
    droplevels()
  
  #Fill the database 
  for(cur_scen in levels(data$pathway)){
    for (cur_prod_group in levels(as.factor(EAT_data$PROD_GROUP))){
      EAT_data_fill$kcal_adj[which(EAT_data_fill$PROD_GROUP == cur_prod_group & which(EAT_data_fill$Pathway == cur_scen))] <- EAT_data$kcal[which(EAT_data$PROD_GROUP == cur_prod_group)]*data$kcal_mder[which(data$year == 2050 & data$pathway == cur_scen)]/kcal_fixed
      EAT_data_fill$maxkcal_adj[which(EAT_data_fill$PROD_GROUP == cur_prod_group & which(EAT_data_fill$Pathway == cur_scen))] <- EAT_data$maxkcal[which(EAT_data$PROD_GROUP == cur_prod_group)]*data$kcal_mder[which(data$year == 2050 & data$pathway == cur_scen)]/kcal_fixed
      EAT_data_fill$minkcal_adj[which(EAT_data_fill$PROD_GROUP == cur_prod_group & which(EAT_data_fill$Pathway == cur_scen))] <- EAT_data$minkcal[which(EAT_data$PROD_GROUP == cur_prod_group)]*data$kcal_mder[which(data$year == 2050 & data$pathway == cur_scen)]/kcal_fixed
    }
  }
  
  #Get at the PROD_GROUP level
  EAT_data_fill <- EAT_data_fill %>%
    group_by(PROD_GROUP, Pathway)%>%
    mutate(kcal_adj = sum(kcal_adj)) %>%
    mutate(minkcal_adj = sum(minkcal_adj)) %>%
    mutate(maxkcal_adj = sum(maxkcal_adj)) %>%
    unique() %>%
    data.frame()
  
  # #If only one sustainable pathway: one of the three graphs will show the historical data from the FAO in 2015
  # if(!("SustPlus" %in% EAT_data_fill$Pathway)){
  #   EAT_data_fill <- rbind.data.frame(EAT_data_fill,
  #                                     data.frame(Pathway = "FAO 2015",
  #                                                select(EAT_data_fill[which(EAT_data_fill$Pathway == "CT"),], -Pathway))) 
  # }
  
  # Compute country specific kcal -------------------------------------------
  
  data_kcal <- aggregate(kcalfeasprod ~  year + Pathway + PROD_GROUP, data = product, sum) ############ BEFORE, NEED TO DO MAPPING FOR PROD_GROUP
  FOOD_missing <- aggregate(kcalfeasprod ~ PROD_GROUP, data = FOOD_missing, sum)
  data_kcal <- rbind(data_kcal,
                     cbind.data.frame(year = 2050,
                                      Pathway = sort(rep(c("CurrentTrends", "NationalCommitments", "GlobalSustainability"), length(FOOD_missing$PROD_GROUP))),   
                                      PROD_GROUP = FOOD_missing$PROD_GROUP,
                                      kcalfeasprod = FOOD_missing$kcalfeasprod))
  data_kcal <- rbind(data_kcal,
                     cbind.data.frame(year = 2015,   #change year to 2020
                                      Pathway = sort(rep(c("CurrentTrends", "NationalCommitments", "GlobalSustainability"), length(FOOD_missing$PROD_GROUP))),   
                                      PROD_GROUP = FOOD_missing$PROD_GROUP,
                                      kcalfeasprod = FOOD_missing$kcalfeasprod))
  data_kcal_2015 <- data_kcal
  data_kcal <- data_kcal[which(data_kcal$year == 2050),]
  
  
  
  if(!("GlobalSustainability" %in% EAT_data_fill$Pathway)){
    if(unique(data$country_id) != 31){                                      
      FAO_F6 <- aggregate(kcalfeasprod ~ PROD_GROUP, FAO_F6, sum)
      
      data_kcal <- rbind.data.frame(data_kcal,
                                    data.frame(Year = 2015,
                                               Pathway = "FAO 2015",
                                               FAO_F6[which(FAO_F6$PROD_GROUP %in% levels(factor(data_kcal$PROD_GROUP))),]))
    }
    if(unique(data$country_id) == 31){#Norway uses national statistics instead of FAO for the third graph
      data_kcal <- rbind.data.frame(data_kcal,
                                    data.frame(Year = 2015,
                                               Pathway = "FAO 2015",
                                               data_kcal_2015[which(data_kcal_2015$Year == 2015 & data_kcal_2015$Pathway == "CurrentTrends"), c("kcalfeasprod", "PROD_GROUP")]) )
      
    }
  }
  
  # if(unique(data$country_id) == 9){#Special case for India, data not coming from scenathon report tab
  #   for(cur_path in levels(as.factor(magpie$Pathway))){
  #     for(cur_prod in levels(as.factor(magpie$PROD_GROUP))){
  #       data_kcal[which(data_kcal$Pathway == cur_path &
  #                         data_kcal$PROD_GROUP == cur_prod), "kcalfeasprod"] <- magpie[which(magpie$Pathway == cur_path &
  #                                                                                              magpie$PROD_GROUP == cur_prod), "kcalfeasprod"]
  #     }
  #   }
  # }
  
  
  # Merge recommendation and actual value together --------------------------
  
  
  finaldata <- EAT_data_fill %>% 
    left_join(data_kcal) %>% 
    select(-year)  


  # Adjust values -----------------------------------------------------------
  finaldata$newkcal <- rep(0, nrow(finaldata))
  finaldata$group <- finaldata$PROD_GROUP
  for(cur_path in levels(as.factor(finaldata$Pathway))){
    for(cur_prod in levels(as.factor(finaldata$PROD_GROUP))){
      feas <- finaldata$kcalfeasprod[which(finaldata$PROD_GROUP == cur_prod & finaldata$Pathway == cur_path)]
      min <- finaldata$minkcal_adj[which(finaldata$PROD_GROUP == cur_prod & finaldata$Pathway == cur_path)]
      max <- finaldata$maxkcal_adj[which(finaldata$PROD_GROUP == cur_prod & finaldata$Pathway == cur_path)]
      adv <- finaldata$kcal_adj[which(finaldata$PROD_GROUP == cur_prod & finaldata$Pathway == cur_path)]
      if(feas <= min & min>0){
        new_feas <- (30*feas)/min}
      if(feas == min & min==0){
        new_feas <- 30}
      if(feas > min){
        new_feas <- 30 + 30*(feas - min)/(max - min)}
      
      
      if(new_feas>90 & cur_prod != "CEREALS"){
        new_feas <- 90
        #This is to get the discontinuous effect on the graph when it is higher than the maximum value
        finaldata <- rbind(finaldata,
                           cbind.data.frame(Pathway = cur_path,
                                            PROD_GROUP = cur_prod,
                                            kcal_adj=0,
                                            minkcal_adj = 0,
                                            maxkcal_adj = 0,
                                            kcalfeasprod = 0,
                                            newkcal = rep(2,4),
                                            group = c(1,paste0(cur_prod,"X"),2,paste0(cur_prod,"Y"))))
      }
      finaldata$newkcal[which(finaldata$PROD_GROUP == cur_prod & finaldata$Pathway == cur_path & finaldata$group == cur_prod)] <- new_feas
    }
  }
  
  if(!("GlobalSustainability" %in% EAT_data_fill$Pathway)){
    finaldata$newkcal[which(finaldata$group == "CEREALS")] <- ifelse(finaldata$kcalfeasprod[which(finaldata$group == "CEREALS")]<finaldata$kcal_adj[which(finaldata$group == "CEREALS")],
                                                                     30,
                                                                     60)}
  if(("GlobalSustainability" %in% EAT_data_fill$Pathway)){
    finaldata$newkcal[which(finaldata$group == "CEREALS")] <- ifelse(finaldata$kcalfeasprod[which(finaldata$group == "CEREALS")]<finaldata$kcal_adj[which(finaldata$group == "CEREALS")],
                                                                     30,
                                                                     60)}
  # 
  #We do not display animal fat and fish in the final graph
  finaldata <- finaldata[-which(finaldata$PROD_GROUP %in% c("ANIMFAT", "FISH")),]
  
  
  # plot --------------------------------------------------------------------
  
  myColors_Food <- c(CEREALS = "#fbb30a", EGGS = "#ffe1a8", 
                     FRUVEG = "#B6CFAF", 
                     MILK = "#a8bbc5", 
                     NUTS = "#a44e12", 
                     OLSOIL = "#FFEC4D", 
                     POULTRY = "#e26d5c", PULSES = "#472d30", 
                     REDMEAT = "#723d46", ROOTS = "#8C806F",
                     SUGAR = "#434955",
                     "1" = "white", "2" = "white")
  
  cat.labs <- c(CurrentTrends = "Current Trends \n2050",    ### to change
                NationalCommitments = "National Commitments\n2050",     ### to change
                GlobalSustainability= "Global\nSustainability\n2050") ### to change
 
  ########### JUST FOR NORWAY ################# 
  if(!("GlobalSustainability" %in% EAT_data_fill$Pathway)){
    if(unique(data$country_id) != 31){
      cat.labs <- c(CurrentTrends = "Current Trends \n2050", 
                    NationalCommitments = "Sustainable \n2050", 
                    "FAO 2015" = "FAO \n2015")}
    if(unique(data$country_id) == 31){
      cat.labs <- c(CurrentTrends = "Current Trends \n2050", 
                    NationalCommitments = "Sustainable \n2050", 
                    "FAO 2015" = "National Statistics \n2015")
    }
    
      finaldata$Pathway <- factor(finaldata$Pathway, levels = c("CurrentTrends", "NationalCommitments", "FAO 2015"))
  }
  
  
  myLinetype <- c("dashed", "dotted","dashed")
  names(myLinetype) <- c("Average", "Minimum", "Maximum")
  
  p_legend_food <- ggplot(data = finaldata, aes(x = PROD_GROUP, y = newkcal)) +
    geom_point(aes(color = PROD_GROUP), size = 6) +
    scale_color_manual(values = myColors_Food, 
                       name = "",
                       labels = c(CEREALS = "Cereals", EGGS = "Eggs",
                                  FRUVEG = "Fruits and Veg", MILK = "Milk", NUTS = "Nuts", OLSOIL = "Veg. Oils and Oilseeds", 
                                  POULTRY = "Poultry", PULSES = "Pulses", REDMEAT = "Red Meat", ROOTS = "Roots",
                                  SUGAR = "Sugar")) +
    guides(colour=guide_legend(ncol=2))+
    theme(axis.title.x=element_blank())+
    theme_minimal() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 13/0.96),
          legend.box.margin = margin(20, 0, 20, 0),
          legend.box = "vertical")
  
  p_legend_food <- get_legend(p_legend_food)
  
  
  p_legend_rec <- ggplot(data = finaldata, aes(x = PROD_GROUP, y = newkcal, fill = PROD_GROUP)) +
    geom_col(position = "dodge",
             width = 1,
             show.legend = F) +
    theme(axis.title.x=element_blank())+
    geom_segment(aes(x = 0.5, y = 30,
                     xend = 11.5, yend = 30,
                     colour = "Minimum",
                     linetype = "Minimum"),
                 size = 1.2)+
    geom_segment(aes(x = 0.5, y = 60,
                     xend = 11.5, yend = 60,
                     colour = "Maximum",
                     linetype = "Maximum"),
                 size = 1.2)+
    scale_linetype_manual(values = myLinetype,
                          name = "",
                          labels = c(Maximum = "Max. Recommended",
                                     Minimum = "Min. Recommended"))+
    guides(alpha = guide_legend(override.aes = list(linetype = 0, shape='')))+
    scale_colour_manual(values = c("black", "black","black"), 
                        name = "",
                        labels = c(Maximum = "Max. Recommended",
                                   Minimum = "Min. Recommended")) + 
    guides(colour=guide_legend(nrow = 1),
           linetype = guide_legend(nrow = 1))+
    theme_minimal() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 13/0.96),
          legend.box = "vertical",
          legend.box.margin = margin(-50, 0, -50, 0, unit = "lines"),
          panel.spacing = unit(-3, "lines"))
  
  p_legend_rec <- cowplot::get_legend(p_legend_rec)
  
  
  
  myColors_Food2 <- myColors_Food
  names(myColors_Food2) <- paste0(names(myColors_Food), "X")
  myColors_Food3 <- myColors_Food
  names(myColors_Food3) <- paste0(names(myColors_Food), "Y")
  
  myColors_Food <- c(myColors_Food, myColors_Food2, myColors_Food3)
  
  
  p <- ggplot(data = finaldata, aes(x = PROD_GROUP, y = newkcal, fill = group, group = PROD_GROUP)) +
    geom_col(position = position_stack(reverse = TRUE),
             width = 1,
             show.legend = F) +
    theme(axis.title.x=element_blank())+
    coord_polar() +
    facet_grid(Pathway~., 
               switch = "y",
               labeller = labeller(Pathway = cat.labs))+
    geom_segment(aes(x = 0.5, y = 30,
                     xend = 11.5, yend = 30,
                     colour = "Minimum",
                     linetype = "Minimum"))+
    geom_segment(aes(x = 0.5, y = 60,
                     xend = 11.5, yend = 60,
                     colour = "Maximum",
                     linetype = "Maximum"))+
    scale_linetype_manual(values = myLinetype)+
    guides(alpha = guide_legend(override.aes = list(linetype = 0, shape='')))+
    scale_colour_manual(values = c("black", "black","black")) + 
    scale_fill_manual(values = myColors_Food) +
    guides(fill=guide_legend(nrow=5),
           colour=guide_legend(nrow = 2),
           linetype = guide_legend(nrow = 2))+
    theme_minimal() +
    labs(x = NULL, 
         y = NULL)+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.caption = element_text(hjust = 0.15, 
                                      color = "#666666", face= "italic"),
          legend.position = "none",
          panel.spacing = unit(-2.5, "lines"),
          strip.text = element_text(size = 15, family = "serif"),
          strip.text.y.left = element_text(angle = 0),
          strip.placement = "outside",
          plot.margin = margin(t = -10, b= -20))
  
  p <- plot_grid(p, p_legend_rec, p_legend_food, 
                 nrow = 3, rel_heights = c(1.2, 0.1, 0.30))
  
  

  outpath <- paste0(outpath, "Figure 6/")
  if(!dir.exists(outpath)){
    dir.create(outpath)
  }
  
  pdf(paste0(outpath, "Figure_6_", country, "_", gsub("-", "",Sys.Date()),".pdf"), bg = "white", height = 9, width = 4.9)
  plot(p)
  dev.off()
  
  png(paste0(outpath, "Figure_6_", country, "_", gsub("-", "",Sys.Date()),".png"), bg = "white", height = 9, width = 4.9, unit = "in", res = 600)
  plot(p)
  dev.off()

  
}
