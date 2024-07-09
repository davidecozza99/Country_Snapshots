
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
  filter(tradeajustment == "Yes") %>% 
  rename(Pathway = pathway) %>% 
  rename(CalcCropAllCO2e = calccropallco2e) %>% 
  rename(CalcLiveAllCO2e = calcliveallco2e) %>% 
  rename(CalcAllLandCO2e = calcalllandco2e) %>% 
  rename(GHGbiofuels = ghgbiofuels)

product <- read.csv(here("data",  "240523_FullProductDataBase.csv")) %>% 
  rename(Pathway = pathway) %>% 
  filter(tradeadjustment == "Yes")

country = "AUS"
fct_Figure4 <- function(data, outpath, data_F4, country){
  
  #datalines dataframe only comprise data from the CT pathway, from 2020 to 2050, with pathway, emissions or removals from Crop, Livestock, Land use and biofuels
  datalines <- data %>% 
    select(country, year, Pathway, CalcCropAllCO2e, CalcLiveAllCO2e, CalcAllLandCO2e, GHGbiofuels) %>% 
    slice(which(Pathway == "CurrentTrends")) %>% 
    slice(which(year %in% as.character(seq(2020, 2050, 5)))) %>% 
    pivot_longer(cols = c(CalcCropAllCO2e, CalcLiveAllCO2e, CalcAllLandCO2e, GHGbiofuels),
                 names_to = "type") %>% 
    data.frame()
  
  #duplicate values from CalcLandCO2e into CalcLandCO2eNeg to be able to plot emissions from land if it goes from positive to negative over the 2020 to 2050 period
  datalines <- rbind(datalines,
                     cbind.data.frame(
                       country = country,
                       year =  as.character(seq(2020, 2050, 5)),
                       Pathway = "CurrentTrends",
                       type = "CalcLandCO2eNeg",
                       value = datalines$value[which(datalines$type == "CalcAllLandCO2e")]))
  #where CalcAllLandCO2e is negative than put to zero because the information is now in CalcLandCO2eNeg
  datalines[which(datalines$value <0 & datalines$type == "CalcAllLandCO2e"), "value"] <- 0
  #If CalcAllLandCO2e is positive still allow for a really small negative values to have a nice plot if switch from neg to pos
  datalines[which(datalines$value >=0 & datalines$type == "CalcLandCO2eNeg"), "value"] <- -0.0000000001
  #no value should be strictly null for this plot to look nice
  datalines[which(datalines$value == 0 & datalines$type != "GHGbiofuels"), "value"] <- 0.0000000001
  #no value should be strictly null for this plot to look nice, and GHGbiofuels is always negative
  datalines[which(datalines$value == 0 & datalines$type == "GHGbiofuels"), "value"] <- -0.0000000001
  
  datalines$type <- factor(datalines$type, levels = c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e", "GHGbiofuels", "CalcLandCO2eNeg"))
  datalines$year <- as.numeric(datalines$year)
  
  data <- data %>% select(year, Pathway, CalcCropAllCO2e, CalcLiveAllCO2e, CalcAllLandCO2e, GHGbiofuels)
  #Emissions is the net emissions from AFOLU per year
  data$Emission <- rowSums(data[, c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e", "GHGbiofuels")])
  data <- data %>% select(year, Pathway, Emission) %>% 
    slice(which(year %in% as.character(seq(2020, 2050, 5))))
  #pivot the data to a wider format
  data <- data %>% 
    pivot_wider(names_from = Pathway,
                values_from = Emission) %>% 
    data.frame()
  #colnames depends on the numbers of pathways
  if("SustPlus" %in% colnames(data)){
    colnames(data) <- c("year", "CT", "SS", "SSPlus")
  }else{
    colnames(data) <- c("year", "CT", "SS")
  }
  #datalinestot will be use to plot the net emissions lines in the figure
  datalinestot <- data
  datalinestot$year <- as.numeric(datalinestot$year)
  if("SSPlus" %in% colnames(data)){
    datalinestot <- rbind(datalinestot,
                          cbind.data.frame(year = 2051, #add this year so that the lines continue a bit after 2050 to touch with the bar chart
                                           CT = datalinestot$CT[which(datalinestot$year == 2050)],
                                           SS = datalinestot$SS[which(datalinestot$year == 2050)],
                                           SSPlus = datalinestot$SSPlus[which(datalinestot$year == 2050)]))
    range_line <- max(datalinestot[,c("CT", "SS", "SSPlus")], 
                      aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value) - 
      min(datalinestot[,c("CT", "SS", "SSPlus")], 
          0, 
          aggregate(value ~ year, datalines[which(datalines$type %in% c("GHGbiofuels", "CalcLandCO2eNeg")),], sum)$value)
  }else{
    datalinestot <- rbind(datalinestot,
                          cbind.data.frame(year = 2051,
                                           CT = datalinestot$CT[which(datalinestot$year == 2050)],
                                           SS = datalinestot$SS[which(datalinestot$year == 2050)]))
    range_line <- max(datalinestot[,c("CT", "SS")],
                      aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value) - 
      min(datalinestot[,c("CT", "SS")], 
          0, 
          aggregate(value ~ year, datalines[which(datalines$type %in% c("GHGbiofuels", "CalcLandCO2eNeg")),], sum)$value)
  }
  
  
  myColors_GHG <- c("black", "Forestgreen", "Lightgreen")
  names(myColors_GHG) <- c("CT", "SS", "SSPlus")
  
  myLinetype_Pathway <- c("solid", "dotted", "dashed")
  names(myLinetype_Pathway) <- c("Current Trends", "Sustainable", "Sustainable Plus")
  
  
  #Get y-axis min and max 
  df_y_axis <- datalines %>% 
    group_by(year, Pathway) %>% 
    mutate(TotNeg = sum(value[value<0])) %>% 
    mutate(TotPos = sum(value[value>0])) 
  
  min_y_axis <- min(df_y_axis$TotNeg, datalinestot$CT, datalinestot$SS)
  max_y_axis <- max(df_y_axis$TotPos, datalinestot$CT, datalinestot$SS)
  
  if("SSPlus" %in% colnames(data)){
    min_y_axis <- min(df_y_axis$TotNeg, datalinestot$CT, datalinestot$SS, datalinestot$SSPlus)
    max_y_axis <- max(df_y_axis$TotPos, datalinestot$CT, datalinestot$SS, datalinestot$SSPlus)
  }
  
  # Creation of plot to display ----------------------------------------------------
  
  #Building the legend for countries with only one sustainable pathway
  
  data_legend <- cbind.data.frame(type = c("CalcCropAllCO2e", "CalcLiveAllCO2e", "GHGbiofuels", "CalcAllLandCO2e"),
                                  value = c(0.001, 0.001, 0.001, 0.001),
                                  year = rep(2020, 4))
  
  
  legend_colours <- ggplot()+
    geom_area(aes(x = year, y = value, fill = type), data = data_legend)+
    scale_fill_manual(values = myColors_GHG_fig4,
                      label = c("Land Use", "Crops", "Livestock", "Biofuels"),
                      name = "")+
    geom_line(aes(x = year, y = CT, linetype = "Current Trends"), data = datalinestot, size = 1.1)+
    geom_line(aes(x = year, y = SS, linetype = "Sustainable"), data = datalinestot, size = 1.1)+
    scale_linetype_manual(values = myLinetype_Pathway,
                          name = "Net AFOLU Emissions: ")+
    guides(fill = guide_legend(order=1),
           linetype = guide_legend(order=2))+
    theme_minimal()+
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(size = 8/0.36),
          legend.title = element_text(size = 8/0.36),
          legend.key.width = unit(10, "mm"))
  legend <-  cowplot::get_legend(legend_colours)
  
  
  
  # line plot for countries with only one sustainable pathway
  
  p_line <- ggplot()+
    geom_area(aes(x = year, y = value, fill = type), data = datalines, position = 'stack', show.legend = F)+
    scale_fill_manual(values = myColors_GHG_fig4)+
    geom_line(aes(x = year, y = CT, linetype = "Current Trends"), data = datalinestot, size = 1.1, show.legend = F)+
    geom_line(aes(x = year, y = SS, linetype = "Sustainable"), data = datalinestot, size = 1.1, show.legend = F)+
    ylab(expression("GHG Emissions (MtCO"[2]*"e)"))+
    xlab(NULL)+
    scale_x_continuous(breaks = seq(2020, 2050, 5),
                       expand = c(0,0))+
    scale_y_continuous(limits=c(min(0,
                                    data$CT, 
                                    data$SS,
                                    aggregate(value ~ year, datalines[which(datalines$type %in% c("GHGbiofuels", "CalcLandCO2eNeg")),], sum)$value),
                                ifelse(max(data$CT == data[which(data$year == 2050), "CT"]), 
                                       max(1.1*data[which(data$year == 2050), "CT"],
                                           data$SS,  
                                           aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value),
                                       max(aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value, 
                                           data$SS)+5)))+
    scale_linetype_manual(values = myLinetype_Pathway,
                          name = "Net Emissions: ")+
    theme_minimal()+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.margin = margin(l = 10, r = -10, unit = "pt"),
          axis.title.y = element_text(size = 10/0.36),
          axis.text = element_text(size = 6.5/0.36),
          axis.ticks = element_line())
  
  
  
  if("SSPlus" %in% colnames(data)){#If more than one sustainable pathway we need an additional line and bar
    
    legend_colours <- ggplot()+
      geom_area(aes(x = year, y = value, fill = type), data = data_legend)+
      scale_fill_manual(values = myColors_GHG_fig4,
                        label = c("Land Use", "Crops", "Livestock", "Biofuels"),
                        name = "")+
      geom_line(aes(x = year, y = CT, linetype = "Current Trends"), data = datalinestot, size = 1.1)+
      geom_line(aes(x = year, y = SS, linetype = "Sustainable"), data = datalinestot, size = 1.1)+
      geom_line(aes(x = year, y = SSPlus, linetype = "Sustainable Plus"), data = datalinestot, size = 1.1)+
      scale_linetype_manual(values = myLinetype_Pathway,
                            label = c("Current Trends", "Sustainable", "Sustainable +"),
                            name = "Net AFOLU Emissions: ")+
      guides(fill = guide_legend(order=1),
             linetype = guide_legend(order=2))+
      theme_minimal()+
      theme(legend.position = "bottom",
            legend.box = "vertical",
            legend.text = element_text(size = 8/0.36),
            legend.title = element_text(size = 8/0.36),
            legend.key.width = unit(10, "mm"))
    
    legend <-  cowplot::get_legend(legend_colours)
    
    p_line <- ggplot(datalines, aes(x=year))+
      geom_area(aes(x = year, y = value, fill = type), 
                data = datalines, 
                position = 'stack', 
                show.legend = F)+
      scale_fill_manual(values = myColors_GHG_fig4)+
      geom_line(aes(x = year, y = CT, linetype = "Current Trends"), 
                data = datalinestot, 
                size = 1.1, 
                show.legend = F)+
      geom_line(aes(x = year, y = SS, linetype = "Sustainable"), data = datalinestot, size = 1.1, show.legend = F)+
      geom_line(aes(x = year, y = SSPlus, linetype = "Sustainable Plus"), data = datalinestot, size = 1.1, show.legend = F)+
      ylab(expression("GHG Emissions (MtCO"[2]*"e)"))+
      xlab(NULL)+
      scale_x_continuous(breaks = seq(2020, 2050, 5),
                         expand = c(0,0))+
      scale_y_continuous(limits=c(min(0,
                                      aggregate(value ~ year, datalines[which(datalines$type %in% c("GHGbiofuels", "CalcLandCO2eNeg")),], sum)$value,
                                      data$CT, 
                                      data$SS, 
                                      data$SSPlus),
                                  ifelse(max(data$CT == data[which(data$year == 2050), "CT"]), 
                                         max(1.1*data[which(data$year == 2050), "CT"],data$SS, data$SSPlus,  aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value),
                                         max( aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "GHGbiofuels", "CalcAllLandCO2e")),], sum)$value, data$SS, data$SSPlus)+5)))+
      scale_linetype_manual(values = myLinetype_Pathway,
                            name = "Net Emissions: ")+
      theme_minimal()+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            plot.margin = margin(l = 10, r = 7, unit = "pt"),
            axis.title.y = element_text(size = 10/0.36),
            axis.text = element_text(size = 6.5/0.36),
            axis.ticks = element_line())
    
    # dataframe needed to build and display the emissions difference between CT and the sustainable pathways
    
    data2 <- cbind.data.frame(x =  rep(c("SSPlus", "SS"), 2),
                              y = c(data$CT[which(data$year == 2050)]-data$SSPlus[which(data$year == 2050)],
                                    data$CT[which(data$year == 2050)]-data$SS[which(data$year == 2050)],
                                    data$SSPlus[which(data$year == 2050)],
                                    data$SS[which(data$year == 2050)]),
                              scenario = as.factor(c("SSPlusdiff", "SSdiff",
                                                     "blank", "blank")))
    
    data2$label <- c(paste0("-", paste0("$", round(data2$y[which(data2$scenario == "SSPlusdiff")[1]],0), "$"), "MtCO$_{",2,"}e"),
                     paste0("-", paste0("$", round(data2$y[which(data2$scenario == "SSdiff")[1]],0), "$"), "MtCO$_{",2,"}e"),
                     NA,
                     NA)
    
    data2$scenario = factor(data2$scenario, levels = levels(data2$scenario)[c(2,3,1)])
    
    data2$colour <- c("SSPlus", "SS", "CT", "CT")
    myColors_scen <- c("lightgrey", "darkgrey", "#FFFFFF")
    names(myColors_scen) <- levels(data2$scenario)
    
    if(length(which(data2$y<0 & data2$scenario != "blank"))>0){#increase in emissions
      data2[which(data2$x  %in% data2$x[which(data2$y<0 & data2$scenario != "blank")] &
                    data2$scenario == "blank"), "y"] <- data2[which(data2$x  %in% data2$x[which(data2$y<0 & data2$scenario != "blank")] &
                                                                      data2$scenario == "blank"), "y"] + data2[which(data2$y<0 & data2$scenario != "blank"), "y"]
      data2$label[which(data2$y<0 & data2$scenario != "blank")] <- paste0("+", paste0("$", -round(data2[which(data2$y<0 & data2$scenario != "blank"), "y"], 1), "$"), "MtCO$_{",2,"}")
      data2[which(data2$y<0 & data2$scenario != "blank"), "y"] <- -data2[which(data2$y<0 & data2$scenario != "blank"), "y"]
      
    }
    
    if (length(which(data2$y<0 & data2$scenario == "blank"))>0 &
        data$CT[data$year=="2050"]<0){# if emissions in the sustainable and CT pathways decrease to net negative
      temp_bottom <- data2[which(data2$colour == "CT"), "y"]
      data2[which(data2$colour == "CT"), "y"] <- rep(data$CT[data$year=="2050"], 2)
      data2[which(data2$colour != "CT"), "y"] <- temp_bottom - data$CT[data$year=="2050"]
      
    }
    
    if (length(which(data2$y<0 & data2$scenario == "blank"))>0 &
        data$CT[data$year=="2050"]>=0){# if emissions in the sustainable pathways decrease to net negative but emissions from CT positive
      data2$scenario <- as.character(data2$scenario)
      for(cur in 1:length(which(data2$y<0 & data2$scenario == "blank"))){
        data2[which(data2$x %in% data2[which(data2$scenario == "blank" & data2$y<0)[1], "x"])[1], "y"] <- data2[which(data2$x %in% data2[which(data2$scenario == "blank" & data2$y<0)[1], "x"])[1], "y"] + data2[which(data2$scenario == "blank" & data2$y<0)[1], "y"]
        data2[which(data2$scenario == "blank" & data2$y<0)[1], "scenario"] <- data2[which(data2$x %in% data2[which(data2$scenario == "blank" & data2$y<0)[1], "x"])[1], "scenario"]
      }
      data2$scenario <- as.factor(data2$scenario)
      data2$scenario = factor(data2$scenario, levels = levels(data2$scenario)[c(2,3,1)])
    }
    
    #buffer to place the text description above the bar chart
    buffer_title <- max(datalinestot$CT[which(datalinestot$year == 2050)], 
                        max(datalinestot$SS[which(datalinestot$year == 2050)], 
                            datalinestot$SSPlus[which(datalinestot$year == 2050)])) + (max_y_axis - min_y_axis)*10/100
    
    
    p_bar <- ggplot(data)+
      geom_bar(aes(x, y, fill = scenario),
               data=data2,
               stat = "identity",
               show.legend = F,
               width = 0.92)+
      scale_fill_manual(name = "Scen",
                        values = myColors_scen)+
      geom_hline(aes(yintercept = data$CT[which(data$year == 2050)],
                     linetype = "Current Trends"),
                 size = 1,
                 colour = myColors_GHG[names(myColors_GHG)=="CT"],
                 show.legend = F)+
      geom_errorbar(aes(ymin = data$SS[which(data$year == 2050)],
                        ymax = data$SS[which(data$year == 2050)],
                        x = 1,
                        linetype = "Sustainable"),
                    size = 1,
                    width = 1.1,
                    show.legend = F)+
      geom_hline(aes(yintercept = data$SSPlus[which(data$year == 2050)],
                     linetype = "Sustainable Plus"),
                 size = 1,
                 show.legend = F)+
      scale_colour_manual(values = myColors_GHG[names(myColors_GHG) %in% c("SS", "SSPlus")]) +
      scale_y_continuous(limits=c(min(0,
                                      aggregate(value ~ year, datalines[which(datalines$type %in% c("GHGbiofuels", "CalcLandCO2eNeg")),], sum)$value,
                                      data$CT, 
                                      data$SS, 
                                      data$SSPlus),
                                  ifelse(max(data$CT == data[which(data$year == 2050), "CT"]), 
                                         max(1.1*data[which(data$year == 2050), "CT"],data$SS, data$SSPlus,  aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value), 
                                         max( aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value, data$SS, data$SSPlus)+5)),
                         oob = rescale_none)+
      annotate(geom="text", x=1.5, y= buffer_title, 
               label="Difference in Net Emissions compared \nwith Current Trends in 2050",
               size = 5.5)+
      scale_x_discrete(labels= c("SS" = "Sustainable",
                                 "SSPlus" = "Sustainable +"))+
      theme(panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 8/0.36))
    
    if(length(which(data2$scenario == "SSdiff"))>1 & length(which(data2$scenario == "SSPlusdiff"))>1){#if both goes below 0
      p_bar <- p_bar + 
        annotate(geom="text", 
                 x = 1, 
                 y = c(mean(data2$y[which(data2$scenario == "SSdiff")]) + (1/32)*range_line,
                       mean(data2$y[which(data2$scenario == "SSdiff")] - (1/32)*range_line)),
                 label = c("Sustainable",
                           TeX(data2$label[which(data2$scenario == "SSdiff")][1], output = "character")),
                 parse = TRUE,
                 size = 6.5,
                 colour = "black") + 
        annotate(geom="text", 
                 x = 2, 
                 y = mean(data2$y[which(data2$scenario == "SSPlusdiff")]) - (1/32)*range_line,
                 label = TeX(data2$label[which(data2$scenario == "SSPlusdiff")][1], output = "character"),
                 parse = TRUE,
                 size = 6.5,
                 colour = "black") + 
        annotate(geom="text", 
                 x = 2, 
                 y = mean(data2$y[which(data2$scenario == "SSPlusdiff")]) + (1/32)*range_line,
                 label = "Sustainable +",
                 size = 6.5,
                 colour = "black")
      
    }
    
    if(length(which(data2$scenario == "SSdiff"))>1 & length(which(data2$scenario == "SSPlusdiff"))==1){#if goes below 0 just for SSdiff
      p_bar <- p_bar + 
        annotate(geom="text", 
                 x = 1, 
                 y = c(mean(data2$y[which(data2$scenario == "SSdiff")]) + (1/32)*range_line,
                       mean(data2$y[which(data2$scenario == "SSdiff")] - (1/32)*range_line)),
                 label = c("Sustainable",
                           TeX(data2$label[which(data2$scenario == "SSdiff")][1], output = "character")),
                 parse = TRUE,
                 size = 6.5,
                 colour = "black")+
        annotate(geom="text", 
                 x = 2, 
                 y = data2$y[which(data2$scenario == "blank" & data2$x == "SSPlus")] + data2$y[which(data2$scenario == "SSPlusdiff")]/2 + (1/32)*range_line,
                 label = "Sustainable +",
                 size = 6.5,
                 colour = "black") +
        annotate(geom="text", 
                 x = 2, 
                 y = data2$y[which(data2$scenario == "blank" & data2$x == "SSPlus")] + data2$y[which(data2$scenario == "SSPlusdiff")]/2 - (1/32)*range_line,
                 label = TeX(data2$label[which(data2$scenario == "SSPlusdiff")][1], output = "character"),
                 parse = TRUE,
                 size = 6.5,
                 colour = "black")
    }
    
    if(length(which(data2$scenario == "SSPlusdiff"))>1 & length(which(data2$scenario == "SSdiff")) == 1){#IF GOES BELOW ZERO JUST FOR SSPLUSdiff
      p_bar <- p_bar + annotate(geom="text", 
                                x = 2, 
                                y = mean(data2$y[which(data2$scenario == "SSPlusdiff")]) - (1/32)*range_line,
                                label = TeX(data2$label[which(data2$scenario == "SSPlusdiff")][1], output = "character"),
                                parse = TRUE,
                                size = 6.5,
                                colour = "black") + 
        annotate(geom="text", 
                 x = 2, 
                 y = mean(data2$y[which(data2$scenario == "SSPlusdiff")]) + (1/32)*range_line,
                 label = "Sustainable+",
                 size = 6.5,
                 colour = "black") +
        annotate(geom="text", 
                 x = 1, 
                 y = c(data2$y[which(data2$scenario == "blank" & data2$x == "SS")] + data2$y[which(data2$scenario == "SSdiff")]/2 + (1/32)*range_line,
                       data2$y[which(data2$scenario == "blank" & data2$x == "SS")] + data2$y[which(data2$scenario == "SSdiff")]/2 - (1/32)*range_line),
                 label = c("Sustainable",
                           TeX(data2$label[which(data2$scenario == "SSdiff")][1], output = "character")),
                 parse = TRUE,
                 size = 6.5,
                 colour = "black") 
      
    }
    if(length(which(data2$scenario == "SSPlusdiff"))== 1 & length(which(data2$scenario == "SSdiff")) == 1){#if none goes below zero
      p_bar <- p_bar + 
        annotate(geom="text", 
                 x = 1, 
                 y = c(data2$y[which(data2$scenario == "blank" & data2$x == "SS")] + data2$y[which(data2$scenario == "SSdiff")]/2 + (1/32)*range_line,
                       data2$y[which(data2$scenario == "blank" & data2$x == "SS")] + data2$y[which(data2$scenario == "SSdiff")]/2 - (1/32)*range_line),
                 label = c("Sustainable",
                           TeX(data2$label[which(data2$scenario == "SSdiff")][1], output = "character")),
                 parse = TRUE,
                 size = 6.5,
                 colour = "black") +
        annotate(geom="text", 
                 x = 2, 
                 y = data2$y[which(data2$scenario == "blank" & data2$x == "SSPlus")] + data2$y[which(data2$scenario == "SSPlusdiff")]/2 + (1/32)*range_line,
                 label = "Sustainable +",
                 size = 6.5,
                 colour = "black") +
        annotate(geom="text", 
                 x = 2, 
                 y = data2$y[which(data2$scenario == "blank" & data2$x == "SSPlus")] + data2$y[which(data2$scenario == "SSPlusdiff")]/2 - (1/32)*range_line,
                 label = TeX(data2$label[which(data2$scenario == "SSPlusdiff")][1], output = "character"),
                 parse = TRUE,
                 size = 6.5,
                 colour = "black")
      
    }
    
  }else{# If only one sustainable pathway
    data2 <- cbind.data.frame(x =  rep(c("SS"), 2),
                              y = c(data$CT[which(data$year == 2050)]-data$SS[which(data$year == 2050)],
                                    data$SS[which(data$year == 2050)]),
                              scenario = as.factor(c("SSdiff",
                                                     "blank")))
    
    data2$label <- c(paste0("-", paste0("$", round(data2$y[which(data2$scenario == "SSdiff")[1]],0), "$"),"MtCO$_{",2,"}e"),
                     NA)
    
    data2$scenario = factor(data2$scenario, levels = levels(data2$scenario)[c(2,1)])
    
    data2$colour <- c("SS", "CT")
    myColors_scen <- c("lightgrey", "#FFFFFF")
    names(myColors_scen) <- levels(data2$scenario)
    
    if(length(which(data2$y<0 & data2$scenario != "blank"))>0){#increase of emission
      data2[which(data2$x  %in% data2$x[which(data2$y<0 & data2$scenario != "blank")] &
                    data2$scenario == "blank"), "y"] <- data2[which(data2$x  %in% data2$x[which(data2$y<0 & data2$scenario != "blank")] &
                                                                      data2$scenario == "blank"), "y"] + data2[which(data2$y<0 & data2$scenario != "blank"), "y"]
      data2$label[which(data2$y<0 & data2$scenario != "blank")] <- paste0("+", paste0("$", -round(data2[which(data2$y<0 & data2$scenario != "blank"), "y"], 0), "$"), "MtCO$_{",2,"}e")
      data2[which(data2$y<0 & data2$scenario != "blank"), "y"] <- -data2[which(data2$y<0 & data2$scenario != "blank"), "y"]
      
    }
    
    if (length(which(data2$y<0 & data2$scenario == "blank"))>0 &
        data$CT[data$year=="2050"]<=0){#decrease goes below 0
      temp_bottom <- data2[which(data2$colour == "CT"), "y"]
      data2[which(data2$colour == "CT"), "y"] <- data$CT[data$year=="2050"]
      data2[which(data2$colour != "CT"), "y"] <- temp_bottom - data$CT[data$year=="2050"]
    }
    
    if (length(which(data2$y<0 & data2$scenario == "blank"))>0 &
        data$CT[data$year=="2050"]>=0){#decrease in emissions below 0 but CT above 0
      data2$scenario <- as.character(data2$scenario)
      for(cur in 1:length(which(data2$y<0 & data2$scenario == "blank"))){
        data2[which(data2$x %in% data2[which(data2$scenario == "blank" & data2$y<0)[1], "x"])[1], "y"] <- data2[which(data2$x %in% data2[which(data2$scenario == "blank" & data2$y<0)[1], "x"])[1], "y"] + data2[which(data2$scenario == "blank" & data2$y<0)[1], "y"]
        data2[which(data2$scenario == "blank" & data2$y<0)[1], "scenario"] <- data2[which(data2$x %in% data2[which(data2$scenario == "blank" & data2$y<0)[1], "x"])[1], "scenario"]
      }
      data2$scenario <- as.factor(data2$scenario)
      data2$scenario = factor(data2$scenario, levels = levels(data2$scenario)[c(2,3,1)])
    }
    
    #buffer to place the text description above the bar chart
    buffer_title <- max(datalinestot$CT[which(datalinestot$year == 2050)], 
                        datalinestot$SS[which(datalinestot$year == 2050)]) + (max_y_axis - min_y_axis)*10/100
    
    p_bar <- ggplot(data)+
      geom_bar(aes(x, y, fill = scenario),
               data=data2,
               stat = "identity",
               show.legend = F,
               width = 0.8)+
      scale_fill_manual(name = "Scen",
                        values = myColors_scen)+
      geom_hline(aes(yintercept = data$CT[which(data$year == 2050)],
                     linetype = "Current Trends"),
                 size = 0.9,
                 alpha = 1,
                 colour = myColors_GHG[names(myColors_GHG)=="CT"],
                 show.legend = F)+
      geom_hline(aes(yintercept = data$SS[which(data$year == 2050)],
                     linetype = "Sustainable"),
                 size = 0.9,
                 alpha = 1,
                 show.legend = F)+
      scale_colour_manual(values = myColors_GHG[names(myColors_GHG) %in% c("SS")]) +
      scale_y_continuous(limits=c(min(0, 
                                      aggregate(value ~ year, datalines[which(datalines$type %in% c("GHGbiofuels", "CalcLandCO2eNeg")),], sum)$value,
                                      data$CT, 
                                      data$SS),
                                  ifelse(max(data$CT == data[which(data$year == 2050), "CT"]), 
                                         max(1.1*data[which(data$year == 2050), "CT"],data$SS,  aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value),
                                         max( aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value, data$SS)+5)),
                         oob = rescale_none)+
      annotate(geom="text", x=1, y= buffer_title,
               label="Difference in Net Emissions compared \nwith Current Trends in 2050",
               size = 6)+
      scale_x_discrete(labels= c("SS" = "Sustainable"),
                       expand = c(-1,0))+
      theme(panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 8/0.36))
    
    
    p_bar <- p_bar + annotate(geom="text", 
                              x = 1,
                              y = c(ifelse(TRUE %in% (data2$scenario == "blank"), 
                                           data2$y[which(data2$scenario == "blank")], 
                                           0) +sum(data2$y[which(data2$scenario == "SSdiff")])/2 + (1/32)*range_line,
                                    ifelse(TRUE %in% (data2$scenario == "blank"), 
                                           data2$y[which(data2$scenario == "blank")], 
                                           0) +sum(data2$y[which(data2$scenario == "SSdiff")])/2 - (1/32)*range_line),
                              label = c("Sustainable",
                                        TeX(data2$label[which(data2$scenario == "SSdiff")][1], output = "character")),
                              parse = TRUE,
                              fontface = 2,
                              size = 6.5,
                              colour = "black")
    
    if(TRUE %in% grepl("+", data2$label, fixed = TRUE)){#increase in GHG compared with CT
      p_bar <- ggplot(data)+
        geom_bar(aes(x, y, fill = scenario),
                 data=data2,
                 stat = "identity",
                 show.legend = F,
                 width = 0.8)+
        scale_fill_manual(name = "Scen",
                          values = myColors_scen)+
        geom_hline(aes(yintercept = data$CT[which(data$year == 2050)],
                       linetype = "Current Trends"),
                   size = 0.9,
                   alpha = 0.9,
                   colour = myColors_GHG[names(myColors_GHG)=="CT"],
                   show.legend = F)+
        geom_hline(aes(yintercept = data$SS[which(data$year == 2050)],
                       linetype = "Sustainable"),
                   size = 0.9,
                   alpha = 0.9,
                   show.legend = F)+
        scale_colour_manual(values = myColors_GHG[names(myColors_GHG) %in% c("SS")]) +
        scale_y_continuous(limits=c(min(0,
                                        aggregate(value ~ year, datalines[which(datalines$type %in% c("GHGbiofuels", "CalcLandCO2eNeg")),], sum)$value,
                                        data$CT, 
                                        data$SS),
                                    ifelse(max(data$CT == data[which(data$year == 2050), "CT"]), 
                                           max(1.1*data[which(data$year == 2050), "CT"],data$SS,  aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value),
                                           max( aggregate(value ~ year, datalines[which(datalines$type %in% c("CalcCropAllCO2e", "CalcLiveAllCO2e", "CalcAllLandCO2e")),], sum)$value, data$SS)+5)),
                           oob = rescale_none)+
        annotate(geom="text", x=1, y= 1.05*buffer_title,
                 label="Difference in Net Emissions compared \nwith Current Trends in 2050",
                 size = 6)+
        scale_x_discrete(labels= c("SS" = "Sustainable"),
                         expand = c(-1,0))+
        theme(panel.background = element_rect(fill = "white"),
              axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 8/0.36))
      
      
      p_bar <- p_bar + annotate(geom="text", 
                                x = 1,
                                y = c(ifelse(TRUE %in% (data2$scenario == "blank"), 
                                             data2$y[which(data2$scenario == "blank")], 
                                             0) +sum(data2$y[which(data2$scenario == "SSdiff")])/2 + (1/32)*range_line,
                                      ifelse(TRUE %in% (data2$scenario == "blank"), 
                                             data2$y[which(data2$scenario == "blank")], 
                                             0) +sum(data2$y[which(data2$scenario == "SSdiff")])/2 - (1/32)*range_line),
                                label = c("Sustainable",
                                          TeX(data2$label[which(data2$scenario == "SSdiff")][1], output = "character")),
                                parse = TRUE,
                                fontface = 2,
                                size = 6.5,
                                colour = "black") 
      
    }
    
    
  }
  
  p_bar <- p_bar + theme(plot.margin =  margin(l = 5, r = 0, unit = "pt")) 
  if("SSPlus" %in% colnames(data)){
    p_bar <- p_bar + theme(plot.margin =  margin(l = -15, r = -5, unit = "pt"))
  }
  
  p_final <- ggarrange(p_line, p_bar, nrow = 1, widths = c(2, 1.4))
  
  p_final <-  plot_grid(grid.arrange(
    grobs = list(p_final, legend),
    heights = c(4,1),
    layout_matrix = rbind(c(1),
                          c(2))))
  
  
  outpath <- paste0(outpath, "Figure 4/")
  if(!dir.exists(outpath)){
    dir.create(outpath)
  }
  
  pdf(paste0(outpath, "Figure_4_", country, "_", gsub("-", "",Sys.Date()),".pdf"),  width = 11, height = 8.5)
  plot(p_final)
  dev.off()
  
  png(paste0(outpath, "Figure_4_", country, "_", gsub("-", "",Sys.Date()), ".png"),  width = 11, height = 8.5, units = "in", res = 600)
  plot(p_final)
  dev.off()
}
