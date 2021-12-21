#####################################
# Generate a plotly scatter plot of fbref data
# w/ dropdown menu between seasons
######################


library(tidyverse)
library(plotly)



# Read in csvs, would be better if I just learned worldfootballR

gl_20 <- read.csv("") %>%
  select(Player,Comp,Age,Born,Pos,Min,npGls_per90,npxG_per90) %>%
  mutate(season = "2020-2021") %>%
  filter(Age <= 23, Min > 900, Pos %in% c("FW","FWMF","MFFW", "DFFW", "FWDF"))

gl_19 <- read.csv("") %>%
  select(Player,Comp,Age,Born,Pos,Min,npGls_per90,npxG_per90) %>%
  mutate(season = "2019-2020") %>%
  filter(Age <= 23, Min > 900, Pos %in% c("FW","FWMF","MFFW", "DFFW", "FWDF"))


gl_18 <- read.csv("") %>%
  select(Player,Comp,Age,Born,Pos,Min,npGls_per90,npxG_per90) %>%
  mutate(season = "2018-2019") %>%
  filter(Age <= 23, Min > 900, Pos %in% c("FW","FWMF","MFFW", "DFFW", "FWDF"))

gl_17 <- read.csv("") %>%
  select(Player,Comp,Age,Born,Pos,Min,npGls_per90,npxG_per90) %>%
  mutate(season = "2017-2018") %>%
  filter(Age <= 23, Min > 900, Pos %in% c("FW","FWMF","MFFW", "DFFW", "FWDF"))


past_data <- gl_20 %>%
  bind_rows(gl_19,gl_18,gl_17) %>%
  mutate(Comp = case_when(Comp == "fr Ligue 1" ~ "Ligue 1", 
                          Comp == "eng Premier League" ~ "Premier League",
                          Comp == "es La Liga" ~ "La Liga",
                          Comp == "de Bundesliga" ~ "Bundesliga",
                          Comp == "it Serie A" ~ "Serie A"))



recent_data <- read.csv("") %>%
  select(Player,Comp,Age,Born,Pos,Min,npGls_per90,npxG_per90) %>%
  mutate(Comp = case_when(Comp == "fr Ligue 1" ~ "Ligue 1", 
                          Comp == "eng Premier League" ~ "Premier League",
                          Comp == "es La Liga" ~ "La Liga",
                          Comp == "de Bundesliga" ~ "Bundesliga",
                          Comp == "it Serie A" ~ "Serie A")) %>%
  # Current season has age in years-days, separate to only get years
  separate(Age,c("Age","Age_days")) %>%
  filter(Age <= 23, Min > 600, Pos %in% c("FW","FWMF","MFFW", "DFFW", "FWDF"))



# Establishes max val for 1:1 line
lin_vals <- c(0,max(c(max(recent_data$npGls_per90),max(recent_data$npxG_per90),
                      max(past_data$npGls_per90),max(past_data$npxG_per90))))



#### Dropdown menu set up
# adapted from https://plotly.com/r/dropdowns/

# List of season names for dropdownn
season_list <- c("2021-2022 (partial)","2020-2021","2019-2020","2018-2019","2017-2018")

# Turns out there are 25 categories not 5
# If you use 5 it shits itsself
# Need 5 (number of seasons) by 5 (number of leagues)
num_cat <- 5*5
# This is a base array of George Boole's finest booleans which I'll manipulate to set the visibility of plots
# Needed to append an additional TRUE for the dashed 1:1 line
booleARR <- c(rep(FALSE,num_cat),TRUE)

# Need to generate a Boolean array to specify  the visibility of plots from the dropdown
# so 1:5 is the first 5 elements (i,e, the 5 leagues of 2021-22)
case_boole <- replace(booleARR,1:5,TRUE)
season_label <- season_list[1]
l_combo <- list(method = "restyle", args = list("visible", as.list(case_boole)),label = season_label)

# Repear for a second season
case_boole <- replace(booleARR,6:10,TRUE)
season_label <- season_list[2]
l2 <- list(method = "restyle", args = list("visible", as.list(case_boole)),label = season_label)

#combine them
l_combo <- list(l_combo,l2)


# Loop trhough the remaining seasons
for (i in 3:5){
  #The middle bit is clunky but just doing the same as above, 11:15, 16:20 etc
  case_boole <- replace(booleARR,(((i-1)*5)+1):(i*5),TRUE)
  season_label <- season_list[i]
  l2 <- list(method = "restyle", args = list("visible", as.list(case_boole)),label = season_label)
  
  l_combo[[i]] <- l2
  
}




#### Generating plot


fig <- plot_ly() %>%
  add_trace(data = recent_data, x = ~npxG_per90, y = ~npGls_per90,
            type = 'scatter', mode = 'markers',
            color = ~Comp, size = 8,
            text = ~paste0(Player,": ",Age, " years"),
            hovertemplate = paste('xG: %{x:.2f}',
                                  '<br>Goals: %{y:.2f}<br>',
                                  '<b>%{text}</b>'))

# Loop through past seasons to plot with visible = F so that they're hidden
for (indiv_season in season_list[-1]){
  fig <- fig %>%
    add_trace(data = filter(past_data, season == indiv_season), x = ~npxG_per90, y = ~npGls_per90,
              type = 'scatter', mode = 'markers',
              color = ~Comp, size = 8,
              text = ~paste0(Player,": ",Age, " years"),
              hovertemplate = paste('xG: %{x:.2f}',
                                    '<br>Goals: %{y:.2f}<br>',
                                    '<b>%{text}</b>'),
              visible = F)
  
}


fig <- fig %>%
  # black dashed 1:1 line
  add_trace(x = lin_vals, y = lin_vals, 
            type = "scatter", mode = "line",
            line = list(color = 'black', width = 4, dash = 'dash'), 
            alpha = 0.5, visible = T, showlegend = F) %>%
  
  # add legend, axis labels and dropdown menu
  layout(legend = list(orientation = 'h', y = -0.15),
         yaxis = list(title = "Non-penalty goals per 90"),
         xaxis = list (title = "Non-penalty xG per 90"),
         updatemenus = list(list(x = 0.35,
                                 buttons = l_combo)))


# Display the plot
fig  
