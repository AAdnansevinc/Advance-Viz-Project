

#install needed packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
               "ggplot2",
               "tidyverse",
               "dplyr",
               "CGPfunctions",
               "rnaturalearth",
               "sf",
               "cowplot"
)


Sys.setlocale("LC_TIME", "English")

# Set Working directory

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Upload Dataset

data <- read.csv("data/DataForTable2.1.csv")
nrow(data)

data <- data[complete.cases(data),]  #only complete dataset
nrow(data)

# Add missing years
countries <- unique(data$Country)
years <- unique(data$Year)
all <- data.frame(expand.grid(countries,years))
names(all) <- c("Country", "Year")
data <- left_join(all,data, by = c("Country"="Country", "Year" = "Year"))

#drop the below columns
# ["NegativeEffect","PositiveEffect","Confidence"]
names(data)


head(data)
tail(data)
#Rename Columns

names(data)<-c("Country", 
               "Year", 
               "HappinessScore",  #"LifeLadder"
               "GDPPer",
               "SocialSupport",
               "LifeExpectancy",
               "Freedom",
               "Generosity",
               "Corruption",
               "PositiveEffect",
               "NegativeEffect",
               "Confidence")


names(data)

head(data)
tail(data)

nrow(data)

str(data)
summary(data)

## Prepare data  


#1) Map #Dustin_3

# 1.1) Countries

world <- ne_countries(scale = "medium", returnclass = "sf")

data_2021 <- data %>% 
  mutate(HappinessScore = round(HappinessScore,2)) %>% 
  filter(Year == 2021)

data_world <- full_join(data_2021, world, by = c("Country"="name"))

theme_set(theme_bw())

gworld_base <- 
  ggplot(data = data_world) +
  # geom_sf(aes(fill = income_grp)) +
  coord_sf(expand = FALSE) +
  geom_sf(aes(geometry =  geometry, fill =  HappinessScore)) +
  scale_fill_gradient(low="red", high="green",na.value = "grey") 
  
gworld <- 
  gworld_base +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20)) +
  labs(title = "Happieness Worldwide in 2021")

gworld

# 1.2) Europe

gworld_sub <- 
  gworld_base +
  geom_rect(xmin = -30, xmax = 50, ymin = 35, ymax = 70, 
            fill = NA, colour = "black", size = 1.5) +
  theme(legend.position= 'none',
        axis.text = element_blank(),
        axis.ticks=element_blank(),
        )


geurope <- ggplot(data = data_world) +
  # geom_sf(aes(fill = income_grp)) +
  coord_sf(expand = FALSE) +
  geom_sf(aes(geometry =  geometry, fill =  HappinessScore)) +
  scale_fill_gradient(low="red", high="green",na.value = "grey") +
  coord_sf(xlim = c(-30, 50), ylim = c(35, 70), expand = TRUE) +
  # scale_fill_brewer(palette = 'YlOrBr') +
  # scale_fill_viridis_c(option = "plasma", trans = "sqrt", 
  #                      breaks = seq(0, 10e09, by=250e6),
  #                      labels = paste(seq(0, 10e09, by=250e6)/1e6, 'mln'),
  #                      guide = guide_colorbar(barwidth = 30), name = NULL) +
  # theme(legend.position = 'right') 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20)) +
  labs(title = "Happieness Europe in 2021")

ggdraw() +
  draw_plot(geurope,
            0, 0, 1, 1) +
  draw_plot(gworld_sub, 
            0.03, 0.25, 0.2, 0.2)


#2) Slope Graph in ggplot2 or  most probably Bump Chart # Adnan_1
# European countries, bottom 10.
# Top 10 counties by happiness score 
data1<-data %>% 
  select("Country","Year","HappinessScore") %>% 
  mutate(HappinessScore = round(HappinessScore,1)) %>% 
  filter(HappinessScore > mean(data$HappinessScore), Year > 2015) 

data1$Year <- as.character(data1$Year)


newggslopegraph(data1, Year, HappinessScore, Country,
                Title = "Happiness Score",
                SubTitle = "2012-2021",
                Caption = "By R CHARTS") +
  theme_gray() +
  theme(legend.position = "none")


#3) Scatter plot relationship, Correlation Matrix # Adnan_2


head(data)

#4) Bobble plot from gganimate()    # Dustin_1
# variable > gdp and happiness score
#https://gganimate.com/
#https://exts.ggplot2.tidyverse.org/gallery/


#5) Line Chart # to observe happiness over time # Dustin_2

#at lest two line one for average
# one for each countinent.

#6) Histogram > distribution of happiness score > look  at the class 7. #Adnan_4

#7) Boxplot by contient #Dustin_4

#8) BarPlot focusing on poland. #Adnan_3

     