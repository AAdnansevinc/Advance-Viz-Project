

#install needed packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
               "ggplot2",
               "tidyverse",
               "dplyr",
               "CGPfunctions"
)


Sys.setlocale("LC_TIME", "English")

# Set Working directory

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Upload Dataset

data <- read.csv("data/DataForTable2.1.csv")
nrow(data)

data <- data[complete.cases(data),]  #only complete dataset
nrow(data)



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
# 1.2) Europe

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

     