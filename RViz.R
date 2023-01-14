

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

#Adnan Graphs

#1) Slope Graph in ggplot2
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


#2) Map


#3) Scatter plot relationship


#4) Correlation Matrix


     