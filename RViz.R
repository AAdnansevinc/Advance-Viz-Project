

#install needed packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
               "ggplot2",
               "tidyverse",
               "dplyr",
               "CGPfunctions",
               "rnaturalearth",
               "sf",
               "cowplot",
               "gganimate",
               "gifski",
               "wbstats",
               "reshape2",
               "ggrepel",
               "directlabels"
)


Sys.setlocale("LC_TIME", "English")

# Set Working directory

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Upload Dataset

data <- read.csv("data/DataForTable2.1.csv")

# Getting additional stats for countries
my_indicators <- c(
  life_exp = "SP.DYN.LE00.IN", 
  gdp_capita ="NY.GDP.PCAP.CD", 
  pop = "SP.POP.TOTL"
)
world_stats <- wb_data(my_indicators, start_date = min(data$year), end_date = max(data$year))

# Inspecting dataframe
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

# Add missing years
countries <- unique(data$Country)
years <- unique(data$Year)
all <- data.frame(expand.grid(countries,years))
names(all) <- c("Country", "Year")
data <- left_join(all,data, by = c("Country"="Country", "Year" = "Year"))

data <- left_join(data,world_stats, by = c("Country"="country", "Year" = "date"))

world <- ne_countries(scale = "medium", returnclass = "sf")

data <- left_join(data, world[c("name","economy","continent", "subregion")], by = c("Country"="name"))

head(data)
tail(data)

nrow(data)

str(data)
summary(data)

## Prepare data  


#1) Map #Dustin_3

# 1.1) Countries

# filter to 2021
data_2021 <- data %>% 
  mutate(HappinessScore = round(HappinessScore,2)) %>% 
  filter(Year == 2021)

# join spacial data with dataset
data_world <- full_join(data_2021, world, by = c("Country"="name","economy","continent", "subregion","geometry"))

# create base world plot
theme_set(theme_bw())

gworld_base <- 
  ggplot(data = data_world) +
  coord_sf(expand = FALSE) +
  geom_sf(aes(geometry =  geometry, fill =  HappinessScore)) +
  scale_fill_gradient(low="red", high="green",na.value = "grey") 

# World Plot  
gworld <- 
  gworld_base +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20, hjust = 0.5)) +
  labs(title = "Happiness Worldwide in 2021",
       caption = "Own creation. Data: World Happiness Report, World Bank")

gworld

# 1.2) Europe

# Use world base plot for subplot
gworld_sub <- 
  gworld_base +
  geom_rect(xmin = -30, xmax = 50, ymin = 35, ymax = 70, 
            fill = NA, colour = "black", size = 1.5) +
  theme(legend.position= 'none',
        axis.text = element_blank(),
        axis.ticks=element_blank(),
        )

# create plot for Europe
geurope <- ggplot(data = data_world) +
  # geom_sf(aes(fill = income_grp)) +
  coord_sf(expand = FALSE) +
  geom_sf(aes(geometry =  geometry, fill =  HappinessScore)) +
  annotate(geom = "text", x = 19, y = 52, label = "Poland", 
           fontface = "italic", color = "black", size = 4) +
  scale_fill_gradient(low="red", high="green",na.value = "grey") +
  coord_sf(xlim = c(-30, 50), ylim = c(35, 70), expand = TRUE) +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20, hjust = 0.5)) +
  labs(title = "Happiness Europe in 2021",
       caption = "Own creation. Data: World Happiness Report, World Bank")

geurope
# generate plot 
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

#4) Bubble plot from gganimate()    # Dustin_1
# variable > gdp and happiness score
#https://gganimate.com/
#https://exts.ggplot2.tidyverse.org/gallery/

# Make a ggplot, but add frame=year: one image per year
bubble <- ggplot(data, aes(GDPPer, HappinessScore, size = pop, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per Capita', y = 'Happiness') +
  transition_time(Year) +
  ease_aes('linear')

# Save at gif:
gif <- animate(bubble,renderer = gifski_renderer())
anim_save(gif,"BubbleChart.gif",animation=bubble)

###################################################################################


p <- ggplot(data, aes(GDPPer, HappinessScore, size = pop, colour = Country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  # scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  # geom_label_repel(aes(label = Country), size = 2) + 
  # facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  labs(title = 'Happiness vs. GDP per Capita Over Time in {round(frame_time,0)}', 
       x = 'GDP per capita', y = 'Happiness',
       caption = "Own creation. Data: World Happiness Report, World Bank") +
  transition_time(Year) +
  ease_aes('linear')

animate(p, renderer = gifski_renderer(), start_pause = 2, end_pause = 30)

a <- ggplot(filter(data, !is.na(data$HappinessScore)), aes(GDPPer, HappinessScore, size = pop)) +
  geom_point(alpha = 0.7, show.legend = FALSE, aes(color = Country), na.rm = FALSE) +
  # scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  geom_label_repel(aes(label = Country), size = 2, na.rm = FALSE) +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  # facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Happiness vs. GDP per Capita Over Time in {round(frame_time,0)}', 
       x = 'GDP per capita', y = 'Happiness',
       caption = "Own creation. Data: World Happiness Report, World Bank") +
  transition_time(Year) +
  shadow_wake(0.5) +
  ease_aes('linear')

a
 
gif <- animate(a, renderer = gifski_renderer(), start_pause = 2, end_pause = 30,duration = 60)

anim_save(a, "HappinessVsGDP.gif")


#5) Line Chart # to observe happiness over time # Dustin_2

#at lest two line one for average
# one for each countinent.

data_line <- data %>%
  group_by(Year, continent) %>%
  summarize(HappinessScore = mean(HappinessScore,na.rm = TRUE)) %>%
  ungroup()

data_line2 <- data %>%
  group_by(Year) %>%
  summarize(HappinessScore = mean(HappinessScore,na.rm = TRUE)) %>%
  ungroup()

data_line2 <- data_line2 %>% add_column("Country"="World")

names(data_line) <- c("Year",
                      "Country",
                      "HappinessScore")

data_line <- union_all(data_line, data_line2)
data_line <- union_all(data_line, filter(data[c("Year", "Country","HappinessScore")], Country == "Poland"))

ggplot(filter(data_line, Country %in% c("World","Europe","Poland") & Year >= 2010),aes(Year,HappinessScore, group = Country, color = Country)) +
  geom_rect(aes(xmin = 2019, ymin = -Inf,
                xmax = Inf, ymax = Inf),
            alpha = .4, fill = "grey", color = NA
            ) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(xintercept = 2019,color = "black", size = 2) +
  geom_dl(aes(label = Country), method = list(dl.combine("last.points"))) +
  annotate("text", x = 2019, y = 7, label = "COVID-19", vjust = 2, hjust=-0.1, size = 5) +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20, hjust = 0.5)) +
  labs(color = NULL,
       title = "Happiness Under COVID-19",
       caption = "Own creation. Data: World Happiness Report, World Bank") +
  scale_color_manual(values = c('#003399', '#D22630', 'black')) 


#6) Histogram > distribution of happiness score > look  at the class 7. #Adnan_4

#7) Boxplot by contient #Dustin_4

ggplot(data = filter(data, !is.na(data$continent)),
       aes(y = HappinessScore, x = continent)) +
  geom_boxplot() +
  stat_summary(geom = 'point', shape = 15, fun = mean, size = 2) + 
  labs(title = "Happiness Compared by Country",
       x = "",
       caption = "Own creation. Data: World Happiness Report, World Bank") + 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20, hjust = 0.5)) 

#8) BarPlot focusing on poland. #Adnan_3

     