library(tidyverse)
library(bomrang)

?bomrang # bomrang package help

id <- c("023000")                                 # target weather station(s) for data
target <- c("2017","2018", "2019","2020", "2021") # target years for filtering

# Retrieve historical data from designated weather station
rain_data <- get_historical(stationid = id,
                            latlon = NULL,
                            radius = NULL,
                            type = "rain") %>% 
  filter(year %in% target) %>% 
  filter(rainfall > -0.1) %>% 
  select(-1, -7, -8)  # removes 'product_code', 'period' and 'quality' columns
  

# Data tidy (part 1)
rain_df.1 <- as_tibble(rain_data)

rain_df.1 <- rain_df.1 %>% 
  mutate(Month = month, Year = year) %>% 
  unite(Date, year, month, day, sep = "/", remove = TRUE, na.rm = FALSE) %>% 
  mutate(Date = as.Date(Date, format = "%Y/%m/%d"),
        Month = as_factor(Month), 
        Year = as_factor(Year))
  
levels(rain_df.1$Month) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 
  
# Data tidy (part 2) note: reassign to 'rain_df.2' for debugging purposes.
rain_df.2 <- rain_df.1

# calculate cumulative and monthly rainfall per year

cr.18 <- rain_df.2 %>% filter(Year == 2018) %>% mutate(CR = cumsum(rainfall))
avg_m.18 <- rain_df.2 %>% filter(Year == 2018) %>% 
  group_by(Month) %>% summarise(val = mean(rainfall)) %>% mutate(Year = paste0("2018"))

cr.19 <- rain_df.2 %>% filter(Year == 2019) %>% mutate(CR = cumsum(rainfall))
avg_m.19 <- rain_df.2 %>% filter(Year == 2019) %>% 
  group_by(Month) %>% summarise(val = mean(rainfall)) %>% mutate(Year = paste0("2019"))

cr.20 <- rain_df.2 %>% filter(Year == 2020) %>% mutate(CR = cumsum(rainfall))
avg_m.20 <- rain_df.2 %>% filter(Year == 2020) %>% 
  group_by(Month) %>% summarise(val = mean(rainfall)) %>% mutate(Year = paste0("2020"))

cr.21 <- rain_df.2 %>% filter(Year == 2021) %>% mutate(CR = cumsum(rainfall))
avg_m.21 <- rain_df.2 %>% filter(Year == 2021) %>% 
  group_by(Month) %>% summarise(val = mean(rainfall)) %>% mutate(Year = paste0("2021"))

# Binding the relevant dataframes together

rain_df.2 <- do.call("rbind", list(cr.18, cr.19, cr.20, cr.21))
rain_df.mean_m <- do.call("rbind", list(avg_m.18, avg_m.19, avg_m.20, avg_m.21)) 

# Setting up yearly summary averages 

rain_df.mean_yr <- rain_df.2 %>% group_by(Year) %>% 
  summarise(mean_rainfall = mean(rainfall))

#

# Plotting the data: 0) Defining a custom theme for the visualisations

theme_bom <- function(){
  theme(plot.background = element_rect(fill = "green"),
        panel.background = element_rect(fill = "lightgreen"))
}

# Plotting the data: 1) Plot of daily and cumulative rainfall

plot1 <- ggplot(data = rain_df.2[rain_df.2$Year != "2017",],
                aes(x = Date, y = rainfall, colour = Year))

plot1 +
  geom_line(aes(x = Date, y = CR), colour = "sienna2") +
  geom_point(aes(y = rainfall), alpha = 0.3) +
  theme_classic() +
  labs(
    title = "Daily & cumulative rainfall 2018 - 2021",
    x = "",
    y = "rainfall in mm",
    caption = "Source: Bureau of Meterology, Adelaide City weather station. Data pulled using R package, 'bomrang'."
  )

plot1
