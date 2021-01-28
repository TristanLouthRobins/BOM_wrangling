library(tidyverse)
library(bomrang)

?bomrang # bomrang package help

id <- c("23034") # target weather station(s) for data
# 23000 = Adelaide
# 23034 = Adelaide Airport
# 23875 = Parawa
# 23886 = Sellicks Hill
# 23804 = Victor Harbor
# 24048 = Renmark
# 26021 = Mount Gambier
# 16090 = Cooper Pedy


# Retrieve historical data from designated weather station
rain_data <- get_historical(stationid = id,
                            latlon = NULL,
                            radius = NULL,
                            type = "rain") %>% 
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

# calculate cumulative and monthly rainfall per year

rain_df.2 <- rain_df.1 %>% 
  group_by(Year) %>% 
  mutate(cr = cumsum(rainfall))

rain_mean.month <- rain_df.2 %>% 
  group_by(Year, Month) %>% 
  summarise(mmr = mean(rainfall))

rain_mean.year <- rain_df.2 %>% 
  group_by(Year) %>% 
  summarise(ymr = mean(rainfall))

# Plotting the data: 0) Defining a custom theme for the visualisations

theme_bom <- function(){
  theme(
    text = element_text(family = "Arial"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "mistyrose1"),
    panel.background = element_rect(fill = "lightyellow2"),
    legend.title = element_text(family = "Courier", size = 9),
    legend.text = element_text(family = "Courier", size = 8),
    legend.background = element_rect(fill = "mistyrose2")
  )
}

# Plotting the data: 1A) Plot of daily and cumulative rainfall

year_range <- c("1996" : "2020")
max.yr <- rain_df.2 %>% filter(Year %in% year_range) %>% group_by(Year) %>% summarise(max = max(cr)) 

plot1 <- ggplot(data = rain_df.2[rain_df.2$Year %in% year_range,],
                aes(x = Date, y = rainfall, colour = Year)) +
  geom_line(aes(x = Date, y = cr), colour = "sienna2") +
  geom_point(aes(y = rainfall), alpha = 0.3) +
  theme_classic() +
  theme_bom() +
  labs(
    title = "Daily & cumulative rainfall 1996 - 2021",
    x = "",
    y = "rainfall in mm",
    caption = "Source: Bureau of Meterology."
  )

plot1

# Plotting the data: 1B) Plot of daily and cumulative rainfall, faceted by year. 

plot1 +
  geom_hline(data = max.yr, aes(yintercept = max, colour = Year), linetype = "dotdash") +
  facet_wrap(. ~ Year, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


