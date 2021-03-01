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
temp_data <- get_historical(stationid = id,
                            latlon = NULL,
                            radius = NULL,
                            type = "max") %>% 
  filter(max_temperature > -0.1) %>% 
  select(-1, -7, -8)  # removes 'product_code', 'period' and 'quality' columns


# Data tidy (part 1)
temp_df.1 <- as_tibble(temp_data)

temp_df.1 <- temp_df.1 %>% 
  mutate(Month = month, Year = year) %>% 
  unite(Date, year, month, day, sep = "/", remove = TRUE, na.rm = FALSE) %>% 
  mutate(Date = as.Date(Date, format = "%Y/%m/%d"),
         Month = as_factor(Month), 
         Year = as_factor(Year))

levels(temp_df.1$Month) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 

# calculate monthly max_temperature per month and year

temp_mean.month <- temp_df.2 %>% 
  group_by(Year, Month) %>% 
  summarise(mmr = mean(max_temperature))

temp_mean.year <- temp_df.2 %>% 
  group_by(Year) %>% 
  summarise(ymt = mean(max_temperature)) %>% 
  mutate(station = "station")

# Plotting examples

# Terrifying year mean temp rise for last 60 years

range <- c(1955 : 2020)

plot <- temp_mean.year %>% 
  filter(Year %in% range) %>%  
  mutate(Year = as.Date(Year, format = "%Y"))


gg1 <- ggplot(plot, aes(x = Year, y = ymt, group = 1)) +
  geom_point() 

gg1 + geom_smooth(method='lm')

ggplot(data = plot,
       aes(x = Year, y = station, fill = ymt)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "", y = "", fill = "Year Mean Temp") +
  theme(legend.position = "bottom")

# Examining the last ten January's 

range <- c(2011 : 2021)

plot <- temp_df.1 %>% 
  filter(Year %in% range) %>% 
  filter(Month == "Jan") %>% 
  mutate(md = format(Date, format = "%m-%d")) 

avg <- plot %>% 
  group_by(Year) %>% 
  summarise(mean_temp = mean(max_temperature))
 
mutate(md = as_factor(Date)) # changing 'Month' into a factor)

 ggplot(plot, aes(x = md, y = max_temperature, group = 1)) +
  geom_line() +
   facet_wrap(~ Year) +
   geom_hline(data = avg, aes(yintercept = mean_temp, colour = "red"), linetype = "dotdash") +
   labs(x = "", y = "") +
   theme(legend.position = "none",
         axis.text.x = element_blank())
  