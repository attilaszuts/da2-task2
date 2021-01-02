# read data ---------------------------------------------------------------
library(tidyverse); library(janitor)

rm(list = ls())
laptops <- "data/raw/laptop-price/"
raw1 <- read_csv(paste0(laptops, "laptop_price.csv")) %>% clean_names()
raw2 <- read_csv(paste0(laptops, "laptops.csv")) %>% clean_names()
raw3 <- read_csv(paste0(laptops, "laptops2.csv")) %>% clean_names()


# how many names match from raw1 and raw2
sum(names(raw1) == names(raw2))
sum(names(raw1) == names(raw3))
sum(names(raw2) == names(raw3))

names(raw1) == names(raw2)
names(raw1) == names(raw3)

names(raw1)
names(raw2)
names(raw3)

head(raw1)
head(raw2)
head(raw3)


# mutate dfs --------------------------------------------------------------


raw1 <- raw1 %>% 
  mutate(laptop_id = seq(1, length(raw1$laptop_id)))
raw2 <- raw2 %>% 
  rename(company = manufacturer,
         prodcut = model_name,
         type_name = category,
         inches = screen_size,
         screen_resolution = screen,
         memory = storage,
         op_sys = operating_system) %>% 
  mutate(op_sys = paste0(op_sys, "-", operating_system_version)) %>% 
  select(-operating_system_version) %>% 
  mutate(laptop_id = seq(length(raw1$laptop_id) + 1, length(raw1$laptop_id) + length(raw2$cpu)),
         inches = as.numeric(gsub('"', "", raw2$screen_size)))
raw3 <- raw3 %>% 
  rename(laptop_id = x1) %>%
  mutate(laptop_id = seq(length(raw1$laptop_id) + length(raw2$laptop_id) + 1, length(raw1$laptop_id) + length(raw2$laptop_id) + length(raw3$cpu)))

laptop <- bind_rows(raw1, raw2, raw3, .id = "laptop_id")
sum(duplicated(laptop))




# use only first df -------------------------------------------------------

laptop <- raw1 %>% 
  mutate(ram = as.numeric(gsub("GB", "", raw1$ram)),
         weight = as.numeric(gsub("kg", "", raw1$weight))) %>% 
  mutate(screen_small = ifelse(inches < 15, T, F),
         screen_mid = ifelse(inches >= 15 & inches < 17, T, F),
         screen_big = ifelse(inches >= 17, T, F)) %>% 
  mutate(touchscreen = ifelse(grepl("Touchscreen", laptop$screen_resolution), T, F),
         ips = ifelse(grepl("IPS", laptop$screen_resolution), T, F)) 

# number of different models for each manufacturer
laptop %>% group_by(company) %>% summarise(count = n()) %>% arrange(desc(count))

# number of different types
laptop %>% group_by(type_name) %>% summarise(count = n()) %>%  arrange(desc(count))

# number of different screen sizes 
laptop %>% group_by(inches) %>% summarise(count = n()) %>%  arrange(desc(inches))
laptop %>% summarise(screen_small = sum(screen_small), screen_mid = sum(screen_mid), screen_big = sum(screen_big))

# number of different sceens 
view(laptop %>% group_by(screen_resolution) %>% summarise(count = n()) %>%  arrange(desc(count)))

# number of different cpus -> needs cleaning
laptop %>% group_by(cpu) %>% summarise(count = n()) %>%  arrange(desc(count))

# number of different ram
laptop %>% group_by(ram) %>% summarise(count = n()) %>%  arrange(desc(ram))

# number of different memory -> needs cleaning
laptop %>% group_by(memory) %>% summarise(count = n()) %>%  arrange(desc(count))

# number of different gpu -> needs cleaning
laptop %>% group_by(gpu) %>% summarise(count = n()) %>%  arrange(desc(count))

# number of different op_sys
laptop %>% group_by(op_sys) %>% summarise(count = n()) %>%  arrange(desc(count))

# weight
laptop %>% 
  ggplot(aes(weight)) + geom_histogram()

# prices
laptop %>% 
  ggplot(aes(price_euros)) + geom_histogram(bins = 50)


# number of different sceens -> needs cleaning
temp <- laptop %>% 
  mutate(touchscreen = ifelse(grepl("Touchscreen", laptop$screen_resolution), T, F)) %>% 
  group_by(screen_resolution, touchscreen) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

view(temp %>% filter(touchscreen != T))
view(temp)
rm(temp)

grepl("Touchscreen", laptop$screen_resolution)


view(laptop %>% select(screen_resolution) %>% filter(grepl("[:digit:]", laptop$screen_resolution) == T))

pattern = "([:digit:]{3,4}x[:digit:]{3,4})"
str_extract_all(laptop$screen_resolution, pattern, simplify = T)