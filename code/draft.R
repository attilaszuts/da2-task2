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

# number of different ram
laptop %>% group_by(ram) %>% summarise(count = n()) %>%  arrange(desc(ram))


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


# cpu ---------------------------------------------------------------------

# number of different cpus -> needs cleaning
laptop %>% group_by(cpu) %>% summarise(count = n()) %>%  arrange(desc(count))

temp3 <- laptop %>% 
  mutate(cpu = tolower(trimws(cpu))) %>% 
  group_by(cpu) %>% 
  summarise(count = n())

temp3 <- temp3 %>% 
  add_column(lencpu = sapply(strsplit(temp3$cpu, " "), length)) %>% 
  arrange(lencpu, cpu)


# number of words in cpu 
## -> AMD
sapply(strsplit(temp1$cpu, " "), length)
## -> Intel
sapply(strsplit(temp2$cpu, " "), length)


# final version will be this
laptop %>% 
  mutate(cpu = tolower(trimws(cpu))) %>% # trim whitespace
  mutate(cpu_manufac = strsplit(x = cpu, split = " ")[[1]][[1]][[1]]) # extract manufacturer

laptop %>% 
  mutate(cpu = tolower(trimws(cpu))) %>% # trim whitespace
  mutate(cpu_manufac = cpuMan(laptop)) %>% # extract manufacturer
  # add_column(lencpu = sapply(strsplit(laptop$cpu, " "), length)) %>% 
  mutate(cpu_model = cpuModel(laptop)) %>% 
  # select(-lencpu) %>% 
  # filter(lencpu == 4, cpu_manufac == "amd") %>% 
  select(cpu, lencpu, cpu_manufac, cpu_model) %>% 
  view()

# function that extracts cpu manufacturer
cpuMan <- function(x) {
  outlist <- c()
  for (e in 1:length(x$cpu)) {
    outlist <- c(outlist, strsplit(x$cpu[e], split = " ")[[1]][[1]])
  }
  return(outlist)
}

x <- laptop
e <- 1
# function that extracts cpu model
cpuModel <- function(x) {
  outlist <- c()
  for (e in 1:length(x$cpu)) {
    if (x$lencpu[e] == 5 & grepl("xeon", x$cpu[e]) != T)  {
      outlist <- c(outlist, paste(strsplit(x$cpu[e], split = " ")[[1]][[2]], strsplit(x$cpu[e], split = " ")[[1]][[3]]))
    } else if (x$lencpu[e] == 4 & x$cpu_manufac[e] == "intel") {
      outlist <- c(outlist, paste(strsplit(x$cpu[e], split = " ")[[1]][[2]], strsplit(x$cpu[e], split = " ")[[1]][[3]]))
    } else {
      outlist <- c(outlist, strsplit(x$cpu[e], split = " ")[[1]][[2]])
    }
  }
  return(outlist)
}

rm(list = c("temp1", "temp2", "temp3", "tcpu"))

# memory ------------------------------------------------------------------

# number of different memory -> needs cleaning
laptop %>% group_by(memory) %>% summarise(count = n()) %>%  arrange(desc(count)) %>% view()

unique(laptop %>% filter(ssd == F, hdd == T) %>% select(memory)) %>% view() # hdd
unique(laptop %>% filter(ssd == T, hdd == F) %>% select(memory)) %>% view() # ssd
unique(laptop %>% filter(ssd == T, hdd == T) %>% select(memory)) %>% view() # both


memory <- function(x) {
  outlist <- list()
  ssd <- c()
  hdd <- c()
  for (e in 1:length(x$memory)) {
    if (x$ssd[e] == T & x$hdd[e] == F) {
      ssd <- c(ssd, str_extract(x$memory[e], "[:digit:]+tb|[:digit:]+gb")[[1]])
      hdd <- c(hdd, 0)
    } else if (x$ssd[e] == F & x$hdd[e] == T) {
      hdd <- c(hdd, str_extract(x$memory[e], "[:digit:]+tb|[:digit:]+gb|1\\.0tb")[[1]])
      ssd <- c(ssd, 0)
    } else {
      tryCatch(
        expr = {
          if (grepl("hybrid", x$memory[e])) {
            hdd <- c(hdd, str_extract_all(x$memory[e], "[:digit:]+gb|[:digit:]+\\.*[:digit:]*tb")[[1]][[1]])
            ssd <- c(ssd, 0)
          } else {
            ssd <- c(ssd, str_extract_all(x$memory[e], "[:digit:]+gb|[:digit:]+\\.*[:digit:]*tb")[[1]][[1]])
            hdd <- c(hdd, str_extract_all(x$memory[e], "[:digit:]+gb|[:digit:]+\\.*[:digit:]*tb")[[1]][[2]])
          }},
        error = function(er) {
          message("there was an error")
          print(laptop$memory[e])
          if (x$ssd[e] == T & x$hdd[e] == F) {
            print("ssd")
          } else if (x$ssd[e] == F & x$hdd[e] == T) {
            print("hdd")
          } else {
            print(paste(str_extract_all(x$memory[e], "[:digit:]+gb|[:digit:]+\\.*[:digit:]*tb")[[1]][[1]], str_extract_all(x$memory[e], "[:digit:]+gb|[:digit:]+\\.*[:digit:]*tb")[[1]][[2]]))
          }
          }
        )
    }
  }
  outlist <- list(ssd = ssd, hdd = hdd)
  return(outlist)
}

laptop %>% 
  add_column(ssd_size = memory(laptop)["ssd"][[1]], hdd_size = memory(laptop)["hdd"][[1]]) %>% 
  # filter(grepl("tb", ssd_size)) %>% select(ssd_size) %>% unique()
  mutate(hdd_size = ifelse(grepl("tb", hdd_size), as.numeric(gsub("tb", "", hdd_size))*1000, as.numeric(gsub("gb", "", hdd_size))),
         ssd_size = ifelse(grepl("gb", ssd_size), as.numeric(gsub("gb", "", ssd_size)), as.numeric(gsub("tb", "", ssd_size))*1000)) %>% select(ssd_size) %>% unique()
  
# gpu --------------------------------------------------------------------

# number of different gpu -> needs cleaning
laptop %>% group_by(gpu) %>% summarise(count = n()) %>%  arrange(desc(gpu)) %>% mutate(lengpu = sapply(strsplit(gpu, split = " "), length)) %>% arrange(lengpu, gpu, count) %>% filter(count > 20) %>%  view()


gpuMan <- function(x) {
  outlist <- c()
  for (e in 1:length(x$gpu)) {
    outlist <- c(outlist, strsplit(x$gpu[e], split = " ")[[1]][[1]])
  }
  return(outlist)
}
gpuMan(laptop)
