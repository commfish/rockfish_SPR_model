# Clean up Kodiak black rockfish data for SPR model examination
# ben.williams@alaska.gov

# load ----
library(tidyverse)
library(lubridate)
library(splitstackshape)

# data ----
# age length weight data
brf <- read.csv("data/black_rockfish_data_sep_18_2015.csv") 
names(brf) <- c('year', 'vessel', 'adfg', 'area', 'port', 'section', 'gear', 'date', 'species.code', 'sex', 'length', 'maturity', 'weight', 'sampler', 'sample', 'age', 'comments')


brf %>% 
  mutate(date = mdy(date),
         Year = factor(year),
         mature = ifelse(maturity>2, 1, 0),
         Mature = factor(mature),
         age = ifelse(age<1, NA, age),
         Sex = factor(sex),
         Age = factor(age)) -> brf

# maturity data
read.csv("data/kodiak_rockfish_maturity.csv") %>% 
  expandRows(., 'n') %>% 
  mutate(Mature = factor(mature)) %>% 
  filter(sex==2) -> mat

m1 <- glm(Mature ~ age, data = mat, family = binomial)
summary(m1)


# create data for app

brf %>%  
  filter(sex == 2, !is.na(age)) %>% 
  group_by(age) %>% 
  summarise(weight = median(weight, na.rm = T))  %>% 
  full_join(data.frame(age = min(brf$age, na.rm = T):max(brf$age, na.rm = T))) %>%  data.frame() %>% arrange(age) %>% 
  fill(weight) %>% 
  mutate(maturity = predict(m1, ., type = 'response'),
         N_F = 1000,
         N_F.0 = 1000,
         N_F1 = 1000,
         N_F2 = 1000,
         N_F3 = 1000,
         N_F4 = 1000,
         N_F5 = 1000,
         N_F6 = 1000,
         N_F7 = 1000,
         N_F8 = 1000,
         N_F9 = 1000,
         N_F10 = 1000,
         N_Fcur = 1000) %>% 
  write.csv(., file = "spr/spr.csv")

         