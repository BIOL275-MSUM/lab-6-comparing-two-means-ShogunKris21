
# load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)

# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")


# put data in tidy format ------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()


# Question A t-test -------------------------------------------------------

fish_ttest <- t.test(species ~ location, data = fish_long)

# Question B difference in means -----------------------------------------

fish_ttest$estimate

# Question C histograms --------------------------------------------------

distinct(fish_long, tributary, species, location)

ggplot(data = fish_long, 
       mapping = aes(x = tributary, 
                     y = species, 
                     fill = location)) + 
  geom_bar(stat = "identity",
           position = "dodge")