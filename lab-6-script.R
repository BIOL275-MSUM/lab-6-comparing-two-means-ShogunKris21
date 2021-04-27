
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

ggplot(data = fish_long, 
       mapping = aes(x = species)) + 
  geom_histogram(bins = 10) +
  facet_wrap(~ location)

# Question D

crabs <- read.csv("chap15q27FiddlerCrabFans.csv")

crabs %>% 
  filter(!is.na(crabType)) %>%
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType), 
    bins = 15, 
    alpha = 0.5, 
    position = "identity",
    na.rm = TRUE
  ) +
  facet_wrap( ~ crabType, ncol=1) +
  scale_fill_manual(values = c("gold", "red", "cyan4", "blue")) +
  theme_minimal()

crab_type_means <-
  crabs %>% 
  filter(!is.na(bodyTemperature)) %>%      # remove missing values
  group_by(crabType) %>% 
  summarize(
    mean = mean(bodyTemperature),
    sd = sd(bodyTemperature),
    n = n(),
    sem = sd / sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()

ggplot(data = crabs, aes(x = crabType, y = bodyTemperature)) +
  geom_jitter(aes(color = crabType),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE,
              na.rm = TRUE) +
  geom_errorbar(aes(y = mean, ymin = lower, ymax = upper), 
                data = crab_type_means,
                width = .1, position = position_nudge(.3)) +
  geom_point(aes(y = mean), data = crab_type_means,
             position = position_nudge(.3)) +
  scale_color_manual(values = c("gold", "red", "cyan4", "blue", "black"))

# Question E

aov_crab_temp <- 
  aov(bodyTemperature ~ crabType, data = crabs)
aov_crab_temp

summary(aov_crab_temp)
