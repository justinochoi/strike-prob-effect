library(tidyverse) 

# Load in the 2023 MLB season 
data <- read.csv(file.choose()) 

# Removing bat tracking data, basic filtering 
data <- 
  data %>% 
  select(-bat_speed, -swing_length) %>% 
  drop_na(plate_x, plate_z, batter, pitcher) 

# Define strike zone and indicate whether pitch was a strike 
data <- 
  data %>%
  mutate(is_strike = case_when(
    between(plate_x, -0.88, 0.88) & 
    between(plate_z, 1.52, 3.67) ~ 1, 
    .default = 0
    )
  ) 

# Ensure each batter-pitcher matchup has enough samples 
# Create count variable 
matchup <- 
  data %>% 
  group_by(pitcher, batter) %>% 
  filter(n() >= 20) %>% 
  ungroup() %>% 
  mutate(count = paste(balls, strikes, sep = "-")) %>% 
  select(pitcher, batter, count, is_strike)
  
matchup$batter <- as.factor(matchup$batter) 
matchup$pitcher <- as.factor(matchup$pitcher)
matchup$count <- as.factor(matchup$count)

library(lme4) 
library(mixedup)

# Random-effects logistic regression 
model <- glmer(is_strike ~ (1 | pitcher) + (1 | batter) + (1 | count), 
                data = matchup, family = "binomial") 

summary(model) 

rand_effects <- as_tibble(extract_random_effects(model)) 

rand_effects$group_var <- as.factor(rand_effects$group_var)

library(scales)

# Visualization 
rand_effects %>% 
  filter(group_var != 'count') %>% 
  ggplot(aes(x=value, fill=group_var)) + 
  geom_density(alpha = 0.5, adjust = 1.5) + 
  theme_bw() +  
  scale_x_continuous(breaks = pretty_breaks(n=8)) + 
  labs(
    x = "Effect Size", 
    y = "Density", 
    title = "Effect of Batter & Pitcher on Strike Probability", 
    subtitle = "2023, includes count adjustment", 
    fill = "Player Type"
  ) + 
  theme(plot.title = element_text(face = "bold")) 



