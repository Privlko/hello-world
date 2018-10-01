# Load packages
library(dplyr)
library(ggplot2)
library(openintro)

email
?email
hist(email$num_char, breaks=30)
# Compute summary statistics
email %>%
  group_by(spam) %>%
  summarise(mean(num_char), sd(num_char))

# Compute summary statistics
email %>%
  group_by(spam) %>%
  summarise(median(num_char), IQR(num_char))


# Create plot
email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = spam, y = log_num_char)) +
  geom_boxplot()
