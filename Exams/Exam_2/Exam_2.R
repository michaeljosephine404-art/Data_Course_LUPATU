# Load the libraries
library(tidyverse)

# Read in the UNICEF dataset
unicef <- read_csv("unicef-u5mr.csv")

# View the dataset
View(unicef)

# Task 2: Convert the UNICEF data into tidy format

unicef_tidy <- unicef %>%
  #Combines the year columns into rows.
  #Creates a column containing the years.
  #Creates a column containing mortality rates.
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  #Changes the Year column into numbers.and Removes "U5MR." from the year values.
  mutate(Year = as.numeric(gsub("U5MR.", "", Year, fixed = TRUE)))

# View the tidy dataset
View(unicef_tidy)

# Task 3: Plot each country's U5MR over time

# Use the tidy UNICEF data to plot Year against U5MR for each country
ggplot(unicef_tidy, aes(x = Year, y = U5MR,
                        group = CountryName)) +
  # Draw slightly transparent lines and separate the graphs by continent
  geom_line(alpha = 0.5) +
  facet_wrap(~ Continent) +
  # Add a title and labels for the X and Y axes
  labs(
    title = "Child Mortality Rates Over Time",
    x = "Year",
    y = "Deaths per 1000 Live Births"
  )
# Task 4: Save the first plot

ggsave("LUPATU_Plot_1.png", width = 12, height = 7)

# Task 5: Calculate the average U5MR for each continent and year

# Group the data by continent and year, then calculate the mean while ignoring missing values
unicef_mean <- unicef_tidy %>%
  group_by(Continent, Year) %>%
  summarise(Mean_U5MR = mean(U5MR, na.rm = TRUE),
            .groups = "drop")

# Create a line graph with a different color for each continent
ggplot(unicef_mean, aes(x = Year, y = Mean_U5MR,
                        color = Continent)) +
  geom_line(linewidth = 1.2) +
  # Add a title and labels for the X and Y axes 
  labs(
    title = "Average Child Mortality Rates by Continent",
    x = "Year",
    y = "Mean U5MR"
  )

# Task 6: Save the second plot

ggsave("LUPATU_Plot_2.png", width = 12, height = 7)


# Task 7: Create three linear models of U5MR

# Model 1: Predict U5MR using only Year
mod1 <- lm(U5MR ~ Year, data = unicef_tidy)

# Model 2: Predict U5MR using Year and Continent
mod2 <- lm(U5MR ~ Year + Continent, data = unicef_tidy)

# Model 3: Predict U5MR using Year, Continent, and their interaction
mod3 <- lm(U5MR ~ Year * Continent, data = unicef_tidy)

# View the results of each model
summary(mod1)
summary(mod2)
summary(mod3)

# Task 8: Compare the performance of the three models

# Compare the models using adjusted R-squared
summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared
summary(mod3)$adj.r.squared

# Model 3 is the best because it has the highest adjusted R-squared
# and accounts for Year, Continent, and their interaction.


# Task 9: Plot the three models' predictions

# Create combinations of years and continents
predictions <- expand_grid(
  Year = 1950:2015,
  Continent = unique(unicef_tidy$Continent)
)

# Predict U5MR using each model
predictions$mod1 <- predict(mod1, predictions)
predictions$mod2 <- predict(mod2, predictions)
predictions$mod3 <- predict(mod3, predictions)

# Combine predictions into one column
predictions <- predictions %>%
  pivot_longer(mod1:mod3, names_to = "Model",
               values_to = "Predicted_U5MR")

# Plot the predictions by model and continent
ggplot(predictions, aes(Year, Predicted_U5MR, color = Continent)) +
  geom_line() +
  facet_wrap(~ Model)
