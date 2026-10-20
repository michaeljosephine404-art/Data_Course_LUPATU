library(tidyverse)
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

# Check column names
colnames(dat)

# Convert data into tidy (long) format
dat_tidy <- dat %>%
  pivot_longer(
    cols = c(Hr_24, Hr_48, Hr_144),
    names_to = "Time",
    values_to = "Absorbance"
  ) %>%
  mutate(
    Time = as.numeric(gsub("Hr_", "", Time))
  )

# Create Sample_Type column
dat_tidy <- dat_tidy %>%
  mutate(
    Sample_Type = case_when(
      `Sample ID` %in% c("Soil_1", "Soil_2") ~ "Soil",
      `Sample ID` %in% c("Clear_Creek", "Waste_Water") ~ "Water"
    )
  )

# View tidy data
View(dat_tidy)

# Filter for dilution 0.1
plot_data <- dat_tidy %>%
  filter(Dilution == 0.1)

# Calculate mean absorbance
plot_summary <- plot_data %>%
  group_by(Substrate, Time, Sample_Type) %>%
  summarise(
    Mean_Absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = "drop"
  )

# Create the first graph
ggplot(
  plot_summary,
  aes(
    x = Time,
    y = Mean_Absorbance,
    color = Sample_Type,
    group = Sample_Type
  )
) +
  geom_line() +
  facet_wrap(~ Substrate) +
  labs(
    title = "Just dilution 0.1",
    x = "Time",
    y = "Absorbance",
    color = "Type"
  ) +
  theme_bw()

# Prepare Itaconic Acid data
itaconic_data <- dat_tidy %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(`Sample ID`, Dilution, Time) %>%
  summarise(
    Mean_Absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = "drop"
  )

# View the summarized data
View(itaconic_data)
# Create Itaconic Acid graph
itaconic_plot <- ggplot(
  itaconic_data,
  aes(
    x = Time,
    y = Mean_Absorbance,
    color = `Sample ID`,
    group = `Sample ID`
  )
) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Dilution, nrow = 1) +
  labs(
    x = "Time",
    y = "Mean_absorbance",
    color = "Sample ID"
  ) +
  scale_y_continuous(limits = c(0, 2.5)) +
  theme_bw()

# Display the graph
itaconic_plot
# Install animation packages if needed
if (!requireNamespace("gganimate", quietly = TRUE)) {
  install.packages("gganimate")
}

if (!requireNamespace("gifski", quietly = TRUE)) {
  install.packages("gifski")
}

# Load animation package
library(gganimate)

# Animate the Itaconic Acid graph
animated_plot <- itaconic_plot +
  transition_reveal(Time)

# Generate the animation
my_animation <- animate(
  animated_plot,
  nframes = 100,
  fps = 10,
  width = 900,
  height = 600,
  renderer = gifski_renderer()
)

# Save the animation as a GIF
anim_save(
  "Itaconic_Acid_Animation.gif",
  animation = my_animation
)

# Display the animation
print(my_animation)

