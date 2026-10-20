library(tidyverse)
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")
colnames(dat)
dat_tidy <- dat %>%
  pivot_longer(
    cols = c(Hr_24, Hr_48, Hr_144),
    names_to = "Time",
    values_to = "Absorbance" ) %>%
  mutate(
    Time = as.numeric(gsub("Hr_", "", Time)))
dat_tidy <- dat_tidy %>%
  mutate(
    Sample_Type = case_when(
      `Sample ID` %in% c("Soil_1", "Soil_2") ~ "Soil",
      `Sample ID` %in% c("Clear_Creek", "Waste_Water") ~ "Water"  ) )
View(dat_tidy)
plot_data <- dat_tidy %>%
  filter(Dilution == 0.1)
plot_summary <- plot_data %>%
  group_by(Substrate, Time, Sample_Type) %>%
  summarise(
    Mean_Absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = "drop")
ggplot(
  plot_summary,
  aes(
    x = Time,
    y = Mean_Absorbance,
    color = Sample_Type,
    group = Sample_Type )) +
  geom_line() +
  facet_wrap(~ Substrate) +
  labs(
    title = "Just dilution 0.1",
    x = "Time",
    y = "Absorbance",
    color = "Type") +
  theme_bw()

itaconic_data <- dat_tidy %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(`Sample ID`, Dilution, Time) %>%
  summarise(
    Mean_Absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = "drop")
View(itaconic_data)
itaconic_plot <- ggplot(
  itaconic_data,
  aes(
    x = Time,
    y = Mean_Absorbance,
    color = `Sample ID`,
    group = `Sample ID` )) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Dilution, nrow = 1) +
  labs(
    x = "Time",
    y = "Mean_absorbance",
    color = "Sample ID") +
  scale_y_continuous(limits = c(0, 2.5)) +
  theme_bw()
itaconic_plot
if (!requireNamespace("gganimate", quietly = TRUE)) {
  install.packages("gganimate")
}

if (!requireNamespace("gifski", quietly = TRUE)) {
  install.packages("gifski")
}
library(gganimate)
animated_plot <- itaconic_plot +
  transition_reveal(Time)
my_animation <- animate(
  animated_plot,
  nframes = 100,
  fps = 10,
  width = 900,
  height = 600,
  renderer = gifski_renderer())
anim_save("Itaconic_Acid_Animation.gif",
  animation = my_animation)
print(my_animation)

