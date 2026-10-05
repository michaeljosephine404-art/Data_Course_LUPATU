#task 1
library(tidyverse)
covid_data <- read_csv("cleaned_covid_data.csv")
covid_data
#task 2
A_states <- covid_data %>%
filter(grepl("^A", Province_State))
unique(A_states$Province_State)
#task 3
ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
geom_point() +
geom_smooth(method = "loess", se = FALSE) +
facet_wrap(~ Province_State, scales = "free")
#task 4
state_max_fatality_rate <- covid_data %>% group_by(Province_State) %>%
summarize( Maximum_Fatality_Ratio = max( Case_Fatality_Ratio, na.rm = TRUE )) %>%
arrange(desc(Maximum_Fatality_Ratio))
View(state_max_fatality_rate)
#task 5
state_max_fatality_rate %>%
mutate(Province_State = factor(Province_State,levels = Province_State )) %>%
ggplot(aes(x = Province_State,y = Maximum_Fatality_Ratio)) +
geom_col() +theme(axis.text.x = element_text(angle = 90,hjust = 1) )
#task 6
us_deaths <- covid_data %>%
group_by(Last_Update) %>%
summarize(Total_Deaths = sum(Deaths, na.rm = TRUE) )
ggplot(us_deaths, aes(x = Last_Update,y = Total_Deaths)) +
geom_line()
