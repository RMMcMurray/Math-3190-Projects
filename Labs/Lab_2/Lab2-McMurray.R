# Math-3190 Lab 2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setwd("~/GitHub/Math_3190_Assignments/Labs/Lab_2")

# Problem 2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Part a
#%%%%%%%%
library(tidyverse)

# Part b
#%%%%%%%%
employee_names <- c("Alice", "Bob", "Charlie", "Diana", "Edward")

# Part c
#%%%%%%%%
monthly_hours <- c(160, 145, 180, 120, 155)

# Part d
#%%%%%%%%
work_data <- cbind(employee_names, monthly_hours)

# Part e
#%%%%%%%%
work_tibble <- tibble(employee_names, monthly_hours)
print(work_data)
print(work_tibble)

# Part f
#%%%%%%%%
print(work_tibble[c(3,5),])

# Part g
#%%%%%%%%
print(work_tibble$employee_names)
print(work_tibble[,1])

# Part h
#%%%%%%%%
mean(work_tibble$monthly_hours)

# Problem 3
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Part a
#%%%%%%%%
to_kelvin <- function(degrees) {
  farenheit <- (5/9)*(degrees + 459.67)
  return(paste("F: ", farenheit))
}
to_kelvin(50)
to_kelvin(60)

# Part b
#%%%%%%%%
fuel_cost <- function(miles, mpg, price_per_gallon) {
  result <- miles/mpg*price_per_gallon
  return(paste("Fuel Cost: $", as.character(result)))
}
fuel_cost(400, 32, 3.25)

# Problem 4
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
?mpg
new_mpg <- as_tibble(mpg) |> 
  filter(manufacturer %in% c("audi","ford", "toyota")) |> 
  mutate(combined_mpg = (cty+hwy)/2) |> 
  select(manufacturer, model, cyl, class, combined_mpg) |> 
  group_by(class) |> 
  summarize(avg = mean(combined_mpg),  std_dev = sd(combined_mpg)) |>
  arrange(desc(avg))

new_mpg

# Problem 5
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Part a
#%%%%%%%%
library(ggplot2)

midwest

ggplot(data = midwest, aes(x = poptotal, y = area)) + 
  geom_point() + 
  labs(x = "Total Population", y = "Area", 
       title = "Midwest Total Population vs Area") + 
  geom_smooth(method = "lm", se = FALSE)


# Part b
#%%%%%%%%
ggplot(midwest, aes(x = poptotal, y = area)) +
  geom_point(aes(color = state)) +
  scale_x_log10() +
  labs(x = "Total Population", y = "Area",
       title = "Midwest Total Population vs Area") +
  geom_smooth(method = "lm", se = FALSE, color = "black")

# Part c
#%%%%%%%%
ggplot(midwest, aes(x = state, y = percollege)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  geom_jitter(color = "black", size = 0.5, width = 0.1) +
  labs(
    x = "State",
    y = "Percent College Educated",
    title = "College Education by State (County Level)"
  )

# Part d
#%%%%%%%%
ggplot(midwest, aes(x = percbelowpoverty)) +
  geom_density(fill = "darkblue") +
  facet_grid(. ~ state) +
  theme_bw() +
  labs(title = "Poverty Rates Across Midwest States",
       x = "Percent Below Poverty",
       y = "Density") +
  theme(plot.title = element_text(size = 18, face = "bold"))

# Part e
#%%%%%%%%
midwest |>
  group_by(state) |>
  summarize(counties = n(), .groups = "drop") |>
  ggplot(aes(x = state, y = counties)) +
  geom_col(fill = 'orange', color = "black") +
  labs(
    x = "State",
    y = "Number of Counties",
    title = "Number of Counties by State"
  ) +
  theme_minimal()

# Problem 6
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Part a
#%%%%%%%%
treeseeds <- read.table("treeseeds.txt", header = TRUE, sep = ",")
head(treeseeds)

# Part b
#%%%%%%%%
blood_pressure <- read.table('blood_pressure.txt', header = TRUE)
head(blood_pressure)

# Part c
#%%%%%%%%
install.packages('readxl')
library('readxl')
concrete_data <- read_excel('Concrete_Data.xls')
head(concrete_data)

# Problem 7
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Part a
#%%%%%%%%
for (hours in 1:length(monthly_hours)){
  if (monthly_hours[hours] > 150){
    print(paste(employee_names[hours], ": Overtime Eligable"))
  } else {
    print(paste(employee_names[hours], ": Standard Hours"))
  }
}

# Part b
#%%%%%%%%
work_tibble <- work_tibble |> 
  mutate(overtime = case_when(
    monthly_hours > 150 ~ "Overtime Eligible",
    TRUE ~ "Standard Hours"
  )) # This one is really cool actually



