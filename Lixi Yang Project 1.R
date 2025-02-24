library(tidyverse)

hpi_data <- read_csv("CSUSHPINSA.csv") |> 
  rename(date = observation_date, hpi = CSUSHPINSA) |> 
  mutate(
    date = as_date(date),
    home_price = hpi * 1000,
    month = floor_date(date, "month")
  ) |> 
  group_by(month) |> 
  summarise(home_price = mean(home_price))

mortgage_data <- read_csv("MORTGAGE30US.csv") |> 
  rename(date = observation_date, rate = MORTGAGE30US) |> 
  mutate(
    date = as_date(date),
    month = floor_date(date, "month")
  ) |> 
  group_by(month) |> 
  summarise(mortgage_rate = mean(rate) / 100)

combined <- hpi_data |> 
  inner_join(mortgage_data, by = "month") |> 
  filter(month >= ymd("1987-01-01"))

final_data <- combined |> 
  mutate(
    monthly_rent = home_price * 0.08 / 12,
    
    loan_amount = home_price * 0.9,
    monthly_rate = mortgage_rate / 12,
    mortgage_payment = (loan_amount * monthly_rate) / 
      (1 - (1 + monthly_rate)^(-360)),
    
    fixed_cost = home_price * 0.017 / 12,
    total_own_cost = mortgage_payment + fixed_cost,
    
    cheaper_own = total_own_cost < monthly_rent
  )

ggplot(final_data, aes(x = month)) +
  geom_line(aes(y = home_price/1e5, color = "Home Price (Hundred Thousand USD)")) +
  geom_line(aes(y = mortgage_rate*100, color = "Mortgage Rate")) +
  scale_y_continuous(
    name = "Home Price",
    sec.axis = sec_axis(~./100, name = "Rate")
  ) +
  labs(title = "Trends of Home Prices and Mortgage Rates (1987-2024)")
