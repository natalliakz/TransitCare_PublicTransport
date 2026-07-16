#!/usr/bin/env Rscript

library(dplyr)
library(lubridate)

set.seed(42)

n_vehicles <- 150
n_days <- 365
start_date <- as.Date("2024-01-01")

vehicle_types <- c("Single Decker", "Double Decker", "Electric", "Hybrid")
depots <- c("Northern Depot", "Central Depot", "Eastern Depot", "Western Depot")
manufacturers <- c("Wrightbus", "Alexander Dennis", "Optare", "BYD")

vehicles <- tibble(

vehicle_id = sprintf("TC-%04d", 1:n_vehicles),
vehicle_type = sample(vehicle_types, n_vehicles, replace = TRUE,
                        prob = c(0.3, 0.35, 0.2, 0.15)),
manufacturer = sample(manufacturers, n_vehicles, replace = TRUE),
depot = sample(depots, n_vehicles, replace = TRUE),
year_manufactured = sample(2015:2023, n_vehicles, replace = TRUE),
capacity = case_when(
    vehicle_type == "Single Decker" ~ sample(40:50, n_vehicles, replace = TRUE),
    vehicle_type == "Double Decker" ~ sample(70:90, n_vehicles, replace = TRUE),
    vehicle_type == "Electric" ~ sample(35:45, n_vehicles, replace = TRUE),
    vehicle_type == "Hybrid" ~ sample(45:55, n_vehicles, replace = TRUE)
),
mileage_at_start = round(runif(n_vehicles, 10000, 250000))
)

dates <- seq(start_date, start_date + n_days - 1, by = "day")
daily_ops <- expand.grid(
vehicle_id = vehicles$vehicle_id,
date = dates,
stringsAsFactors = FALSE
) |>
as_tibble() |>
left_join(vehicles, by = "vehicle_id") |>
mutate(
    daily_mileage = round(rnorm(n(), mean = 150, sd = 40)),
    daily_mileage = pmax(daily_mileage, 0),
    trips_completed = round(daily_mileage / 12),
    fuel_consumption = case_when(
    vehicle_type == "Electric" ~ daily_mileage * runif(n(), 1.2, 1.8),
    vehicle_type == "Hybrid" ~ daily_mileage * runif(n(), 0.25, 0.35),
    TRUE ~ daily_mileage * runif(n(), 0.35, 0.50)
    ),
    passenger_count = round(trips_completed * capacity * runif(n(), 0.3, 0.7)),
    driver_id = sprintf("DRV-%03d", sample(1:80, n(), replace = TRUE))
)

component_types <- c("Engine", "Transmission", "Brakes", "Suspension",
                    "Electrical", "HVAC", "Doors", "Battery")
severity_levels <- c("Minor", "Moderate", "Major", "Critical")

n_maintenance <- 2500
maintenance <- tibble(
maintenance_id = sprintf("MNT-%06d", 1:n_maintenance),
vehicle_id = sample(vehicles$vehicle_id, n_maintenance, replace = TRUE),
date = sample(dates, n_maintenance, replace = TRUE),
component = sample(component_types, n_maintenance, replace = TRUE,
                    prob = c(0.15, 0.12, 0.18, 0.10, 0.15, 0.12, 0.08, 0.10)),
severity = sample(severity_levels, n_maintenance, replace = TRUE,
                    prob = c(0.45, 0.30, 0.18, 0.07)),
repair_hours = case_when(
    severity == "Minor" ~ round(runif(n_maintenance, 0.5, 2), 1),
    severity == "Moderate" ~ round(runif(n_maintenance, 2, 6), 1),
    severity == "Major" ~ round(runif(n_maintenance, 6, 16), 1),
    severity == "Critical" ~ round(runif(n_maintenance, 16, 48), 1)
),
parts_cost = case_when(
    severity == "Minor" ~ round(runif(n_maintenance, 20, 150), 2),
    severity == "Moderate" ~ round(runif(n_maintenance, 150, 800), 2),
    severity == "Major" ~ round(runif(n_maintenance, 800, 3000), 2),
    severity == "Critical" ~ round(runif(n_maintenance, 3000, 15000), 2)
),
labor_cost = repair_hours * runif(n_maintenance, 45, 65),
scheduled = sample(c(TRUE, FALSE), n_maintenance, replace = TRUE, prob = c(0.6, 0.4)),
downtime_hours = repair_hours + runif(n_maintenance, 0, 4)
) |>
left_join(vehicles |> select(vehicle_id, vehicle_type, year_manufactured, depot),
            by = "vehicle_id") |>
mutate(
    labor_cost = round(labor_cost, 2),
    downtime_hours = round(downtime_hours, 1)
)

vehicle_health <- vehicles |>
mutate(
    age_years = 2024 - year_manufactured,
    total_mileage = mileage_at_start + round(runif(n_vehicles, 40000, 60000)),
    maintenance_count = sample(5:25, n_vehicles, replace = TRUE),
    critical_issues_ytd = rpois(n_vehicles, lambda = 0.8),
    avg_fuel_efficiency = case_when(
    vehicle_type == "Electric" ~ round(runif(n_vehicles, 1.3, 1.6), 2),
    vehicle_type == "Hybrid" ~ round(runif(n_vehicles, 0.28, 0.33), 2),
    TRUE ~ round(runif(n_vehicles, 0.38, 0.48), 2)
    ),
    days_since_last_service = sample(1:90, n_vehicles, replace = TRUE),
    health_score = round(100 - (age_years * 3) - (critical_issues_ytd * 8) -
                        (days_since_last_service * 0.1) + rnorm(n_vehicles, 0, 5)),
    health_score = pmin(pmax(health_score, 20), 100),
    failure_risk = case_when(
    health_score >= 80 ~ "Low",
    health_score >= 60 ~ "Medium",
    health_score >= 40 ~ "High",
    TRUE ~ "Critical"
    ),
    next_service_due = Sys.Date() + days_since_last_service - 90 + sample(-10:30, n_vehicles, replace = TRUE),
    estimated_failure_probability = round((100 - health_score) / 100 *
                                            runif(n_vehicles, 0.8, 1.2), 3),
    estimated_failure_probability = pmin(estimated_failure_probability, 0.95)
)

sensor_readings <- expand.grid(
vehicle_id = sample(vehicles$vehicle_id, 50),
reading_date = seq(Sys.Date() - 30, Sys.Date(), by = "day"),
stringsAsFactors = FALSE
) |>
as_tibble() |>
left_join(vehicle_health |> select(vehicle_id, health_score, vehicle_type),
            by = "vehicle_id") |>
mutate(
    engine_temp = round(rnorm(n(), mean = 85 + (100 - health_score) * 0.2, sd = 8), 1),
    oil_pressure = round(rnorm(n(), mean = 45 - (100 - health_score) * 0.1, sd = 5), 1),
    battery_voltage = round(rnorm(n(), mean = 12.6 - (100 - health_score) * 0.02, sd = 0.3), 2),
    brake_wear_pct = round(pmin(100, pmax(0, rnorm(n(), mean = 30 + (100 - health_score) * 0.5, sd = 10))), 1),
    tire_pressure_avg = round(rnorm(n(), mean = 100 - (100 - health_score) * 0.1, sd = 3), 1)
) |>
select(-health_score, -vehicle_type)

write.csv(vehicles, "data/synthetic-vehicles.csv", row.names = FALSE)
write.csv(daily_ops, "data/synthetic-daily-operations.csv", row.names = FALSE)
write.csv(maintenance, "data/synthetic-maintenance.csv", row.names = FALSE)
write.csv(vehicle_health, "data/synthetic-vehicle-health.csv", row.names = FALSE)
write.csv(sensor_readings, "data/synthetic-sensor-readings.csv", row.names = FALSE)

cat("Synthetic data generated successfully!\n")
cat("Files created:\n")
cat("  - data/synthetic-vehicles.csv\n")
cat("  - data/synthetic-daily-operations.csv\n")
cat("  - data/synthetic-maintenance.csv\n")
cat("  - data/synthetic-vehicle-health.csv\n")
cat("  - data/synthetic-sensor-readings.csv\n")
