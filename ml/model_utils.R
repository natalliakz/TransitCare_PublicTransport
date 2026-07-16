library(tidymodels)

load_model <- function(path = "ml/failure_risk_model.rds") {
  if (!file.exists(path)) {
    stop("Model not found. Run ml/train_model.R first.")
  }
  readRDS(path)
}

predict_failure_risk <- function(model, new_data) {
  predictions <- augment(model, new_data)
  predictions |>
    select(
      starts_with(".pred"),
      .pred_class
    ) |>
    rename(
      risk_probability = .pred_TRUE,
      low_risk_probability = .pred_FALSE,
      predicted_risk = .pred_class
    )
}

prepare_prediction_data <- function(
  age_years,
  total_mileage,
  maintenance_count,
  critical_issues_ytd,
  days_since_last_service,
  avg_fuel_efficiency,
  total_maintenance_events,
  unscheduled_events,
  total_downtime,
  total_cost,
  vehicle_type
) {
  tibble(
    age_years = age_years,
    total_mileage = total_mileage,
    maintenance_count = maintenance_count,
    critical_issues_ytd = critical_issues_ytd,
    days_since_last_service = days_since_last_service,
    avg_fuel_efficiency = avg_fuel_efficiency,
    total_maintenance_events = total_maintenance_events,
    unscheduled_events = unscheduled_events,
    total_downtime = total_downtime,
    total_cost = total_cost,
    vehicle_type = vehicle_type
  )
}
