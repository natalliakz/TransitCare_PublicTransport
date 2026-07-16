#!/usr/bin/env Rscript

library(tidymodels)
library(ranger)
library(readr)
library(dplyr)

set.seed(42)

if (!file.exists("data/synthetic-vehicle-health.csv")) {
  source("data/generate_data.R")
}

vehicle_health <- read_csv("data/synthetic-vehicle-health.csv", show_col_types = FALSE)
maintenance <- read_csv("data/synthetic-maintenance.csv", show_col_types = FALSE)

maintenance_summary <- maintenance |>
  group_by(vehicle_id) |>
  summarise(
    total_maintenance_events = n(),
    unscheduled_events = sum(!scheduled),
    critical_events = sum(severity == "Critical"),
    major_events = sum(severity == "Major"),
    total_downtime = sum(downtime_hours),
    total_cost = sum(parts_cost + labor_cost),
    .groups = "drop"
  )

model_data <- vehicle_health |>
  left_join(maintenance_summary, by = "vehicle_id") |>
  mutate(
    across(starts_with("total_"), ~replace_na(., 0)),
    across(c(unscheduled_events, critical_events, major_events), ~replace_na(., 0)),
    high_risk = factor(failure_risk %in% c("High", "Critical"), levels = c(FALSE, TRUE))
  ) |>
  select(
    high_risk,
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
  )

data_split <- initial_split(model_data, prop = 0.8, strata = high_risk)
train_data <- training(data_split)
test_data <- testing(data_split)

rec <- recipe(high_risk ~ ., data = train_data) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_zv(all_predictors())

rf_spec <- rand_forest(
  trees = 500,
  mtry = tune(),
  min_n = tune()
) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

rf_workflow <- workflow() |>
  add_recipe(rec) |>
  add_model(rf_spec)

cv_folds <- vfold_cv(train_data, v = 5, strata = high_risk)

rf_grid <- grid_regular(
  mtry(range = c(2, 6)),
  min_n(range = c(2, 10)),
  levels = 3
)

cat("Training model with cross-validation...\n")

rf_results <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc, accuracy, sensitivity, specificity)
)

best_params <- select_best(rf_results, metric = "roc_auc")
cat("\nBest parameters:\n")
print(best_params)

final_workflow <- finalize_workflow(rf_workflow, best_params)
final_fit <- fit(final_workflow, data = train_data)

test_predictions <- augment(final_fit, test_data)
test_metrics <- test_predictions |>
  metrics(truth = high_risk, estimate = .pred_class, .pred_TRUE)

cat("\nTest Set Performance:\n")
print(test_metrics)

conf_mat <- test_predictions |>
  conf_mat(truth = high_risk, estimate = .pred_class)

cat("\nConfusion Matrix:\n")
print(conf_mat)

if (!dir.exists("ml")) dir.create("ml")
saveRDS(final_fit, "ml/failure_risk_model.rds")

cat("\nModel saved to ml/failure_risk_model.rds\n")

rf_fit <- extract_fit_parsnip(final_fit)
importance <- rf_fit$fit$variable.importance |>
  sort(decreasing = TRUE)

cat("\nFeature Importance:\n")
print(round(importance, 2))
