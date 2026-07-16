library(plumber)
library(tidymodels)
library(readr)
library(dplyr)

if (!file.exists("data/synthetic-vehicles.csv")) {
source("data/generate_data.R")
}

if (!file.exists("ml/failure_risk_model.rds")) {
source("ml/train_model.R")
}

vehicles <- read_csv("data/synthetic-vehicles.csv", show_col_types = FALSE)
vehicle_health <- read_csv("data/synthetic-vehicle-health.csv", show_col_types = FALSE)
maintenance <- read_csv("data/synthetic-maintenance.csv", show_col_types = FALSE)
model <- readRDS("ml/failure_risk_model.rds")

#* @apiTitle TransitCare Fleet Maintenance API
#* @apiDescription API for fleet maintenance data and failure risk predictions
#* @apiVersion 1.0.0

#* Health check endpoint
#* @get /health
function() {
list(
    status = "healthy",
    timestamp = Sys.time(),
    model_loaded = TRUE,
    data_loaded = TRUE
)
}

#* Get fleet summary statistics
#* @get /fleet/summary
function() {
list(
    total_vehicles = nrow(vehicles),
    vehicle_types = vehicles |> count(vehicle_type) |> as.list(),
    depots = vehicles |> count(depot) |> as.list(),
    avg_health_score = round(mean(vehicle_health$health_score), 2),
    high_risk_count = sum(vehicle_health$failure_risk %in% c("High", "Critical")),
    risk_distribution = vehicle_health |> count(failure_risk) |> as.list()
)
}

#* Get all vehicles
#* @get /vehicles
#* @param depot Filter by depot name (optional)
#* @param vehicle_type Filter by vehicle type (optional)
function(depot = NULL, vehicle_type = NULL) {
result <- vehicles

if (!is.null(depot)) {
    result <- result |> filter(depot == !!depot)
}

if (!is.null(vehicle_type)) {
    result <- result |> filter(vehicle_type == !!vehicle_type)
}

result
}

#* Get vehicle health data
#* @get /vehicles/health
#* @param risk_level Filter by risk level (Low, Medium, High, Critical)
function(risk_level = NULL) {
result <- vehicle_health |>
    select(vehicle_id, vehicle_type, depot, age_years, total_mileage,
            health_score, failure_risk, estimated_failure_probability,
            days_since_last_service, next_service_due)

if (!is.null(risk_level)) {
    result <- result |> filter(failure_risk == risk_level)
}

result
}
#* Get details for a specific vehicle
#* @get /vehicles/<vehicle_id>
#* @param vehicle_id Vehicle ID (e.g., TC-0001)
function(vehicle_id) {
vehicle <- vehicles |> filter(vehicle_id == !!vehicle_id)

if (nrow(vehicle) == 0) {
    stop("Vehicle not found", call. = FALSE)
}

health <- vehicle_health |> filter(vehicle_id == !!vehicle_id)
maint <- maintenance |>
    filter(vehicle_id == !!vehicle_id) |>
    arrange(desc(date)) |>
    head(10)

list(
    vehicle = as.list(vehicle),
    health = as.list(health),
    recent_maintenance = maint
)
}

#* Get maintenance records
#* @get /maintenance
#* @param vehicle_id Filter by vehicle ID (optional)
#* @param component Filter by component (optional)
#* @param severity Filter by severity (optional)
#* @param limit Maximum records to return (default 100)
function(vehicle_id = NULL, component = NULL, severity = NULL, limit = 100) {
result <- maintenance |> arrange(desc(date))

if (!is.null(vehicle_id)) {
    result <- result |> filter(vehicle_id == !!vehicle_id)
}

if (!is.null(component)) {
    result <- result |> filter(component == !!component)
}

if (!is.null(severity)) {
    result <- result |> filter(severity == !!severity)
}

result |> head(as.integer(limit))
}

#* Predict failure risk for a vehicle
#* @post /predict
#* @param age_years:int Vehicle age in years
#* @param total_mileage:int Total mileage
#* @param maintenance_count:int Number of maintenance events
#* @param critical_issues_ytd:int Critical issues this year
#* @param days_since_last_service:int Days since last service
#* @param avg_fuel_efficiency:dbl Average fuel efficiency
#* @param total_maintenance_events:int Total maintenance events
#* @param unscheduled_events:int Unscheduled maintenance events
#* @param total_downtime:dbl Total downtime hours
#* @param total_cost:dbl Total maintenance cost
#* @param vehicle_type Vehicle type (Single Decker, Double Decker, Electric, Hybrid)
function(
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
new_data <- tibble(
    age_years = as.integer(age_years),
    total_mileage = as.integer(total_mileage),
    maintenance_count = as.integer(maintenance_count),
    critical_issues_ytd = as.integer(critical_issues_ytd),
    days_since_last_service = as.integer(days_since_last_service),
    avg_fuel_efficiency = as.numeric(avg_fuel_efficiency),
    total_maintenance_events = as.integer(total_maintenance_events),
    unscheduled_events = as.integer(unscheduled_events),
    total_downtime = as.numeric(total_downtime),
    total_cost = as.numeric(total_cost),
    vehicle_type = vehicle_type
)

pred <- augment(model, new_data)

list(
    prediction = list(
    high_risk = as.logical(pred$.pred_class),
    risk_probability = round(pred$.pred_TRUE, 4),
    low_risk_probability = round(pred$.pred_FALSE, 4)
    ),
    input = as.list(new_data),
    model_info = list(
    model_type = "Random Forest",
    trained_on = "TransitCare Fleet Data"
    )
)
}

#* Get model information
#* @get /model-info
function() {
rf_fit <- extract_fit_parsnip(model)
importance <- rf_fit$fit$variable.importance |>
    sort(decreasing = TRUE)

list(
    model_type = "Random Forest Classifier",
    target = "High Risk Vehicle Classification",
    features = names(importance),
    feature_importance = as.list(round(importance, 4)),
    n_trees = rf_fit$fit$num.trees
)
}
