library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)
library(plotly)
library(tidymodels)
library(shiny.telemetry)
library(logger)

log_appender(appender_file("app.log"))
log_threshold(INFO)
log_info("TransitCare app starting")

use_postgres <- Sys.getenv("TELEMETRY_USE_POSTGRES", "false") == "true"

if (use_postgres) {
  data_storage <- DataStoragePostgreSQL$new(
    host = Sys.getenv("TELEMETRY_DB_HOST", "localhost"),
    port = as.integer(Sys.getenv("TELEMETRY_DB_PORT", "5432")),
    dbname = Sys.getenv("TELEMETRY_DB_NAME", "telemetry"),
    user = Sys.getenv("TELEMETRY_DB_USER", ""),
    password = Sys.getenv("TELEMETRY_DB_PASSWORD", "")
  )
  log_info("Using PostgreSQL for telemetry storage")
} else {
  telemetry_db_path <- Sys.getenv("TELEMETRY_DB_PATH", "telemetry.sqlite")
  data_storage <- DataStorageSQLite$new(db_path = telemetry_db_path)
  log_info("Using SQLite for telemetry storage")
}

telemetry <- Telemetry$new(
  app_name = "transitcare_maintenance",
  data_storage = data_storage
)

if (!file.exists("data/synthetic-vehicles.csv")) {
source("data/generate_data.R")
}

if (!file.exists("ml/failure_risk_model.rds")) {
source("ml/train_model.R")
}

vehicles <- read_csv("data/synthetic-vehicles.csv", show_col_types = FALSE)
maintenance <- read_csv("data/synthetic-maintenance.csv", show_col_types = FALSE)
vehicle_health <- read_csv("data/synthetic-vehicle-health.csv", show_col_types = FALSE)
sensor_readings <- read_csv("data/synthetic-sensor-readings.csv", show_col_types = FALSE)
model <- readRDS("ml/failure_risk_model.rds")

ui <- page_navbar(
title = "TransitCare Fleet Maintenance",
theme = bs_theme(preset = "shiny", brand = "_brand.yml"),
nav_panel(
    title = "Dashboard",
    icon = bs_icon("speedometer2"),
    layout_columns(
    value_box(
        title = "Total Fleet",
        value = nrow(vehicles),
        showcase = bs_icon("bus-front"),
        theme = "primary"
    ),
    value_box(
        title = "High Risk Vehicles",
        value = sum(vehicle_health$failure_risk %in% c("High", "Critical")),
        showcase = bs_icon("exclamation-triangle"),
        theme = "danger"
    ),
    value_box(
        title = "Avg Health Score",
        value = round(mean(vehicle_health$health_score), 1),
        showcase = bs_icon("heart-pulse"),
        theme = "success"
    ),
    value_box(
        title = "Service Due Soon",
        value = sum(vehicle_health$days_since_last_service > 60),
        showcase = bs_icon("wrench"),
        theme = "warning"
    ),
    col_widths = c(3, 3, 3, 3)
    ),
    layout_columns(
    card(
        card_header("Fleet Health Distribution"),
        plotlyOutput("health_distribution", height = "300px")
    ),
    card(
        card_header("Maintenance by Component"),
        plotlyOutput("maintenance_by_component", height = "300px")
    ),
    col_widths = c(6, 6)
    ),
    card(
    card_header("High Risk Vehicles"),
    DTOutput("high_risk_table")
    )
),
nav_panel(
    title = "Vehicle Details",
    icon = bs_icon("truck"),
    layout_sidebar(
    sidebar = sidebar(
        selectInput("selected_vehicle", "Select Vehicle",
                    choices = vehicle_health$vehicle_id,
                    selected = vehicle_health$vehicle_id[1]),
        hr(),
        uiOutput("vehicle_info")
    ),
    layout_columns(
        card(
        card_header("Health Metrics"),
        plotlyOutput("vehicle_gauges", height = "200px")
        ),
        card(
        card_header("Maintenance History"),
        DTOutput("vehicle_maintenance")
        ),
        col_widths = c(6, 6)
    ),
    card(
        card_header("Sensor Readings (Last 30 Days)"),
        plotlyOutput("sensor_chart", height = "300px")
    )
    )
),
nav_panel(
    title = "Predictions",
    icon = bs_icon("graph-up-arrow"),
    layout_sidebar(
    sidebar = sidebar(
        width = 350,
        h5("Enter Vehicle Parameters"),
        numericInput("pred_age", "Vehicle Age (years)", value = 5, min = 1, max = 20),
        numericInput("pred_mileage", "Total Mileage", value = 100000, min = 0, step = 10000),
        numericInput("pred_maintenance", "Maintenance Count", value = 10, min = 0),
        numericInput("pred_critical", "Critical Issues YTD", value = 0, min = 0, max = 10),
        numericInput("pred_days_service", "Days Since Last Service", value = 30, min = 0, max = 365),
        numericInput("pred_fuel_eff", "Avg Fuel Efficiency", value = 0.4, min = 0, max = 2, step = 0.1),
        numericInput("pred_total_events", "Total Maintenance Events", value = 15, min = 0),
        numericInput("pred_unscheduled", "Unscheduled Events", value = 5, min = 0),
        numericInput("pred_downtime", "Total Downtime (hrs)", value = 50, min = 0),
        numericInput("pred_cost", "Total Maintenance Cost", value = 5000, min = 0, step = 500),
        selectInput("pred_type", "Vehicle Type",
                    choices = c("Single Decker", "Double Decker", "Electric", "Hybrid")),
        actionButton("predict_btn", "Predict Risk", class = "btn-primary w-100")
    ),
    card(
        card_header("Prediction Result"),
        uiOutput("prediction_result"),
        uiOutput("feedback_ui")
    ),
    card(
        card_header("Feature Importance"),
        plotlyOutput("feature_importance", height = "400px")
    )
    )
),
nav_spacer(),
nav_item(
    tags$span(
    class = "text-muted small",
    "Demo data - Not real fleet information"
    )
)
)

server <- function(input, output, session) {

log_info("New session started: {session$token}")

telemetry$start_session()

telemetry$log_navigation("Dashboard")

feedback_given <- reactiveVal(FALSE)

observeEvent(input[["nav"]], {
    if (!is.null(input[["nav"]])) {
    telemetry$log_navigation(input[["nav"]])
    }
}, ignoreInit = TRUE)

observeEvent(input$selected_vehicle, {
    telemetry$log_custom_event(paste0("vehicle_selection: ", input$selected_vehicle))
}, ignoreInit = TRUE)

observeEvent(input$predict_btn, {
    telemetry$log_custom_event("prediction_request")
}, ignoreInit = TRUE)

output$health_distribution <- renderPlotly({
    p <- vehicle_health |>
    ggplot(aes(x = health_score, fill = failure_risk)) +
    geom_histogram(binwidth = 5, color = "white") +
    scale_fill_manual(values = c(
        "Low" = "#00A651",
        "Medium" = "#FFB81C",
        "High" = "#DC3545",
        "Critical" = "#2D2A54"
    )) +
    labs(x = "Health Score", y = "Count", fill = "Risk") +
    theme_minimal()

    ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.2))
})

output$maintenance_by_component <- renderPlotly({
    p <- maintenance |>
    count(component) |>
    ggplot(aes(x = reorder(component, n), y = n, fill = n)) +
    geom_col() +
    coord_flip() +
    scale_fill_gradient(low = "#FFB3D1", high = "#E6007E") +
    labs(x = NULL, y = "Events") +
    theme_minimal() +
    theme(legend.position = "none")

    ggplotly(p)
})

output$high_risk_table <- renderDT({
    vehicle_health |>
    filter(failure_risk %in% c("High", "Critical")) |>
    select(vehicle_id, vehicle_type, depot, age_years, health_score,
            failure_risk, estimated_failure_probability) |>
    arrange(desc(estimated_failure_probability)) |>
    mutate(estimated_failure_probability = scales::percent(estimated_failure_probability)) |>
    datatable(
        options = list(pageLength = 10, dom = 'tip'),
        rownames = FALSE,
        colnames = c("Vehicle", "Type", "Depot", "Age", "Health", "Risk", "Failure Prob")
    )
})

selected_vehicle_data <- reactive({
    vehicle_health |> filter(vehicle_id == input$selected_vehicle)
})

output$vehicle_info <- renderUI({
    v <- selected_vehicle_data()
    tagList(
    tags$div(
        class = "mb-3",
        tags$strong("Type: "), v$vehicle_type, tags$br(),
        tags$strong("Depot: "), v$depot, tags$br(),
        tags$strong("Age: "), v$age_years, " years", tags$br(),
        tags$strong("Mileage: "), scales::comma(v$total_mileage), tags$br(),
        tags$strong("Risk Level: "),
        tags$span(
        class = case_when(
            v$failure_risk == "Low" ~ "text-success",
            v$failure_risk == "Medium" ~ "text-warning",
            TRUE ~ "text-danger"
        ),
        v$failure_risk
        )
    )
    )
})

output$vehicle_gauges <- renderPlotly({
    v <- selected_vehicle_data()

    plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = v$health_score,
    title = list(text = "Health Score"),
    gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "#E6007E"),
        steps = list(
        list(range = c(0, 40), color = "#DC3545"),
        list(range = c(40, 60), color = "#FFB81C"),
        list(range = c(60, 80), color = "#6E6AAF"),
        list(range = c(80, 100), color = "#00A651")
        )
    )
    ) |>
    layout(margin = list(t = 50, b = 20))
})

output$vehicle_maintenance <- renderDT({
    maintenance |>
    filter(vehicle_id == input$selected_vehicle) |>
    select(date, component, severity, repair_hours, parts_cost, scheduled) |>
    arrange(desc(date)) |>
    head(20) |>
    mutate(
        parts_cost = scales::dollar(parts_cost, prefix = "£"),
        scheduled = ifelse(scheduled, "Yes", "No")
    ) |>
    datatable(
        options = list(pageLength = 5, dom = 'tip'),
        rownames = FALSE,
        colnames = c("Date", "Component", "Severity", "Hours", "Parts Cost", "Scheduled")
    )
})

output$sensor_chart <- renderPlotly({
    sensor_data <- sensor_readings |>
    filter(vehicle_id == input$selected_vehicle)

    if (nrow(sensor_data) == 0) {
    return(plotly_empty() |> layout(title = "No sensor data available"))
    }

    plot_ly(sensor_data, x = ~reading_date) |>
    add_trace(y = ~engine_temp, name = "Engine Temp (°C)", type = "scatter", mode = "lines") |>
    add_trace(y = ~oil_pressure, name = "Oil Pressure (PSI)", type = "scatter", mode = "lines") |>
    add_trace(y = ~brake_wear_pct, name = "Brake Wear (%)", type = "scatter", mode = "lines") |>
    layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Value"),
        legend = list(orientation = "h", y = -0.2)
    )
})

prediction_data <- eventReactive(input$predict_btn, {
    tibble(
    age_years = input$pred_age,
    total_mileage = input$pred_mileage,
    maintenance_count = input$pred_maintenance,
    critical_issues_ytd = input$pred_critical,
    days_since_last_service = input$pred_days_service,
    avg_fuel_efficiency = input$pred_fuel_eff,
    total_maintenance_events = input$pred_total_events,
    unscheduled_events = input$pred_unscheduled,
    total_downtime = input$pred_downtime,
    total_cost = input$pred_cost,
    vehicle_type = input$pred_type
    )
})

output$prediction_result <- renderUI({
    req(input$predict_btn)

    new_data <- prediction_data()
    pred <- augment(model, new_data)

    risk_prob <- pred$.pred_TRUE
    risk_class <- pred$.pred_class

    risk_color <- if (risk_class == TRUE) "#DC3545" else "#00A651"
    risk_label <- if (risk_class == TRUE) "HIGH RISK" else "LOW RISK"

    tagList(
    tags$div(
        class = "text-center p-4",
        tags$h2(
        style = paste0("color: ", risk_color),
        risk_label
        ),
        tags$h4(
        "Failure Probability: ",
        tags$strong(scales::percent(risk_prob, accuracy = 0.1))
        ),
        tags$hr(),
        tags$p(
        class = "text-muted",
        if (risk_class == TRUE) {
            "This vehicle shows elevated risk factors. Consider scheduling preventive maintenance."
        } else {
            "This vehicle is within normal operating parameters."
        }
        )
    )
    )
})

output$feature_importance <- renderPlotly({
    rf_fit <- extract_fit_parsnip(model)
    importance <- rf_fit$fit$variable.importance |>
    sort(decreasing = TRUE) |>
    head(10)

    plot_ly(
    x = importance,
    y = reorder(names(importance), importance),
    type = "bar",
    orientation = "h",
    marker = list(color = "#E6007E")
    ) |>
    layout(
        xaxis = list(title = "Importance"),
        yaxis = list(title = ""),
        margin = list(l = 150)
    )
})

output$feedback_ui <- renderUI({
    req(input$predict_btn)

    if (feedback_given()) {
    return(tags$div(
        class = "text-center text-muted p-3",
        bs_icon("check-circle", size = "1.5em"),
        " Thank you for your feedback!"
    ))
    }

    tags$div(
    class = "text-center p-3 border-top mt-3",
    tags$p(class = "mb-2", "Was this prediction helpful?"),
    actionButton("feedback_useful",
                    label = tagList(bs_icon("hand-thumbs-up"), " Useful"),
                    class = "btn-outline-success me-2"),
    actionButton("feedback_not_useful",
                    label = tagList(bs_icon("hand-thumbs-down"), " Not Useful"),
                    class = "btn-outline-danger")
    )
})

observeEvent(input$feedback_useful, {
    log_info("Feedback received: USEFUL for prediction (session: {session$token})")
    telemetry$log_custom_event("feedback_useful")
    feedback_given(TRUE)
    showNotification("Thanks for the positive feedback!", type = "message")
})

observeEvent(input$feedback_not_useful, {
    log_info("Feedback received: NOT USEFUL for prediction (session: {session$token})")
    telemetry$log_custom_event("feedback_not_useful")
    feedback_given(TRUE)
    showNotification("Thanks for your feedback. We'll work to improve!", type = "message")
})

observeEvent(input$predict_btn, {
    feedback_given(FALSE)
    log_info("Prediction requested (session: {session$token})")
})

onSessionEnded(function() {
    log_info("Session ended: {session$token}")
})
}

shinyApp(ui, server)
