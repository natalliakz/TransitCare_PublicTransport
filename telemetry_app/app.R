library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(lubridate)
library(shiny.telemetry)

demo_mode <- Sys.getenv("TELEMETRY_DEMO_MODE", "false") == "true"
data_storage <- NULL

if (!demo_mode) {
  use_postgres <- Sys.getenv("TELEMETRY_USE_POSTGRES", "false") == "true"

  if (use_postgres) {
    data_storage <- DataStoragePostgreSQL$new(
      host = Sys.getenv("TELEMETRY_DB_HOST", "localhost"),
      port = as.integer(Sys.getenv("TELEMETRY_DB_PORT", "5432")),
      dbname = Sys.getenv("TELEMETRY_DB_NAME", "telemetry"),
      user = Sys.getenv("TELEMETRY_DB_USER", ""),
      password = Sys.getenv("TELEMETRY_DB_PASSWORD", "")
    )
  } else {
    telemetry_db_path <- Sys.getenv("TELEMETRY_DB_PATH", "telemetry.sqlite")
    data_storage <- DataStorageSQLite$new(db_path = telemetry_db_path)
  }
}

generate_synthetic_telemetry <- function() {
  set.seed(42)

  n_sessions <- 87
  session_ids <- paste0("session_", sprintf("%04d", 1:n_sessions))

  start_date <- Sys.Date() - 30
  end_date <- Sys.Date()

  pages <- c("Dashboard", "Vehicle Details", "Predictions")
  vehicles <- paste0("TC-", sprintf("%04d", sample(1:150, 25)))

  records <- list()

  for (i in seq_along(session_ids)) {
    sid <- session_ids[i]
    session_start <- as.POSIXct(runif(1,
      as.numeric(as.POSIXct(start_date)),
      as.numeric(as.POSIXct(end_date))
    ), origin = "1970-01-01")

    records[[length(records) + 1]] <- tibble(
      session_id = sid,
      time = session_start,
      type = "session_start",
      details = "session_start"
    )

    n_nav <- sample(2:6, 1)
    for (j in 1:n_nav) {
      records[[length(records) + 1]] <- tibble(
        session_id = sid,
        time = session_start + j * runif(1, 10, 60),
        type = "navigation",
        details = sample(pages, 1, prob = c(0.4, 0.35, 0.25))
      )
    }

    n_vehicles <- sample(0:4, 1)
    if (n_vehicles > 0) {
      for (j in 1:n_vehicles) {
        records[[length(records) + 1]] <- tibble(
          session_id = sid,
          time = session_start + runif(1, 30, 180),
          type = "input",
          details = paste0("vehicle_selection: ", sample(vehicles, 1))
        )
      }
    }

    if (runif(1) < 0.6) {
      records[[length(records) + 1]] <- tibble(
        session_id = sid,
        time = session_start + runif(1, 60, 240),
        type = "input",
        details = "prediction_request"
      )

      if (runif(1) < 0.7) {
        feedback_type <- if (runif(1) < 0.78) "feedback_useful" else "feedback_not_useful"
        records[[length(records) + 1]] <- tibble(
          session_id = sid,
          time = session_start + runif(1, 120, 300),
          type = "custom_event",
          details = feedback_type
        )
      }
    }
  }

  bind_rows(records) |> arrange(time)
}

demo_banner <- if (demo_mode) {
  tags$div(
    class = "alert alert-info text-center mb-0 rounded-0",
    bs_icon("info-circle"), " ",
    tags$strong("Demo Mode:"),
    " Showing synthetic telemetry data to demonstrate the art of the possible."
  )
} else {
  NULL
}

ui <- page_navbar(
title = "TransitCare Usage Analytics",
theme = bs_theme(preset = "shiny", brand = "_brand.yml"),
header = demo_banner,
nav_panel(
    title = "Overview",
    icon = bs_icon("graph-up"),
    layout_columns(
    value_box(
        title = "Total Sessions",
        value = textOutput("total_sessions"),
        showcase = bs_icon("people"),
        theme = "primary"
    ),
    value_box(
        title = "Sessions Today",
        value = textOutput("sessions_today"),
        showcase = bs_icon("calendar-check"),
        theme = "success"
    ),
    value_box(
        title = "Unique Users",
        value = textOutput("unique_users"),
        showcase = bs_icon("person-badge"),
        theme = "info"
    ),
    value_box(
        title = "Avg Session Duration",
        value = textOutput("avg_duration"),
        showcase = bs_icon("clock"),
        theme = "warning"
    ),
    col_widths = c(3, 3, 3, 3)
    ),
    layout_columns(
    card(
        card_header("Sessions Over Time"),
        plotlyOutput("sessions_timeline", height = "300px")
    ),
    card(
        card_header("Page Navigation"),
        plotlyOutput("navigation_chart", height = "300px")
    ),
    col_widths = c(6, 6)
    ),
    card(
    card_header("Recent Activity"),
    DTOutput("activity_log")
    )
),
nav_panel(
    title = "User Behavior",
    icon = bs_icon("activity"),
    layout_columns(
    card(
        card_header("Input Interactions"),
        DTOutput("input_stats")
    ),
    card(
        card_header("Most Viewed Vehicles"),
        plotlyOutput("vehicle_views", height = "300px")
    ),
    col_widths = c(6, 6)
    ),
    card(
    card_header("Session Details"),
    DTOutput("session_details")
    )
),
nav_panel(
    title = "Feedback",
    icon = bs_icon("hand-thumbs-up"),
    layout_columns(
    value_box(
        title = "Total Feedback",
        value = textOutput("total_feedback"),
        showcase = bs_icon("chat-square-text"),
        theme = "primary"
    ),
    value_box(
        title = "Useful",
        value = textOutput("useful_count"),
        showcase = bs_icon("hand-thumbs-up"),
        theme = "success"
    ),
    value_box(
        title = "Not Useful",
        value = textOutput("not_useful_count"),
        showcase = bs_icon("hand-thumbs-down"),
        theme = "danger"
    ),
    value_box(
        title = "Satisfaction Rate",
        value = textOutput("satisfaction_rate"),
        showcase = bs_icon("percent"),
        theme = "info"
    ),
    col_widths = c(3, 3, 3, 3)
    ),
    layout_columns(
    card(
        card_header("Feedback Over Time"),
        plotlyOutput("feedback_timeline", height = "300px")
    ),
    card(
        card_header("Feedback Distribution"),
        plotlyOutput("feedback_pie", height = "300px")
    ),
    col_widths = c(6, 6)
    ),
    card(
    card_header("Recent Feedback"),
    DTOutput("feedback_log")
    )
),
nav_panel(
    title = "About Telemetry",
    icon = bs_icon("info-circle"),
    card(
    card_header("About shiny.telemetry"),
    card_body(
        tags$h4("What is shiny.telemetry?"),
        tags$p(
        "shiny.telemetry is an R package from ",
        tags$a(href = "https://appsilon.com", "Appsilon"),
        " that enables tracking of user interactions in Shiny applications."
        ),
        tags$h5("Key Features:"),
        tags$ul(
        tags$li("Track user sessions and navigation patterns"),
        tags$li("Log input changes and button clicks"),
        tags$li("Monitor application performance"),
        tags$li("Multiple storage backends (SQLite, PostgreSQL, MariaDB)")
        ),
        tags$h5("Data Collected in This Demo:"),
        tags$ul(
        tags$li(tags$strong("Sessions:"), " When users start and end their sessions"),
        tags$li(tags$strong("Navigation:"), " Which pages/tabs users visit"),
        tags$li(tags$strong("Inputs:"), " Which vehicles are selected, prediction requests made")
        ),
        tags$hr(),
        tags$p(
        class = "text-muted",
        "This dashboard demonstrates how Posit Connect can host both the main application ",
        "and a separate analytics dashboard, all powered by the same data source."
        ),
        tags$h5("Learn More:"),
        tags$ul(
        tags$li(tags$a(href = "https://appsilon.github.io/shiny.telemetry/", "shiny.telemetry Documentation")),
        tags$li(tags$a(href = "https://posit.co/products/enterprise/connect/", "Posit Connect"))
        )
    )
    )
),
nav_spacer(),
nav_item(
    actionButton("refresh_data", "Refresh Data", icon = icon("refresh"), class = "btn-sm")
)
)

server <- function(input, output, session) {

telemetry_data <- reactiveVal(list(
    sessions = NULL,
    navigation = NULL,
    inputs = NULL,
    feedback = NULL
))

load_telemetry_data <- function() {
    tryCatch({
    if (demo_mode) {
        sessions <- generate_synthetic_telemetry()
    } else {
        sessions <- data_storage$read_all()
    }

    if (is.null(sessions) || nrow(sessions) == 0) {
        return(list(
        sessions = tibble(
            session_id = character(),
            time = as.POSIXct(character()),
            type = character(),
            details = character()
        ),
        navigation = tibble(),
        inputs = tibble(),
        feedback = tibble()
        ))
    }

    nav_data <- sessions |>
        filter(type == "navigation") |>
        mutate(page = details)

    input_data <- sessions |>
        filter(type == "input")

    feedback_data <- sessions |>
        filter(type == "custom_event" & grepl("feedback", details))

    list(
        sessions = sessions,
        navigation = nav_data,
        inputs = input_data,
        feedback = feedback_data
    )
    }, error = function(e) {
    list(
        sessions = tibble(
        session_id = character(),
        time = as.POSIXct(character()),
        type = character(),
        details = character()
        ),
        navigation = tibble(),
        inputs = tibble(),
        feedback = tibble()
    )
    })
}

observe({
    telemetry_data(load_telemetry_data())
})

observeEvent(input$refresh_data, {
    telemetry_data(load_telemetry_data())
})

output$total_sessions <- renderText({
    data <- telemetry_data()$sessions
    if (is.null(data) || nrow(data) == 0) return("0")
    n_distinct(data$session_id)
})

output$sessions_today <- renderText({
    data <- telemetry_data()$sessions
    if (is.null(data) || nrow(data) == 0) return("0")
    data |>
    filter(as.Date(time) == Sys.Date()) |>
    summarise(n = n_distinct(session_id)) |>
    pull(n)
})

output$unique_users <- renderText({
    data <- telemetry_data()$sessions
    if (is.null(data) || nrow(data) == 0) return("0")
    n_distinct(data$session_id)
})

output$avg_duration <- renderText({
    data <- telemetry_data()$sessions
    if (is.null(data) || nrow(data) == 0) return("N/A")

    session_durations <- data |>
    group_by(session_id) |>
    summarise(
        start = min(time),
        end = max(time),
        duration = as.numeric(difftime(end, start, units = "mins"))
    )

    if (nrow(session_durations) == 0) return("N/A")
    paste0(round(mean(session_durations$duration), 1), " min")
})

output$sessions_timeline <- renderPlotly({
    data <- telemetry_data()$sessions
    if (is.null(data) || nrow(data) == 0) {
    return(plotly_empty() |> layout(title = "No data yet - use the main app to generate telemetry"))
    }

    sessions_by_day <- data |>
    mutate(date = as.Date(time)) |>
    group_by(date) |>
    summarise(sessions = n_distinct(session_id))

    plot_ly(sessions_by_day, x = ~date, y = ~sessions, type = "scatter", mode = "lines+markers",
            line = list(color = "#0F4C75"), marker = list(color = "#0F4C75")) |>
    layout(xaxis = list(title = ""), yaxis = list(title = "Sessions"))
})

output$navigation_chart <- renderPlotly({
    nav_data <- telemetry_data()$navigation
    if (is.null(nav_data) || nrow(nav_data) == 0) {
    return(plotly_empty() |> layout(title = "No navigation data yet"))
    }

    nav_counts <- nav_data |>
    count(page) |>
    arrange(desc(n))

    plot_ly(nav_counts, x = ~n, y = ~reorder(page, n), type = "bar", orientation = "h",
            marker = list(color = "#3282B8")) |>
    layout(xaxis = list(title = "Visits"), yaxis = list(title = ""))
})

output$activity_log <- renderDT({
    data <- telemetry_data()$sessions
    if (is.null(data) || nrow(data) == 0) {
    return(datatable(tibble(Message = "No activity recorded yet. Use the main app to generate telemetry data.")))
    }

    data |>
    arrange(desc(time)) |>
    head(50) |>
    select(time, session_id, type, details) |>
    mutate(
        time = format(time, "%Y-%m-%d %H:%M:%S"),
        session_id = substr(session_id, 1, 8)
    ) |>
    datatable(
        options = list(pageLength = 10, dom = 'tip'),
        rownames = FALSE,
        colnames = c("Timestamp", "Session", "Type", "Details")
    )
})

output$input_stats <- renderDT({
    input_data <- telemetry_data()$inputs
    if (is.null(input_data) || nrow(input_data) == 0) {
    return(datatable(tibble(Message = "No input interactions recorded yet")))
    }

    input_data |>
    count(details, name = "interactions") |>
    arrange(desc(interactions)) |>
    datatable(
        options = list(pageLength = 10, dom = 'tip'),
        rownames = FALSE,
        colnames = c("Input", "Interactions")
    )
})

output$vehicle_views <- renderPlotly({
    input_data <- telemetry_data()$inputs
    if (is.null(input_data) || nrow(input_data) == 0) {
    return(plotly_empty() |> layout(title = "No vehicle selection data yet"))
    }

    vehicle_data <- input_data |>
    filter(grepl("vehicle_selection", details)) |>
    mutate(vehicle = gsub(".*: ", "", details)) |>
    count(vehicle) |>
    arrange(desc(n)) |>
    head(10)

    if (nrow(vehicle_data) == 0) {
    return(plotly_empty() |> layout(title = "No vehicle selections recorded"))
    }

    plot_ly(vehicle_data, x = ~n, y = ~reorder(vehicle, n), type = "bar", orientation = "h",
            marker = list(color = "#2ECC71")) |>
    layout(xaxis = list(title = "Views"), yaxis = list(title = ""))
})

output$session_details <- renderDT({
    data <- telemetry_data()$sessions
    if (is.null(data) || nrow(data) == 0) {
    return(datatable(tibble(Message = "No sessions recorded yet")))
    }

    data |>
    group_by(session_id) |>
    summarise(
        start_time = min(time),
        end_time = max(time),
        duration_mins = round(as.numeric(difftime(max(time), min(time), units = "mins")), 1),
        events = n(),
        pages_visited = n_distinct(details[type == "navigation"])
    ) |>
    arrange(desc(start_time)) |>
    mutate(
        session_id = substr(session_id, 1, 8),
        start_time = format(start_time, "%Y-%m-%d %H:%M"),
        end_time = format(end_time, "%H:%M")
    ) |>
    datatable(
        options = list(pageLength = 10, dom = 'tip'),
        rownames = FALSE,
        colnames = c("Session", "Started", "Ended", "Duration (min)", "Events", "Pages")
    )
})

output$total_feedback <- renderText({
    feedback <- telemetry_data()$feedback
    if (is.null(feedback) || nrow(feedback) == 0) return("0")
    nrow(feedback)
})

output$useful_count <- renderText({
    feedback <- telemetry_data()$feedback
    if (is.null(feedback) || nrow(feedback) == 0) return("0")
    sum(grepl("feedback_useful", feedback$details))
})

output$not_useful_count <- renderText({
    feedback <- telemetry_data()$feedback
    if (is.null(feedback) || nrow(feedback) == 0) return("0")
    sum(grepl("feedback_not_useful", feedback$details))
})

output$satisfaction_rate <- renderText({
    feedback <- telemetry_data()$feedback
    if (is.null(feedback) || nrow(feedback) == 0) return("N/A")
    useful <- sum(grepl("feedback_useful", feedback$details))
    total <- nrow(feedback)
    if (total == 0) return("N/A")
    paste0(round(useful / total * 100, 0), "%")
})

output$feedback_timeline <- renderPlotly({
    feedback <- telemetry_data()$feedback
    if (is.null(feedback) || nrow(feedback) == 0) {
    return(plotly_empty() |> layout(title = "No feedback data yet"))
    }

    feedback_by_day <- feedback |>
    mutate(
        date = as.Date(time),
        type = ifelse(grepl("feedback_useful", details), "Useful", "Not Useful")
    ) |>
    count(date, type)

    plot_ly(feedback_by_day, x = ~date, y = ~n, color = ~type, type = "bar",
            colors = c("Useful" = "#2ECC71", "Not Useful" = "#E74C3C")) |>
    layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Feedback Count"),
        barmode = "stack"
    )
})

output$feedback_pie <- renderPlotly({
    feedback <- telemetry_data()$feedback
    if (is.null(feedback) || nrow(feedback) == 0) {
    return(plotly_empty() |> layout(title = "No feedback data yet"))
    }

    useful <- sum(grepl("feedback_useful", feedback$details))
    not_useful <- sum(grepl("feedback_not_useful", feedback$details))

    plot_ly(
    labels = c("Useful", "Not Useful"),
    values = c(useful, not_useful),
    type = "pie",
    marker = list(colors = c("#2ECC71", "#E74C3C")),
    textinfo = "label+percent",
    hole = 0.4
    ) |>
    layout(showlegend = FALSE)
})

output$feedback_log <- renderDT({
    feedback <- telemetry_data()$feedback
    if (is.null(feedback) || nrow(feedback) == 0) {
    return(datatable(tibble(Message = "No feedback recorded yet. Use the prediction feature in the main app and provide feedback.")))
    }

    feedback |>
    arrange(desc(time)) |>
    mutate(
        time = format(time, "%Y-%m-%d %H:%M:%S"),
        session_id = substr(session_id, 1, 8),
        feedback_type = ifelse(grepl("feedback_useful", details), "Useful", "Not Useful")
    ) |>
    select(time, session_id, feedback_type) |>
    datatable(
        options = list(pageLength = 15, dom = 'tip'),
        rownames = FALSE,
        colnames = c("Timestamp", "Session", "Feedback")
    )
})
}

shinyApp(ui, server)
