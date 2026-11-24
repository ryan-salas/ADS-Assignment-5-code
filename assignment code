library(shiny)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(urca)
library(dplyr)
library(tidyr)
library(ggplot2)

raw <- read.csv("AustralianWines.csv", check.names = FALSE, na.strings = c("*", "NA", ""))
names(raw) <- trimws(names(raw))
names(raw)[names(raw) == "sparkling"] <- "Sparkling"
# Parse Jan-80 independently of the server's locale.
parts <- strsplit(raw$Month, "-", fixed = TRUE)
month_number <- match(vapply(parts, `[`, "", 1), month.abb)
year_number <- 1900L + as.integer(vapply(parts, `[`, "", 2))
raw$Month <- yearmonth(sprintf("%04d-%02d-01", year_number, month_number))
wine <- raw |>
  pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

varieties <- sort(unique(wine$Varietal))
dates <- as.Date(range(wine$Month))
families <- c("TSLM", "ETS", "ARIMA")
colors <- c(TSLM = "#326989", ETS = "#ae4a67", ARIMA = "#13796b")

fit_models <- function(data, selected) {
  # For missing sales, use the previous year's same month when available.
  # If the selected history starts near a gap, use its observed median.
  data <- data |>
    group_by_key() |>
    mutate(Sales = coalesce(Sales, lag(Sales, 12), median(Sales, na.rm = TRUE))) |>
    ungroup()
  definitions <- list(
    TSLM = TSLM(Sales ~ trend() + season()),
    ETS = ETS(Sales),
    ARIMA = ARIMA(Sales)
  )
  model(data, !!!definitions[selected])
}

plot_style <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        strip.text = element_text(face = "bold", color = "#53243a"))

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {background:#f9f6f4;color:#27202a;font-family:system-ui,sans-serif}
    h2 {color:#53243a;font-weight:750;margin:25px 0}
    h3 {color:#53243a;font-size:21px}
    .well {background:#f0e8eb;border:0;border-radius:12px}
    .chart-card {background:white;padding:12px;margin:14px 0;border-radius:10px}
    .nav-tabs>li>a {color:#53243a}
  "))),
  titlePanel("Australian wine sales forecasts"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("varietal", "Wine varieties", varieties,
                     selected = c("Sparkling", "Red"), multiple = TRUE),
      dateRangeInput("dates", "Historical date range", start = dates[1], end = dates[2],
                     min = dates[1], max = dates[2], format = "yyyy-mm-dd"),
      checkboxGroupInput("models", "Models", families, selected = families),
      sliderInput("holdout", "Validation months", 3, 36, 12),
      sliderInput("horizon", "Future forecast months", 1, 24, 12),
      radioButtons("level", "Prediction interval", c("80%" = 80, "95%" = 95),
                   selected = 80, inline = TRUE),
      helpText("The validation window is the last group of months in your selected date range.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 div(class = "chart-card", plotOutput("history", height = 360)),
                 div(class = "chart-card", plotOutput("season", height = 340)),
                 downloadButton("download", "Download model results")),
        tabPanel("Forecasts",
                 radioButtons("view", "Forecast window",
                              c("Validation" = "validation", "Future" = "future"),
                              inline = TRUE),
                 textOutput("windows"),
                 div(class = "chart-card", plotOutput("forecast", height = 510))),
        tabPanel("Model results",
                 textOutput("score_windows"),
                 h3("Model specifications"), tableOutput("specs"),
                 h3("Training and validation accuracy"), tableOutput("metrics"),
                 helpText("Lower is better. MAPE excludes months with zero actual sales.")),
        tabPanel("About", h3("Data and modeling"),
                 p("Source: ADS 506 AustralianWines.csv. Monthly Australian wine sales by variety."),
                 p("TSLM fits a linear trend and month-of-year effects. ETS and ARIMA automatically select their model forms."),
                 p("Validation forecasts are generated from training data only. Future forecasts refit the models on the full selected history."),
                 p("Two Rose observations are missing. Model fitting uses the prior year's month when available. Accuracy uses observed sales only."),
                 p("AI assistance: ChatGPT helped draft the Shiny scaffold and documentation."))
      )
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    validate(need(length(input$varietal) > 0, "Select at least one wine variety."),
             need(length(input$dates) == 2 && all(!is.na(input$dates)),
                  "Choose a start and end date."))
    result <- filter(wine, Varietal %in% input$varietal,
                     Month >= yearmonth(input$dates[1]), Month <= yearmonth(input$dates[2]))
    validate(need(nrow(result) > 0, "Choose a date range with sales data."))
    result
  })

  split <- reactive({
    data <- filtered()
    months <- sort(unique(data$Month))
    validate(need(length(input$models) > 0, "Select at least one model."),
             need(length(months) >= 36 + input$holdout,
                  "Select enough months for at least 36 training months plus validation."))
    cutoff <- months[length(months) - input$holdout]
    list(data = data, train = filter(data, Month <= cutoff),
         validation = filter(data, Month > cutoff), cutoff = cutoff)
  })

  training_fit <- reactive(fit_models(split()$train, input$models))
  validation_fc <- reactive(forecast(training_fit(), new_data = select(split()$validation, -Sales)))
  full_fit <- reactive(fit_models(split()$data, input$models))
  future_fc <- reactive(forecast(full_fit(), h = input$horizon))

  window_text <- reactive({
    s <- split()
    paste("Training", min(s$train$Month), "through", s$cutoff,
          "| Validation", min(s$validation$Month), "through", max(s$validation$Month))
  })
  output$score_windows <- renderText(window_text())
  output$windows <- renderText({
    if (input$view == "future") {
      paste("Refit through", max(split()$data$Month), "| Forecast months:", input$horizon)
    } else window_text()
  })

  output$history <- renderPlot({
    ggplot(filtered(), aes(as.Date(Month), Sales)) +
      geom_line(color = "#53243a", linewidth = .55) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      labs(title = "Monthly sales history", x = NULL, y = "Sales (thousand litres)") + plot_style
  })

  output$season <- renderPlot({
    seasonal <- filtered() |>
      as_tibble() |>
      mutate(Calendar = factor(format(as.Date(Month), "%m"), levels = sprintf("%02d", 1:12),
                               labels = month.abb))
    ggplot(seasonal, aes(Calendar, Sales)) +
      geom_boxplot(fill = "#eedce4", color = "#53243a", outlier.size = 1) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      labs(title = "How sales vary by calendar month", x = NULL, y = "Sales (thousand litres)") +
      plot_style
  })

  output$forecast <- renderPlot({
    s <- split()
    show_future <- input$view == "future"
    fc <- if (show_future) future_fc() else validation_fc()
    boundary <- if (show_future) max(s$data$Month) else s$cutoff
    observations <- filter(s$data, Month >= boundary - 35)
    level <- as.numeric(input$level)
    with_intervals <- hilo(fc, level = level)
    intervals <- with_intervals[[paste0(level, "%")]]
    chart_data <- as_tibble(fc) |>
      mutate(Lower = intervals$lower, Upper = intervals$upper)
    ggplot() +
      geom_ribbon(data = chart_data, aes(as.Date(Month), ymin = Lower, ymax = Upper,
                                        fill = .model, group = .model), alpha = .15) +
      geom_vline(xintercept = as.Date(boundary), linetype = "dashed", color = "grey45") +
      geom_line(data = observations, aes(as.Date(Month), Sales, color = "Actual"),
                linewidth = .65) +
      geom_line(data = chart_data, aes(as.Date(Month), .mean, color = .model),
                linewidth = .8) +
      geom_point(data = chart_data, aes(as.Date(Month), .mean, color = .model), size = .8) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      scale_color_manual(values = c(Actual = "#29252b", colors)) +
      scale_fill_manual(values = colors, guide = "none") +
      labs(title = if (show_future) "Future sales forecasts" else "Forecasts against held-out sales",
           subtitle = paste0("Shading shows ", level, "% prediction intervals"),
           x = NULL, y = "Sales (thousand litres)", color = NULL) + plot_style
  })

  specs <- reactive({
    fits <- training_fit()
    bind_rows(lapply(input$models, function(family) {
      column <- fits[[family]]
      data.frame(Varietal = fits$Varietal, Model = family,
                 Specification = format(column), check.names = FALSE)
    }))
  })

  scores <- reactive({
    s <- split()
    training_errors <- augment(training_fit()) |>
      as_tibble() |>
      select(Varietal, Month, .model, .fitted) |>
      left_join(as_tibble(s$train), by = c("Varietal", "Month")) |>
      transmute(Varietal, Model = .model, Window = "Training", Actual = Sales,
                Error = Sales - .fitted)
    validation_errors <- as_tibble(validation_fc()) |>
      select(Varietal, Month, .model, .mean) |>
      left_join(as_tibble(s$validation), by = c("Varietal", "Month")) |>
      transmute(Varietal, Model = .model, Window = "Validation", Actual = Sales,
                Error = Sales - .mean)
    bind_rows(training_errors, validation_errors) |>
      filter(is.finite(Error)) |>
      group_by(Varietal, Model, Window) |>
      summarise(RMSE = sqrt(mean(Error^2)), MAE = mean(abs(Error)),
                MAPE = if (any(Actual != 0)) {
                  100 * mean(abs(Error[Actual != 0] / Actual[Actual != 0]))
                } else NA_real_, .groups = "drop") |>
      arrange(Varietal, Window, RMSE)
  })

  output$specs <- renderTable(specs(), striped = TRUE)
  output$metrics <- renderTable(scores(), digits = 2, striped = TRUE)
  output$download <- downloadHandler(
    filename = function() "wine-model-results.csv",
    content = function(file) write.csv(left_join(scores(), specs(), by = c("Varietal", "Model")),
                                       file, row.names = FALSE)
  )
}

shinyApp(ui, server)
