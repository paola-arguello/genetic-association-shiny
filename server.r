# =====================
# server.R
# =====================
library(shiny)
library(ggplot2)
source("helpers.r")

server <- function(input, output, session) {

  valid_inputs <- reactive({
    msgs <- character(0)

    if (input$case_carriers > input$n_cases) {
      msgs <- c(msgs, "Variant carriers among cases cannot exceed the total number of cases.")
    }
    if (input$control_carriers > input$n_controls) {
      msgs <- c(msgs, "Variant carriers among controls cannot exceed the total number of controls.")
    }
    if (input$n_cases <= 0 || input$n_controls <= 0) {
      msgs <- c(msgs, "The number of cases and controls must both be greater than zero.")
    }

    list(valid = length(msgs) == 0, messages = msgs)
  })

  observed_table <- reactive({
    req(valid_inputs()$valid)
    build_table(
      input$n_cases,
      input$n_controls,
      input$case_carriers,
      input$control_carriers
    )
  })

  observed_stats <- reactive({
    req(valid_inputs()$valid)
    tab <- observed_table()
    or_ci <- compute_or_ci(tab)
    test <- compute_test(tab, input$test_method)

    list(
      table = tab,
      or_ci = or_ci,
      test = test
    )
  })

  small_counts_flag <- reactive({
    req(valid_inputs()$valid)
    any(observed_table() < 5)
  })

  output$contingency_table <- renderTable({
    req(valid_inputs()$valid)
    observed_table()
  }, rownames = TRUE)

  output$validation_messages <- renderUI({
    v <- valid_inputs()

    if (!v$valid) {
      tagList(
        lapply(v$messages, function(msg) {
          div(class = "warning-text", paste0("Warning: ", msg))
        })
      )
    } else if (small_counts_flag()) {
      div(class = "warning-text", "Warning: some cell counts are below 5. Estimates may be unstable.")
    } else {
      div(class = "small-note", "Counts look suitable for estimation. Continue by examining uncertainty, not just the point estimate.")
    }
  })

    output$test_explanation <- renderText({
    if (input$test_method == "fisher") {
      "Prefer this when counts are small or sparse."
    } else {
      "Prefer this when expected counts are reasonably large."
    }
  })

    output$or_value <- renderText({
    req(valid_inputs()$valid)
    sprintf("%.2f", observed_stats()$or_ci$odds_ratio)
  })

  output$ci_value <- renderText({
    req(valid_inputs()$valid)
    paste0(
      sprintf("%.2f", observed_stats()$or_ci$ci_low),
      " to ",
      sprintf("%.2f", observed_stats()$or_ci$ci_high)
    )
  })

  output$p_value <- renderText({
    req(valid_inputs()$valid)
    p <- observed_stats()$test$p_value
    if (p < 0.001) "<0.001" else sprintf("%.3f", p)
  })

  output$forest_plot <- renderPlot({
    req(valid_inputs()$valid)

    current <- observed_stats()$or_ci

    scaled <- scale_scenario(
      input$n_cases,
      input$n_controls,
      input$case_carriers,
      input$control_carriers,
      input$sample_mult
    )

    scaled_tab <- build_table(
      scaled$n_cases,
      scaled$n_controls,
      scaled$case_carriers,
      scaled$control_carriers
    )
    scaled_or_ci <- compute_or_ci(scaled_tab)

    plot_df <- data.frame(
      Scenario = factor(c("Observed data", "With more data"),
      levels = c("Observed data", "With more data")),
      OR = c(current$odds_ratio, scaled_or_ci$odds_ratio),
      Lower = c(current$ci_low, scaled_or_ci$ci_low),
      Upper = c(current$ci_high, scaled_or_ci$ci_high)
    )

    ggplot(plot_df, aes(x = OR, y = Scenario)) +
      geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.7, color = "#7A8450") +
      geom_errorbarh(aes(xmin = Lower, xmax = Upper),
      height = 0.18,linewidth = 1.1, color = "#2F8F9D") +
      geom_point(size = 3.5, color = "#5C6B2F") +
      scale_x_log10() +
      labs(
        x = "Odds ratio (log scale)",
        y = NULL,
        caption = "The dashed vertical line marks an odds ratio of 1 (no association)."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(color = "#2F3E1F", face = "bold"),
        axis.text = element_text(color = "#2A2A2A"),
        plot.caption = element_text(hjust = 0, color = "#5F6B46")
      )
  })

  output$precision_plot <- renderPlot({
    req(valid_inputs()$valid)

    multipliers <- seq(1, input$sample_mult, by = 0.5)
    if (length(multipliers) == 1) multipliers <- c(1, input$sample_mult)
    multipliers <- unique(multipliers)

    precision_df <- do.call(rbind, lapply(multipliers, function(m) {
      scaled <- scale_scenario(
        input$n_cases,
        input$n_controls,
        input$case_carriers,
        input$control_carriers,
        m
      )
      tab <- build_table(
        scaled$n_cases,
        scaled$n_controls,
        scaled$case_carriers,
        scaled$control_carriers
      )
      stats <- compute_or_ci(tab)
      data.frame(
        multiplier = m,
        ci_width = stats$ci_high - stats$ci_low,
        ci_low = stats$ci_low,
        ci_high = stats$ci_high,
        or = stats$odds_ratio
      )
    }))

    ggplot(precision_df, aes(x = multiplier, y = ci_width)) +
      geom_line(linewidth = 1.1, color = "#2F8F9D") +
      geom_point(size = 2.8, color = "#5C6B2F") +
      labs(
        x = "Sample size multiplier",
        y = "95% CI width",
        caption = "Narrower intervals indicate greater precision."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "#2F3E1F", face = "bold"),
        axis.text = element_text(color = "#2A2A2A"),
        plot.caption = element_text(hjust = 0, color = "#5F6B46")
      )
  })

  output$interpretation_panel <- renderUI({
    req(valid_inputs()$valid)

    stats <- observed_stats()
    interp <- make_interpretation(
      or_ci = stats$or_ci,
      p_value = stats$test$p_value,
      multiplier = input$sample_mult,
      test_method = input$test_method,
      small_counts_flag = small_counts_flag()
    )

    tagList(
      div(class = "interp-headline", interp$headline),
      tags$p(tags$strong("Confidence interval: "), interp$ci_statement),
      tags$p(tags$strong("Statistical meaning: "), interp$significance_statement),
      tags$p(tags$strong("Plain-language interpretation: "), interp$plain_language),
      tags$p(tags$strong("Caution: "), interp$caution),
      tags$p(class = "small-note", tags$strong("Why this test? "), interp$method_note),
      if (stats$or_ci$corrected) {
        tags$p(class = "small-note", "A continuity correction was applied because at least one cell contained zero observations.")
      }
    )
  })
}
