
# =====================
# ui.R
# =====================

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  titlePanel(
    div(
      h2("When is a genetic association real?", class = "app-title"),
      div(
        "A case-control genetics example for understanding odds ratios, confidence intervals, statistical significance, and why sample size changes what you can conclude.",
        class = "app-subtitle"
      )
    )
  ),

  fluidRow(
    column(
      width = 4,
      div(
        class = "panel-card",
        div("Study setup", class = "section-title"),
        numericInput("n_cases", "Number of cases", value = 100, min = 1, step = 1),
        numericInput("n_controls", "Number of controls", value = 100, min = 1, step = 1),
        numericInput("case_carriers", "Variant carriers among cases", value = 30, min = 0, step = 1),
        numericInput("control_carriers", "Variant carriers among controls", value = 15, min = 0, step = 1),
        radioButtons(
          "test_method",
          "Statistical test",
          choices = c("Fisher's exact test" = "fisher", "Chi-square test" = "chisq"),
          selected = "fisher"
        ),
        div(
          class = "small-note",
          strong("Test guidance: "),
          textOutput("test_explanation", inline = TRUE)
        )
      ),
      div(
        class = "panel-card",
        div("What if I had more data?", class = "section-title"),
        sliderInput(
          "sample_mult",
          "Increase total sample size while keeping the same carrier proportions",
          min = 1,
          max = 10,
          value = 1,
          step = 0.5
        ),
        div(
          class = "small-note",
          "This shows how precision changes when the underlying pattern stays the same but the study gets larger."
        )
      ),
      div(
        class = "panel-card",
        div("Observed 2 x 2 table", class = "section-title"),
        tableOutput("contingency_table"),
        br(),
        uiOutput("validation_messages")
      )
    ),


    column(
      width = 8,
      fluidRow(
        column(
          width = 4,
          div(
            class = "panel-card metric-card",
            div(textOutput("or_value"), class = "metric-value"),
            div("Odds ratio", class = "metric-label")
          )
        ),
        column(
          width = 4,
          div(
            class = "panel-card metric-card",
            div(textOutput("ci_value"), class = "metric-value"),
            div("95% confidence interval", class = "metric-label")
          )
        ),
        column(
          width = 4,
          div(
            class = "panel-card metric-card",
            div(textOutput("p_value"), class = "metric-value"),
            div("P-value", class = "metric-label")
          )
        )
      ),

      div(
        class = "panel-card",
        div("Effect estimate and uncertainty", class = "section-title"),
        plotOutput("forest_plot", height = "260px")
      ),

      div(
        class = "panel-card",
        div("How confidence changes as sample size grows", class = "section-title"),
        plotOutput("precision_plot", height = "300px")
      ),

      div(
        class = "panel-card interpretation-card",
        div("Interpretation", class = "section-title"),
        htmlOutput("interpretation_panel")
      )
    )
  )
)

