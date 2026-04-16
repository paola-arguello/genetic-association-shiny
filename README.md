# When is a genetic association real?

This repository contains a small Shiny app built to explain how statistical evidence is interpreted in a case-control genetics setting.

The app focuses on one question: when does an observed association between a genetic variant and disease status look convincing, and when is it still too uncertain to interpret confidently?

Using simple user inputs, the app calculates:

- odds ratios
- 95% confidence intervals
- p-values
- dynamic interpretation in plain language

It also includes a **"What if I had more data?"** feature to show how increasing sample size can narrow confidence intervals and improve estimate stability.

## Why this project?

This app was designed as a portfolio piece for clinical, biostatistical, and bioinformatics roles. Its goal is not just to perform calculations, but to communicate statistical reasoning clearly.

## Main ideas demonstrated

- effect size is not enough on its own
- confidence intervals matter for interpretation
- statistical significance depends on uncertainty, not just magnitude
- larger samples can make estimates more stable, but do not fix bias or poor study design

## Files & App Structure


`genetic-association-shiny/` 


├── `app.R`: launcher for the application


├── `ui.R`: user interface


├── `server.R`: server logic and statistical calculations


├── `helpers.R`: auxiliary functions 


└── `www/`


   └── `styles.css`: style and aesthetics


## Run locally

Open R in this project folder and run:

```r
shiny::runApp()
