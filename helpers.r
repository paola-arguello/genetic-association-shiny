# =====================

# =====================
# helpers.R
# =====================

build_table <- function(n_cases, n_controls, case_carriers, control_carriers) {
  matrix(
    c(
      case_carriers,
      n_cases - case_carriers,
      control_carriers,
      n_controls - control_carriers
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      Group = c("Cases", "Controls"),
      Variant = c("Carrier", "Non-carrier")
    )
  )
}

compute_or_ci <- function(tab) {
  a <- tab[1, 1]
  b <- tab[1, 2]
  c <- tab[2, 1]
  d <- tab[2, 2]

  corrected <- FALSE
  if (any(c(a, b, c, d) == 0)) {
    a <- a + 0.5
    b <- b + 0.5
    c <- c + 0.5
    d <- d + 0.5
    corrected <- TRUE
  }

  or <- (a * d) / (b * c)
  se_log_or <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  log_or <- log(or)
  ci_low <- exp(log_or - 1.96 * se_log_or)
  ci_high <- exp(log_or + 1.96 * se_log_or)

  list(
    odds_ratio = or,
    ci_low = ci_low,
    ci_high = ci_high,
    corrected = corrected,
    se_log_or = se_log_or
  )
}


compute_test <- function(tab, method = "fisher") {
  if (method == "fisher") {
    test <- fisher.test(tab)
    list(
      method_label = "Fisher's exact test",
      p_value = unname(test$p.value)
    )
  } else {
    test <- suppressWarnings(chisq.test(tab, correct = FALSE))
    list(
      method_label = "Chi-square test",
      p_value = unname(test$p.value),
      expected = test$expected
    )
  }
}

scale_scenario <- function(n_cases, n_controls, case_carriers, control_carriers, multiplier) {
  case_rate <- ifelse(n_cases > 0, case_carriers / n_cases, 0)
  control_rate <- ifelse(n_controls > 0, control_carriers / n_controls, 0)

  new_n_cases <- round(n_cases * multiplier)
  new_n_controls <- round(n_controls * multiplier)

  new_case_carriers <- round(new_n_cases * case_rate)
  new_control_carriers <- round(new_n_controls * control_rate)

  list(
    n_cases = new_n_cases,
    n_controls = new_n_controls,
    case_carriers = new_case_carriers,
    control_carriers = new_control_carriers
  )
}

make_interpretation <- function(or_ci, p_value, multiplier, test_method, small_counts_flag) {
  or <- or_ci$odds_ratio
  ci_low <- or_ci$ci_low
  ci_high <- or_ci$ci_high

  ci_includes_1 <- ci_low <= 1 && ci_high >= 1
  significant <- !ci_includes_1 && !is.na(p_value) && p_value < 0.05

  direction <- if (or > 1) {
    "higher odds of carrying the variant among cases than controls"
  } else if (or < 1) {
    "lower odds of carrying the variant among cases than controls"
  } else {
    "similar odds of carrying the variant in both groups"
  }

  headline <- if (significant) {
    "This result is compatible with a detectable association in this sample."
  } else {
    "This result does not provide strong evidence of a detectable association in this sample."
  }

  plain_language <- if (significant) {
    paste0(
      "The estimated odds ratio is ", sprintf("%.2f", or),
      ", suggesting ", direction, ". The 95% confidence interval does not include 1, ",
      "so this sample is consistent with a statistically meaningful association under the selected test."
    )
  } else {
    paste0(
      "The estimated odds ratio is ", sprintf("%.2f", or),
      ", suggesting ", direction, ". However, the 95% confidence interval includes 1, ",
      "which means the data are also compatible with no association."
    )
  }

  caution <- if (small_counts_flag) {
    "Caution: one or more cell counts are very small. The estimate may be unstable, the confidence interval may be wide, and Fisher's exact test is usually more appropriate in this setting."
  } else if (multiplier <= 1.5) {
    "Caution: with limited sample size, the estimate can look large while still being imprecise. Statistical uncertainty matters as much as the point estimate."
  } else {
    "Caution: adding more data narrows uncertainty when the underlying proportions stay similar, but bigger samples do not fix bias, confounding, or poor study design."
  }

  method_note <- if (test_method == "fisher") {
    "Fisher's exact test is useful when counts are small because it does not rely on large-sample approximations."
  } else {
    "The Chi-square test works best when expected cell counts are not too small and the large-sample approximation is reasonable."
  }

  list(
    headline = headline,
    ci_statement = if (ci_includes_1) {
      "The 95% confidence interval includes 1."
    } else {
      "The 95% confidence interval does not include 1."
    },
    significance_statement = if (significant) {
      paste0(
        "Using ", ifelse(test_method == "fisher", "Fisher's exact test", "the Chi-square test"),
        ", the association is statistically significant at the 0.05 level (p = ",
        sprintf("%.4f", p_value), ")."
      )
    } else {
      paste0(
        "Using ", ifelse(test_method == "fisher", "Fisher's exact test", "the Chi-square test"),
        ", the association is not statistically significant at the 0.05 level (p = ",
        sprintf("%.4f", p_value), ")."
      )
    },
    plain_language = plain_language,
    caution = caution,
    method_note = method_note
  )
}