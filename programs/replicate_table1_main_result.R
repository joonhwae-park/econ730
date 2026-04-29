#!/usr/bin/env Rscript

required_packages <- c("haven", "dplyr", "did", "fixest", "ggplot2", "jsonlite", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them with install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    ")).",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(did)
  library(fixest)
  library(ggplot2)
  library(haven)
  library(jsonlite)
  library(tibble)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd_args[grepl("^--file=", cmd_args)])
script_dir <- if (length(file_arg) > 0) dirname(file_arg[1]) else getwd()
repo_root <- file.path(script_dir, "..")
if (!dir.exists(file.path(repo_root, "analysis_dataset"))) repo_root <- "."
setwd(repo_root)

minimal_input_rel <- file.path("analysis_dataset", "table1_minimal.csv.gz")
clean_input_rel <- file.path("analysis_dataset", "api_compustat_clean.dta")
raw_input_rel <- file.path("analysis_dataset", "api_compustat_gvkey_quarterly_sictreated.dta")
output_rel <- file.path("tables", "TABLE_01_main_result_replication.json")
pretrend_png_rel <- file.path("figures", "TABLE_01_pretrend_event_study_replication.png")
did_png_rel <- file.path("figures", "TABLE_01_did_santanna_dynamic_replication.png")

minimal_input_path <- minimal_input_rel
clean_input_path <- clean_input_rel
raw_input_path <- raw_input_rel
input_path <- if (file.exists(minimal_input_path)) {
  minimal_input_path
} else if (file.exists(clean_input_path)) {
  clean_input_path
} else {
  raw_input_path
}
input_rel <- input_path
output_path <- output_rel
pretrend_png_path <- pretrend_png_rel
did_png_path <- did_png_rel
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(pretrend_png_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(did_png_path), recursive = TRUE, showWarnings = FALSE)

stata_quarter_id <- function(dateq) {
  year <- floor(as.numeric(dateq))
  quarter <- round((as.numeric(dateq) - year) * 10)
  year * 4 + quarter
}

date_quarter_id <- function(x) {
  x_date <- as.Date(x)
  x_lt <- as.POSIXlt(x_date)
  year <- x_lt$year + 1900
  quarter <- floor(x_lt$mon / 3) + 1
  ifelse(is.na(x_date), NA_integer_, year * 4 + quarter)
}

drop_twfe_singletons <- function(data, firm_var = "gvkey", time_var = "date") {
  out <- data
  repeat {
    n_before <- nrow(out)
    out <- out %>%
      add_count(.data[[firm_var]], name = ".firm_n") %>%
      add_count(.data[[time_var]], name = ".time_n") %>%
      filter(.firm_n > 1, .time_n > 1) %>%
      select(-.firm_n, -.time_n)
    if (nrow(out) == n_before) break
  }
  out
}

add_event_study_terms <- function(data) {
  out <- data
  for (i in 51:1) {
    out[[paste0("sinceAPIq_F", i)]] <- as.integer(!is.na(out$sinceAPIq) & out$sinceAPIq == -i)
  }
  for (i in 1:60) {
    out[[paste0("sinceAPIq_L", i)]] <- as.integer(!is.na(out$sinceAPIq) & out$sinceAPIq == i)
  }
  out$sinceAPIq_0 <- as.integer(!is.na(out$sinceAPIq) & out$sinceAPIq == 0)
  out
}

make_analysis_data <- function(path) {
  data <- if (grepl("\\.csv\\.gz$|\\.csv$", path)) {
    read.csv(path, stringsAsFactors = FALSE)
  } else {
    read_dta(path)
  }

  data <- data %>%
    mutate(
      sic_code = as.character(sic_code),
      sic_code2 = as.character(sic_code2)
    ) %>%
    filter(!is.na(ln_mkt_val), !is.na(sic_code), sic_code != "")

  if (!"date" %in% names(data)) {
    data <- mutate(data, date = stata_quarter_id(dateq))
  } else if (is.character(data$date)) {
    data <- mutate(data, date = date_quarter_id(date))
  } else if (inherits(data$date, "Date") || inherits(data$date, "POSIXt")) {
    data <- mutate(data, date = date_quarter_id(date))
  }
  if (!"api_treat_date_q" %in% names(data)) {
    data <- mutate(data, api_treat_date_q = date_quarter_id(api_treat_date))
  } else if (inherits(data$api_treat_date_q, "Date") || inherits(data$api_treat_date_q, "POSIXt")) {
    data <- mutate(data, api_treat_date_q = date_quarter_id(api_treat_date_q))
  }
  if (!"sinceAPIq" %in% names(data)) {
    data <- mutate(data, sinceAPIq = date - api_treat_date_q)
  }
  if (!"ln_r_and_d_expense" %in% names(data)) {
    data <- mutate(data, ln_r_and_d_expense = log(r_and_d_expense + 1))
  }
  if (!"ln_assets_total" %in% names(data)) {
    data <- mutate(data, ln_assets_total = log(assets_total + 1))
  }
  if (!"sic_treat_rate" %in% names(data)) {
    data <- data %>%
      group_by(sic_code2) %>%
      mutate(sic_treat_rate = mean(api_ever_treated, na.rm = TRUE)) %>%
      ungroup()
  }
  if (!"api_ever_treated_01" %in% names(data)) {
    data <- mutate(data, api_ever_treated_01 = as.integer(!is.na(sic_treat_rate) & sic_treat_rate > 0.01))
  }
  if (!"api_treat_year" %in% names(data)) {
    data <- mutate(data, api_treat_year = as.integer(format(as.Date(api_treat_date), "%Y")))
  }
  if (!"sic_comp_prog" %in% names(data)) {
    data <- mutate(data, sic_comp_prog = as.integer(sic_code == "7370"))
  }

  add_event_study_terms(data)
}

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ "+",
    TRUE ~ ""
  )
}

extract_model_row <- function(model, sample_data, title, vcov_type) {
  model_summary <- summary(model)
  coef_table <- model_summary$coeftable
  term <- "api_treat_post"
  if (!term %in% rownames(coef_table)) {
    stop(
      "Could not find coefficient '", term, "' in model '", title, "'. Available coefficients: ",
      paste(rownames(coef_table), collapse = ", "),
      call. = FALSE
    )
  }
  estimate_col <- grep("estimate", colnames(coef_table), ignore.case = TRUE, value = TRUE)[1]
  se_col <- grep("std\\.? error|s\\.e\\.", colnames(coef_table), ignore.case = TRUE, value = TRUE)[1]
  stat_col <- grep("t value|z value|stat", colnames(coef_table), ignore.case = TRUE, value = TRUE)[1]
  p_col <- grep("^Pr\\(|p\\.?value|p-value", colnames(coef_table), ignore.case = TRUE, value = TRUE)[1]
  estimate <- if (!is.na(estimate_col)) coef_table[term, estimate_col] else coef_table[term, 1]
  std_error <- if (!is.na(se_col)) coef_table[term, se_col] else coef_table[term, 2]
  statistic <- if (!is.na(stat_col)) coef_table[term, stat_col] else coef_table[term, 3]
  p_value <- if (!is.na(p_col)) coef_table[term, p_col] else coef_table[term, 4]
  adj_r2 <- tryCatch(as.numeric(fixest::r2(model, type = "ar2")), error = function(e) NA_real_)
  n_obs <- if (!is.null(model_summary$nobs)) model_summary$nobs else nrow(sample_data)

  tibble(
    model = title,
    vcov = vcov_type,
    term = "Post x API",
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    p_value = p_value,
    stars = stars(p_value),
    r2_adjusted = adj_r2,
    observations = n_obs,
    firms = n_distinct(sample_data$gvkey),
    api_adopters = n_distinct(sample_data$gvkey[sample_data$api_ever_treated == 1]),
    firm_fe = "Yes",
    quarter_fe = "Yes"
  )
}

run_model <- function(data, title, filter_expr, vcov_formula = ~gvkey, vcov_type = "clustered_by_gvkey") {
  sample_data <- data %>%
    filter({{ filter_expr }}) %>%
    filter(
      !is.na(ln_mkt_val),
      !is.na(api_treat_post),
      !is.na(gvkey),
      !is.na(date)
    ) %>%
    drop_twfe_singletons()

  model <- feols(
    ln_mkt_val ~ api_treat_post | gvkey + date,
    data = sample_data,
    vcov = vcov_formula,
    ssc = ssc(K.adj = TRUE, K.fixef = "nested", G.adj = TRUE),
    fixef.rm = "none",
    notes = FALSE
  )

  extract_model_row(model, sample_data, title, vcov_type)
}

run_pretrend_event_study <- function(data, png_path) {
  event_terms <- c(paste0("sinceAPIq_F", 51:1), paste0("sinceAPIq_L", 1:60))
  visible_terms <- c(paste0("sinceAPIq_F", 8:1), paste0("sinceAPIq_L", 1:8))
  sample_data <- data %>%
    filter(!is.na(ln_mkt_val), !is.na(gvkey), !is.na(date)) %>%
    drop_twfe_singletons()

  event_formula <- as.formula(
    paste("ln_mkt_val ~", paste(event_terms, collapse = " + "), "| gvkey + date")
  )

  model <- feols(
    event_formula,
    data = sample_data,
    vcov = ~gvkey,
    ssc = ssc(K.adj = TRUE, K.fixef = "nested", G.adj = TRUE),
    fixef.rm = "none",
    notes = FALSE
  )

  model_summary <- summary(model)
  coef_table <- model_summary$coeftable
  estimate_col <- grep("estimate", colnames(coef_table), ignore.case = TRUE, value = TRUE)[1]
  se_col <- grep("std\\.? error|s\\.e\\.", colnames(coef_table), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(estimate_col)) estimate_col <- colnames(coef_table)[1]
  if (is.na(se_col)) se_col <- colnames(coef_table)[2]
  available_visible_terms <- intersect(visible_terms, rownames(coef_table))
  plot_data <- tibble(term = available_visible_terms) %>%
    mutate(
      relative_quarter = case_when(
        grepl("_F", term) ~ -as.integer(sub(".*_F", "", term)),
        grepl("_L", term) ~ as.integer(sub(".*_L", "", term)),
        TRUE ~ NA_integer_
      ),
      estimate = coef_table[term, estimate_col],
      std_error = coef_table[term, se_col],
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error
    ) %>%
    bind_rows(tibble(relative_quarter = 0, estimate = 0, std_error = 0, conf_low = 0, conf_high = 0)) %>%
    arrange(relative_quarter)

  pretrend_terms <- intersect(paste0("sinceAPIq_F", 8:1), rownames(coef_table))
  beta_pre <- coef_table[pretrend_terms, estimate_col]
  covariance_matrix <- model_summary$cov.scaled
  if (is.null(covariance_matrix)) {
    covariance_matrix <- model_summary$cov.iid
  }
  if (is.null(covariance_matrix)) {
    covariance_matrix <- matrix(NA_real_, nrow = nrow(coef_table), ncol = nrow(coef_table))
    rownames(covariance_matrix) <- rownames(coef_table)
    colnames(covariance_matrix) <- rownames(coef_table)
  }
  vcov_pre <- covariance_matrix[pretrend_terms, pretrend_terms, drop = FALSE]
  wald_stat <- if (length(beta_pre) > 0 && !anyNA(vcov_pre)) {
    as.numeric(t(beta_pre) %*% qr.solve(vcov_pre, beta_pre))
  } else {
    NA_real_
  }
  wald_df <- length(beta_pre)
  wald_p_value <- if (!is.na(wald_stat)) pchisq(wald_stat, df = wald_df, lower.tail = FALSE) else NA_real_

  plot <- ggplot(plot_data, aes(x = relative_quarter, y = estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey45") +
    geom_vline(xintercept = -0.5, linewidth = 0.35, linetype = "dashed", color = "grey45") +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.18, color = "grey35") +
    geom_point(size = 2.1, color = "black") +
    scale_x_continuous(breaks = -8:8) +
    labs(
      x = "Quarters Since API Adoption",
      y = "Coefficient on Log Market Value",
      title = "Pretrend Event Study: Public API Adoption"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )

  ggsave(png_path, plot = plot, width = 8, height = 5, dpi = 300)

  list(
    figure = pretrend_png_rel,
    visible_coefficients = plot_data,
    pretrend_joint_test = tibble(
      tested_terms = paste(pretrend_terms, collapse = ", "),
      statistic = wald_stat,
      df = wald_df,
      p_value = wald_p_value
    )
  )
}

make_did_data <- function(data) {
  data %>%
    filter(!is.na(ln_mkt_val), !is.na(gvkey), !is.na(date), !is.na(api_treat_post)) %>%
    arrange(gvkey, date) %>%
    distinct(gvkey, date, .keep_all = TRUE) %>%
    group_by(gvkey) %>%
    mutate(
      first_treat_date = ifelse(any(api_treat_post == 1, na.rm = TRUE), min(date[api_treat_post == 1], na.rm = TRUE), 0),
      first_treat_date = ifelse(is.infinite(first_treat_date), 0, first_treat_date)
    ) %>%
    ungroup() %>%
    mutate(
      gvkey_did = as.integer(factor(gvkey)),
      first_treat_date = as.integer(first_treat_date),
      date = as.integer(date)
    )
}

run_santanna_did <- function(data, png_path) {
  did_data <- make_did_data(data)
  did_warnings <- character()

  att_gt_fit <- withCallingHandlers(
    att_gt(
      yname = "ln_mkt_val",
      tname = "date",
      idname = "gvkey_did",
      gname = "first_treat_date",
      data = did_data,
      xformla = ~1,
      control_group = "notyettreated",
      allow_unbalanced_panel = TRUE,
      bstrap = FALSE,
      print_details = FALSE
    ),
    warning = function(w) {
      did_warnings <<- c(did_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  simple_fit <- withCallingHandlers(
    aggte(att_gt_fit, type = "simple", bstrap = FALSE, na.rm = TRUE),
    warning = function(w) {
      did_warnings <<- c(did_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  dynamic_fit <- withCallingHandlers(
    aggte(att_gt_fit, type = "dynamic", bstrap = FALSE, na.rm = TRUE),
    warning = function(w) {
      did_warnings <<- c(did_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  dynamic_data <- tibble(
    event_time = dynamic_fit$egt,
    estimate = dynamic_fit$att.egt,
    std_error = dynamic_fit$se.egt
  ) %>%
    mutate(
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error
    ) %>%
    filter(event_time >= -8, event_time <= 8)

  plot <- ggplot(dynamic_data, aes(x = event_time, y = estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey45") +
    geom_vline(xintercept = -0.5, linewidth = 0.35, linetype = "dashed", color = "grey45") +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.18, color = "grey35") +
    geom_point(size = 2.1, color = "black") +
    scale_x_continuous(breaks = -8:8) +
    labs(
      x = "Quarters Since API Adoption",
      y = "ATT on Log Market Value",
      title = "Callaway-Sant'Anna DID: Dynamic ATT"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )

  ggsave(png_path, plot = plot, width = 8, height = 5, dpi = 300)

  list(
    estimator = "did::att_gt by Callaway and Sant'Anna",
    control_group = "notyettreated",
    figure = did_png_rel,
    warnings = unique(did_warnings),
    simple_att = tibble(
      estimate = simple_fit$overall.att,
      std_error = simple_fit$overall.se,
      statistic = simple_fit$overall.att / simple_fit$overall.se,
      p_value = 2 * pnorm(abs(simple_fit$overall.att / simple_fit$overall.se), lower.tail = FALSE),
      observations = nrow(did_data),
      firms = n_distinct(did_data$gvkey_did),
      api_adopters = n_distinct(did_data$gvkey_did[did_data$first_treat_date > 0])
    ),
    dynamic_att_visible = dynamic_data
  )
}

analysis_data <- make_analysis_data(input_path)

results <- bind_rows(
  run_model(analysis_data, "All Firms", TRUE),
  run_model(analysis_data, "Excluding Top 20 Firms with Most Popular APIs", api_followcount < 382),
  run_model(analysis_data, "Excluding Industries with Fewer than 1% Firms with APIs", api_ever_treated_01 == 1),
  run_model(analysis_data, "Excluding Any Firm in Computer Services", sic_comp_prog != 1),
  run_model(analysis_data, "Year API < 2012", api_treat_year < 2012 | is.na(api_treat_year)),
  run_model(analysis_data, "Year API >= 2012", api_treat_year >= 2012 | is.na(api_treat_year))
)

double_cluster_results <- run_model(
  analysis_data,
  "All Firms",
  TRUE,
  vcov_formula = ~gvkey + date,
  vcov_type = "clustered_by_gvkey_and_date"
)

pretrend_results <- run_pretrend_event_study(analysis_data, pretrend_png_path)
did_results <- run_santanna_did(analysis_data, did_png_path)

json_payload <- list(
  source = list(
    paper = "How APIs Create Growth by Inverting the Firm",
    table = "Table 1 main result",
    stata_source = "programs/2_stata_regressions.do",
    data = input_rel
  ),
  specification = list(
    outcome = "ln_mkt_val",
    treatment = "api_treat_post",
    fixed_effects = c("gvkey", "date"),
    standard_errors = "Main results clustered by gvkey; double-cluster result clustered by gvkey and date.",
    estimator = "fixest::feols",
    singleton_handling = "Firm and quarter singleton fixed-effect observations are iteratively removed before estimation to match reghdfe.",
    duplicate_handling = "The published Table 1 sample keeps the 11 duplicate gvkey-quarter rows present after preprocessing; this script keeps them as well."
  ),
  results = results,
  double_cluster_results = double_cluster_results,
  pretrend = pretrend_results,
  santanna_did = did_results
)

write_json(json_payload, output_path, pretty = TRUE, auto_unbox = TRUE, digits = NA)

print(
  results %>%
    transmute(
      VCOV = vcov,
      Model = model,
      `Post x API` = sprintf("%.3f%s", estimate, stars),
      `Std. Error` = sprintf("(%.4f)", std_error),
      `Adj. R2` = sprintf("%.3f", r2_adjusted),
      Obs = observations,
      Firms = firms,
      `API Adopters` = api_adopters
    ),
  n = Inf
)

cat("\nDouble-clustered all-firms result:\n")
print(
  double_cluster_results %>%
    transmute(
      VCOV = vcov,
      Model = model,
      `Post x API` = sprintf("%.3f%s", estimate, stars),
      `Std. Error` = sprintf("(%.4f)", std_error),
      `Adj. R2` = sprintf("%.3f", r2_adjusted),
      Obs = observations,
      Firms = firms,
      `API Adopters` = api_adopters
    ),
  n = Inf
)

cat("\nPretrend joint test, quarters -8 through -1:\n")
print(pretrend_results$pretrend_joint_test, n = Inf)

cat("\nCallaway-Sant'Anna DID simple ATT:\n")
print(did_results$simple_att, n = Inf)
cat("\nCallaway-Sant'Anna DID warnings captured: ", length(did_results$warnings), "\n", sep = "")

cat("\nJSON saved to: ", output_path, "\n", sep = "")
cat("Pretrend PNG saved to: ", pretrend_png_path, "\n", sep = "")
cat("Callaway-Sant'Anna DID PNG saved to: ", did_png_path, "\n", sep = "")
