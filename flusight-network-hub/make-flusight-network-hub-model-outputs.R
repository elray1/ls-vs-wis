library(tidyverse)
library(hubverse)
library(zeallot)
library(MMWRweek)
library(distfromq)

source_root <- "../../flu/cdc-flusight-ensemble"
source_mo_rels <- c("model-forecasts/component-models", "model-forecasts/real-time-component-models")
target_dir <- "flusight-network-hub/model-output"

get_ew_from_filename <- function(submission) {
  ew_ww <- substr(submission, 3, 4)
  ew_yyyy <- substr(submission, 6, 9)
  return(c(ew_yyyy, ew_ww))
}

process_file <- function(model_dir, submission) {
  c(ew_yyyy, ew_ww) %<-% get_ew_from_filename(submission)
  ref_date <- MMWRweek2Date(as.integer(ew_yyyy), as.integer(ew_ww))

  submission_path <- file.path(model_dir, submission)
  mo <- readr::read_csv(
    submission_path#,
    # col_types = cols(
    #   Location = col_character(),
    #   Target = col_character(),
    #   Type = col_character(),
    #   Unit = col_character(),
    #   Bin_start_incl = col_character(),
    #   Bin_end_notincl = col_character(),
    #   Value = col_double()
    # )
  )
  colnames(mo) <- tolower(colnames(mo))
  
  mo <- mo |>
    dplyr::filter(
      target %in% paste0(1:4, " wk ahead"),
      type == "Bin"
    ) |>
    dplyr::mutate(
      value = as.numeric(value),
      horizon = as.integer(substr(target, 1, 1)),
      output_type = "pmf",
      output_type_id = paste0("[", format(as.numeric(bin_start_incl), digits=1, nsmall=1, trim=TRUE), ",", format(as.numeric(bin_end_notincl), digits=1, nsmall=1, trim=TRUE), ")"),
      reference_date = ref_date,
      target_end_date = ref_date + 7 * horizon,
      bin_upper = as.numeric(bin_end_notincl)
    ) |>
    dplyr::select(
      location = location, reference_date = reference_date,
      horizon = horizon, target_end_date = target_end_date,
      output_type = output_type, output_type_id = output_type_id,
      value = value,
      bin_upper = bin_upper
    )

  q_levels <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5,
                0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99)
  q_levels_char <- as.character(q_levels)
  mo_quantile <- mo |>
    dplyr::group_by(location, reference_date, horizon, target_end_date) |>
    dplyr::summarize(
      output_type_id = list(q_levels),
      value = list(
        # TODO: it may be better to drop the uppermost bin before doing this operation
        distfromq::make_q_fn(ps = cumsum(value) / sum(value), qs = bin_upper, tail_dist = "norm")(q_levels)
      ),
      .groups = "drop"
    ) |>
    tidyr::unnest(cols = c("output_type_id", "value"))

  mo_combined <- dplyr::bind_rows(
    mo |> dplyr::select(-bin_upper),
    mo_quantile |>
      dplyr::mutate(
        output_type = "quantile",
        output_type_id = as.character(output_type_id)
      )
  )

  return(mo_combined)
}

get_new_submission_path <- function(target_dir, model, submission) {
  c(ew_yyyy, ew_ww) %<-% get_ew_from_filename(submission)
  filename <- paste0("EW", ew_yyyy, ew_ww, "-", model, ".csv")
  model_dir <- file.path(target_dir, model)
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  path <- file.path(model_dir, filename)
  return(path)
}

process_and_save_file <- function(model_dir, model, submission, target_dir) {
  c(ew_yyyy, ew_ww) %<-% get_ew_from_filename(submission)
  target_submission_path <- get_new_submission_path(target_dir, model,
                                                    submission)
  if (!file.exists(target_submission_path) && as.integer(ew_ww) >= 1 && as.integer(ew_ww) <= 53) {
    mo <- process_file(model_dir, submission)
    write.csv(mo, target_submission_path, row.names = FALSE)
  }
}

for (source_mo_rel in source_mo_rels) {
  source_mo <- file.path(source_root, source_mo_rel)
  models <- list.dirs(source_mo, full.names = FALSE, recursive = FALSE)
  for (model in models) {
    print(model)
    model_dir <- file.path(source_mo, model)
    submissions <- list.files(model_dir, pattern = "*.csv")
    for (submission in submissions) {
      process_and_save_file(model_dir, model, submission, target_dir)
    }
  }
}
