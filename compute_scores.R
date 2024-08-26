# remotes::install_github("Infectious-Disease-Modeling-Hubs/hubData")
library(hubData)
# remotes::install_github("Infectious-Disease-Modeling-Hubs/hubEvals")
library(hubEvals)
library(dplyr)

hub_path <- "flusight-network-hub"
hub_con <- connect_hub(hub_path)

pmf_forecasts <- hub_con %>%
  filter(
    output_type == "pmf", target_end_date <= "2019-08-01",
    !model %in% c("CU_EAKFC_SEIRS", "CU_RHF_SEIRS")
  ) |>
  collect()

quantile_forecasts <- hub_con %>%
  filter(
    output_type == "quantile", target_end_date <= "2019-08-01",
    !model %in% c("CU_EAKFC_SEIRS", "CU_RHF_SEIRS")
  ) |>
  collect()

target_observations <- readr::read_csv(file.path(hub_path, "target-data", "target-observations.csv"))

pmf_scores <- hubEvals::score_model_out(
  pmf_forecasts |>
    group_by(model_id, location, reference_date, horizon, target_end_date) |>
    mutate(value = value / sum(value)) |>
    ungroup(),
  target_observations
)
saveRDS(pmf_scores, "pmf_scores.rds")

quantile_scores <- hubEvals::score_model_out(
  quantile_forecasts,
  target_observations
)
saveRDS(quantile_scores, "quantile_scores.rds")
