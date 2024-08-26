library(dplyr)
library(tidyr)
library(readr)
library(epidatr)

res <- pub_fluview(
  regions = c("nat", paste0("hhs", 1:10)),
  epiweeks = epirange(201040, 201940)
)

res <- res |>
  dplyr::mutate(
    location = gsub("hhs", "HHS Region ", region),
    location = gsub("nat", "US National", location),
    target_end_date = epiweek
  )

to_obs_bin <- res |>
  dplyr::mutate(
    wili_floor = floor(wili * 10) / 10,
    output_type = "pmf",
    output_type_id = paste0("[", format(wili_floor, digits=1, nsmall=1, trim=TRUE), ",", format(wili_floor + 0.1, digits=1, nsmall=1, trim=TRUE), ")"),
    observation = 1
  ) |>
  dplyr::select(location, target_end_date, output_type, output_type_id, observation)

bin_lowers <- seq(from = 0, to = 12.9, by = 0.1)
to_all_bins <- tidyr::expand_grid(
  location = c("US National", paste0("HHS Region ", 1:10)),
  target_end_date = sort(unique(to_obs_bin$target_end_date)),
  output_type = "pmf",
  output_type_id = c(
    paste0("[", format(bin_lowers, digits=1, nsmall=1, trim=TRUE), ",", format(bin_lowers + 0.1, digits=1, nsmall=1, trim=TRUE), ")"),
    "[13.0,100.0)"
  )
)

to_bin <- dplyr::left_join(
  to_all_bins,
  to_obs_bin,
  by = c("location", "target_end_date", "output_type", "output_type_id")
) |>
  dplyr::mutate(observation = ifelse(is.na(observation), 0, 1))


to_quantile <- res |>
  dplyr::mutate(
    output_type = "quantile",
    output_type_id = "NA",
    observation = wili
  ) |>
  dplyr::select(location, target_end_date, output_type, output_type_id, observation)

to <- dplyr::bind_rows(to_bin, to_quantile)

readr::write_csv(to, "flusight-network-hub/target-data/target-observations.csv")
