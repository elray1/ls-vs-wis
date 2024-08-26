library(tidyverse)

hub_path <- "flusight-network-hub"

target_observations <- readr::read_csv(file.path(hub_path, "target-data", "target-observations.csv"))
pmf_scores <- readRDS("pmf_scores.rds")
quantile_scores <- readRDS("quantile_scores.rds")

scores <- dplyr::full_join(
  pmf_scores, quantile_scores,
  by = c("model", "location", "reference_date", "horizon", "target_end_date")
) |>
  dplyr::mutate(
    trunc_log_score = pmin(log_score, 10),
    task = paste(location, reference_date, horizon, sep = "_")
  )

dates_keep <- unique(
  scores$reference_date[scores$model == "ReichLab_sarima_seasonal_difference_TRUE"]
)
scores <- scores |> filter(reference_date %in% dates_keep)

model_counts <- scores |> 
  dplyr::group_by(model) |>
  dplyr::summarize(n = n())


ggplot(data = scores) +
  geom_point(mapping = aes(x = log_score, y = wis, color = interval_coverage_90))

ggplot(data = scores) +
  geom_point(mapping = aes(x = log(1 + log_score), y = log(1 + wis), color = interval_coverage_90))

ggplot(
  data = scores |>
    select(trunc_log_score, wis, interval_coverage_90) |>
    pivot_longer(cols = c("trunc_log_score", "wis"), names_to = "score_name", values_to = "score")
) +
  geom_density(mapping = aes(x = score, color = interval_coverage_90)) +
  facet_wrap( ~ score_name, scales = "free_x", ncol = 1) +
  theme_bw()

ggplot(
  data = scores |>
    mutate(trunc_wis = pmin(wis, 10)) |>
    select(trunc_log_score, trunc_wis, interval_coverage_90) |>
    pivot_longer(cols = c("trunc_log_score", "trunc_wis"), names_to = "score_name", values_to = "score")
) +
  geom_density(mapping = aes(x = score, color = interval_coverage_90)) +
  xlim(0, 10) +
  facet_wrap( ~ score_name, ncol = 1) +
  theme_bw()

# this is almost certainly not the right idea
transport::wasserstein1d(
  scores$trunc_log_score[scores$interval_coverage_90],
  scores$trunc_log_score[!scores$interval_coverage_90]
)

transport::wasserstein1d(
  scores$wis[scores$interval_coverage_90],
  scores$wis[!scores$interval_coverage_90]
)

max(scores$wis)

scores <- scores |>
  group_by(location, reference_date, horizon, target_end_date) |>
  mutate(
    rel_rank_wis = (rank(wis) - 1) / (n() - 1),
    rel_rank_log_score = (rank(log_score) - 1) / (n() - 1),
  )

ggplot(
  data = scores |>
    dplyr::select(rel_rank_log_score, rel_rank_wis, interval_coverage_90) |>
    tidyr::pivot_longer(cols = c("rel_rank_log_score", "rel_rank_wis"),
                        names_to = "score_name", names_prefix = "rel_rank_",
                        values_to = "rel_rank")
  ) +
  geom_density(
    mapping = aes(x = rel_rank, color = interval_coverage_90)
  ) +
  facet_wrap( ~ score_name, ncol = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(
  data = scores |>
    dplyr::select(rel_rank_log_score, rel_rank_wis, interval_coverage_90) |>
    tidyr::pivot_longer(cols = c("rel_rank_log_score", "rel_rank_wis"),
                        names_to = "score_name", names_prefix = "rel_rank_",
                        values_to = "rel_rank")
  ) +
  geom_bar(
    mapping = aes(x = rel_rank, fill = interval_coverage_90),
    position = position_fill(),
    just = 0.5,
    width = 0.1
  ) +
#   coord_cartesian(xlim = c(0, 1), expand=FALSE) +
  facet_wrap( ~ score_name, ncol = 1) +
  theme_bw()


ggplot() +
  geom_col(
    data = data.frame(
      binned_rank = seq(from = 0.05, to = 0.95, by = 0.1),
      prop_cov = 1
    ),
    mapping = aes(x = binned_rank, y = prop_cov, fill = "90% interval didn't cover obs"),
    just = 0.5,
    width = 0.1
  ) +
  geom_col(
    data = scores |>
      dplyr::select(rel_rank_log_score, rel_rank_wis, interval_coverage_90) |>
      tidyr::pivot_longer(cols = c("rel_rank_log_score", "rel_rank_wis"),
                          names_to = "score_name", names_prefix = "rel_rank_",
                          values_to = "rel_rank") |>
      mutate(
        binned_rank = dplyr::case_when(
          rel_rank >= 0 & rel_rank < 0.1 ~ 0.05,
          rel_rank >= 0.1 & rel_rank < 0.2 ~ 0.15,
          rel_rank >= 0.2 & rel_rank < 0.3 ~ 0.25,
          rel_rank >= 0.3 & rel_rank < 0.4 ~ 0.35,
          rel_rank >= 0.4 & rel_rank < 0.5 ~ 0.45,
          rel_rank >= 0.5 & rel_rank < 0.6 ~ 0.55,
          rel_rank >= 0.6 & rel_rank < 0.7 ~ 0.65,
          rel_rank >= 0.7 & rel_rank < 0.8 ~ 0.75,
          rel_rank >= 0.8 & rel_rank < 0.9 ~ 0.85,
          rel_rank >= 0.9  ~ 0.95,
        )
      ) |>
      group_by(score_name, binned_rank) |>
      summarize(
        prop_cov = mean(interval_coverage_90)
      ),
    mapping = aes(x = binned_rank, y = prop_cov, fill = "90% interval covered obs"),
    just = 0.5,
    width = 0.1
  ) +
#   coord_cartesian(xlim = c(0, 1), expand=FALSE) +
  facet_wrap( ~ score_name, ncol = 1) +
  theme_bw() +
  theme(legend.position = "bottom")



ggplot(
  data = scores |>
    dplyr::select(rel_rank_log_score, rel_rank_wis, interval_coverage_90) |>
    tidyr::pivot_longer(cols = c("rel_rank_log_score", "rel_rank_wis"),
                        names_to = "score_name", names_prefix = "rel_rank_",
                        values_to = "rel_rank")
  ) +
  geom_freqpoly(
    mapping = aes(x = rel_rank, color = interval_coverage_90),
    bins = 10
  ) +
  coord_cartesian(xlim = c(0, 1), expand=FALSE) +
  facet_wrap( ~ score_name, ncol = 1) +
  theme_bw()



# ggplot() +
#   geom_line(
#     data = scores |>
#       mutate(model_task = paste(model, task, sep = "_")) |>
#       dplyr::select(model_task, rel_rank_log_score, rel_rank_wis, interval_coverage_90) |>
#       tidyr::pivot_longer(cols = c("rel_rank_log_score", "rel_rank_wis"),
#                           names_to = "score_name", names_prefix = "rel_rank_",
#                           values_to = "rel_rank"),
#     mapping = aes(x = score_name, y = rel_rank, color = interval_coverage_90, group = model_task)
#   ) +
#   theme_bw()

scores |>
  ungroup() |>
  mutate(rank_diff = rel_rank_log_score - rel_rank_wis) |>
  slice_max(rank_diff) |>
  select(model, location, reference_date, horizon, target_end_date, log_score_rel_rank, rel_rank_wis)



scores |>
  filter(location == "HHS Region 6", reference_date == "2017-12-10", horizon == 3) |>
  select(model, log_score, wis, rel_rank_log_score, rel_rank_wis) |>
  as.data.frame()


library(hubData)
library(hubVis)
hub_path <- "flusight-network-hub"
hub_con <- connect_hub(hub_path)
forecasts <- hub_con %>%
  filter(output_type == "pmf", location == "HHS Region 6", reference_date == "2017-12-10", horizon == 3) %>%
  collect()

forecasts <- forecasts |>
  filter(!model_id %in% c("CU_EAKFC_SEIRS", "CU_RHF_SEIRS")) |>
  dplyr::mutate(
    bin_lower = as.numeric(substr(output_type_id, 2, str_locate(output_type_id, ",") - 1)),
    bin_upper = as.numeric(substr(output_type_id, str_locate(output_type_id, ",") + 1, nchar(output_type_id) - 1)),
    bin_mid = bin_lower + 0.05
  )

target_observations <- readr::read_csv(file.path(hub_path, "target-data", "target-observations.csv"))

ggplot() +
  geom_col(
    data = forecasts,
    mapping = aes(x = bin_mid, y = value),
    color = "cornflowerblue",
    fill = "cornflowerblue"
  ) +
  geom_vline(
    data = target_observations |>
      filter(output_type == "quantile", location == "HHS Region 6", target_end_date == as.Date("2017-12-10") + 3 * 7),
    mapping = aes(xintercept = observation),
    color = "orange",
    linetype = 2
  ) +
  facet_wrap( ~ model_id) +
  xlab("wILI percent") +
  ylab("Predicted probability") +
  theme_bw()



scores |>
  filter(
    !interval_coverage_90,
    rel_rank_log_score > 0.9,
    rel_rank_wis > 0.4, rel_rank_wis < 0.6
  ) |>
  arrange(desc(ae_median)) |>
  select(model, location, reference_date, horizon, target_end_date)




forecasts <- hub_con %>%
  filter(output_type == "pmf", location == "US National", reference_date == "2013-10-06", horizon == 1) %>%
  collect() |>
  filter(!model_id %in% c("CU_EAKFC_SEIRS", "CU_RHF_SEIRS")) |>
  dplyr::mutate(
    bin_lower = as.numeric(substr(output_type_id, 2, str_locate(output_type_id, ",") - 1)),
    bin_upper = as.numeric(substr(output_type_id, str_locate(output_type_id, ",") + 1, nchar(output_type_id) - 1)),
    bin_mid = bin_lower + 0.05
  )

ggplot() +
  geom_col(
    data = forecasts |> filter(bin_mid <= 5),
    mapping = aes(x = bin_mid, y = value),
    color = "cornflowerblue",
    fill = "cornflowerblue"
  ) +
  geom_vline(
    data = target_observations |>
      filter(output_type == "quantile", location == "US National", target_end_date == as.Date("2013-10-06") + 1 * 7),
    mapping = aes(xintercept = observation),
    color = "orange",
    linetype = 2
  ) +
  facet_wrap( ~ model_id) +
  xlab("wILI percent") +
  ylab("Predicted probability") +
  theme_bw()






forecasts <- hub_con %>%
  filter(output_type == "pmf", location == "HHS Region 6", reference_date == "2018-01-21", horizon == 1) %>%
  collect() |>
  filter(!model_id %in% c("CU_EAKFC_SEIRS", "CU_RHF_SEIRS")) |>
  dplyr::mutate(
    bin_lower = as.numeric(substr(output_type_id, 2, str_locate(output_type_id, ",") - 1)),
    bin_upper = as.numeric(substr(output_type_id, str_locate(output_type_id, ",") + 1, nchar(output_type_id) - 1)),
    bin_mid = bin_lower + 0.05
  )

ggplot() +
  geom_col(
    data = forecasts,
    mapping = aes(x = bin_mid, y = value),
    color = "cornflowerblue",
    fill = "cornflowerblue"
  ) +
  geom_vline(
    data = target_observations |>
      filter(output_type == "quantile", location == "HHS Region 6", target_end_date == as.Date("2018-01-21") + 1 * 7),
    mapping = aes(xintercept = observation),
    color = "orange",
    linetype = 2
  ) +
  facet_wrap( ~ model_id) +
  xlab("wILI percent") +
  ylab("Predicted probability") +
  theme_bw()





q_forecasts <- hub_con %>%
  filter(output_type == "quantile", !model_id %in% c("CU_EAKFC_SEIRS", "CU_RHF_SEIRS")) %>%
  collect()

underpred_props <- q_forecasts |>
  filter(output_type_id == "0.95") |>
  left_join(
    target_observations |> filter(output_type == "quantile"),
    by = c("location", "target_end_date")
  ) |>
  mutate(
    underpred = (value <= observation)
  ) |>
  group_by(location, reference_date, horizon) |>
  summarize(
    prop_underpred = mean(value < observation)
  )

underpred_props |>
  ungroup() |>
  slice_max(prop_underpred)


scores_subset <- scores |>
  ungroup() |>
  filter(location == "HHS Region 2", reference_date == "2018-01-07", horizon == 4) |>
  select(model, log_score, wis, rel_rank_log_score, rel_rank_wis, interval_coverage_90) |>
  arrange(rel_rank_wis) |>
  as.data.frame()

ggplot(
  data = scores_subset |>
    select(-c("log_score", "wis")) |>
    tidyr::pivot_longer(cols = c("rel_rank_log_score", "rel_rank_wis"),
                        names_to = "score_name",
                        values_to = "rel_rank")
) +
  geom_line(
    mapping = aes(x = score_name, y = rel_rank, color = interval_coverage_90, group = model)
  ) +
  theme_bw()


forecasts <- hub_con %>%
  filter(output_type == "pmf", location == "HHS Region 2", reference_date == "2018-01-07", horizon == 4) %>%
  collect() |>
  filter(!model_id %in% c("CU_EAKFC_SEIRS", "CU_RHF_SEIRS")) |>
  dplyr::mutate(
    bin_lower = as.numeric(substr(output_type_id, 2, str_locate(output_type_id, ",") - 1)),
    bin_upper = as.numeric(substr(output_type_id, str_locate(output_type_id, ",") + 1, nchar(output_type_id) - 1)),
    bin_mid = bin_lower + 0.05
  )

ggplot() +
  geom_col(
    data = forecasts,
    mapping = aes(x = bin_mid, y = value),
    color = "cornflowerblue",
    fill = "cornflowerblue"
  ) +
  geom_vline(
    data = target_observations |>
      filter(output_type == "quantile", location == "HHS Region 2", target_end_date == as.Date("2018-01-07") + 4 * 7),
    mapping = aes(xintercept = observation),
    color = "orange",
    linetype = 2
  ) +
  facet_wrap( ~ model_id) +
  xlab("wILI percent") +
  ylab("Predicted probability") +
  theme_bw()







q_forecasts |>
  left_join(
    target_observations |> filter(output_type == "quantile"),
    by = c("location", "target_end_date")
  ) |>
  group_by(model)


   |>
  filter() |>
  dplyr::mutate(
    bin_lower = as.numeric(substr(output_type_id, 2, str_locate(output_type_id, ",") - 1)),
    bin_upper = as.numeric(substr(output_type_id, str_locate(output_type_id, ",") + 1, nchar(output_type_id) - 1)),
    bin_mid = bin_lower + 0.05
  )
