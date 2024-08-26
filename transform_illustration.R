library(tidyverse)

forecasts <- readr::read_csv("flusight-network-hub/model-output/LANL_DBMplus/EW201704-LANL_DBMplus.csv")


pmf_forecasts <- forecasts |>
  dplyr::group_by(location, reference_date, horizon) |>
  dplyr::mutate(
    bin_lower = as.numeric(substr(output_type_id, 2, str_locate(output_type_id, ",") - 1)),
    bin_upper = as.numeric(substr(output_type_id, str_locate(output_type_id, ",") + 1, nchar(output_type_id) - 1)),
    bin_mid = bin_lower + 0.05,
    cum_prob = cumsum(value)
  )

ggplot() +
  geom_col(
    data = pmf_forecasts |> filter(location == "US National", horizon == 3),
    mapping = aes(x = bin_mid, y = cum_prob),
    color = "darkblue",
    fill = "darkblue"
  ) +
  geom_col(
    data = pmf_forecasts |> filter(location == "US National", horizon == 3),
    mapping = aes(x = bin_mid, y = value),
    color = "cornflowerblue",
    fill = "cornflowerblue"
  ) +
  geom_vline(
    data = forecasts |> filter(output_type == "quantile", location == "US National", horizon == 3),
    mapping = aes(xintercept = value),
    color = "orange",
    linetype = 2
  ) +
  xlab("wILI percent") +
  ylab("Predicted probability") +
  theme_bw()
