library(tidyverse)
library(MMWRweek)

mo_files <- list.files(
  "flusight-network-hub/model-output", "*/*.csv",
  recursive = TRUE,
  full.names = TRUE
)

for (mo_file in mo_files) {
  df <- read.csv(mo_file)
  df <- df |>
    select(-X) |>
    mutate(
      output_type = ifelse(is.na(output_type), "quantile", output_type)
    )
  write.csv(df, mo_file, row.names = FALSE)
}

all_ews <- substr(basename(mo_files), 3, 8)
all_ews <- sort(unique(all_ews))
ew_y <- substr(all_ews, 1, 4)
ew_w <- substr(all_ews, 5, 6)

ref_dates <- MMWRweek2Date(as.integer(ew_y), as.integer(ew_w))
cat(as.character(ref_dates), sep = '\", \"')

target_dates <- lapply(ref_dates, function(d) { as.character(d + 7 * (1:4)) }) |>
  unlist() |>
  unique() |>
  sort()

cat(as.character(target_dates), sep = '\", \"')


bin_lowers <- seq(from = 0, to = 12.9, by = 0.1)
output_type_id = c(
  paste0("[", bin_lowers, ",", bin_lowers + 0.1, ")"),
  "[13,100)"
)
cat(output_type_id, sep = '\", \"')

