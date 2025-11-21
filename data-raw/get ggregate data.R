

#Load in file created in cycle repository

load("C:/Users/tpritch/OneDrive - Oregon/R Projects/IR_2026/Validation/2026_aggregate_data.Rdata")


aggregate_data <- aggregate_data |>
  dplyr::mutate(Result_UID = as.numeric(Result_UID))

#Save in package
usethis::use_data(aggregate_data,overwrite = TRUE)
