

#Load in file created in cycle repository

aggregate_data <- load("C:/Users/tpritch/OneDrive - Oregon/R Projects/IR_2026/Validation/2026_aggregate_data.Rdata")


#Save in package
usethis::use_data(aggregate_data,overwrite = TRUE)
