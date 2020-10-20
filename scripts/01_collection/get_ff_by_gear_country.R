##############################
##  get_ff_by_gear_country  ##
##############################


library(startR)

ff_by_gear_country <- get_table(project = "ucsb-gfw",
                                dataset = "enso_gfw",
                                table = "ff_by_gear_country")

saveRDS(object = ff_by_gear_country,
        file = here("raw_data", "ff_by_gear_country.rds"))
