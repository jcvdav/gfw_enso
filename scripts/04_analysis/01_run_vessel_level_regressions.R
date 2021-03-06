
library(modelsummary)
library(gt)
library(fixest)
library(furrr)
library(tidyverse)

hyp_sine <- function(x) {
  log(x + sqrt(((x ^ 2) + 1)))
  }

panel <- readRDS(file.path(project_path, "processed_data", "vessel_foreign_fishing_panel.rds")) %>% 
  mutate_at(vars(contains("hours")), hyp_sine)


dep_vars <- c("foreign_fishing_hours", "foreign_fishing_hours_share", "foreign_fishing_kilowatthours", "foreign_fishing_kilowatthours_share")
extra_dep_vars <- c("total_fishing_hours", "total_fishing_kilowatthours")

indep_vars <- c("nino34anom | ssvid + month",
                "nino34anom | ssvid + month + year",
                "nino34anom | ssvid + month + year + flag")


my_lm <- function(formula, data){
  formula <- as.formula(formula)
  feols(formula, data) %>% 
    summary(cluster = c("ssvid"))
}


ps_panel <- panel %>% 
  filter(gear == "tuna_purse_seines")

ll_panel <- panel %>% 
  filter(gear == "drifting_longlines")

ps_results <- expand_grid(dep_vars, indep_vars) %>% 
  mutate(formula = paste(dep_vars, indep_vars, sep = " ~ ")) %>% 
  mutate(model = purrr::map(formula, my_lm, data = ps_panel))

ll_results <- expand_grid(dep_vars, indep_vars) %>% 
  mutate(formula = paste(dep_vars, indep_vars, sep = " ~ ")) %>% 
  mutate(model = future_map(formula, my_lm, data = ll_panel))

make_table <- function(models, dep_var, title, filename) {
  
  dep_var1 <- dep_var2 <- dep_var
  dep_var2 <- paste("Share", tolower(dep_var2))
  

  if(length(dep_var) > 1){
    dep_var1 <- dep_var[1]
    dep_var2 <- dep_var[2]
  }
  

  models %>% 
    modelsummary(title = title,
                 coef_omit = "year|month|ssvid|flag",
                 stars = T,
                 statistic = "std.error",
                 output = "gt",
                 gof_omit = "Adj|Pse|With|AIC|BIC|Log",
                 coef_map = c("(Intercept)" = "Constant", "nino34anom" = "nino34anom")) %>% 
    tab_spanner(label = dep_var1, columns = 2:4) %>%
    tab_spanner(label = dep_var2, columns = 5:7) %>%
    tab_style(style = cell_text(color = 'red'),
              locations = cells_body(rows = 1)) %>% 
    gtsave(filename = here::here("results", "tab", filename))
}

# Purse seine hours table
ps_results %>% 
  filter(!str_detect(dep_vars, "kilo")) %>% 
  pull(model) %>% 
  make_table(dep_var = "Foreign fishing hours",
             title = "Purse Seines hours",
             filename = "vessel_level_purse_seines_regressions_hours.html")

# Purse seines energy table
ps_results %>% 
  filter(str_detect(dep_vars, "kilo")) %>% 
  pull(model) %>% 
  make_table(dep_var = "Foreign fishing kilowatthours",
             title = "Purse Seines Kilowatt-hours",
             filename = "vessel_level_purse_seines_regressions_kilowatthours.html")


# Longlines
# Hours

ll_results %>% 
  filter(!str_detect(dep_vars, "kilo")) %>% 
  pull(model) %>% 
  make_table(dep_var = "Foreign fishing hours",
             title = "Longlines hours",
             filename = "vessel_level_longlines_regressions_hours.html")


# Kilowatthours

ll_results %>% 
  filter(str_detect(dep_vars, "kilo")) %>% 
  pull(model) %>% 
  make_table(dep_var = "Foreign fishing kilowatthours",
             title = "Longlines Kilowatt-hours",
             filename = "vessel_level_longlines_regressions_kilowatthours.html")


########### TOTAL FISHING HOURS

ps_totals <- expand_grid(extra_dep_vars, indep_vars) %>% 
  mutate(formula = paste(dep_vars, indep_vars, sep = " ~ ")) %>% 
  mutate(model = purrr::map(formula, my_lm, data = ps_panel))

ll_totals <- expand_grid(extra_dep_vars, indep_vars) %>% 
  mutate(formula = paste(dep_vars, indep_vars, sep = " ~ ")) %>% 
  mutate(model = purrr::map(formula, my_lm, data = ll_panel))
  

## Make tables
ps_totals %>% 
  pull(model) %>% 
  make_table(dep_var = c("Hours", "Kilowatt-hours"),
             title = "Purse Seine total hour and kWh",
             "vessel_level_purse_seines_regressions_totals.html")

ll_totals %>% 
  pull(model) %>% 
  make_table(dep_var = c("Hours", "Kilowatt-hours"),
             title = "Longline total hour and kWh",
             "vessel_level_longline_regressions_totals.html")


















