# Load packages
suppressPackageStartupMessages({
  library(startR)
  library(here)
  library(tidyverse)
})

# Read the coastline
world_coastline <- rnaturalearth::ne_countries(returnclass = "sf")

# Analyses here
sst_df <- readRDS(here::here("data", "sst_nino3_df.rds"))

treatment_regions <- sst_df %>% 
  group_by(longitude, latitude) %>% 
  summarize(tele = sum(tele)) %>% 
  ungroup() %>% 
  mutate(tele_1 = tele >= 1,
         tele_2 = tele >= 2,
         tele_3 = tele >= 3,
         tele_4 = tele >= 4,
         tele_5 = tele >= 5,
         tele_6 = tele >= 6,
         tele_7 = tele >= 7,
         tele_8 = tele >= 8,
         tele_9 = tele >= 9,
         tele_10 = tele >= 10,
         tele_11 = tele >= 11,
         tele_12 = tele >= 12) %>% 
  select(-tele) %>% 
  gather(months, tele_binary, -c(longitude, latitude)) %>% 
  mutate(months = as.numeric(str_extract(string = months, pattern = "\\d+")))

# ENSO teleconnection depending on number of months. Number above figures indicate the minimum number of months for which a particular parcel was correlated to nino3 (red). For example, the panel 6 indicates that all red regions where SST showed a positive ( r > 0) and significant (p < 0.1) correlation with nino3 index for at least 6 months.
p <- ggplot(treatment_regions) +
  ggtheme_map() +
  geom_sf(data = world_coastline, fill = "grey96", color = "grey40", size = .10) +
  geom_raster(aes(x = longitude, y = latitude, fill = as.factor(tele_binary*1))) +
  facet_wrap(~months, ncol = 3) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  theme(legend.position = "none")

ggsave(plot = p, filename = here("img", "cor_sst_nino3_var.pdf"), width = 6, height = 5)

treatment_3 <- treatment_regions %>% 
  filter(months == 3) %>% 
  select(longitude, latitude, tele_binary) %>% 
  rasterFromXYZ() %>% 
  disaggregate(fact = 10) %>% 
  as.data.frame(xy = T) %>% 
  magrittr::set_colnames(value = c("longitude", "latitude", "tele_binary")) %>% 
  as.tibble() %>% 
  mutate(latitude = round(latitude, digits = 2),
         longitude = round(longitude, digits = 2))

# Read the gridded effort data (see scripts/download_gridded_ff_by_gear_country)
gridded_ff <- readRDS(file = here("raw_data", "gridded_ff_by_gear_country.rds"))

model_data <- gridded_ff %>% 
  filter(is_foreign) %>%
  left_join(nino3, by = c("year", "month", "date")) %>% 
  left_join(treatment_3, by = c("longitude", "latitude")) %>% 
  rename(treated = tele_binary) %>% 
  mutate(treated = treated * 1,
         hours2 = log(hours + sqrt(1 + hours ^ 2))) %>% #Hyperbolic transformation
  mutate(month = as.factor(month))


# Run the regressions
model1 <- lm(hours ~ nino3anom * treated, data = model_data)
model2 <- lm(hours ~ nino3anom * treated + best_label, data = model_data)
model3 <- lm(hours ~ nino3anom * treated + best_label + month, data = model_data)
model4 <- lm(hours ~ nino3anom * treated + best_label + month + iso3, data = model_data)

model5 <- lm(hours2 ~ nino3anom * treated, data = model_data)
model6 <- lm(hours2 ~ nino3anom * treated + best_label, data = model_data)
model7 <- lm(hours2 ~ nino3anom * treated + best_label + month, data = model_data)
model8 <- lm(hours2 ~ nino3anom * treated + best_label + month + iso3, data = model_data)

models <- list(model1, model2, model3, model4, model5, model5, model7, model8)

# Stargazer table
stargazer::stargazer(models,
                     se = commarobust::makerobustseslist(models),
                     type = "latex",
                     omit = c("best", "iso3", "month"),
                     add.lines = list(c("Gear FE", "No", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes"),
                                      c("Month FE", "No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes"),
                                      c("Country FE", "No", "No", "No", "Yes", "No", "No", "No", "Yes")),
                     omit.stat = c("adj.rsq", "f", "ser"),
                     header = F,
                     title = "\\label{tab:ff_reg}Foreign fishing hours and nino3",
                     out = here("writing", "DID.tex"),
                     font.size = "small")

# Coefficient estimates for the models ran above. Graphs on the left show estimates after the hyperbolic sine transformation of hours. Right side show no transformation of hours. Model numbers (x - axis) correspond to the columns in table 1 (1 - 4) and table 2 (5 - 8).
p <- purrr::map_df(models, broom::tidy, .id = "Model") %>%
  filter(term %in% c("(Intercept)", "nino3anom", "treated", "nino3anom:treated")) %>%
  mutate(class = ifelse(Model > 4, "Linear", "Hyperbolic Sine")) %>% 
  ggplot(aes(x = Model, y = estimate, group = Model)) +
  ggtheme_plot() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1, size = 1, color = "red") +
  geom_point(size = 2, shape = 21, fill = "steelblue") +
  facet_wrap(term~class, scales = "free", ncol = 2)

ggsave(plot = p, filename = here("img", "coef_estimates.pdf"), width = 6, height = 8)
