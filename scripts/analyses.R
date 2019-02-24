# Load packages
library(startR)
library(raster)
library(here)
library(tidyverse)

# Analyses here
sst_nino34_cor_df <- readRDS(here::here("data", "sst_nino34_cor_df.rds"))

treatment_regions <- sst_nino34_cor_df %>% 
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

treatment_3 <- treatment_regions %>% 
  filter(months == 3) %>% 
  select(longitude, latitude, tele_binary) %>% 
  rasterFromXYZ() %>% 
  disaggregate(fact = 10) %>% 
  as.data.frame(xy = T) %>% 
  magrittr::set_colnames(value = c("longitude", "latitude", "tele_binary")) %>% 
  as_tibble() %>% 
  mutate(latitude = round(latitude, digits = 2),
         longitude = round(longitude, digits = 2))

# Read the gridded effort data (see scripts/download_gridded_ff_by_gear_country)
gridded_ff <- readRDS(file = here("raw_data", "gridded_ff_by_gear_country.rds"))

# nino34 index
nino34 <- read.csv(here::here("data","all_indices.csv"),
                  stringsAsFactors = F)%>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall")) %>% 
  filter(year > 1950) %>% 
  mutate(date = lubridate::date(date)) %>% 
  select(year, month = month_n, date, nino34anom)

model_data <- gridded_ff %>% 
  filter(is_foreign,
         hours > 0) %>% # Must filter out h = 0 because log(0) = !
  left_join(nino34, by = c("year", "month", "date")) %>% 
  left_join(treatment_3, by = c("longitude", "latitude")) %>% 
  rename(treated = tele_binary) %>% 
  mutate(treated = treated * 1,
         log_hours = log(hours)) %>% #log transformation
  mutate(month = as.factor(month))

model_data2 <- gridded_ff %>% 
  filter(is_foreign,
         hours > 0) %>%
  left_join(nino34, by = c("year", "month", "date")) %>% 
  left_join(treatment_3, by = c("longitude", "latitude")) %>% 
  rename(treated = tele_binary) %>% 
  mutate(treated = treated * 1,
         hours2 = log(hours + sqrt(1 + hours ^ 2))) %>% #Hyperbolic transformation
  mutate(month = as.factor(month))

# Run the regressions
model1 <- lm(log_hours ~ nino34anom * treated, data = model_data)
model2 <- lm(log_hours ~ nino34anom * treated + best_label, data = model_data)
model3 <- lm(log_hours ~ nino34anom * treated + best_label + month, data = model_data)
model4 <- lm(log_hours ~ nino34anom * treated + best_label + month + iso3, data = model_data)

model5 <- lm(hours2 ~ nino34anom * treated, data = model_data2)
model6 <- lm(hours2 ~ nino34anom * treated + best_label, data = model_data2)
model7 <- lm(hours2 ~ nino34anom * treated + best_label + month, data = model_data2)
model8 <- lm(hours2 ~ nino34anom * treated + best_label + month + iso3, data = model_data2)


models <- list(model1, model2, model3, model4, model5, model5, model7, model8)

# Stargazer table
stargazer::stargazer(models,
                     se = commarobust::makerobustseslist(models),
                     t.auto = T,
                     se.auto = T,
                     type = "latex",
                     omit = c("best", "iso3", "month"),
                     add.lines = list(c("Gear FE", "No", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes"),
                                      c("Month FE", "No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes"),
                                      c("Country FE", "No", "No", "No", "Yes", "No", "No", "No", "Yes")),
                     omit.stat = c("adj.rsq", "f", "ser"),
                     header = F,
                     title = "\\label{tab:ff_reg}Foreign fishing hours and nino3",
                     out = here("writing", "tab", "DID.tex"),
                     font.size = "small")

# Coefficient estimates for the models ran above. Graphs on the left show estimates after the hyperbolic sine transformation of hours. Right side show no transformation of hours. Model numbers (x - axis) correspond to the columns in table 1 (1 - 4) and table 2 (5 - 8).

pd <- position_dodge(width = 0.5)

p <- purrr::map_df(models, broom::tidy, .id = "Model") %>%
  filter(term == "nino34anom:treated") %>%
  mutate(class = ifelse(Model > 4, "Log-transformed", "Hyperbolic Sine"),
         Model2 = case_when(Model %in% c(1, 5) ~ "base",
                           Model %in% c(2, 6) ~ "+ gear",
                           Model %in% c(3, 7) ~ "+ month",
                           T ~ "+ flag"),
         Model2 = fct_relevel(Model2, c("base", "+ gear", "+ month", "+ flag"))) %>% 
  ggplot(aes(x = Model2, y = estimate, group = Model, color = class, fill = class)) +
  cowplot::theme_cowplot() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                width = 0.2,
                position = pd) +
  geom_point(size = 4,
             shape = 21,
             position = pd,
             color = "black") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1),
        text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  labs(x = "Model specification", y = "Coefficient estimate") +
  guides(color = guide_legend(title = "Outcome Variable"),
         fill = guide_legend(title = "Outcome Variable"))

ggsave(plot = p,
       filename = here("writing", "img", "coef_estimates.pdf"),
       width = 6,
       height = 4)
