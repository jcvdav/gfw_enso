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

model_data_ps <- gridded_ff %>% 
  filter(foreign,
         best_label == "tuna_purse_seines") %>%
  left_join(nino34, by = c("year", "month", "date")) %>% 
  left_join(treatment_3, by = c("longitude", "latitude")) %>% 
  rename(treated = tele_binary) %>% 
  mutate(treated = treated * 1,
         hours2 = log(hours + sqrt(1 + hours ^ 2))) %>% #Hyperbolic transformation
  mutate(month = as.factor(month))

model_data_ll <- gridded_ff %>% 
  filter(foreign,
         !best_label == "tuna_purse_seines") %>%
  left_join(nino34, by = c("year", "month", "date")) %>% 
  left_join(treatment_3, by = c("longitude", "latitude")) %>% 
  rename(treated = tele_binary) %>% 
  mutate(treated = treated * 1,
         hours2 = log(hours + sqrt(1 + hours ^ 2))) %>% #Hyperbolic transformation
  mutate(month = as.factor(month))

# Run the regressions
ps1 <- lm(hours2 ~ nino34anom * treated, data = model_data_ps)
ps2 <- lm(hours2 ~ nino34anom * treated + month, data = model_data_ps)
ps3 <- lm(hours2 ~ nino34anom * treated + month + best_flag, data = model_data_ps)

# Run the regressions
ll1 <- lm(hours2 ~ nino34anom * treated, data = model_data_ll)
ll2 <- lm(hours2 ~ nino34anom * treated + month, data = model_data_ll)
ll3 <- lm(hours2 ~ nino34anom * treated + month + best_flag, data = model_data_ll)


models <- list(ps1, ps2, ps3, ll1, ll2, ll3)

# Stargazer table
stargazer::stargazer(models, 
                     se = commarobust::makerobustseslist(models),
                     dep.var.labels = "Monthly foreign fishing hours",
                     t.auto = T,
                     se.auto = T,
                     type = "latex",
                     omit = c("flag", "month"),
                     add.lines = list(c("Month FE", rep(c("No", "Yes", "Yes"), 2)),
                                      c("Country FE", rep(c("No", "No", "Yes"), 2))),
                     omit.stat = c("adj.rsq", "f", "ser"),
                     header = F,
                     title = "\\label{tab:ff_reg}Effect of NINO3.4 anomally index on foreign fishing for purse seiners (1-3) and longliners(4-6).",
                     out = here("writing", "tab", "DID.tex"))

# Coefficient estimates for the models ran above. Graphs on the left show estimates after the hyperbolic sine transformation of hours. Right side show no transformation of hours. Model numbers (x - axis) correspond to the columns in table 1 (1 - 4) and table 2 (5 - 8).

pd <- position_dodge(width = 0.5)

p <- purrr::map_df(models, commarobust::commarobust_tidy, .id = "Model") %>%
  filter(term == "nino34anom:treated") %>%
  mutate(class = ifelse(Model < 4, "Purse Seines", "Longlines"),
         Model2 = case_when(Model %in% c(1, 4) ~ "base",
                            Model %in% c(2, 5) ~ " + month",
                            T ~ "+ flag"),
         Model2 = fct_relevel(Model2, c("base", "+ month", "+ flag"))) %>% 
  ggplot(aes(x = Model2, y = est, group = Model, color = class, fill = class)) +
  cowplot::theme_cowplot() +
  geom_errorbar(aes(ymin = est - se, ymax = est + se),
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
  guides(color = guide_legend(title = "Gear"),
         fill = guide_legend(title = "Gear")) +
  scale_y_continuous(limits = c(0, 0.1))

ggsave(plot = p,
       filename = here("writing", "img", "coef_estimates.pdf"),
       width = 6,
       height = 4)
