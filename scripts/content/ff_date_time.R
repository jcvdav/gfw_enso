model_data_ps <- readRDS(file = here("data", "model_data_ps.rds")) %>% 
  filter(top_countries) %>% 
  filter(year > 2012) %>% 
  mutate(treated = treated == 1) %>% 
  drop_na()

# Load indices
all_indices <- read.csv(here("data","all_indices.csv"),
                        stringsAsFactors = F) %>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall"))

# Keep nino34 index after 2002, and only year-month columns
nino34test <- all_indices %>% 
  filter(year > 2002) %>% 
  select(year, month = month_n, nino34anom) %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "-")))

date_hours <- ggplot(model_data_ps, aes(x = date, y = hours, color = treated, group = treated)) +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(x = "Date", y = "FF") +
  ggtheme_plot() +
  theme(legend.position = "None")

nino_hours <- ggplot(model_data_ps, aes(x = nino34anom, y = hours, color = treated, group = treated)) +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(x = "NINO 3.4 Anomaly", y = "FF") +
  ggtheme_plot() +
  theme(legend.position = "None")

date_pct <- ggplot(model_data_ps, aes(x = date, y = hours_pct, color = treated, group = treated)) +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(x = "Date", y = "FF / TF") +
  ggtheme_plot() +
  theme(legend.position = "None")

nino_pct <- ggplot(model_data_ps, aes(x = nino34anom, y = hours_pct, color = treated, group = treated)) +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(x = "NINO 3.4 Anomaly", y = "FF / TF") +
  ggtheme_plot() +
  theme(legend.position = "None")

nino34 <- ggplot(data = nino34test) +
  geom_rect(aes(xmin = date("2012-01-01"),
                xmax = date("2018-12-01"),
                ymin = -Inf, ymax = Inf),
            fill = "lightgray") +
  geom_line(aes(x = date,
                y = nino34anom,
                color = nino34anom)) +
  scale_color_gradientn(colours = colorRamps::matlab.like(10)) +
  ggtheme_plot() +
  guides(color = guide_colorbar(title = "NINO3.4\nAnomaly",
                                frame.colour = "black",
                                ticks.colour = "black")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "NINO3.4\nAnomaly")

hours <- plot_grid(date_hours, nino_hours, date_pct, nino_pct, ncol = 2, labels = "AUTO")

p <- plot_grid(hours, nino34, ncol = 1, rel_heights = c(2, 1), labels = c("", "E"))

ggsave(plot = p,
       filename = here::here("writing", "img", "ff_date_time.png"),
       width = 4,
       height = 5)
