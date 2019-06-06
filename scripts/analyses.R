# Load packages
library(startR)
library(here)
library(tidyverse)

# Load data
model_data_ps <- readRDS(file = here("data", "model_data_ps.rds")) %>% 
  filter(top_countries) %>% 
  filter(year > 2012) %>% 
  mutate(treated = treated == 1) %>% 
  drop_na()

model_data_ll <- readRDS(file = here("data", "model_data_ll.rds")) %>% 
  filter(top_countries) %>% 
  filter(year > 2012) %>% 
  mutate(treated = treated == 1) %>% 
  drop_na()

ggplot(model_data_ps, aes(x = nino34anom, y = hours_pct, color = treated, group = treated)) +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1")

# Run the regressions here



models <- list(ps1, ps2, ll1, ll2)

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
