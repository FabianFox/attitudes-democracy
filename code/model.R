# Analysis ----

# Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "hrbrthemes", "fixest", "modelsummary",
                  "conflicted", "lubridate", "here", "Cairo", "Hmisc", "gt", "gtExtras")

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("expand", "tidyr")
conflict_prefer("summarize", "dplyr")

# Fonts
extrafont::loadfonts()

# Functions
# Weighted 95%-conf
# from: https://stackoverflow.com/questions/37973240/how-to-create-a-confidence-interval-for-a-weighted-average-of-areas-under-the-ro
weighted.ttest.ci <- function(x, weights, conf.level = 0.95) {
  require(Hmisc)
  nx <- length(x)
  df <- nx - 1
  vx <- wtd.var(x, weights, normwt = FALSE) ## From Hmisc
  mx <- weighted.mean(x, weights, na.rm = TRUE)
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  alpha <- 1 - conf.level
  cint <- qt(1 - alpha/2, df)
  cint <- tstat + c(-cint, cint)
  cint * stderr
}

# Load data ----
# ------------------------------------------------------------------------------------------------ #
# All respondents
ib22_democ.df <- import(here("data", "ib22_democ.rds"))

# Foreign born
ib22_fborn.df <- import(here("data", "ib22_fborn.rds"))

# Formative years
ib22_fyear.df <- import(here("data", "ib22_fyear.rds"))

# Independent variables: dwf, dpu, dmk, drm, dgg, dme, dra
# Add mean score of independent variables
ib_nest.df <- tibble(
  sample = c("all", "foreign born", "formative years"),
  data = list(ib22_democ.df, ib22_fborn.df, ib22_fyear.df)) %>%
  mutate(data = map(data, ~.x %>%
                      mutate(democ = rowMeans(across(c(dwf, dpu, dmk, drm, dgg)), 
                                              na.rm = FALSE))))

# Create factor variables
ib_nest.df <- ib_nest.df %>%
  mutate(data = map(data, ~.x %>%
                      mutate(
                        muslim = case_when(
                          religion == 2 ~ 1,
                          religion %in% c(1, 3, 4) ~ 0,
                          TRUE ~ NA_integer_),
                        religion = case_when(
                          religion == 1 ~ "Christian",
                          religion == 2 ~ "Muslim",
                          religion == 3 ~ "Other",
                          religion == 4 ~ "No religion",
                          TRUE ~ NA_character_),
                        religion = factor(religion))))

# Description ----
# ------------------------------------------------------------------------------------------------ #
# Mean by natives, first and second generation
mean.df <- ib_nest.df %>%
  filter(sample == "all") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    mig_type = case_when(
      migra == 1 ~ "natives",
      migra != 1 & iso3c != "DEU" ~ "first generation",
      migra != 1 & iso3c == "DEU" ~ "second generation",
      TRUE ~ NA_character_)) %>%
  filter(if_all(c(democ, mig_type, weight), ~!is.na(.)))

# Get mean
mean.tbl <- mean.df %>%
  group_by(mig_type) %>%
  summarise(
    mean = wtd.mean(democ, 
                     weights = weight,
                     na.rm = TRUE),
    conf.low = weighted.ttest.ci(democ, 
                                  weights = weight, 
                                  conf.level = 0.95)[1],
    conf.high = weighted.ttest.ci(democ, 
                                   weights = weight, 
                                   conf.level = 0.95)[2],
    n = n()) %>% 
  ungroup() %>%
  left_join(y = mean.df %>% 
              group_by(mig_type) %>%
              summarize(data = list(democ), .groups = "drop"), by = "mig_type")

# Table 
mean.gt <- mean.tbl %>%
  mutate(
    across(starts_with("conf"), ~format(round(., 2), nsmall = 2)),
    "95%CI" = str_c("[", conf.low, ", ", conf.high, "]"),
    mig_type = factor(mig_type, 
                      levels = c("natives", "first generation", "second generation"),
                      labels = c("Natives", "First Generation", "Second Generation"))) %>%
  select(-starts_with("conf")) %>%
  arrange(mig_type) %>%
  gt() %>%
  gt_plt_dist(data, type = "histogram") %>%
  fmt_number(columns = c("mean", "95%CI"),
             decimals = 2) %>%
  cols_align(
    align = "left",
    columns = "mig_type") %>%
  cols_align_decimal() %>%
  cols_label(
    mig_type = "",
    mean = "Mean",
    `95%CI` = "95%-CI",
    n = "N") %>%
  tab_header(title = md("Importance of democracy")) %>%
  tab_source_note(source_note = md("**Source**: SVR-Integrationsbarometer 2022; weighted")) %>%
  tab_spanner(label = "Index",
              columns = c("mean", "95%CI")) %>%
  tab_spanner(label = "95")  

# FE model ----
# for country-of-origin (dummy)
mod <- fixest::feols(c(dwf, dpu, dmk, drm, dgg, dme, dra, democ) ~ gender + age + I(age^2) + educ +
                       religion*religion_str + v2x_polyarchy + stay_length
                   | iso3c,
                   data = ib_nest.df$data[[2]]) 

### Table ----
mod %>%
  modelsummary(stars = TRUE,
               coef_map =
                 rev(c("gender" = "Gender: Female",
                   "I(age^2)" = "Age^2",
                   "age" = "Age",
                   "educSchüler" = "in school",
                   "educhoch" = "high",
                   "educmittel" = "Education: medium\n(Ref.: low)",
                   "religionNo religion:religion_str" = "no religion × Religiosity",
                   "religionOther:religion_str" = "Other religion × Religiosity",
                   "religionMuslim:religion_str" = "Religion: Muslim\n(Ref.: Christian) × Religiosity",
                   "religionNo religion" = "No religion",
                   "religionOther" = "Other religion",
                   "religionMuslim" = "Religion: Muslim\n(Ref.: Christian)",
                   "religion_str" = "Religiosity",
                   "v2x_polyarchy" = "VDem: Polyarchy",
                   "stay_length" = "Period of residence")))

### Plot ----
fig <- modelplot(mod, colour = "black", 
                 coef_map =
                   c(
                     "gender" = "Gender: Female",
                     "I(age^2)" = "Age^2",
                     "age" = "Age",
                     "educSchüler" = "in school",
                     "educhoch" = "high",
                     "educmittel" = "Education: medium\n(Ref.: low)",
                     "religionNo religion:religion_str" = "no religion × Religiosity",
                     "religionOther:religion_str" = "Other religion × Religiosity",
                     "religionMuslim:religion_str" = "Religion: Muslim\n(Ref.: Christian) × Religiosity",
                     "religionNo religion" = "No religion",
                     "religionOther" = "Other religion",
                     "religionMuslim" = "Religion: Muslim\n(Ref.: Christian)",
                     "religion_str" = "Religiosity",
                     "v2x_polyarchy" = "VDem: Polyarchy",
                     "stay_length" = "Period of residence")) +
  geom_vline(xintercept = 0, color = 'orange') + 
  facet_grid(~factor(model,
                     levels = c("lhs: dwf", "lhs: dpu", "lhs: dmk", "lhs: drm", "lhs: dgg", "lhs: dme",
                                "lhs: dra", "lhs: democ"),
                     labels = c("Elections free and\nfair", "Parties clear\nalternatives", "Media free to\ncriticize",
                                "Minority rights\nprotected", "Courts same treat-\nment", "Balance inequalities\nby taxation",
                                "Protection against\npoverty", "Index"))) +
  labs(title = "Linear regression: Democracy", 
       subtitle = "Only foreign born",
       caption = "Includes dummy variables for country-of-origin (not shown); clustered standard errors") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom", legend.text = element_text(size = 12), 
        legend.justification = "left",
        plot.caption = element_text(hjust = 0, size = 12),
        plot.caption.position = "plot",
        strip.text = element_text(colour = "black", size = 10),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black", linewidth = 0.5),
        axis.ticks.y = element_line(colour = "black", linewidth = 0.5),
        axis.text = element_text(family = "Verdana", size = 11, colour = "black"),
        axis.text.x = element_text(family = "Verdana", size = 11, color = "black"))

# Hierachical model ----
### Rescale weights ----
ib_nest.df <- ib_nest.df %>%
  mutate(data = map(data, ~datawizard::rescale_weights(.x %>% 
                                            filter(if_all(c(iso3c, weight), 
                                                          ~!is.na(.))), 
                                          group = "iso3c", 
                                          probability_weights = "weight")))

## Run models ----
model.df <- tibble(
  dv = "democ",
  type = c("Unconditional",
           "Base", 
           "Family × VDem", 
           "Residence × VDem",
           "Random slope"),
  iv = c("1 + (1 | iso3c)", 
      "gender + age + I(age^2) + stay_length + muslim*religion_str + v2x_polyarchy + (1 | iso3c)",
      "gender + age + I(age^2) + stay_length + muslim*religion_str + fh*v2x_polyarchy + (1 | iso3c)",
      "gender + age + I(age^2) + muslim*religion_str + fh*v2x_polyarchy + stay_length*v2x_polyarchy + (1 | iso3c)",
      "gender + age + I(age^2) + muslim*religion_str + fh*v2x_polyarchy + stay_length*v2x_polyarchy + (1 + v2x_polyarchy | iso3c)"))

# Add different samples
model.df <- tibble(
  sample = c("foreign born", "formative years"),
  data = c("ib_nest.df %>% filter(sample == 'foreign born') %>% pull(data) %>% .[[1]]", 
           "ib_nest.df %>% filter(sample == 'formative years') %>% pull(data) %>% .[[1]] %>% filter(!is.na(wandjahr))")) %>%
  expand_grid(model.df)

# Run models
# VDem at year of immigration
model.df <- model.df %>%
  mutate(model = pmap(list(dv, iv, data), ~lmerTest::lmer(
    str_c(..1, " ~ ", ..2),
    weights = pweights_a,
    data = eval(rlang::parse_expr(..3)))))

# Name list column
names(model.df$model) <- model.df$type

# modelsummary
mlm.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
             model.df$model,
             stars = TRUE,
             coef_map = c("(Intercept)" = "Intercept",
                          "gender" = "Gender: Female",
                          "I(age^2)" = "Age^2",
                          "age" = "Age",
                          "educSchüler" = "in school",
                          "educhoch" = "high",
                          "educmittel" = "Education: medium\n(Ref.: low)",
                          "stay_length" = "Period of residence (years)",
                          "muslim" = "Muslim",
                          "religion_str" = "Religiosity",
                          "v2x_polyarchy" = "Polyarchy (VDem)",
                          "fh" = "Close family (country-of-origin)",
                          "muslim:religion_str" = "Muslim × Religiosity",
                          "fh:v2x_polyarchy" = "Close family × Polyarchy (VDem)",
                          "v2x_polyarchy:fh" = "Close family × Polyarchy (VDem)",
                          "v2x_polyarchy:stay_length" = "Period of residence × Polyarchy (VDem)",
                          "SD (Intercept iso3c)" = "SD (Intercept: Country)",
                          "SD (v2x_polyarchy iso3c)" = "SD (v2x_polyarchy iso3c)",
                          "SD (Observations)" = "SD (Observations)"),
             gof_map = tribble(
               ~raw, ~clean, ~fmt,
               "nobs", "N", 0,
               "r2,marginal", "R2 Marg.", 3,
               "r2.conditional", "R2 Cond.", 3,
               "aic", "AIC", 0,
               "bic", "BIC", 0,
               "icc", "ICC", 2,
               "rmse", "RMSE", 2)) %>%
  tab_spanner(label = md("**VDem at year of immigration**"), columns = 2:6) %>%
  tab_spanner(label = md("**VDem at age 14**"), columns = 7:11) %>%
  tab_footnote(footnote = md("**Source**: SVR-Integrationsbarometer 2022; weighted"))

# Plot random effects
# Using ggeffects
adj.predictions <- ggeffects::ggpredict(model = model.df$model[[10]], 
                                        terms = c("v2x_polyarchy [all]", "iso3c"),
                                        type = "random")

# Using marginaleffects
require(marginaleffects) 
# Predictions
unit.predictions <- predictions(
  model.df$model[[10]], 
  newdata = datagrid(v2x_polyarchy = seq(0.,1, .25), iso3c = unique),
  re_formula = NULL,
  vcov = "satterthwaite")

# Marginal effects
unit.slope <- slopes(
  model.df$model[[10]], 
  vcov = "satterthwaite")

# Export
ggsave(here("figure", "model_foreignborn.pdf"), dpi = 300, device = cairo_pdf, 
       width = 34, height = 24, units = "cm")

# Mean democ index
gtsave(mean.tbl, filename = "./figure/mean_democ.png")

# MLM results
gtsave(mlm.tbl, filename = "./figure/MLM_results.png")
gtsave(mlm.tbl, filename = "./figure/MLM_results.png")
