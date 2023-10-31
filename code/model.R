# Analysis ----

# Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "hrbrthemes", "fixest", "modelsummary", "marginaleffects",
                  "conflicted", "lubridate", "here", "Cairo", "Hmisc", "gt", "gtExtras")

conflicts_prefer(dplyr::filter(),
                 dplyr::select(),
                 dplyr::summarize(),
                 tidyr::expand())

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
  sample = c("total", "foreign born", "formative years"),
  data = list(ib22_democ.df, ib22_fborn.df, ib22_fyear.df)) %>%
  mutate(data = map(data, ~.x %>%
                      mutate(democ = rowMeans(across(c(dwf, dpu, dmk, drm, dgg)), 
                                              na.rm = FALSE))))

#### Create factor variables ----
ib_nest.df <- ib_nest.df %>%
  mutate(data = map(data, ~.x %>%
                      mutate(
                        muslim = case_match(
                          religion,
                          2 ~ 1,
                          c(1, 3, 4) ~ 0,
                          TRUE ~ NA_integer_),
                        muslim = factor(muslim, levels = c(0, 1), 
                                        labels = c("Other", "Muslim")),
                        religion = case_match(
                          religion,
                          1 ~ "Christian",
                          2 ~ "Muslim",
                          3 ~ "Other",
                          4 ~ "No religion",
                          TRUE ~ NA_character_),
                        religion = factor(religion,
                                          levels = c("Christian", "Muslim", 
                                                     "Other", "No religion")),
                        discrimination = case_match(
                          discrimination,
                          0 ~ "not at all",
                          1 ~ "low",
                          2 ~ "high",
                          3 ~ "very high",
                          TRUE ~ NA_character_),
                        discrimination = factor(discrimination, 
                                                levels = c("not at all", "low", 
                                                           "high", "very high")),
                        gender = factor(gender, 
                                            levels = c(0, 1), 
                                            labels = c("Male", "Female")),
                        fh = factor(fh, 
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")))))

# Description ----
# ------------------------------------------------------------------------------------------------ #
# Mean by natives, first and second generation
mean.df <- ib_nest.df %>%
  filter(sample == "total") %>%
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
mod <- fixest::feols(c(dwf, dpu, dmk, drm, dgg, dme, dra, democ) ~ gender + educ + age + I(age^2) +
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
                   "age_mig" = "Age at immigration",
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
                     "age_mig" = "Age at immigration",
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
           "Interactions",
           "Random slope"),
  iv = c(
    "1 + (1 | iso3c)", 
    "gender + age + I(age^2) + educ + stay_length + muslim*religion_str + v2x_polyarchy + discrimination + (1 | iso3c)",
    "gender + age + I(age^2) + educ + stay_length + muslim*religion_str + stay_length*v2x_polyarchy + discrimination + (1 | iso3c)",
    "gender + age + I(age^2) + educ + muslim*religion_str + stay_length*v2x_polyarchy + discrimination + (1 + v2x_polyarchy | iso3c)"))

# Add different samples
model.df <- tibble(
  sample = c("foreign born", "formative years"),
  data = c("ib_nest.df %>% filter(sample == 'foreign born') %>% pull(data) %>% .[[1]]", 
           "ib_nest.df %>% filter(sample == 'formative years') %>% pull(data) %>% .[[1]] %>% filter(!is.na(wandjahr))")) %>%
  expand_grid(model.df)

# Run models
# VDem at year of immigration
model.df <- model.df %>%
  mutate(model = pmap(list(dv, iv, data), ~lme4::lmer(
    str_c(..1, " ~ ", ..2),
    weights = pweights_a,
    data = eval(rlang::parse_expr(..3)))))

# Name list column
names(model.df$model) <- model.df$type

# modelsummary
mlm.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
             model.df[model.df$sample != "foreign born",]$model,
             stars = TRUE,
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             coef_map = c("(Intercept)" = "Intercept",
                          "genderFemale" = "Gender: Female",
                          "age" = "Age",
                          "I(age^2)" = "Age^2",
                          "educmittel" = "Education: medium\n(Ref.: low)",
                          "educhoch" = "Educ.: high",
                          "educSchüler" = "Educ.: in school",
                          "stay_length" = "Period of residence (in years)",
                          "muslimMuslim" = "Muslim",
                          "religion_str" = "Religiosity",
                          "discriminationlow" = "Discrimination: low\n(Ref.: no at all)",
                          "discriminationhigh" = "Dis.: high",
                          "discriminationvery high" = "Dis.: very high",
                          "v2x_polyarchy" = "Polyarchy (VDem)",
                          "muslimMuslim:religion_str" = "Muslim × Religiosity",
                          "fhYes" = "Close family (country-of-origin)",
                          "fhYes:v2x_polyarchy" = "Close family × Polyarchy (VDem)",
                          "v2x_polyarchy:fh" = "Close family × Polyarchy (VDem)",
                          "stay_length:v2x_polyarchy" = "Period of residence × Polyarchy (VDem)",
                          "v2x_polyarchy:stay_length" = "Period of residence × Polyarchy (VDem)",
                          "v2x_polyarchy:discriminationlow" = "Discrimination (low) × Polyarchy (VDem)", 
                          "v2x_polyarchy:discriminationhigh" = "Discrimination (high) × Polyarchy (VDem)", 
                          "v2x_polyarchy:discriminationvery high" = "Discrimination (very high) × Polyarchy (VDem)", 
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
  tab_spanner(label = md("**VDem at age 14**"), columns = 2:5) %>%
# tab_spanner(label = md("**VDem at year of immigration**"), columns = 8:12) %>%
  tab_footnote(footnote = md("**Source**: SVR-Integrationsbarometer 2022; weighted"))

# Plot random effects
# Using ggeffects (analogous to marginaleffects::predictions, see below)
# Residence period × VDem
residence.pred <- ggeffects::ggpredict(model = model.df %>%
                                         filter(
                                           sample == "formative years" & 
                                             type == "Random slope") %>%
                                         pull(model) %>% 
                                         .[[1]], 
                                        terms = c("stay_length [all]", 
                                                  "v2x_polyarchy [0:1 by = 0.25]"),
                                       type = "fe") 

# Plot
residence.fig <- plot(residence.pred, facets = TRUE, colors = "bw") + 
  scale_x_continuous(breaks = seq(0, 90, 10), labels = seq(0, 90, 10)) +
  labs(title = "Predicted democratic attitudes by residence period and VDem", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0))

# Quantity of interest
residence.qt <- marginaleffects::avg_predictions(model = model.df %>%
                  filter(
                    sample == "formative years" & 
                      type == "Random slope") %>%
                  pull(model) %>% 
                  .[[1]],
                variables = list(v2x_polyarchy = "minmax", stay_length = "minmax"),
                re.form = NULL)

# Plot
{residence.qt.fig <- residence.qt %>%
  ggplot(aes(x = factor(stay_length, 
                        levels = c(0, 72), 
                        labels = c("Min: 0 years", "Max: 72 years")),
             y = estimate, 
             ymin = conf.low, ymax = conf.high)) +
  geom_point(stat = "identity", position = position_dodge(width = 0.9), size = 3) +
  geom_linerange(stat = "identity", position = position_dodge(width = 0.9)) +
  coord_flip() +
  facet_wrap(~factor(v2x_polyarchy, 
                     levels = c(0.013, 0.915), 
                     labels = c("VDem (min)", "VDem (max)"))) +
  labs(title = "Predicted democratic attitudes at residence period and VDem (min/max)", 
       subtitle = "Holding covariates constant (mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "", shape = "") +
  cowplot::theme_minimal_grid() +
  theme(plot.caption = element_text(hjust = 0))}
  
# Discrimination × VDem
discrimination.pred <- ggeffects::ggpredict(model = model.df %>%
                                              filter(
                                                sample == "formative years" & 
                                                  type == "Random slope") %>%
                                              pull(model) %>% 
                                              .[[1]], 
                                        terms = c("v2x_polyarchy [all]",
                                                  "discrimination [all]"),
                                        type = "fe") 

# Plot 
discrimination.fig <- plot(discrimination.pred, facets = T, colors = "bw") + 
  geom_line(linetype = "dashed") +
  labs(title = "Predicted democratic attitudes by perceived discrimination and VDem", 
       subtitle = "Holding covariates constant (mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "") +
  facet_wrap(~fct_relabel(group, ~str_c("Discrimination: ", .x))) +
  scale_y_continuous(breaks = seq(2.4, 3, .2), limits = c(2.3, 3)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0))

# Using marginaleffects
# Predictions
# see: https://marginaleffects.com/articles/lme4.html
# re.form if NULL include all random effects, if NA include no random effects
# Unit level
unit.predictions <- predictions(
  model.df %>% 
    filter(sample == "formative years" & type == "Random slope") %>%
    pull(model) %>%
    .[[1]], 
  newdata = datagrid(v2x_polyarchy = seq(0., 1, .25), iso3c = unique),
  re.form = NULL)

# Population level
pop.predictions <- predictions(
  model.df %>% 
    filter(sample == "formative years" & type == "Random slope") %>%
    pull(model) %>%
    .[[1]], 
  newdata = datagrid(v2x_polyarchy = seq(0, 1, .25), stay_length = 0:70),
  re.form = NA)

# Marginal effects
avg_margins <- avg_slopes(model.df %>% 
                            filter(sample == "formative years" & type == "Random slope") %>%
                            pull(model) %>%
                            .[[1]], 
                          variable = "v2x_polyarchy",
                          re.form = NULL)

# Total sample ----
total_sample.df <- ib_nest.df %>% 
  filter(sample == 'total') %>% 
  pull(data) %>% 
  .[[1]] %>%
  mutate(mig_type = case_when(
    migra == 1 ~ "natives",
    migra == 2 & iso3c != "DEU" ~ "(Resettlers) first generation",
    migra == 2 & iso3c == "DEU" ~ "(Resettlers) second generation",
    migra == 3 & iso3c != "DEU" ~ "(Turkish) first generation",
    migra == 3 & iso3c == "DEU" ~ "(Turkish) second generation",
    migra == 4 & iso3c != "DEU" ~ "(European) first generation",
    migra == 4 & iso3c == "DEU" ~ "(European) second generation",
    migra == 5 & iso3c != "DEU" ~ "(Other) first generation",
    migra == 5 & iso3c == "DEU" ~ "(Other) second generation",
    .default = NA_character_),
    mig_type = factor(mig_type, levels = c("natives", 
                                           "(Resettlers) first generation",
                                           "(Resettlers) second generation",
                                           "(Turkish) first generation",
                                           "(Turkish) second generation",
                                           "(European) first generation",
                                           "(European) second generation",
                                           "(Other) first generation",
                                           "(Other) second generation")))

# Model
full_sample.mod <- lm(democ ~ gender + age + I(age^2) + educ + muslim*religion_str + 
                        mig_type + discrimination, 
                      weights = weight,
                      data = total_sample.df)

# Modelsummary
full_sample_result.tbl <- modelsummary(title = md("**Linear Regression Model for Importance of Democracy**"),
             full_sample.mod,
             stars = TRUE,
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             coef_map = c("(Intercept)" = "Intercept",
                          "genderFemale" = "Gender: Female",
                          "age" = "Age",
                          "I(age^2)" = "Age^2",
                          "educmittel" = "Education: medium\n(Ref.: low)",
                          "educhoch" = "Educ.: high",
                          "educSchüler" = "Educ.: in school",
                          "stay_length" = "Period of residence (in years)",
                          "muslimMuslim" = "Muslim",
                          "religion_str" = "Religiosity",
                          "mig_type(Resettlers) first generation" = "Resettlers: first generation (Ref.: Natives)",
                          "mig_type(Resettlers) second generation" = "Resettlers: second generation",
                          "mig_type(Turkish) first generation" = "Turkish: first generation",
                          "mig_type(Turkish) second generation" = "Turkish: second generation",
                          "mig_type(European) first generation" = "European: first generation",
                          "mig_type(European) second generation" = "European: second generation", 
                          "mig_type(Other) first generation" = "Other: first generation",
                          "mig_type(Other) second generation" = "Other: second generation",
                          "discriminationlow" = "Discrimination: low\n(Ref.: no at all)",
                          "discriminationhigh" = "Dis.: high",
                          "discriminationvery high" = "Dis.: very high",
                          "v2x_polyarchy" = "Polyarchy (VDem)",
                          "muslimMuslim:religion_str" = "Muslim × Religiosity")) %>%
  tab_footnote(footnote = md("**Source**: SVR-Integrationsbarometer 2022; weighted"))

# Export ----
ggsave(here("figure", "democ_residence-vdem.pdf"), plot = residence.fig, 
       dpi = 300, device = cairo_pdf, 
       width = 20, height = 14, units = "cm")

ggsave(here("figure", "democ_discrimination-vdem.pdf"), plot = discrimination.fig,
       dpi = 300, device = cairo_pdf, 
       width = 22, height = 14, units = "cm")

# Mean democ index
gtsave(mean.gt, filename = "./figure/mean_democ.png")

# MLM results
gtsave(mlm.tbl, filename = "./figure/MLM_results.png")

# Full sample results
gtsave(full_sample_result.tbl, filename = "./figure/FullSample_results.png")
