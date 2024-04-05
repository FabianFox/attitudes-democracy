# Analysis ----

# Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "hrbrthemes", "fixest", "modelsummary", "marginaleffects",
                  "conflicted", "lubridate", "here", "Cairo", "Hmisc", "gt", "gtExtras", "patchwork")

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

# Formative years (cutoff: 15)
ib22_f14year.df <- import(here("data", "ib22_f14year.rds"))

# Formative years (cutoff: 17)
ib22_f17year.df <- import(here("data", "ib22_f17year.rds"))

# Independent variables: dwf, dpu, dmk, drm, dgg, dme, dra
# Add mean score of independent variables
ib_nest.df <- tibble(
  sample = c("total", "formative years: 14", "formative years: 17"),
  data = list(ib22_democ.df, ib22_f14year.df, ib22_f17year.df)) %>%
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
## Summary statistics ----
### Mean ----
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
    n = "N",
    data = "Distribution") %>%
  tab_header(title = md("Importance of democracy")) %>%
  tab_source_note(source_note = md("**Source**: SVR-Integrationsbarometer 2022; weighted")) %>%
  tab_spanner(label = "Index",
              columns = c("mean", "95%CI")) %>%
  tab_spanner(label = "95")  

### Demo: Single items ----
items_democ.gt <- ib_nest.df %>%
  filter(sample == "total") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    mig_type = case_when(
      migra == 1 | migra != 1 & iso3c == "DEU" ~ "Non-migrant",
      migra != 1 & iso3c != "DEU" ~ "First-generation",
      TRUE ~ NA_character_)) %>%
  group_by(first_gen = factor(mig_type, 
                              labels = c("First-generation", "Non-migrant")), 
           .add = TRUE) %>%
  reframe(across(c(dwf, dpu, dmk, drm, dgg),
                 list(mean = ~wtd.mean(.x, weights = weight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = weight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "dwf" ~ "Fair elections",
                               "dpu" ~ "Party competition",
                               "dmk" ~ "Free media",
                               "drm" ~ "Protected minorities",
                               "dgg" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

# Boxplot: Democ
demo_boxplot_ib.fig <- ib_nest.df %>%
  filter(sample == "total") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    mig_type = case_when(
      migra == 1 | migra != 1 & iso3c == "DEU" ~ "Non-migrant",
      migra != 1 & iso3c != "DEU" ~ "First-generation",
      TRUE ~ NA_character_)) %>%
  ggplot(aes(x = mig_type, y = democ, weight = weight)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "", y = "Democratic Values", title = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"))

### by VDEM ----
#### Plots ----
democ_vdem.df <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    across(starts_with("v2x"), 
           ~factor(case_when(. <= 0.25 ~ "low",
                             . > 0.25 & . <= 0.5 ~ "rather low",
                             . > 0.5 & . <= 0.75 ~ "rather high",
                             . > 0.75 ~ "high",
                             .default = NA_character_),
                   levels = c("non-migrant", "low", "rather low", "rather high", "high")),
                .names = "{.col}_cut"))

# Add non-migrants
democ_vdem_nonmig.df <- ib_nest.df %>%
  filter(sample == "total") %>% 
  pull(data) %>%
  .[[1]] %>%
  filter(migra == 1, a_recno != 210084983) %>% # Remove single duplicated individual (wandjahr == 1945)
  mutate(
    v2x_polyarchy_cut = factor(ifelse(migra == 1, "non-migrant", NA_character_),
                        levels = c("non-migrant", "low", "rather low", "rather high", "high")),
    v2xed_ed_dmcon_cut = factor(ifelse(migra == 1, "non-migrant", NA_character_),
                             levels = c("non-migrant", "low", "rather low", "rather high", "high")))

# Polyarchy
poly.fig <- democ_vdem.df %>%
  bind_rows(democ_vdem_nonmig.df) %>%
  filter(!is.na(democ), !is.na(v2x_polyarchy_cut)) %>%
  ggplot(aes(x = fct_rev(v2x_polyarchy_cut), y = democ, weight = weight)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Electoral Democracy Index", y = "Democratic Values", title = "Electoral Democracy Index") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"))

# Dmcon
dmcon.fig <- democ_vdem.df %>%
  bind_rows(democ_vdem_nonmig.df) %>%
  filter(!is.na(democ), !is.na(v2xed_ed_dmcon_cut)) %>%
  ggplot(aes(x = fct_rev(v2xed_ed_dmcon_cut), y = democ, weight = weight)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Democratic Indoctrination Index", y = "Democratic Values", title = "Democratic Indoctrination Index") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"))

# Combine
democ_vdem.fig <- poly.fig + dmcon.fig

#### Table ----
# Polyarchy
items_democ_poly_ib.gt <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    across(starts_with("v2x"), 
           ~factor(case_when(. <= 0.25 ~ "low",
                             . > 0.25 & . <= 0.5 ~ "rather low",
                             . > 0.5 & . <= 0.75 ~ "rather high",
                             . > 0.75 ~ "high",
                             .default = NA_character_),
                   levels = c("low", "rather low", "rather high", "high")),
           .names = "{.col}_cut")) %>%
  filter(!is.na(v2x_polyarchy_cut)) %>%
  group_by(v2x_polyarchy_cut) %>%
  reframe(across(c(dwf, dpu, dmk, drm, dgg),
                 list(mean = ~wtd.mean(.x, weights = weight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = weight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "dwf" ~ "Fair elections",
                               "dpu" ~ "Party competition",
                               "dmk" ~ "Free media",
                               "drm" ~ "Protected minorities",
                               "dgg" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

# Indoctrination
items_democ_ind_ib.gt <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    across(starts_with("v2x"), 
           ~factor(case_when(. <= 0.25 ~ "low",
                             . > 0.25 & . <= 0.5 ~ "rather low",
                             . > 0.5 & . <= 0.75 ~ "rather high",
                             . > 0.75 ~ "high",
                             .default = NA_character_),
                   levels = c("low", "rather low", "rather high", "high")),
           .names = "{.col}_cut")) %>%
  filter(!is.na(v2xed_ed_dmcon_cut)) %>%
  group_by(v2xed_ed_dmcon_cut) %>%
  reframe(across(c(dwf, dpu, dmk, drm, dgg),
                 list(mean = ~wtd.mean(.x, weights = weight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = weight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "dwf" ~ "Fair elections",
                               "dpu" ~ "Party competition",
                               "dmk" ~ "Free media",
                               "drm" ~ "Protected minorities",
                               "dgg" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

### Correlation between VDem measures ----
vdem_cor.df <- ib_nest.df %>%
  filter(sample %in% c("formative years: 14", "formative years: 17")) %>%
  mutate(cor.tbl = map(data, ~.x %>%
                         select(contains("v2x"), "weight") %>%
                         collapse::pwcor(X = .[], 
                                         use = "pairwise.complete.obs",
                                         w = .$weight) %>%
                         # remove weights
                         .[-7, -7])) 

# Table of correlations in analysis (Sample: formative years)
vdem_cor.tbl <- vdem_cor.df %>%
  filter(sample == "formative years: 14") %>%
  pull(cor.tbl) %>%
  .[[1]] %>%
  as.data.frame(row.names = c("v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2xed_ed_dmcon",
                              "v2xed_ed_ptcon", "v2xed_ptcon")) %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = md("Correlation table")) %>%
  tab_source_note(source_note = md("**Source**: SVR-Integrationsbarometer 2022; weighted")) %>%
  fmt_number(decimals = 3)

# FE model ----
# for country-of-origin (dummy)
mod <- fixest::feols(c(dwf, dpu, dmk, drm, dgg, dme, dra, democ) ~ gender + educ +
                       muslim*religion_str + v2xed_ed_dmcon*timeorig + v2xed_ed_dmcon*timedest
                   | iso3c,
                   data = ib_nest.df[ib_nest.df$sample == "formative years: 15",]$data[[1]]) 

### Table ----
mod %>%
  modelsummary(stars = TRUE,
               coef_map =
                 c("(Intercept)" = "Intercept",
                   "genderFemale" = "Gender: Female",
                   "educmittel" = "Education: medium\n(Ref.: low)",
                   "educhoch" = "Educ.: high",
                   "educSchüler" = "Educ.: in school",
                   "stay_length" = "Period of residence (Germany)",
                   "timeorig" = "Period of residence (Country of origin)",
                   "muslimMuslim" = "Muslim",
                   "religion_str" = "Religiosity",
                   "discriminationlow" = "Discrimination: low\n(Ref.: no at all)",
                   "discriminationhigh" = "Dis.: high",
                   "discriminationvery high" = "Dis.: very high",
                   "v2xed_ed_dmcon" = "Democratic indoctrination (VDem)",
                   "muslimMuslim:religion_str" = "Muslim × Religiosity",
                   "timedest:v2xed_ed_dmcon" = "Period of residence (Germany) × DemInd (VDem)",
                   "v2xed_ed_dmcon:timedest" = "Period of residence (Germany) × DemInd (VDem)",
                   "timeorig:v2xed_ed_dmcon" = "Period of residence (CoO) × DemInd (VDem)",
                   "v2xed_ed_dmcon:timeorig" = "Period of residence (CoO) × DemInd (VDem)"))

### Plot ----
fig <- modelplot(mod, colour = "black", 
                 coef_map = 
                   rev(
                     c("(Intercept)" = "Intercept",
                     "genderFemale" = "Gender: Female",
                     "educmittel" = "Education: medium\n(Ref.: low)",
                     "educhoch" = "Educ.: high",
                     "educSchüler" = "Educ.: in school",
                     "stay_length" = "Period of residence (Germany)",
                     "timeorig" = "Period of residence (Country of origin)",
                     "muslimMuslim" = "Muslim",
                     "religion_str" = "Religiosity",
                     "discriminationlow" = "Discrimination: low\n(Ref.: no at all)",
                     "discriminationhigh" = "Dis.: high",
                     "discriminationvery high" = "Dis.: very high",
                     "v2xed_ed_dmcon" = "Democratic indoctrination (VDem)",
                     "muslimMuslim:religion_str" = "Muslim × Religiosity",
                     "timedest:v2xed_ed_dmcon" = "Period of residence (Germany) × DemInd (VDem)",
                     "v2xed_ed_dmcon:timedest" = "Period of residence (Germany) × DemInd (VDem)",
                     "timeorig:v2xed_ed_dmcon" = "Period of residence (CoO) × DemInd (VDem)",
                     "v2xed_ed_dmcon:timeorig" = "Period of residence (CoO) × DemInd (VDem)"))) +
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
  type = rep(
    c("Unconditional",
      "Base", 
      "Interactions",
      "Random slope"), 2),
  iv = c(
    "1 + (1 | iso3c)", 
    "gender + educ + timedest + timeorig + muslim + religion_str + v2x_polyarchy + discrimination + (1 | iso3c)",
    "gender + educ + muslim + religion_str +  + discrimination + timedest*v2x_polyarchy + timeorig*v2x_polyarchy + (1 | iso3c)",
    "gender + educ + muslim + religion_str + discrimination + timedest*v2x_polyarchy + timeorig*v2x_polyarchy + (1 + v2x_polyarchy | iso3c)",
    "1 + (1 | iso3c)", 
    "gender + educ + timedest + timeorig + muslim + religion_str + v2xed_ed_dmcon + discrimination + (1 | iso3c)",
    "gender + educ + muslim + religion_str + discrimination + timedest*v2xed_ed_dmcon + timeorig*v2xed_ed_dmcon + (1 | iso3c)",
    "gender + educ + muslim + religion_str + discrimination + timedest*v2xed_ed_dmcon + timeorig*v2xed_ed_dmcon + (1 + v2xed_ed_dmcon | iso3c)"),
  main_iv = c(
    rep("vdem-poly", 4),
    rep("vdem-ind", 4)))

# Add different samples
model.df <- tibble(
  sample = c("formative years: 14", 
             "formative years: 17"),
  data = c("ib_nest.df %>% filter(sample == 'formative years: 14') %>% pull(data) %>% .[[1]]",
           "ib_nest.df %>% filter(sample == 'formative years: 17') %>% pull(data) %>% .[[1]]")) %>%
  expand_grid(model.df)

# Run models
# VDem at year of immigration
model.df <- model.df %>%
  mutate(model = pmap(list(dv, iv, data), ~lme4::lmer(
    str_c(..1, " ~ ", ..2),
    REML = TRUE,
    weights = weight, # or pweights_a
    data = eval(rlang::parse_expr(..3)))))

# Name list column
names(model.df$model) <- model.df$type

### Model output ----
# modelsummary
mlm.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
             models = model.df %>% filter(sample == "formative years: 14") %>% pull(model),
             stars = TRUE,
             estimate = "{estimate}{stars}",
             statistic = "({std.error})",
             coef_map = c("(Intercept)" = "Intercept",
                          "genderFemale" = "Gender: Female",
                          # "age" = "Age",
                          "educmittel" = "Education: medium\n(Ref.: low)",
                          "educhoch" = "Educ.: high",
                          "educSchüler" = "Educ.: in school",
                          "timedest" = "Period of residence (Germany)",
                          "timeorig" = "Period of residence (Country of origin)",
                          "muslimMuslim" = "Muslim",
                          "religion_str" = "Religiosity",
                          "discriminationlow" = "Discrimination: low\n(Ref.: no at all)",
                          "discriminationhigh" = "Dis.: high",
                          "discriminationvery high" = "Dis.: very high",
                          "v2xed_ed_dmcon" = "Political socialization (V-Dem)",
                          "v2x_polyarchy" = "Political socialization (V-Dem)",
                          # "muslimMuslim:religion_str" = "Muslim × Religiosity",
                          # "fhYes" = "Close family (country-of-origin)",
                          # "fhYes:v2x_polyarchy" = "Close family × Polyarchy (VDem)",
                          # "v2x_polyarchy:fh" = "Close family × Polyarchy (VDem)",
                          "timedest:v2xed_ed_dmcon" = "Period of residence (Germany) × V-Dem",
                          "v2xed_ed_dmcon:timedest" = "Period of residence (Germany) × V-Dem",
                          "timeorig:v2xed_ed_dmcon" = "Period of residence (CoO) × V-Dem",
                          "v2xed_ed_dmcon:timeorig" = "Period of residence (CoO) × V-Dem",
                          "timedest:v2x_polyarchy" = "Period of residence (Germany) × V-Dem",
                          "v2x_polyarchy:timedest" = "Period of residence (Germany) × V-Dem",
                          "timeorig:v2x_polyarchy" = "Period of residence (CoO) × V-Dem",
                          "v2x_polyarchy:timeorig" = "Period of residence (CoO) × V-Dem",
                          # "v2x_polyarchy:discriminationlow" = "Discrimination: low × V-Dem",
                          # "v2x_polyarchy:discriminationhigh" = "Discrimination: high × V-Dem",
                          # "v2x_polyarchy:discriminationvery high" = "Discrimination: very high × V-Dem",
                          # "v2xed_ed_dmcon:discriminationlow" = "Discrimination: low × V-Dem",
                          # "v2xed_ed_dmcon:discriminationhigh" = "Discrimination: high × V-Dem",
                          # "v2xed_ed_dmcon:discriminationvery high" = "Discrimination: very high × V-Dem",
                          "SD (Intercept iso3c)" = "SD (Intercept: Country)",
                          "SD (v2xed_ed_dmcon iso3c)" = "SD (V-Dem iso3c)",
                          "SD (v2x_polyarchy iso3c)" = "SD (V-Dem iso3c)",
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
  tab_spanner(label = md("**Electoral Democracy**"), columns = 2:5) %>%
  tab_spanner(label = md("**Democratic Indoctrination**"), columns = 6:9) %>%
  tab_footnote(footnote = md("**Source**: SVR-Integrationsbarometer 2022; weighted"))

# Plot
# (needs refinement)
modelsummary::modelplot(
  model.df[model.df$sample == "formative years: 14",]$model$`Random slope`,
  coef_map = c("(Intercept)" = "Intercept",
               "genderFemale" = "Gender: Female",
               "educmittel" = "Education: medium\n(Ref.: low)",
               "educhoch" = "Educ.: high",
               "educSchüler" = "Educ.: in school",
               "timedest" = "Period of residence (Germany)",
               "timeorig" = "Period of residence (Country of origin)",
               "muslimMuslim" = "Muslim",
               "religion_str" = "Religiosity",
               "discriminationlow" = "Discrimination: low\n(Ref.: no at all)",
               "discriminationhigh" = "Dis.: high",
               "discriminationvery high" = "Dis.: very high",
               "v2xed_ed_dmcon" = "Democratic indoctrination (VDem)",
               "muslimMuslim:religion_str" = "Muslim × Religiosity",
               # "fhYes" = "Close family (country-of-origin)",
               # "fhYes:v2x_polyarchy" = "Close family × Polyarchy (VDem)",
               # "v2x_polyarchy:fh" = "Close family × Polyarchy (VDem)",
               "timedest:v2xed_ed_dmcon" = "Period of residence (Germany) × DemInd (VDem)",
               "v2xed_ed_dmcon:timedest" = "Period of residence (Germany) × DemInd (VDem)",
               "timeorig:v2xed_ed_dmcon" = "Period of residence (CoO) × DemInd (VDem)",
               "v2xed_ed_dmcon:timeorig" = "Period of residence (CoO) × DemInd (VDem)"))

### Marginal effects ----
# Using ggeffects (analogous to marginaleffects::predictions, see below)
#### Residence period (DEU) × VDem ----
# VDem: Democratic indoctrination
residence_vind.pred <- ggeffects::ggpredict(model = model.df %>%
                                         filter(
                                           sample == "formative years: 14",
                                           type == "Random slope",
                                           main_iv == "vdem-ind") %>%
                                         pull(model) %>% 
                                         .[[1]], 
                                        terms = c("timedest [all]", 
                                                  "v2xed_ed_dmcon [0:1 by = 0.25]"),
                                       type = "fe") 

# VDem: Polyarchy
residence_vpoly.pred <- ggeffects::ggpredict(model = model.df %>%
                                               filter(
                                                 sample == "formative years: 14",
                                                 type == "Random slope",
                                                 main_iv == "vdem-poly") %>%
                                               pull(model) %>% 
                                               .[[1]], 
                                             terms = c("timedest [all]", 
                                                       "v2x_polyarchy [0:1 by = 0.25]"),
                                             type = "fe") 

#### Residence period (CoO) × VDem ----
# VDem: Democratic indoctrination
residence_coo_vind.pred <- ggeffects::ggpredict(model = model.df %>%
                                         filter(
                                           sample == "formative years: 14",
                                           type == "Random slope",
                                           main_iv == "vdem-ind") %>%
                                         pull(model) %>% 
                                         .[[1]], 
                                       terms = c("timeorig [all]", 
                                                 "v2xed_ed_dmcon [0:1 by = 0.25]"),
                                       type = "fe") 

# VDem: Polyarchy
residence_coo_vpoly.pred <- ggeffects::ggpredict(model = model.df %>%
                                                  filter(
                                                    sample == "formative years: 14",
                                                    type == "Random slope",
                                                    main_iv == "vdem-poly") %>%
                                                  pull(model) %>% 
                                                  .[[1]], 
                                                terms = c("timeorig [all]", 
                                                          "v2x_polyarchy [0:1 by = 0.25]"),
                                                type = "fe")

# Residence coo and Germany combined
# VDem: Democratic indoctrination
residence_comb_vind.fig <- residence_vind.pred %>%
  as.data.frame() %>%
  mutate(where = "Germany") %>%
  bind_rows(residence_coo_vind.pred %>%
              as.data.frame() %>%
              mutate(where = "County of origin")) %>%
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 90, 10), labels = seq(0, 90, 10)) +
  scale_y_continuous(breaks = seq(2.4, 3.0, .2), labels = seq(2.4, 3.0, 0.2)) +
  labs(title = 
         "Predicted democratic values by residence period and democratic indoctrination", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0)) 

# Put legend into empty facet
residence_comb_vind.fig <- lemon::reposition_legend(residence_comb_vind.fig, 
                                               position = "center", 
                                               panel = "panel-3-2")

# Residence coo and Germany combined
residence_comb_vpoly.fig <- residence_vpoly.pred %>%
  as.data.frame() %>%
  mutate(where = "Germany") %>%
  bind_rows(residence_coo_vpoly.pred %>%
              as.data.frame() %>%
              mutate(where = "County of origin")) %>%
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 90, 10), labels = seq(0, 90, 10)) +
  scale_y_continuous(breaks = seq(2.4, 3.0, .2), labels = seq(2.4, 3.0, 0.2)) +
  labs(title = 
         "Predicted democratic values by residence period and electoral democracy", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0)) 

# Put legend into empty facet
residence_comb_vpoly.fig <- lemon::reposition_legend(residence_comb_vpoly.fig, 
                                                    position = "center", 
                                                    panel = "panel-3-2")
  

#### Discrimination × VDem ----
discrimination.pred <- ggeffects::ggpredict(model = model.df %>%
                                              filter(
                                                sample == "formative years: 15",
                                                type == "Random slope",
                                                main_iv == "vdem-ind") %>%
                                              pull(model) %>% 
                                              .[[1]], 
                                            terms = c("v2xed_ed_dmcon [0:1 by = 0.25]",
                                                      "discrimination [all]"),
                                            type = "fe") 

# Plot 
discrimination.fig <- plot(discrimination.pred, facets = T, colors = "bw") + 
  labs(title = "Predicted democratic values by perceived discrimination and democratic indoctrination", 
       subtitle = "Holding covariates constant (mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "") +
  facet_wrap(~fct_relabel(group, ~str_c("Discrimination: ", .x))) +
  scale_y_continuous(breaks = seq(2.4, 3, .2), limits = c(2.3, 3)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0))

# 
discrimination.pred %>%
  ggplot(aes(x = group, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  labs(title = 
         "Predicted democratic attitudes by residence period (in country of origin) and VDem", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "") +
  facet_wrap(~str_c("VDem: ", x)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0))

# Using marginaleffects
# Predictions
# see: https://marginaleffects.com/articles/lme4.html
# re.form if NULL include all random effects, if NA include no random effects
# Unit level
unit.predictions <- predictions(
  model.df %>%
    filter(
      sample == "formative years: 15",
      type == "Random slope",
      main_iv == "vdem-ind") %>%
    pull(model) %>% 
    .[[1]], 
  newdata = datagrid(v2x_polyarchy = seq(0., 1, .25), timeorig = 0:70, iso3c = unique),
  re.form = NULL)

# Population level
pop.predictions <- predictions(
  model.df %>% 
    filter(sample == "formative years: 15" & type == "Random slope") %>%
    pull(model) %>%
    .[[1]], 
  newdata = datagrid(v2xed_ed_dmcon = seq(.1, .9, .2), timeorig = 0:70, iso3c = NA),
  re.form = NA)

# Marginal effects
avg_margins <- avg_slopes(model.df %>% 
                            filter(sample == "formative years: 15" & type == "Random slope") %>%
                            pull(model) %>%
                            .[[1]], 
                          variable = "v2xed_ed_dmcon",
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

# Interaction between religion and religiosity
muslim_religiosity.fig <- interactions::interact_plot(full_sample.mod, modx = "muslim", pred = "religion_str", interval = T)

# Export ----
ggsave(here("figure", "IB", "democ_by_vdem.png"), plot = democ_vdem.fig,
       dpi = 300, device = ragg::agg_png, background = "white",
       width = 25, height = 15, units = "cm")

# Democratic indoctrination
ggsave(here("figure", "IB", "residence_x_vdem-ind.png"), plot = residence_comb_vind.fig,
       dpi = 300, device = ragg::agg_png, background = "white",
       width = 25, height = 14, units = "cm")

# Electoral democracy
ggsave(here("figure", "IB", "residence_x_vdem-poly.png"), plot = residence_comb_vpoly.fig,
       dpi = 300, device = ragg::agg_png, background = "white",
       width = 25, height = 14, units = "cm")

# Mean democ index
gtsave(mean.gt, filename = "./figure/mean_democ.png")

# Correlation between VDem measures
gtsave(vdem_cor.tbl, filename = "./figure/VDem-correlation.png")

# MLM results
gtsave(mlm.tbl, filename = "./figure/IB/MLM_results.rtf")

# Export IB22
export(ib_nest.df %>%
         filter(sample == "formative years: 14") %>%
         select(data) %>%
         pull(data) %>%
         .[[1]] %>%
         select(a_recno, a_datum, weight, pweights_a, pweights_b, democ, iso3c, gender, educ, muslim, religion_str, discrimination, 
                timedest, timeorig, starts_with("v2x")) %>%
         tibble(), file = here("data", "ib22-analysis-sample", "ib22_analysis-sample.dta"))

# 
ggsave(here("figure", "democ_discrimination_ind.pdf"), plot = discrimination.fig,
       dpi = 300, device = cairo_pdf, 
       width = 25, height = 14, units = "cm")

# Appendix
ggsave(here("figure", "IB", "Democ-boxplot-IB.png"), plot = demo_boxplot_ib.fig,
       dpi = 300, device = ragg::agg_png(),
       width = 25, height = 15, units = "cm")

gtsave(items_democ.gt, filename = here("figure", "IB", "Demo-items_IB.rtf"))

# 
gtsave(items_democ_poly_ib.gt, filename = here("figure", "IB", "Demo-VDem-Poly_IB.rtf"))
gtsave(items_democ_ind_ib.gt, filename = here("figure", "IB", "Demo-VDem-Ind_IB.rtf"))

#
gtsave(vdem_cor.tbl, filename = here("figure", "IB", "VDem-Correlation.rtf"))
