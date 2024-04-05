# Analysis (ESS data) ----

# Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "hrbrthemes", "fixest", "modelsummary", "marginaleffects", "lme4",
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
ess_democ.orig <- tibble(import(here("data", "mergeddataset_V_ESS_14_16_nativesfirstgen.dta"))) 

# Add quantiles of Vdem
ess_democ.df <- ess_democ.orig %>%
  mutate(across(matches(c("v2x_polyarchy_4nat", "v2xed_ed_dmcon_4nat")),
                ~factor(tolower(sjlabelled::as_character(.)),
                levels = c("non-migrant", "low", "rather low", "rather high", "high"))))

# Description ----
## By VDem ----
### DV: Single items ----
items_democ_ess.gt <- ess_democ.df %>%
  group_by(first_gen = factor(first_gen, 
                              levels = c(1, 0), 
                              labels = c("First-generation", "Non-migrant")), .add = TRUE) %>%
  reframe(across(c(fairelc, dfprtal, medcrgv, rghmgpr, cttresa),
                 list(mean = ~wtd.mean(.x, weights = anweight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = anweight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "fairelc" ~ "Fair elections",
                               "dfprtal" ~ "Party competition",
                               "medcrgv" ~ "Free media",
                               "rghmgpr" ~ "Protected minorities",
                               "cttresa" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

### Tables ----
# Polyarchy
items_democ_poly_ess.gt <- ess_democ.df %>%
  filter(first_gen == 1, 
         !is.na(v2x_polyarchy_4nat)) %>%
  group_by(v2x_polyarchy_4nat) %>%
  reframe(across(c(fairelc, dfprtal, medcrgv, rghmgpr, cttresa),
                 list(mean = ~wtd.mean(.x, weights = anweight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = anweight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "fairelc" ~ "Fair elections",
                               "dfprtal" ~ "Party competition",
                               "medcrgv" ~ "Free media",
                               "rghmgpr" ~ "Protected minorities",
                               "cttresa" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

# Indoctrination
items_democ_ind_ess.gt <- ess_democ.df %>%
  filter(first_gen == 1, 
         !is.na(v2xed_ed_dmcon_4nat)) %>%
  group_by(v2xed_ed_dmcon_4nat) %>%
  reframe(across(c(fairelc, dfprtal, medcrgv, rghmgpr, cttresa),
                 list(mean = ~wtd.mean(.x, weights = anweight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = anweight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "fairelc" ~ "Fair elections",
                               "dfprtal" ~ "Party competition",
                               "medcrgv" ~ "Free media",
                               "rghmgpr" ~ "Protected minorities",
                               "cttresa" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

## Boxplot: Demo1 ----
demo_boxplot_ess.fig <- ess_democ.df %>%
  mutate(first_gen = factor(first_gen, 
                   levels = c(1, 0), 
                   labels = c("First-generation", "Non-migrant"))) %>%
  ggplot(aes(x = first_gen, y = demo1, weight = anweight)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "", y = "Democratic Values", title = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"))

## Democratic Values by VDem in CoO ----
# Polyarchy
poly_ess.fig <- ess_democ.df %>%
  filter(!is.na(demo1), !is.na(v2x_polyarchy_4nat)) %>%
  ggplot(aes(x = fct_rev(v2x_polyarchy_4nat), y = demo1, weight = anweight)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  labs(x = "Electoral Democracy Index", y = "Democratic Values", title = "Electoral Democracy Index") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"))

# Dmcon
dmcon_ess.fig <- ess_democ.df %>%
  filter(!is.na(demo1), !is.na(v2xed_ed_dmcon_4nat)) %>%
  ggplot(aes(x = fct_rev(v2xed_ed_dmcon_4nat), y = demo1, weight = anweight)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  labs(x = "Democratic Indoctrination Index", y = "Democratic Values", title = "Democratic Indoctrination Index") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"))

# Combine
democ_vdem_ess.fig <- poly_ess.fig + dmcon_ess.fig

## Prepare ----
# Reduce data to individuals with migback and rename variables
ess_democ_mod.df <- ess_democ.df %>%
  filter(first_gen == 1) %>%
  select(gender = gndr, educ = edulvlb, discrimination = discri, religion_str = religiosity, 
         democ = demo1, iso3c = country_text_id, weight = anweight, v2x_polyarchy, v2x_libdem,
         v2x_partipdem, v2xed_ed_dmcon, v2xed_ed_ptcon, v2xed_ptcon, muslim, timedest, timeorig, 
         cntry) %>%
  mutate(
    muslim = factor(muslim, levels = c(0, 1), 
                    labels = c("Other", "Muslim")),
    discrimination = case_match(
      discrimination,
      0 ~ "no",
      1 ~ "yes",
      TRUE ~ NA_character_),
    discrimination = factor(discrimination, 
                            levels = c("no", "yes")),
    gender = gender - 1,
    gender = factor(gender, 
                    levels = c(0, 1), 
                    labels = c("Male", "Female")),
    educ = case_match(educ,
                      c(0, 113, 129, 212, 213, 221, 222, 223) ~ "ISCED 2 and lower",
                      c(229, 311, 312, 313, 321, 322, 323) ~ "ISCED 3",
                      c(412, 413, 421, 422, 423, 510, 520) ~ "ISCED 4 and 5A/B short",
                      c(610, 620, 710, 720, 800) ~ "ISCED 5 medium and higher",
                      .default = NA_character_),
    educ = factor(educ, levels = c("ISCED 2 and lower", "ISCED 3", "ISCED 4 and 5A/B short", 
                                   "ISCED 5 medium and higher")),
    timeorig = timeorig - 14) # Count from zero since age 14 (just like in IB analysis)

## Correlations of VDem ----
# Correlation between VDem measures
vdem_ess_cor.df <- ess_democ_mod.df %>%
  select(contains("v2x"), "weight") %>%
  collapse::pwcor(X = .[], 
                  use = "pairwise.complete.obs",
                  w = .$weight) %>%
  # remove weights
  .[-7, -7]

# Table of correlations in analysis (Sample: formative years)
vdem_ess_cor.tbl <- vdem_ess_cor.df %>%
  as.data.frame(row.names = c("v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2xed_ed_dmcon",
                              "v2xed_ed_ptcon", "v2xed_ptcon")) %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = md("Correlation table")) %>%
  tab_source_note(source_note = md("**Source**: ESS10; weighted")) %>%
  fmt_number(decimals = 3)

# Run models ----
model.df <- tibble(
  dv = "democ",
  type = rep(
    c("Unconditional",
      "Base", 
      "Interactions",
      "Random slope"), 2),
  iv = c(
    "1 + (1 | cntry/iso3c)", 
    "gender + educ + timedest + timeorig + muslim + religion_str + v2x_polyarchy + discrimination + (1 | cntry/iso3c)",
    "gender + educ + muslim + religion_str +  + discrimination + timedest*v2x_polyarchy + timeorig*v2x_polyarchy + (1 | cntry/iso3c)",
    "gender + educ + muslim + religion_str + discrimination + timedest*v2x_polyarchy + timeorig*v2x_polyarchy + (1 + v2x_polyarchy | cntry/iso3c)",
    "1 + (1 | cntry/iso3c)", 
    "gender + educ + timedest + timeorig + muslim + religion_str + v2xed_ed_dmcon + discrimination + (1 | cntry/iso3c)",
    "gender + educ + muslim + religion_str + discrimination + timedest*v2xed_ed_dmcon + timeorig*v2xed_ed_dmcon + (1 | cntry/iso3c)",
    "gender + educ + muslim + religion_str + discrimination + timedest*v2xed_ed_dmcon + timeorig*v2xed_ed_dmcon + (1 + v2xed_ed_dmcon | cntry/iso3c)"),
  main_iv = c(
    rep("vdem-poly", 4),
    rep("vdem-ind", 4)),
  data = "ess_democ_mod.df")

# Run models
# VDem at year of immigration
model.df <- model.df %>%
  mutate(model = pmap(list(dv, iv, data), ~lmer(
    str_c(..1, " ~ ", ..2),
    REML = TRUE,
    control = lmerControl(optimizer = "optimx", 
                          optCtrl = list(method = "nlminb")),
    weights = weight,
    data = eval(rlang::parse_expr(..3)))))

# Name list column
names(model.df$model) <- model.df$type

### Model output ----
# modelsummary
mlm.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
                        models = model.df %>% pull(model),
                        stars = TRUE,
                        estimate = "{estimate}{stars}",
                        statistic = "({std.error})",
                        coef_map = c("(Intercept)" = "Intercept",
                                     "genderFemale" = "Gender: Female",
                                     "educISCED 3" = "Education: ISCED 3\n(Ref.: ISCED 2 and lower)",
                                     "educISCED 4 and 5A/B short" = "Educ.: ISCED 4 and 5A/B short",
                                     "educISCED 5 medium and higher" = "Educ.: ISCED 5 medium and higher",
                                     "timedest" = "Period of residence (Country of destination)",
                                     "timeorig" = "Period of residence (Country of origin)",
                                     "muslimMuslim" = "Muslim",
                                     "religion_str" = "Religiosity",
                                     "discriminationyes" = "Discrimination: Yes\n(Ref.: No)",
                                     "v2xed_ed_dmcon" = "Political socialization (V-Dem)",
                                     "v2x_polyarchy" = "Political socialization (V-Dem)",
                                     "timedest:v2xed_ed_dmcon" = "Period of residence (CoD) × V-Dem",
                                     "v2xed_ed_dmcon:timedest" = "Period of residence (CoD) × V-Dem",
                                     "timeorig:v2xed_ed_dmcon" = "Period of residence (CoO) × V-Dem",
                                     "v2xed_ed_dmcon:timeorig" = "Period of residence (CoO) × V-Dem",
                                     "timedest:v2x_polyarchy" = "Period of residence (CoD) × V-Dem",
                                     "v2x_polyarchy:timedest" = "Period of residence (CoD) × V-Dem",
                                     "timeorig:v2x_polyarchy" = "Period of residence (CoO) × V-Dem",
                                     "v2x_polyarchy:timeorig" = "Period of residence (CoO) × V-Dem",
                                     "SD (Intercept iso3c)" = "SD (Intercept: CoO)",
                                     "SD (Intercept cntry)" = "SD (Intercept: CoD)",
                                     "SD (Intercept iso3ccntry)" = "SD (Intercept CoD/CoO)",
                                     "SD (v2xed_ed_dmcon iso3c)" = "SD (V-Dem CoO)",
                                     "SD (v2xed_ed_dmcon cntry)" = "SD (V-Dem CoD)",
                                     "SD (v2xed_ed_dmcon iso3ccntry)" = "SD (V-Dem CoD/CoO)",
                                     "SD (v2x_polyarchy iso3c)" = "SD (V-Dem CoO)",
                                     "SD (v2x_polyarchy cntry)" = "SD (V-Dem CoD)",
                                     "SD (v2x_polyarchy iso3ccntry)" = "SD (V-Dem CoD/CoO)",
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
  tab_footnote(footnote = md("**Source**: ESS10; weighted"))

### Marginal effects ----
# Using ggeffects (analogous to marginaleffects::predictions, see below)
#### Residence period (CoD) × VDem ----
# VDem: Democratic indoctrination
residence_vind.pred <- ggeffects::ggpredict(model = model.df %>%
                                              filter(
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
  mutate(where = "Country of Destination") %>%
  bind_rows(residence_coo_vind.pred %>%
              as.data.frame() %>%
              mutate(where = "County of origin")) %>%
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 75, 10), labels = seq(0, 75, 10)) +
  scale_y_continuous(breaks = seq(7.5, 9.5, .5), labels = seq(7.5, 9.5, .5)) +
  labs(title = 
         "Predicted democratic values by residence period and democratic indoctrination", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: ESS 10; weighted data",
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
  mutate(where = "Country of destination") %>%
  bind_rows(residence_coo_vpoly.pred %>%
              as.data.frame() %>%
              mutate(where = "County of origin")) %>%
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 75, 10), labels = seq(0, 75, 10)) +
  scale_y_continuous(breaks = seq(7.5, 9.5, .5), labels = seq(7.5, 9.5, .5)) +
  labs(title = 
         "Predicted democratic values by residence period and electoral democracy", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: ESS 10; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0)) 

# Put legend into empty facet
residence_comb_vpoly.fig <- lemon::reposition_legend(residence_comb_vpoly.fig, 
                                                     position = "center", 
                                                     panel = "panel-3-2")

# Export
# Export ----
ggsave(here("figure", "ESS", "democ_by_vdem_ess.png"), plot = democ_vdem_ess.fig,
       dpi = 300, device = ragg::agg_png(),
       width = 25, height = 15, units = "cm")

# Democratic indoctrination
ggsave(here("figure", "ESS", "residence_x_vdem-ind_ess.png"), plot = residence_comb_vind.fig,
       dpi = 300, device = ragg::agg_png(), 
       width = 25, height = 14, units = "cm")

# Electoral democracy
ggsave(here("figure", "ESS", "residence_x_vdem-poly_ess.png"), plot = residence_comb_vpoly.fig,
       dpi = 300, device = ragg::agg_png(), 
       width = 25, height = 14, units = "cm")

# MLM results
gtsave(mlm.tbl, filename = "./figure/ESS/MLM_results_ess_weighted.png")

gtsave(mlm.tbl, filename = "./figure/ESS/MLM_results.rtf")

# Appendix
ggsave(here("figure", "ESS", "Democ-boxplot-ESS.png"), plot = demo_boxplot_ess.fig,
       dpi = 300, device = ragg::agg_png(),
       width = 25, height = 15, units = "cm")

gtsave(items_democ_ess.gt, filename = here("figure", "ESS", "Demo-items.rtf"))

# 
gtsave(items_democ_poly_ess.gt, filename = here("figure", "ESS", "Demo-VDem-Poly_ESS.rtf"))
gtsave(items_democ_ind_ess.gt, filename = here("figure", "ESS", "Demo-VDem-Ind_ESS.rtf"))

gtsave(vdem_ess_cor.tbl, filename = here("figure", "ESS", "VDem-Correlation_ESS.rtf"))
