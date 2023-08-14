# Exploratory factor analysis ----

# Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "hrbrthemes", "fixest", "modelsummary",
                  "conflicted", "lubridate", "here", "Cairo", "Hmisc")

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("expand", "tidyr")

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
ib22_pol.df <- import(here("data", "ib22_pol.rds"))

# Foreign born
ib22_fb.df <- import(here("data", "ib22_fbpol.rds"))

# Independent variables: dwf, dpu, dmk, drm, dgg, dme, dra
# Add mean score of independent variables
ib_nest.df <- tibble(
  sample = c("all", "foreign born"),
  data = list(ib22_pol.df, ib22_fb.df)) %>%
  mutate(data = map(data, ~.x %>%
                      mutate(democ = rowMeans(across(c(dwf, dpu, dmk, drm, dgg)), 
                                              na.rm = FALSE))))

# Mean by natives, first and second generation
mean.df <- ib_nest.df %>%
  filter(sample == "all") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    mig_type = case_when(
      migra == 1 ~ "natives",
      migra != 1 & gebland != 1 ~ "first generation",
      migra != 1 & gebland == 1 ~ "second generation",
      TRUE ~ NA_character_)) %>%
  filter(if_all(c(democ, mig_type, weight), ~!is.na(.))) %>%
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
  ungroup() 

# Create factor variables
ib22_fb.df <- ib_nest.df %>%
  filter(sample == "foreign born") %>%
  pull(data) %>%
  .[[1]] %>%
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
    religion = factor(religion))

# FE model ----
# for country-of-origin (dummy)
mod <- fixest::feols(c(dwf, dpu, dmk, drm, dgg, dme, dra, democ) ~ gender + age + I(age^2) + educ +
                       religion + v2x_polyarchy + stay_length
                   | iso3c,
                   data = ib22_fb.df) 

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
                     "religionNo religion" = "No religion",
                     "religionOther" = "Other religion",
                     "religionMuslim" = "Religion: Muslim\n(Ref.: Christian)",
                     "religion_str" = "Religiosity",
                     "v2x_polyarchy" = "VDEM: Electoral democracy index",
                     "stay_length" = "Period of residence")) +
  geom_vline(xintercept = 0, color = 'orange') + 
  facet_grid(~factor(model,
                     levels = c("lhs: dwf", "lhs: dpu", "lhs: dmk", "lhs: drm", "lhs: dgg", "lhs: dme",
                                "lhs: dra"),
                     labels = c("Elections free and\nfair", "Parties clear\nalternatives", "Media free to\ncriticize",
                                "Minority rights\nprotected", "Courts same treat-\nment", "Balance inequalities\nby taxation",
                                "Protection against\npoverty"))) +
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
### Baseline model ----
# Model: baseline (null model)
v0.mod <- lmerTest::lmer(democ ~ v2x_polyarchy + (1 | iso3c), 
                      data = ib22_fb.df)

### Variance components ----
# ICC
performance::icc(v0.mod)

# M1
v1.mod <- lmerTest::lmer(democ ~ stay_length + muslim + religion_str + v2x_polyarchy + (1 | iso3c),
                         data = ib22_fb.df)

# M1 with weights
v1_w.mod <- lmerTest::lmer(democ ~ stay_length + muslim + religion_str + v2x_polyarchy + (1 | iso3c),
                           weights = weight,
                         data = ib22_fb.df)

# M2
# Transnational contacts
v2_w.mod <- lmerTest::lmer(democ ~ stay_length + muslim + religion_str + fh + v2x_polyarchy + 
                           (1 | iso3c),
                         weights = weight,
                         data = ib22_fb.df)

# modelsummary
modelsummary(list("MLM" = v1_w.mod), estimate = "{estimate}{stars}",
             coef_map = c("(Intercept)" = "Intercept",
                          "stay_length" = "Period of residence (years)",
                          "muslim" = "Muslim",
                          "religion_str" = "Religiosity",
                          "v2x_polyarchy" = "Polyarchy (VDem)"),
             gof_map = tribble(
               ~raw, ~clean, ~fmt,
               "nobs", "N", 0,
               "icc", "ICC", 2,
               "aic", "AIC", 0)) 

# 
ggsave(here("figure", "model_foreignborn.pdf"), dpi = 300, device = cairo_pdf, 
       width = 34, height = 24, units = "cm")
