# Exploratory factor analysis ----

# Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "hrbrthemes", "fixest", "modelsummary",
                  "conflicted", "lubridate", "here", "Cairo")

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("expand", "tidyr")

# Fonts
extrafont::loadfonts()

# Load data ----
# ------------------------------------------------------------------------------------------------ #
ib22_pol.df <- import(here("data", "ib22_fbpol.rds"))

# Independent variables: dwf, dpu, dmk, drm, dgg, dme, dra

# Create factor variables
ib22_pol.df <- ib22_pol.df %>%
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

# Add mean score of independent variables
ib22_pol.df <- ib22_pol.df %>%
  mutate(democ = rowMeans(across(c(dwf, dpu, dmk, drm, dgg)), na.rm = TRUE))

# FE model ----
# for country-of-origin (dummy)
mod <- fixest::feols(c(dwf, dpu, dmk, drm, dgg, dme, dra, democ) ~ gender + age + I(age^2) + educ +
                       religion + v2x_polyarchy + stay_length
                   | iso3c,
                   data = ib22_pol.df) 

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

# MLM ----
### Baseline model ----
# Model: baseline
v0.mod <- lmerTest::lmer(demo ~ 1 + v2x_polyarchy + (1 | iso3c), 
                      ib22_pol.df)

### Variance components ----
# Variance on land/year-level
performance::icc(v0.mod)

# 
v1.mod <- lmerTest::lmer(democ ~ stay_length + muslim + religion_str + v2x_polyarchy + (1 | iso3c),
                         data = ib22_pol.df)

v1_w.mod <- lmerTest::lmer(democ ~ stay_length + muslim + religion_str + v2x_polyarchy + (1 | iso3c),
                           weights = weight,
                         data = ib22_pol.df)

# 
ggsave(here("figure", "model_foreignborn.pdf"), dpi = 300, device = cairo_pdf, 
       width = 34, height = 24, units = "cm")
