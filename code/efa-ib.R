# Exploratory factor analysis ----

# Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "Hmisc", "weights", "sjlabelled",
                  "tidystringdist", "corrr", "hrbrthemes", "gt",
                  "psych", "conflicted", "lubridate", "here")

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("expand", "tidyr")

# Fonts
extrafont::loadfonts()

# Load data ----
# ------------------------------------------------------------------------------------------------ #
ib22_pol.df <- import(here("data", "ib22_democ.rds"))

# Items
qst.df <- tibble(
  item = names(ib22_pol.df)[5:18],
  q = c(
    "Courts treat everyone the same",
    "National elections are free and fair",
    "Courts that can prevent the government from overstepping its powers",
    "The rights of minority groups are protected",
    "Different political parties offer clear alternatives to one another",
    "Government takes measures to reduce differences in income levels",
    "Government protects all citizens against poverty",
    "Large majority for political decisions",
    "Will of the people cannot be stopped",
    "Politicians being attentive to people's problems",
    "Media are free to criticize the government",
    "To protest against government decisions?",
    "To keep one's life private from the government",
    "To say what one thinks in public"))

# Correlations ----
# ------------------------------------------------------------------------------------------------ #
# Variables
vars <- ib22_pol.df %>%
  select(dgg, dwf, dgb, drm, dpu, dme, dra, dmp, dwb, dop, dmk, drp, dlp, dmf)

# Map over
vars <- tidy_comb_all(names(vars)) %>%
  add_column(weight = "weight")

# Mirror df to duplicate observations (i.e. matrix-like format)
vars2 <- vars %>%
  rename(V1 = V2, V2 = V1)

# Bind
vars <- vars %>%
  bind_rows(vars2)

# Data w/o NA
ib22_cor.df <- ib22_pol.df %>%
  filter(if_all(c(dgg, dwf, dgb, drm, dpu, dme, dra, dmp, dwb, dop, dmk, 
                  drp, dlp, dmf, weight), ~!is.na(.x)))

# Map wtd.cor (weighted correlation)
cor.df <- pmap(vars, ~ wCorr::weightedCorr(ib22_cor.df[[..1]], 
                                           ib22_cor.df[[..2]], 
                                           weights = ib22_cor.df[[..3]],
                                           method = "Polychoric")) %>% 
  map_df(as_tibble) %>%
  cbind(vars, .) %>%
  retract(V1, V2, value) %>%
  relocate(dgg, .after = term) %>%
  as_cordf(diagonal = 0) %>%
  shave() 

# Correlation plot
rplot(cor.df, print_cor = T, colours = viridisLite::plasma(3))

# Create lookup
lookup <- qst.df %>%
  mutate(q = str_c(q, " (", item, ")")) %>%
  pull(q) %>% 
  set_names(qst.df$item)

# Create correlation table
cor.tbl <- cor.df %>%
  mutate(term = recode(term, !!!lookup)) %>%
  gt() %>%
  fmt_number() %>%
  fmt_missing(missing_text = "-") %>%
  tab_header(title = "Correlation matrix: Democracy") %>%
  tab_source_note("Note: Polychoric correlation; weighted data") %>%
  tab_source_note("Source: SVR-Integrationsbarometer 2022") %>%
  cols_label(term = "")

# Export
gtsave(cor.tbl, here("output", "Correlation-matrix.rtf"))

# Factor analysis ----
# ------------------------------------------------------------------------------------------------ #
## Scree plot ----
scree_pol.df <- psych::fa.parallel(
  x = ib22_pol.df %>%
    select(dgg, dwf, dgb, drm, dpu, dme, dra, dmp, dwb, dop, dmk, drp, dlp, dmf), 
  fm = "pa", fa = "fa", cor = "poly", n.iter = 100, SMC = TRUE, 
  quant = .95)

# Plot
scree_plot.df <- tibble(
  type = "Observed",
  nfact = scree_pol.df$nfact,
  eigenvalue = scree_pol.df$fa.values) %>%
  mutate(num = row_number())

# Simulated data
scree_sim.df <- tibble(
  type = "Simulated (95%-Quantil)",
  nfact = scree_pol.df$nfact,
  percentile = list(scree_pol.df$values %>% 
    as_tibble() %>% 
    select(starts_with("Fsim")) %>% 
    summarise(across(everything(), ~quantile(.x, .95))) %>%
    pivot_longer(cols = starts_with("Fsim"), 
                 names_to = "num", 
                 names_prefix = "Fsim", 
                 values_to = "eigenvalue"))) %>%
  unnest(percentile) %>%
  mutate(num = strtoi(num))

# Join
scree_plot.df <- scree_plot.df %>%
  bind_rows(scree_sim.df)

# Plot
scree_plot.fig <- ggplot(scree_plot.df, aes(x = num, y = eigenvalue, shape = type)) +
  geom_line() +
  geom_point(size = 4) +
  scale_y_continuous(name = "Eigenvalue") +
  scale_x_continuous(name = "Factor", breaks = min(scree_plot.df$num):max(scree_plot.df$num)) +
  scale_shape_manual(values = c(16, 1)) +
  geom_vline(xintercept = scree_plot.df$nfact, linetype = "dashed") +
  labs(title = "Screeplot with Parallel analysis",
       caption = "Source: SVR-Integrationsbarometer 2022") +
  cowplot::theme_minimal_grid(font_size = 16) +
  theme(legend.title = element_blank(), legend.position = "bottom", 
        panel.spacing = unit(1, "cm"), plot.caption = element_text(hjust = 0))

## EFA ----
efa.df <- ib22_pol.df %>%
  select(dwf, dpu, dmk, drm, dgg, dra, dme, dwb, dmp, dop) %>%  
  drop_na() %>%
  rename_with(~recode(., !!!lookup)) %>%
  nest() %>%
  expand(nesting(.), factors = 1:4)

# Run FAs
set.seed(2710)
efa.df <- efa.df %>%
  mutate(efa_result = map2(.x = data, .y = factors, ~fa(
           .x, nfactors = .y, alpha = .05, fm = "ml", 
           cor = "poly", SMC = FALSE, rotate = "promax",
           n.iter = 100)))

# Add fit indices
efa.df <- efa.df %>%
  mutate(rmsea = map(efa_result, "RMSEA"),
         bic = map(efa_result, "BIC"),
         rmsea = map(rmsea, ~bind_rows(.x))) 

# Output: RMSEA and BIC
fit.df <- efa.df %>%
  select(factors, rmsea, bic) %>%
  unnest(c(rmsea, bic)) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2)),
         `95%-CI` = str_c(lower, upper, sep = ", ")) %>%
  select(factors, RMSEA, `95%-CI`, BIC = bic)

# Table of factor loadings
# ------------------------------------------------------------------------------------------------ #
# gt-tables also available from:
# https://github.com/franciscowilhelm/r-collection/blob/master/fa_table.R
# initial implementation: https://www.anthonyschmidt.co/post/2020-09-27-efa-tables-in-r/

# Load function
source(here("code", "fa-table.R"))

# Map across EFAs
efa.df <- efa.df %>%
  mutate(
    tables = pmap(list(efa_result, factors), 
                       ~fa_table(..1, title = str_c("Factor analysis (", 
                                                    ..2, " factors)"),
                                 diffuse = 0, sort = FALSE) %>%
                    tab_source_note("Note: Polychoric correlation") %>%
                    tab_source_note("Source: SVR-Integrationsbarometer 2022"))
    )

# Save tables for further inspection
# Saved as .rtf then copied and combined in Word
efa.df %>%
  select(tables, factors) %>%
  filter(factors == 2) %>%
  pmap(., ~gtsave(..1, filename = str_c("./output/", "FactorAnalysis_", 
                                        ..2, ".rtf")))

## Reliability ----
### Polychoric ----
scale.f1.poly <- ib22_pol.df %>%
  select(matches("dpu$|dme$|dra$|dmp$|$dwb|dop$")) %>%
  drop_na() %>%
  polychoric()

scale.f2.poly <- ib22_pol.df %>%
  select(matches("dgg$|dwf$|drm$|dmk$")) %>%
  drop_na() %>%
  polychoric()

# Cronbach's Alpha
psych::alpha(scale.f1.poly$rho)$total
psych::alpha(scale.f2.poly$rho)$total

# Export ----
library(Cairo)
ggsave(plot = scree_plot.fig, here("figure", "Parallelanalysis.pdf"),
       device = cairo_pdf, dpi = 300)

export(fit.df, "./output/Modelfit.csv", sep = ";")
