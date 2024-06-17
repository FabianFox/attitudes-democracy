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
    "Gerichte alle Menschen gleich behandeln?",
    "Wahlen zum nationalen Parlament frei und fair sind?",
    "Gerichte die Regierung daran hindern können, ihre Befugnisse zu überschreiten?",
    "Rechte von Minderheiten geschützt werden?",
    "Parteien inhaltlich klar voneinander unterscheiden?",
    "Menschen mit höherem Einkommen stärker besteuert, um Einkommensunterschiede zu verringern?",
    "die Regierung alle Bürger vor Armut schützt?",
    "große Mehrheit für eine politische Entscheidung gibt?",
    "Politiker dem Willen der Bevölkerung folgen?",
    "Politiker immer ein offenes Ohr für die Probleme der Menschen haben?",
    "Medien das Recht haben, Kritik an der Regierung zu üben?",
    "gegen Regierungsentscheidungen zu protestieren?",
    "sein Leben vor der Regierung privat zu halten?",
    "in der Öffentlichkeit zu sagen was er denkt?"),
  short_q = c(
    "Gerichte behandeln alle gleich",
    "Wahlen frei und fair",
    "Gerichte kontrollieren Regierung",
    "Minderheitenrechte schützen",
    "Parteien unterscheidbar",
    "Höhere Einkommen besteuern",
    "Schutz vor Armut",
    "Mehrheiten für pol. Entscheidungen",
    "Politiker folgen Willen der Bevölkerung",
    "Politiker offen für Probleme",
    "Medien dürfen Kritik üben",
    "Recht auf Protest",
    "Recht auf Privatsphäre",
    "Recht auf Meinungsäußerung"
  )
)

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
  mutate(short_q = str_c(short_q, " (", item, ")")) %>%
  pull(short_q) %>% 
  set_names(qst.df$item)

# Create correlation table
cor.tbl <- cor.df %>%
  mutate(term = recode(term, !!!lookup)) %>%
  gt() %>%
  fmt_number() %>%
  fmt_missing(missing_text = "-") %>%
  tab_header(title = "Korrelationsmatrix: Demokratie") %>%
  tab_source_note("Anmerkung: Polychorische Korrelation; gewichtete Daten") %>%
  tab_source_note("Quelle: SVR-Integrationsbarometer 2022") %>%
  cols_label(term = "")

# Export
gtsave(cor.tbl, here("output", "Korrelationsmatrix.rtf"))

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
  type = "Beobachtete Daten",
  nfact = scree_pol.df$nfact,
  eigenvalue = scree_pol.df$fa.values) %>%
  mutate(num = row_number())

# Simulated data
scree_sim.df <- tibble(
  type = "Simulierte Daten (95%-Quantile)",
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
  scale_y_continuous(name = "Eigenwert") +
  scale_x_continuous(name = "Faktor", breaks = min(scree_plot.df$num):max(scree_plot.df$num)) +
  scale_shape_manual(values = c(16, 1)) +
  geom_vline(xintercept = scree_plot.df$nfact, linetype = "dashed") +
  labs(title = "Screeplot mit Parallelanalyse",
       caption = "Quelle: SVR-Integrationsbarometer 2022") +
  cowplot::theme_minimal_grid(font_size = 16) +
  theme(legend.title = element_blank(), legend.position = "bottom", 
        panel.spacing = unit(1, "cm"), plot.caption = element_text(hjust = 0))

## EFA ----
efa.df <- ib22_pol.df %>%
  select(dgg, dwf, drm, dpu, dme, dra, dmp, dwb, dop, dmk) %>%  #  dgb, drp, dlp, dmf
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
         `95%-Konfidenzintervall` = str_c(lower, upper, sep = ", ")) %>%
  select(Faktoren = factors, RMSEA, `95%-Konfidenzintervall`, BIC = bic)

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
                    tab_source_note("Anmerkung: Polychorische Korrelation; gewichtete Daten") %>%
                    tab_source_note("Quelle: SVR-Integrationsbarometer 2022"))
    )

# Save tables for further inspection
# Saved as .rtf then copied and combined in Word
efa.df %>%
  select(tables, factors) %>%
  filter(factors == 2) %>%
  pmap(., ~gtsave(..1, filename = str_c("./output/", "FactorAnalysis_", 
                                        ..2, ".rtf")))

# Path diagram
psych::fa.diagram(efa_pol)

## Reliability ----
# Latent factors (identified by EFA)
# 1: dgg, dwf, dgb, drm = Gleichbehandlung & Recht
# 2: dwb, dop, dmp = Responsivität
# 3: dlp, dmf, drp = Meinungsfreiheit & Privatsphäre
# 4: dra, dme = ökonomischer Ausgleich
# 5: dmk: Medien
# 6: dpu: vielfältige Parteienlandschaft

# Cronbach's alpha (items determined by EFA)
scales_fa.df <- tibble(
  general = "dgg$|dwf$|drm$|dpu$|dmk$"
)

# Cronbach's alpha (subject matter knowledge)
scales.df <- tibble(
  equality = "dgg$|dgb$|drm$",
  responsiveness = "dwb$|dop$",
  free_speech = "dmf$|drp$",
  redistribution = "dra$|dme$"
)

# Get alphas
alpha_raw <- 
  map_df(scales_fa.df, ~ 
           ib22_pol.df %>% 
           select(matches(.x)) %>% 
           psych::alpha(check.keys = TRUE) %>%
           .$total %>% 
           rownames_to_column()
         ,.id = "scale"
  )

# Create scales
ib22_pol.df <- ib22_pol.df %>%
  mutate(
    fair_scl = rowMeans(across(c(dgg, dwf, dgb, drm)), na.rm = TRUE),
    resp_scl = rowMeans(across(c(dwb, dop, dmp)), na.rm = TRUE),
    speech_scl = rowMeans(across(c(dlp, dmf, drp)), na.rm = TRUE)
  )

### Polychoric ----
scale.poly <- ib22_pol.df %>%
  select(matches("dgg$|dwf$|drm$|dpu$|dmk$")) %>%
  drop_na() %>%
  polychoric()

scale.f1.poly <- ib22_pol.df %>%
  select(matches("dpu$|dme$|dra$|dmp$|$dwb|dop$")) %>%
  drop_na() %>%
  polychoric()

scale.f2.poly <- ib22_pol.df %>%
  select(matches("dgg$|dwf$|drm$|dmk$")) %>%
  drop_na() %>%
  polychoric()


# 
psych::alpha(scale.poly$rho)$total
psych::alpha(scale.f1.poly$rho)$total
psych::alpha(scale.f2.poly$rho)$total

# Prob tables of all democracy items 
# ---------------------------------------------------------------------------- #
# Create tibble of variables to map across
vars <- tibble(
  q = names(ib22_pol.df)[c(4:17)],
  grp = "migra",
  w = "weight"
)

# Data to map across
ib22_pol.df <- ib22_pol.df %>%
  mutate(migra = case_when(
    migra == 1 ~ "ohne Migrationshintergrund",
    migra == 2 ~ "Spät-/Aussiedlerstatus",
    migra == 3 ~ "Türkei",
    migra == 4 ~ "EU",
    migra == 5 ~ "übrige Welt",
    TRUE ~ NA_character_))

# Create weighted counts across IKI items
ib22_prob.df <- pmap(vars, ~count(x = ib22_pol.df %>%
                                    filter(!is.na(.data[[..1]])),
                                  .data[[..1]], 
                                  .data[[..2]], 
                                  wt = .data[[..3]]) %>%
                       rename("response" = 1))

# Name list elements
ib22_prob.df <- ib22_prob.df %>%
  set_names(., vars[[1]])

# Create probabilities by migra
ib22_prob.df <- ib22_prob.df %>%
  map(~.x %>%
        group_by(migra) %>%
        mutate(prob = n / sum(n) * 100) %>%
        ungroup())

# Make long for plotting
ib22_prob.df <- ib22_prob.df %>%
  bind_rows(.id = "item") %>%
  left_join(y = qst.df, by = "item")

# Plot all items
democracy_migra.fig <- ib22_prob.df %>%
  ggplot(aes(x = factor(migra,
                        levels = rev(c("ohne Migrationshintergrund", "Spät-/Aussiedlerstatus",
                                       "Türkei", "EU", "übrige Welt"))),
             y = prob, fill = factor(response, 
                                     levels = c(0, 1, 2, 3), 
                                     labels = c("gar nicht wichtig", 
                                                "eher unwichtig", 
                                                "eher wichtig", 
                                                "sehr wichtig")))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(prob > 10,
                               str_replace(as.character(round(prob, digit = 1)),
                                           "[.]", ","), "")),
            colour = "white", 
            position = position_stack(vjust = 0.5), family = "Tahoma", 
            size = 4) +
  labs(title = "Einstellungen zu Dimensionen des politischen Systems",
       subtitle = "(nach Herkunftsgruppe der Befragten)",
       fill = "Antwort", x = "", y = "") +
  scale_y_continuous(labels = function(x) str_c(x, "%")) +
  scale_fill_manual(values = c("sehr wichtig" = "#003c76",
                               "eher wichtig" = "#0098d4",
                               "eher unwichtig" = "#BBB985",
                               "gar nicht wichtig" = "#7e8015")) +
  coord_flip() +
  facet_wrap(~q) +
  theme_ipsum(strip_text_size = 9) +
  theme(legend.position = "bottom", legend.justification = "left")

# Plot items 'dpu'
party_diff.fig <- ib22_prob.df %>%
  filter(item == "dpu") %>%
  ggplot(aes(x = factor(migra,
                        levels = rev(c("ohne Migrationshintergrund", "Spät-/Aussiedlerstatus",
                                       "Türkei", "EU", "übrige Welt"))),
             y = prob, fill = factor(response, 
                                     levels = c(0, 1, 2, 3), 
                                     labels = c("gar nicht wichtig", 
                                                "eher unwichtig", 
                                                "eher wichtig", 
                                                "sehr wichtig")))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(prob > 5, str_replace(as.character(round(prob, digit = 1)), "[.]", ","), "")),
            colour = "white", 
            position = position_stack(vjust = 0.5), family = "Tahoma", 
            size = 5) +
  labs(fill = "", x = "", y = "") +
  scale_y_continuous(labels = function(x) str_c(x, "%")) +
  scale_fill_manual(values = c("sehr wichtig" = "#003c76",
                               "eher wichtig" = "#0098d4",
                               "eher unwichtig" = "#BBB985",
                               "gar nicht wichtig" = "#7e8015")) +
  coord_flip() +
  theme_ipsum(ticks = TRUE) +
  theme(legend.position = "bottom", legend.text = element_text(size = 12), 
        text = element_text(family = "Tahoma", colour = "black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.5),
        axis.ticks.y = element_line(colour = "black", size = 0.5))

# Export ----
library(Cairo)
ggsave(plot = scree_plot.fig, here("figure", "Parallelanalyse.pdf"),
       device = cairo_pdf, dpi = 300)

export(fit.df, "./output/Modelfit.csv", sep = ";")
