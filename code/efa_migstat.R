
# Load data ----
# ------------------------------------------------------------------------------------------------ #
# Analysis ----

# Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "hrbrthemes", "fixest", "modelsummary",
                  "conflicted", "lubridate", "here", "Cairo", "Hmisc", "gt")

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

# Add category: natives, first and second generation
ib22_pol.df <- ib_nest.df %>%
  filter(sample == "all") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    mig_type = case_when(
      migra == 1 ~ "natives",
      migra != 1 & iso3c != "DEU" ~ "first generation",
      migra != 1 & iso3c == "DEU" ~ "second generation",
      TRUE ~ NA_character_))

# Items
qst.df <- tibble(
  item = names(ib22_pol.df)[4:17],
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

# Prob tables of all democracy items 
# ---------------------------------------------------------------------------- #
# Create tibble of variables to map across
vars <- tibble(
  q = names(ib22_pol.df)[c(4:17)],
  grp = "mig_type",
  w = "weight"
)

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
        group_by(mig_type) %>%
        mutate(prob = n / sum(n) * 100) %>%
        ungroup())

# Make long for plotting
ib22_prob.df <- ib22_prob.df %>%
  bind_rows(.id = "item") %>%
  left_join(y = qst.df, by = "item")

# Plot all items
democracy_mig_status.fig <- ib22_prob.df %>%
  ggplot(aes(x = fct_rev(factor(mig_type, 
                        levels = c("natives", "first generation", "second generation"),
                        labels = c("Natives", "First Generation", "Second Generation"))),
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
       subtitle = "(nach Migrationsstatus der Befragten)",
       fill = "Antwort", x = "", y = "") +
  scale_y_continuous(labels = function(x) str_c(x, "%")) +
  scale_fill_manual(values = c("sehr wichtig" = "#003c76",
                               "eher wichtig" = "#0098d4",
                               "eher unwichtig" = "#BBB985",
                               "gar nicht wichtig" = "#7e8015")) +
  coord_flip() +
  facet_wrap(~q) +
  theme_ipsum(ticks = TRUE, base_family = "Tahoma", base_size = 12, caption_face = "plain",
              axis_col = "black", grid_col = "black") +
  theme(legend.position = "bottom", legend.text = element_text(size = 12), 
        legend.justification = "left",
        plot.caption = element_text(hjust = 0),
        text = element_text(colour = "black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.ticks.x = element_line(colour = "black", linewidth = 0.5),
        axis.ticks.y = element_line(colour = "black", linewidth = 0.5))

# Export
library(Cairo)
ggsave(democracy_mig_status.fig, file = "./figure/Democracy_Prob.pdf", dpi = 330, 
       device = cairo_pdf, height = 14, width = 28)
