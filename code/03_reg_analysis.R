library(tidyverse)
library(lubridate)
#library(vtable) 
library(fixest)        # feols
library(broom)         # tidy()
library(ggplot2)
library(modelsummary)
# =========================
# 1) DATA PREP
# =========================
ds <- read.csv("regression_vars_db_v2.csv")
# save in ../results
#restrict to analysis window to 2018 2019
ds$imagedate <- ymd(ds$imagedate)
ds <- ds %>% filter(imagedate >= as.Date("2018-01-01"),
                    imagedate <  as.Date("2020-01-01"))

# convert distance to Sh to 10km
ds$nearest_fedSh_km<- (ds$nearest_fedSh_km*10)
# State FE
ds$State <- factor(ds$State, levels = c("PA","RO","AC","AM"))


# Controls used in main spec (adjust as appropriate)
controls_main <- c(
  "area_ha",
  'stocking_rate_2013',
  "buffer_forest_perc_2013",
  'forest_perc_2013_2014',
  "precipitation_2018_19",
  "mean_temp_18_19",
  "pop_density_avg_18_19"
)

# =========================
# 2) Supplementary Table S6
# =========================
ds_for_table <-ds %>% dplyr::select(State,n_cattle,area_ha, 
                     pastue_tot_201819_ha,
                     stocking_rate_ha,
                     defo_perc_13_17,
                     nearest_fedSh_km,
                     stocking_rate_2013,
                     buffer_forest_perc_2013,
                     forest_perc_2013_2014,
                     precipitation_2018_19,
                     mean_temp_18_19,
                     pop_density_avg_18_19,
                     defo_perc_2013_14,
                     defo_perc_2014_15,
                     defo_perc_2015_16, 
                     defo_perc_2016_17 )
#vtable::st(ds_for_table, file='results/summarystats.csv')
#vtable is not installable with the latest docker mirror on codeocean
modelsummary::datasummary_skim(
  ds_for_table,
  output = "../results/summary.html"
)
# =========================
# 2) MAIN MODEL (paper)
# =========================
# fixest syntax with FE as character vector
f_main <- as.formula(
  paste0("stocking_rate_ha ~ ",
         paste(c("defo_perc_13_17",
                 "nearest_fedSh_km", 
                 controls_main),
               collapse = " + "))
)

m_main <- feols(f_main, data = ds, fixef = "State",vcov = ~ municipality )

# Extract only headline terms for the plot
plot_terms <- c("defo_perc_13_17", 
                "nearest_fedSh_km")
labels_map <- c(
  defo_perc_13_17 = "Deforestation % (2013–2017)",
  nearest_fedSh_km= "Distance to next \n federal slaughterhouse (in 10 km)"
)

est_main <- broom::tidy(m_main, conf.int = TRUE) %>%
  filter(term %in% plot_terms) %>%
  mutate(term = factor(term, levels = plot_terms, labels = labels_map[plot_terms]))

# Coefficient plot (paper figure)
est_main$term <- factor(
  est_main$term,
  levels = c("Distance to next \n federal slaughterhouse (in 10 km)",
             "Deforestation % (2013–2017)")
)

p_main_pretty <- ggplot(est_main, aes(y = term, x = estimate)) +
  # 0-line
  geom_vline(xintercept = 0,  colour = "grey55") +
  # CI whiskers with end caps
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.18, colour = "black") +
  # point estimate
  geom_point(size = 2.8, shape = 16, colour = "black") +
  # labels
  labs(
    y = NULL,
    x = "Coefficient(95% CI)",
    title = NULL
  ) +
  # tidy, journal-ish theme
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12, margin = margin(r = 6)),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    plot.margin = margin(10, 16, 10, 16),
    panel.grid.major.x = element_line(colour = "grey90"),
    panel.grid.minor = element_blank()
  )

p_main_pretty
ggsave('../results/main_model_coeficients.png', width = 6, height = 2)
# =========================
# 3) SUPPLEMENTARY ROBUSTNESS
# =========================

## 3a) Alternative specifications
# (i) No FE
m_main_noFE <- feols(f_main, data = ds, vcov = ~ municipality)

# (ii) Drop climate controls
#controls_no_climate <- setdiff(controls_main, c("precipitation_2018_19","mean_temp_18_19"))
f_no_sr <- update(f_main, paste(". ~ . -stocking_rate_2013"))
m_no_sr <- feols(f_no_sr, data = ds, fixef = "State", vcov = ~ municipality)

f_main_nodefo <- as.formula(
  paste0("stocking_rate_ha ~ ",
         paste(c( 
                  #"forest_201819_perc", 
                  "nearest_fedSh_km", controls_main),
               collapse = " + "))
)
m_main_nodefo <- feols(f_main_nodefo, data = ds, fixef = "State",vcov = ~ municipality )

# (iii) Add deforestation timing dummies 
defor_time <- c("defo_perc_2013_14","defo_perc_2014_15","defo_perc_2015_16","defo_perc_2016_17")
have_all <- all(defor_time %in% names(ds))
m_timing <- if (have_all) {
  f_timing <- as.formula(paste(
    "stocking_rate_ha ~ +nearest_fedSh_km + ",
    paste(defor_time, collapse = " + ") , "+",
    paste(controls_main, collapse = " + ")
  ))
  feols(f_timing, data = ds, fixef = "State",  vcov = ~ municipality)
} else NULL

m_timing
f_timing_hetero <- as.formula(paste(
  "stocking_rate_ha ~ nearest_fedSh_km +",
  paste(defor_time, collapse = " + "), "+",
  paste(controls_main, collapse = " + ")
))
m_timing_hetero <- feols(
  f_timing_hetero,
  data = ds,
  fixef = "State",
  vcov = "hetero"
)


## 3d) Cluster-robust SEs (clustering unit municipality)
m_main_hetero <- feols(f_main, data = ds, fixef = "State", se='hetero')



model_list <- list(
  "Main (FE)"                        = m_main,
  "No FE"                            = m_main_noFE,
  "FE + Heteroskedasticity-robust SE"= m_main_hetero,
  "Timing controls"                  = m_timing,
  "FE + Timing controls + Hetero SE"     = m_timing_hetero
  # log? 
  
)

# Supplementary Table S7
modelsummary(
  model_list,
  gof_omit = "IC|Log|Adj|Pseudo|F|Within",
  stars = TRUE,
  coef_map = c(
    "defo_perc_13_17"        = "Deforestation (2013–2017, %)",
    "nearest_fedSh_km"       = "Distance to nearest slaughterhouse (10 km)",
    "area_ha"                = "Property size (ha)",
    "stocking_rate_2013"     = "PPM stocking rate (2013, municipality-level)",
    "buffer_forest_perc_2013"= "Forest in 10 km buffer (2013, %)",
    "forest_perc_2013_2014"  = "Forest cover (2013-2014, %)",
    "precipitation_2018_19"  = "Avg. precipitation (2018–2019)",
    "mean_temp_18_19"        = "Avg. temperature (2018–2019)",
    "pop_density_avg_18_19"  = "Population density (2018–2019)",
    "defo_perc_2013_14"      = "Deforestation (2013–2014, %)",
    "defo_perc_2014_15"      = "Deforestation (2014–2015, %)",
    "defo_perc_2015_16"      = "Deforestation (2015–2016, %)",
    "defo_perc_2016_17"      = "Deforestation (2016–2017, %)"
  ),
  output = "../results/Supplementary_Table_S7.html"
)
