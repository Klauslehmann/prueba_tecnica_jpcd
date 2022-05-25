
library(tidyverse)
library(rlang)
library(feather)

casen <- read_feather("casen_2020_reducida.feather")

############
# Pregunta 1
############
coinciden <- casen %>% 
  group_by(folio) %>% 
  mutate(tot_per2 = n()) %>% 
  ungroup() %>% 
  mutate(coincide = if_else(tot_per2 == tot_per, 1, 0 ) ) %>% 
  filter(coincide == 1) %>% 
  nrow()

# Solo tenemos valores 1
coinciden == nrow(casen)

############
# Pregunta 2
############

casen %>% 
  mutate(edad_t = case_when(
    edad >= 0 & edad <= 9 ~ 1, 
    edad >= 10 & edad <= 19 ~ 2,
    edad >= 20 & edad <= 29 ~ 3,
    edad >= 30 & edad <= 39 ~ 4,
    edad >= 40 & edad <= 49 ~ 5,
    edad >= 50 & edad <= 59 ~ 6,
    edad >= 60 & edad <= 69 ~ 7,
    edad >= 70 & edad <= 79 ~ 8,
    edad >= 80  ~ 9
  )) %>% 
  group_by(edad_t, sexo) %>% 
  summarise(conteo = sum(expr) )

############
# Pregunta 3
############

resumen <- function(data, var, group) {
  
  name_min <- paste0(var, "_min")
  name_max <- paste0(var, "_max")
  name_mean <- paste0(var, "_mean")
  name_median <- paste0(var, "_median")
  
  data %>% 
    group_by(!!parse_expr(group)) %>% 
    summarise(
      !!parse_expr(name_min) := min(!!parse_expr(var), na.rm = T),
      !!parse_expr(name_max) := max(!!parse_expr(var), na.rm = T),
      !!parse_expr(name_mean) := mean(!!parse_expr(var), na.rm = T),
      !!parse_expr(name_median) := median(!!parse_expr(var), na.rm = T)) 
  
}

casen %>%
  group_by(folio) %>%
  slice(1) %>%
  ungroup() %>%
  resumen( "ytotcorh", "region")


############
# Pregunta 4
############

graficar <- function(data, var, group, title_text = "") {
  name_var <- paste0(var, "_mean")
  mean <- mean(data[[var]], na.rm = T)
  
  data %>% 
    group_by(!!parse_expr(group)) %>% 
    summarise(!!parse_expr(name_var) := mean(!!parse_expr(var), na.rm = T)) %>% 
    ggplot(aes(!!parse_expr(group), !!parse_expr(name_var))) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = mean) +
    labs(title = title_text)
  
  
}

casen %>%
  mutate(across(c("ytotcorh", "region"), as.numeric)) %>% 
  group_by(folio) %>%
  slice(1) %>%
  ungroup() %>% 
  graficar( "ytotcorh", "region", "ingreso promedio por región")


























