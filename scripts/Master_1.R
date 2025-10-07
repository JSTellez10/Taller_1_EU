##########################################################
# Taller 1 - Económia Urbana
# Ejercicio 1
# author: Eimmy Nicoll Tovar Escobar y Juan Sebastian Tellez Melo
##########################################################

# Clean the workspace -----------------------------------------------------

rm(list=ls())

# Definir directorios -----------------------------------------------------

users <- tolower(Sys.info()[["user"]])  

rutas <- list(
  usuario = "C:/Users/Usuario/OneDrive - RADDAR/Documentos/Documents/Sebastian Tellez/MAESTRIA/ECONOMIA URBANA/TALLER/TALLER 1/Taller_1_EU/",
  Eimmy  = "C:/Users/mora/Path/To/TALLER/TALLER 1/"
)

root <- rutas[[users]]

setwd(root)

stores <- file.path(root, "stores")
scripts <- file.path(root, "scripts")
views <- file.path(root, "views")

# Load Packages -----------------------------------------------------------

#install.packages("pacman")
require("pacman")

p_load(
  tidyverse,  # Manipulación y visualización de datos
  rio,        # Importar/exportar distintos formatos (Excel, Stata, etc.)
  skimr,      # resúmenes exploratorios rápidos de data frames
  modeldata,  # Datos de ejemplo (incluye Ames Housing)
  stargazer,  # Tablas elegantes para resultados econom?tricos
  broom,      # Resultados de modelos en dataframes limpios
  fixest,     # Estimaciones con efectos fijos y robustas
  dplyr,      # Manipulación de datos (incluido en tidyverse, pero lo dejamos explícito)
  ggplot2,
  stringr,
)

# Cargar datos -----------------------------------------------------------

data_input <- import("stores/dataTaller01_PriceIndeces.rds") %>% as_tibble()




### a) metodología de indice hedónico --------------------------------------------------------

# Inspección inicial del dataframe -----------------------------------------------------------

colnames(data_input)
head(data_input)  # primeras filas
skim(data_input)
table(data_input$year) # frecuencias del año de venta
class(data_input$year)
summary(data_input$sale_price)


# graficas Descriptivas -----------------------------------------------------------
ggplot(data_input, aes(x = sale_price)) + 
  geom_histogram(bins = 50, col= "white")

# Filtramos datos sin NA en building_sqft, num_bedrooms, num_rooms y num_full_baths ----
data_clean<- data_input %>% 
  filter(!is.na(building_sqft)) 
skim(data_clean)

# Transformar el año de venta a factor -----------------------------------------------------------

data_clean <- data_clean %>%
  mutate(
    year_Sold = factor(
      year,
      levels = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      labels = c("d2000", "d2001", "d2002", "d2003", "d2004", "d2005", "d2006", "d2007", "d2008", "d2009", "d2010", "d2011", "d2012", "d2013", "d2014", "d2015", "d2016", "d2017", "d2018", "d2019", "d2020")
    )
  )

table(data_clean$year_Sold)
class(data_clean$year_Sold)

# Añadimos el logaritmo del precio de venta -----------------------------------------------------------

data_clean <- data_clean %>%
  mutate(log_Sale_Price = log(sale_price))

ggplot(data_clean, aes(x = log_Sale_Price)) + 
  geom_histogram(bins = 50, col= "white")

# Añadimos el logaritmo del Superficie del edificio -----------------------------------------------------------

summary(data_clean$building_sqft)

data_clean <- data_clean %>%
  mutate(log_building_sqft = log(building_sqft))

ggplot(data_clean, aes(x = log_building_sqft)) + 
  geom_histogram(bins = 50, col= "white")

# Creamos antiguaedad de la casa

summary(data_clean$year_built)

data_clean <- data_clean %>%
  mutate(age_years = year - year_built)

summary(data_clean$age_years)

ggplot(data_clean, aes(x = age_years)) + 
  geom_histogram(bins = 50, col= "white")

# Creamos numero de baños

data_clean <- data_clean %>%
  mutate(num_baths = num_full_baths + 0.5*num_half_baths)

summary(data_clean$num_baths)

# Modelo central ---------------------------------------------------------

base_year <- sort(unique(data_clean$year))[1]   # primera categoría como base

reg1 <- feols(log_Sale_Price ~
                i(year, ref = base_year) +                   # dummies de año
                log_building_sqft +                          # tamaño
                num_baths + num_bedrooms +                   # habitabilidad
                age_years +                                  # envejecimiento
                num_fireplaces + garage_size +               # amenities
                central_air +                                # servicios
                construction_quality +                       # calidad
                ext_wall_material +                          # materiales
                site_desirability +                          # sitio
                renovation                                   # renovación
              | class + type_of_residence + township_code, # Efectos fijos
              data = data_clean,
              vcov = ~township_code                        # errores clouster
)

summary(reg1)

reg2 <- feols(log_Sale_Price ~
                i(year, ref = base_year) +                   # dummies de año
                log_building_sqft +                          # tamaño
                num_baths + num_bedrooms +                   # habitabilidad
                age_years +                                  # envejecimiento
                num_fireplaces + garage_size +               # amenities
                central_air +                                # servicios
                construction_quality +                       # calidad
                ext_wall_material +                          # materiales
                site_desirability +                          # sitio
                renovation                                   # renovación
              |class  + township_code,                     # Efectos fijos
              data = data_clean,
              vcov = "HC1"                                 # errores robustos
)

summary(reg2)

etable(reg1, reg2)

# Desarrollo de índice ---------------------------------------------------------

m <- reg2  # o reg1

# 1) Extraer β_t y Var(β_t) de los dummies year::2001,...,year::2020
yrs <- 2001:2020
nm  <- paste0("year::", yrs)
bet <- coef(m)[nm]                 # β_t (en el orden de nm)
var <- diag(vcov(m))[nm]           # Var(β_t)

# 2) Construir índice con corrección de Goetzmann y IC95% (base = 2000 → β=0, Var=0)
idx_df_A <- tibble(
  year  = c(2000, yrs),
  beta  = c(0, unname(bet)),
  var_b = c(0, unname(var))
) |>
  mutate(
    se_b      = sqrt(var_b),                     # EE(β_t)
    index     = 100 * exp(beta + 0.5 * var_b),   # Goetzmann (1992)
    se_index  = index * se_b,                    # Delta method: SE(I) ≈ I·SE(β)
    lwr       = index - qnorm(0.975) * se_index, # IC95% inferior
    upr       = index + qnorm(0.975) * se_index  # IC95% superior
  )

# 3) Gráfica: índice y banda de confianza
ggplot(idx_df_A, aes(year, index)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 100, linetype = 2) +
  labs(title = "Índice hedónico (Goetzmann) con IC 95%",
       x = "Año", y = "Índice (base = 100)") +
  theme_minimal()

### b) metodolog´ıa de el estimador de ventas repetidas --------------------------------------------------------

# 1) Preparación: pares consecutivos de ventas (s, t) por inmueble ---------------------------------

# Numerar ventas y filtrar inmuebles con 2+ ventas

house_sales <- data_input %>%
  arrange(pin, year) %>%
  group_by(pin) %>%
  mutate(times_sold = row_number()) %>%
  filter(n() > 1) %>%                 # solo inmuebles con ventas repetidas
  ungroup()

# Construir pares consecutivos: (price0, time0) → (price1, time1)

rep_sales <- house_sales %>%
  arrange(pin, year) %>%
  group_by(pin) %>%
  mutate(price0 = lag(sale_price),
         time0  = lag(year)) %>%
  ungroup() %>%
  filter(!is.na(price0), !is.na(time0)) %>%
  transmute(
    pin,
    price1 = sale_price,  price0 = price0,
    time1  = year,   time0  = time0
  )

# Variable dependiente: diferencia en log-precios (ΔV_i = lnP_t − lnP_s)
rep_sales <- rep_sales %>%
  mutate(dv = log(price1) - log(price0))


# 2) Matriz de diseño D (dummies de tiempo para 2001–2020, base 2000) ---------------

years_all <- 2000:2020
yrs       <- 2001:2020                 # columnas del diseño (base=2000)
n         <- nrow(rep_sales)

# Construir X (n × 20) con la regla {+1, −1, 0}

xmat <- matrix(0, nrow = n, ncol = length(yrs))
for (j in seq_along(yrs)) {
  yrj <- yrs[j]
  xmat[, j] <- ifelse(rep_sales$time1 == yrj,  1,
                      ifelse(rep_sales$time0 == yrj, -1, 0))
}
colnames(xmat) <- paste0("Time", yrs)



# 3) Etapa 1 (MCO): ΔV = D·β + ε (sin intercepto) -------------------------------


fit1 <- lm(rep_sales$dv ~ xmat + 0)   # "+ 0" = sin intercepto (base 2000)
summary(fit1)
e1 <- resid(fit1)                      # residuos etapa 1


# 4) Etapa 2 (Varianza): ajustar Var(ε) ≈ A·Δ + B·Δ² + C y construir pesos -----------

delta <- rep_sales$time1 - rep_sales$time0
fit_var <- lm(I(e1^2) ~ delta + I(delta^2))           # Var(ε) = AΔ + BΔ² + C
vhat    <- pmax(fitted(fit_var), 1e-8)                 # asegurar positividad
wgt     <- 1 / vhat

# 5) Etapa 3 (GLS/MCG): ΔV /√v̂ = (D/√v̂)·β + ε/√v̂ → estimación eficiente de β -------------

fit3 <- lm(rep_sales$dv ~ xmat + 0, weights = wgt)
summary(fit3)

# 6) Índice de precios (base 2000) con corrección de Goetzmann + IC95% ------------------

# Extraer β_t y Var(β_t) en el orden de 2001..2020

nm   <- paste0("xmatTime", yrs)                 # nombres de coeficientes en fit3
beta <- coef(fit3)[nm]
vbet <- diag(vcov(fit3))[nm]

# Tibble del índice (agregando el año base 2000 con β=0, Var=0)

idx_df_B <- tibble(
  year  = c(2000, yrs),
  beta  = c(0, unname(beta)),
  vbeta = c(0, unname(vbet))
) %>%
  mutate(
    se_beta   = sqrt(vbeta),
    index     = 100 * exp(beta + 0.5 * vbeta),      # Goetzmann
    se_index  = index * se_beta,                    # Delta method
    lwr       = index - qnorm(0.975) * se_index,    # IC 95% inferior
    upr       = index + qnorm(0.975) * se_index     # IC 95% superior
  )

# Gráfica: índice (línea) + banda de IC

ggplot(idx_df_B, aes(x = year, y = index)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.20) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 100, linetype = 2) +
  labs(title = "Índice de ventas repetidas (base 2000) — Goetzmann corregido",
       x = "Año", y = "Índice (base = 100)") +
  theme_minimal()


### c) Estimar FE por propiedad, cluster por id --------------------------------------------------------

# Usamos i(year, ref=2000) para fijar explícitamente el año base

reg_fe <- feols(
  log(sale_price) ~ i(year, ref = 2000) | pin,   # FE: id (propiedad)
  data = data_input,
  vcov = ~ pin                                   # errores agrupados por id
)

etable(reg_fe)

# Extraer betas de año y varianzas (2001–2020)

yrs <- 2001:2020
nm  <- paste0("year::", yrs)               # nombres esperados en feols con i(year)
beta <- coef(reg_fe)[nm]                   # β_t
vbet <- diag(vcov(reg_fe))[nm]             # Var(β_t) con clustering por id


# Índice con corrección de Goetzmann + IC95%

idx_df_C <- tibble(
  year  = c(2000, yrs),
  beta  = c(0, unname(beta)),              # β_2000 = 0 por construcción
  vbeta = c(0, unname(vbet))               # Var_2000 = 0
) %>%
  mutate(
    se_beta  = sqrt(vbeta),
    index    = 100 * exp(beta + 0.5 * vbeta),      # Goetzmann (1992)
    se_index = index * se_beta,                     # delta method
    lwr      = index - qnorm(0.975) * se_index,     # IC 95% inferior
    upr      = index + qnorm(0.975) * se_index      # IC 95% superior
  )

#Gráfica del índice con banda de IC

ggplot(idx_df_C, aes(x = year, y = index)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.20) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 100, linetype = 2) +
  labs(title = "Índice (FE por propiedad, errores cluster a nivel id)\nBase 2000 — Goetzmann corregido",
       x = "Año", y = "Índice (base = 100)") +
  theme_minimal()

###  2. Presente los resultados en una grafica --------------------------------------------------------

# 1) Combinar las tres tablas ----------

idx_long <- bind_rows(
  idx_df_A %>%
    transmute(year, index, lwr, upr, metodo = "Hedónico"),
  idx_df_B %>%
    transmute(year, index, lwr, upr, metodo = "Ventas repetidas"),
  idx_df_C %>%
    transmute(year, index, lwr, upr, metodo = "FE por propiedad")
) %>%
  mutate(
    metodo = factor(metodo, levels = c("Hedónico", "Ventas repetidas", "FE por propiedad"))
  )

# 2) Gráfica: tres líneas con sus bandas de confianza (IC 95%)-------

ggplot(idx_long, aes(x = year, y = index, color = metodo)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = metodo), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, linetype = 2) +
  labs(
    title = "Índices de precios: Hedónico vs Ventas repetidas vs FE por propiedad",
    subtitle = "Base = 2000, corrección de Goetzmann, IC 95%",
    x = "Año", y = "Índice (base = 100)", color = "Método", fill = "Método"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )






