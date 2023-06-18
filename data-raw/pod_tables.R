# Summarize key facts into a single wide table

# Libraries
library(RSQLite)
library(dplyr)
library(dbplyr)
library(tidyr)
library(stringr)
source("R/utils.R")

# DB ----------------------------------------------------------------------

arquivo_db <- "data-raw/DB_ORIGEM_DESTINO_SP"
# Create a connection with the database
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = arquivo_db)
lista_tbl <- dbListTables(con)


# Overall -----------------------------------------------------------------

overall <- tbl(con, "dados_gerais_por_zona_de_pesquisa_2017")

zones <- sf::st_read("data-raw/zones_od.gpkg")
id_zones <- tidyr::as_tibble(sf::st_drop_geometry(zones))

tab_overall <- overall |>
  select(
    code_zone = COD_ZONA,
    hh = DOMICILIOS,
    fami = FAMILIAS,
    pop = POPULACAO,
    jobs = EMPREGOS,
    cars = AUTOMOVEIS
  ) |>
  mutate(
    pop_household = pop / hh,
    car_rate = cars / fami
  ) |>
  collect()

tab_overall <- id_zones |>
  left_join(tab_overall) |>
  mutate(pop_density = pop / area_ha, jobs_density = jobs / area_ha)

# Demographics ------------------------------------------------------------

age <- tbl(con, "populacao_por_faixa_etaria_e_zona_de_residencia_2017")

age <- age |>
  rename(
    code_zone = COD_ZONA,
    age_group = FAIXA_ETARIA,
    value = QTD) |>
  collect()

df_label <- data.frame(
  age_group = unique(age$age_group),
  age_label = c(rep("young", 5), rep("young-adult", 2), rep("adult", 3), "elder"),
  age_label_ibge = c(rep("young", 4), rep("adult", 6), "elder")
)

tab_age <- age |>
  left_join(df_label, by = "age_group") |>
  group_by(code_zone, age_label_ibge) |>
  summarise(total = sum(value)) |>
  mutate(prop = total / sum(total)) |>
  pivot_wider(
    names_from = "age_label_ibge",
    values_from = c("total", "prop")
  ) |>
  mutate(
    aging_index = total_elder / total_young,
    dry = total_adult / total_young,
    dre = total_adult / total_elder,
    dependency_ratio = dry + dre
  )


# Education ---------------------------------------------------------------

educ <- tbl(con, "populacao_por_grau_de_instrucao_e_zona_de_residencia_2017")

educ <- educ |>
  rename(
    code_zone = COD_ZONA,
    value = QTD,
    educ_level = GRAU_INSTRUCAO
  ) |>
  collect() |>
  mutate(educ_level = janitor::make_clean_names(educ_level))

df_label <- data.frame(
  educ_level = unique(educ$educ_level),
  educ_label = c(
    "Analfabetos ou Fund. Inc.", "Fundamental Comp.", "Fundamental Comp.",
    "Médio Completo", "Superior Completo"
  ),
  educ_code = paste0("educ_", c("analf", "fund", "fund", "medio", "superior"))
)

tab_educ <- educ |>
  left_join(df_label, by = "educ_level") |>
  group_by(code_zone, educ_code) |>
  summarise(total = sum(value)) |>
  group_by(code_zone) |>
  mutate(prop = total / sum (total)) |>
  pivot_wider(
    id_cols = "code_zone",
    names_from = "educ_code",
    values_from = c("total", "prop")
  )


# Income ------------------------------------------------------------------

income <- tbl(con, "populacao_por_faixa_de_renda_familiar_mensal_e_zona_de_residencia_2017")

income <- income |>
  rename(
    code_zone = COD_ZONA,
    income_group = FAIXA_RENDA,
    total = QTD) |>
  collect() |>
  group_by(code_zone) |>
  mutate(prop = total / sum(total)) |>
  ungroup()

df_label <- data.frame(
  income_group = unique(income$income_group),
  income_label_wage = paste0(c("Ate 2", "2 a 4", "4 a 8", "8 a 12", "Mais de 12"), " s.m."),
  income_label = c("low_income", "medium_low_income", "medium_income", "medium_high_income", "high_income")
)

income_group <- left_join(income, df_label, by = "income_group")

tab_income_group <- income_group |>
  pivot_wider(
    id_cols = "code_zone",
    names_from = "income_label",
    values_from = c("total", "prop")
  )

income <- tbl(con, "renda_total_renda_media_familiar_renda_per_capita_e_renda_mediana_familiar_por_zona_de_residencia_2017")

tab_income <- income |>
  rename(
    code_zone = COD_ZONA,
    name = TIPO_RENDA,
    value = QTD
  ) |>
  collect() |>
  mutate(
    name = str_replace(name, "Familiar...3", "income_avg"),
    name = str_replace(name, "Familiar...5", "income_med"),
    name = str_replace(name, "Per Capita", "income_pc"),
    name = factor(name),
    value_ipc = value * ipc_fipe_adjust
  ) |>
  pivot_wider(
    id_cols = "code_zone",
    names_from = "name",
    values_from = "value_ipc"
  )


# Cars --------------------------------------------------------------------

cars <- tbl(con, "familias_por_numero_de_automoveis_particulares_e_zona_de_residencia_2017")

cars <- cars |>
  rename(
    code_zone = COD_ZONA,
    n_cars = NUMERO_AUTOMOVEIS,
    total = QTD
  ) |>
  collect()

tab_cars <- cars |>
  filter(n_cars != "...6") |>
  group_by(code_zone) |>
  mutate(
    n_cars = if_else(
      str_detect(n_cars, "Nenhum"), 0, as.numeric(str_sub(n_cars, 1, 1))
    ),
    prop = total / sum(total)
  ) |>
  pivot_wider(
    id_cols = "code_zone",
    names_from = "n_cars",
    values_from = c("total", "prop"),
    names_prefix = "car_"
  )


# Jobs --------------------------------------------------------------------

jobs <- tbl(con, "empregos_por_classe_de_atividade_e_zona_de_emprego_2017")

# Rename variables and compute zone proportions #
jobs <- jobs |>
  rename(
    code_zone = COD_ZONA,
    jobs_group = EMPREGOS_POR_CLASSE_ATIVIDADE,
    value = QTD
  ) |>
  collect()

jobs <- jobs |>
  mutate(value = as.numeric(value)) |>
  group_by(code_zone) |>
  mutate(prop = value / sum(value))

# Broader employment groups #
df_label <- data.frame(
  jobs_group = unique(jobs$jobs_group),
  jobs_label = c(
    "agriculture", rep("industry", 2), "commerce", rep("services", 8), "pub_adm", "other"
  )
)

# Aggregate into broader groups #
tab_jobs <- jobs |>
  left_join(df_label, by = "jobs_group") |>
  group_by(code_zone, jobs_label) |>
  summarise(total = sum(value)) |>
  group_by(code_zone) |>
  mutate(prop = total / sum(total)) |>
  pivot_wider(
    id_cols = "code_zone",
    names_from = "jobs_label",
    values_from = c("total", "prop"),
    names_prefix = "jobs_"
  )

jobs_sector <- tbl(con, "empregos_por_setor_de_atividade_e_zona_de_emprego_2017")

tab_jobs_sector <- jobs_sector |>
  collect() |>
  rename(
    code_zone = COD_ZONA,
    sector = EMPREGO_POR_SETOR_ATIVIDADE,
    total = QTD
  ) |>
  group_by(code_zone) |>
  mutate(
    sector = case_when(
      sector == "Secundário" ~ "secondary",
      sector == "Terciário" ~ "tertiary",
      sector == "Outros" ~ "other"
    ),
    prop = total / sum(total)
  ) |>
  pivot_wider(
    id_cols = "code_zone",
    names_from = "sector",
    values_from = c("total", "prop"),
    names_prefix = "jobs_")

# Join #
tables <- list(
  tab_overall,
  tab_age,
  tab_educ,
  tab_income,
  tab_income_group,
  tab_jobs
)

joined <- purrr::reduce(tables, full_join, by = "code_zone")

joined <- joined |>
  mutate(across(contains("prop"), ~.x * 100))

# Export to csv
readr::write_csv(joined, "data-raw/table_pod.csv")

zones <- left_join(select(zones, code_zone), joined, by = "code_zone")

zones <- zones %>%
  mutate(across(contains("income"), ~na_if(.x, 0)))

sf::st_write(zones, "data-raw/table_pod.gpkg", append = FALSE)
