# Adaptación BBDD GBARD antiguas:
# Paquetes:
library(tidyverse)
library(openxlsx)
# Seteo directorio de trabajo:
setwd("C:/Users/tgarrido/Desktop/Estudios/GBARD/2022-2023/1. Resultados y presentación")

###############################################################################################
gbard_2020_2021 <- read.csv2(
  "https://raw.githubusercontent.com/TomasGarrido/GBARD/refs/heads/main/2020-2021/BBDD_GBARD_2020_2021.csv") %>% 
  mutate(Gore = ifelse(grepl("GORE", Institución), 1, 0), #identificamos a los GOREs
         Partida_institucional = case_when(
           Gore == 1 ~ "GORE",
           TRUE ~ Ministerio_Poder_Ley))

gbard_corriente_ministerios_2020_2021 <- gbard_2020_2021 %>% 
  group_by(Partida_institucional, Año) %>% 
  summarize(x = sum(GBARD_Miles_corrientes)) %>% 
  pivot_wider(names_from = Año, values_from = x, names_prefix = "Año_")

# Exportar el resultado a una planilla Excel:
write.xlsx(gbard_corriente_ministerios_2020_2021, "gbard_corriente_ministerios_2020_2021.xlsx")

###############################################################################################
gbard_2018_2019 <- read.csv2("https://raw.githubusercontent.com/TomasGarrido/GBARD/refs/heads/main/2018-2019/BdD_GBARD_2018_2019.csv") %>% 
  mutate(Año = ifelse(Presupuesto.I.D.2018 != 0, 2018, ifelse(Presupuesto.I.D.2019 != 0, 2019, 0)),
         Gore = ifelse(grepl("GORE", Institución.del.Sector.Público),1,0),
         Partida_institucional = case_when(
           Gore == 1 ~ "Gore",
           Gore == 0 & Institución.del.Sector.Público == "Poder Judicial" ~ "Poder Judicial",
           Gore == 0 & Institución.del.Sector.Público == "CONAF" ~ "MINAGRI",
           Gore == 0 & Institución.del.Sector.Público == "CONICYT" ~ "MINEDUC",
           Gore == 0 & Institución.del.Sector.Público == "DGAC MeteoChile" ~ "MINDEFENSA",
           Gore == 0 & Institución.del.Sector.Público == "CORFO" ~ "MINECON",
           Gore == 0 & Institución.del.Sector.Público == "DIPRES" ~ "MINHACIENDA",
           Gore == 0 & Institución.del.Sector.Público == "Innova Chile" ~ "MINECON",
           Gore == 0 & Institución.del.Sector.Público == "Ley de Incentivo Tributario a la Inversión Privada en I+D" ~ "Ley I+D",
           Gore == 0 & Institución.del.Sector.Público == "Min. Defensa - SHOA" ~ "MINDEFENSA",
           Gore == 0 & Institución.del.Sector.Público == "Min. Energía - CChEN" ~ "MINENERGIA",
           Gore == 0 & Institución.del.Sector.Público == "Min. Minería - SERNAGEOMIN" ~ "MINMINERIA",
           Gore == 0 & Institución.del.Sector.Público == "Min. RR.EE. - INACh" ~ "MINREL",
           Gore == 0 & Institución.del.Sector.Público == "MINAGRI - FIA" ~ "MINAGRI",
           Gore == 0 & Institución.del.Sector.Público == "MINAGRI - INFOR" ~ "MINAGRI",
           Gore == 0 & Institución.del.Sector.Público == "MINAGRI - INIA" ~ "MINAGRI",
           Gore == 0 & Institución.del.Sector.Público == "MINECON" ~ "MINECON",
           Gore == 0 & Institución.del.Sector.Público == "MINECON - SubPesca" ~ "MINECON",
           Gore == 0 & Institución.del.Sector.Público == "MINECON - SubPESCA" ~ "MINECON",
           Gore == 0 & Institución.del.Sector.Público == "MINEDUC" ~ "MINEDUC",
           Gore == 0 & Institución.del.Sector.Público == "MINEDUC - FGU" ~ "MINEDUC",
           Gore == 0 & Institución.del.Sector.Público == "MINSAL" ~ "MINSAL",
           Gore == 0 & Institución.del.Sector.Público == "MINSAL - ISP" ~ "MINSAL",
           Gore == 0 & Institución.del.Sector.Público == "MOP - INH" ~ "MOP",
           Gore == 0 & Institución.del.Sector.Público == "SAG" ~ "MINAGRI",
           Gore == 0 & Institución.del.Sector.Público == "SEGEGOB" ~ "SEGEGOB",
           Gore == 0 & Institución.del.Sector.Público == "SS Metropolitano Oriente - H. Dr. L. Calvo Mackenna" ~ "MINSAL"
           ))

gbard_corriente_ministerios_2018_2019 <- gbard_2018_2019 %>% 
  group_by(Partida_institucional) %>% 
  summarize(Año_2018 = sum(Presupuesto.I.D.2018),
            Año_2019 = sum(Presupuesto.I.D.2019))

# Exportar el resultado a una planilla Excel:
write.xlsx(gbard_corriente_ministerios_2018_2019, "gbard_corriente_ministerios_2018_2019.xlsx")
###############################################################################################