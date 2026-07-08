#---- Dependencias ----

# Lectura de archivos
library(readr)
# Diseño de encuesta
library(srvyr)
# PCA
library(factoextra)
# Edición de datos
library(dplyr)
library(naniar)
library(MissMech)
library(stringr)
library(magrittr)
library(tidyverse)
library(estimatr)
# Imputación de datos
library(missForest)
library(miceRanger)
# Gráficos/tablas
library(ggplot2)
library(stargazer)
library(modelsummary)
library(ggpubr)
# CatPCA
library(Gifi)

#---- Directorio ----

dirs <- c("/home/fed_scha/Documentos/Rama Antióquia/Datos/",
          "/home/nix_scha/Documentos/Rama Antióquia/Datos/",
          "/home/deb_scha/Documentos/Rama Antióquia/Datos/",
          "/home/cach_scha/Documentos/Rama Antióquia/Datos/")

for (i in dirs) {
  dir_val <- dir.exists(i)
  print(dir_val)
  if (dir_val == T) {
    dir_datos <- i
    setwd(dir_datos)
    break
  }
}

rm(dir_datos,dir_val,dirs,i)

#---- Datos ----

# Semilla
set.seed(20260101)

# Variables
vars <- read.csv("vars.csv")

# Formato numérico
options(scipen=999)

# Datos ECV
## Diseño muestral
dis_mue <- read.csv2("dis_mue.csv")
## Datos de la vivienda
data_viv <- read.csv2("data_viv.csv")
## Fuerza de trabajo
fuerza_tr <- read.csv2("fuerza_tr.csv")
## Características y composición del hogar
ca_co_hogar <- read.csv2("ca_co_hogar.csv")
## Servicios del hogar
serv_hogar <- read.csv2("serv_hogar.csv")
## Condiciones de vida del hogar y tenencia de bienes
cvh_tenbi <- read.csv2("cvh_tenbi.csv")
## Educación
educ <- read.csv2("educación.csv")

#---- Selección de variables ----

# Lista de tablas
ecv <- list("dis_mue" = dis_mue,"data_viv" = data_viv, "fuerza_tr" = fuerza_tr, 
            "ca_co_hogar" = ca_co_hogar,"serv_hogar" = serv_hogar, 
            "cvh_tenbi" = cvh_tenbi, "educ" = educ)

# Eliminar DF's restantes
rm(list = setdiff(ls(), c("ecv","vars")))

# Títulos en minúscula
ecv_2 <- lapply(ecv, function(df){
  names(df) <- tolower(names(df))
  return(df)})

# Selección de variables
ecv_2 <- lapply(X = ecv_2,FUN = function(df){
  df %<>% select(any_of(tolower(vars$Dimensión)))
  return(df)})

# Ambiente global
list2env(x = ecv_2, envir = .GlobalEnv)

#---- Renombrar variables ----

# Componente 1 - Calidad de la vivienda (C1CV)

data_viv %<>% rename("c1cv_zonaubic" = clase,"c1cv_iv_p2102" = p2102,
                     "c1cv_iv_p3155" = p3155,"c1cv_iv_p3156" = p3156,
                     "c1cv_cv_p4005" = p4005,"c1cv_cv_p4015" = p4015,
                     "c1cv_cv_p4567" = p4567, "c1cv_sp_p8520s1" = p8520s1,
                     "c1cv_sp_p8520s5" = p8520s5,"c1cv_sp_p8520s3" = p8520s3,
                     "c1cv_sp_p8520s4" = p8520s4)

# Componente 2 - Calidad del entorno (C2CE)

data_viv %<>% rename("c2ce_pa_p5661s1" = p5661s1,"c2ce_pa_p5661s2" = p5661s2,
                     "c2ce_pa_p5661s3" = p5661s3,"c2ce_pa_p5661s4" = p5661s4,
                     "c2ce_pa_p5661s5" = p5661s5,"c2ce_pa_p5661s6" = p5661s6,
                     "c2ce_pa_p5661s7" = p5661s7,"c2ce_pa_p5661s9" = p5661s9)

# Componente 3 - Ingresos y estilo de vida (C3IV)

serv_hogar %<>% rename("c3iv_hac_perhog" = cant_personas_hogar,
                       "c3iv_hac_p5000" = p5000,
                       "c3iv_hac_p5010" = p5010,
                       "c3iv_ing_inghogar" = i_hogar,
                       "c3iv_ing_ingpercap" = percapita,
                       "c3iv_ing_ingugasto" = i_ugasto)

educ %<>% rename("c3iv_ed_p8587" = p8587)

# Componente 4 - Caracterización del hogar (C4CH)

ca_co_hogar %<>% rename("c4ch_sn_p6020" = p6020,"c4ch_pjh_p6051" = p6051,
                        "c4ch_age_p6040" = p6040)

# Componente 5 - Percepción sentimental (C5PI)

ca_co_hogar %<>% rename("c5pi_meva_p1927" = p1927,
                        "c5pi_mafe_p1901" = p1901,"c5pi_mafe_p1903" = p1903,
                        "c5pi_mafe_p1904" = p1904,"c5pi_meud_p1905" = p1905)

# Componente 6 - Percepción material (C6PM)

cvh_tenbi %<>% rename("c6pm_psf_p9090" = p9090,"c6pm_app_p5230" = p5230,
                      "c6pm_pins_p9010" = p9010)

ca_co_hogar %<>% rename("c6pm_pins_p1898" = p1898)

# Fuerza de trabajo
fuerza_tr %<>% rename("sector_empleo" = p6435)

#---- Datos faltantes - Edición de variables ----

#(°): Sin datos faltantes
#*: Datos faltantes ajustados
#**: Es posible imputar
#***: No es posible imputar

## Datos de la vivienda*
### Datos faltantes
miss_var_summary(data_viv)
data_viv$c1cv_iv_p3155[data_viv$c1cv_iv_p2102 == 1] <- 4
data_viv$c1cv_iv_p3156[is.na(data_viv$c1cv_iv_p3156)] <- 3

## Fuerza de trabajo***
### Datos faltantes
miss_var_summary(fuerza_tr)

## Características y composición del hogar**
# 1: Generación grandiosa
# 2: Generación silenciosa
# 3: Baby boomers
# 4: Generación X
# 5: Generación Y
# 6: Generación Z
# 7: Generación alfa
# 8: Gneración beta
# Referencia: https://es.wikipedia.org/wiki/Generaci%C3%B3n
# Referencia 2: https://shorturl.at/MLcMu
miss_var_summary(ca_co_hogar)
ca_co_hogar %<>% mutate(c4ch_age_anacaprx = 2025-c4ch_age_p6040)
ca_co_hogar %<>% mutate(c4ch_age_gen = case_when(c4ch_age_anacaprx > 1901 &
                                                   c4ch_age_anacaprx <= 1927 ~ 1,
                                                 c4ch_age_anacaprx > 1927 &
                                                   c4ch_age_anacaprx <= 1945 ~ 2,
                                                 c4ch_age_anacaprx > 1945 &
                                                   c4ch_age_anacaprx <= 1964 ~ 3,
                                                 c4ch_age_anacaprx > 1964 &
                                                   c4ch_age_anacaprx <= 1980 ~ 4,
                                                 c4ch_age_anacaprx > 1980 &
                                                   c4ch_age_anacaprx <= 1996 ~ 5,
                                                 c4ch_age_anacaprx > 1996 &
                                                   c4ch_age_anacaprx <= 2010 ~ 6,
                                                 c4ch_age_anacaprx > 2010 &
                                                   c4ch_age_anacaprx <= 2024 ~ 7,
                                                 c4ch_age_anacaprx > 2024 &
                                                   c4ch_age_anacaprx <= 2039 ~ 8))
ca_co_hogar %<>% select(-c4ch_age_anacaprx)

## Servicios del hogar
miss_var_summary(serv_hogar)
serv_hogar %<>% mutate(c3iv_hac_tbhac = c3iv_hac_perhog / c3iv_hac_p5000,
                       c3iv_hac_tnhac = c3iv_hac_perhog / c3iv_hac_p5010,
                       c3iv_ing_linghogar = ifelse(test = c3iv_ing_inghogar == 0,
                                                   yes = 0,
                                                   no = log(x = c3iv_ing_inghogar)),
                       c3iv_ing_lingpercap = ifelse(test = c3iv_ing_ingpercap == 0,
                                                    yes = 0,
                                                    no = log(x = c3iv_ing_ingpercap)),
                       c3iv_ing_lingugasto = ifelse(test = c3iv_ing_ingugasto == 0,
                                                    yes = 0,
                                                    no = log(x = c3iv_ing_ingugasto)))

## Condiciones de vida del hogar y tenencia de bienes°
miss_var_summary(cvh_tenbi)

## Educación***
miss_var_summary(educ)

#---- Diseño muestral - Datos vivienda ----

# Identificador individual
dis_mue %<>% mutate(id_individuo = paste0(directorio,secuencia_p,orden),
                    id_hogar = paste0(directorio,secuencia_p))

# Datos de la vivienda
data_viv %<>% select(-c(secuencia_p,orden,fex_c))

# Diseño muestral - Datos de la vivienda
data_dismue <- left_join(dis_mue,data_viv)

#---- ECV - Identificación ----

## Individuos
ecv_3i <- list("ca_co_hogar" = ca_co_hogar,"educ" = educ,"fuerza_tr" = fuerza_tr)

ecv_3i <- lapply(ecv_3i, function(df){
  df %<>% mutate(id_individuo = paste0(directorio,secuencia_p,orden)) %>% 
    select(-c(directorio,secuencia_p,orden,fex_c))
  return(df)
})

df_1 <- ecv_3i %>% reduce(left_join)
df_ind <- left_join(x = df_1,y = data_dismue)

## Hogares
ecv_3h <- list("serv_hogar" = serv_hogar,"cvh_tenbi" = cvh_tenbi)

ecv_3h <- lapply(ecv_3h, function(df){
  df %<>% mutate(id_hogar = paste0(directorio,orden)) %>% 
    select(-c(directorio,secuencia_p,orden,fex_c))
  return(df)
})

df_2 <- ecv_3h %>% reduce(left_join)

#---- ECV - Final ----

# Compilación de datos
ecv_final <- left_join(x = df_ind,y = df_2)

# Sexo del jefe del hogar
sjh <- ecv_final %>% select(id_hogar,contains("c4")) %>% 
  rename("c4ch_sjh" = c4ch_sn_p6020) %>% filter(c4ch_pjh_p6051 == 1)
sjh %<>% select(id_hogar,c4ch_sjh)
ecv_final <- left_join(ecv_final,sjh)

# Corrección - Educación
ecv_final$c3iv_ed_p8587[ecv_final$c4ch_age_p6040 == 0] <- 1

# DF's intermedios
lista_int <- c("ecv","ecv_2","ecv_3h","ecv_3i","ecv_final","data_dismue","vars")
rm(list = setdiff(ls(),lista_int))

#---- Datos auxiliares - Imputación ----

## Datos auxiliares

ecv_final_2 <- ecv_final %>% select(-sector_empleo)
ecv_final_2 %<>% mutate(c4ch_age_gen = as.integer(c4ch_age_gen),
                        c1cv_iv_p3155 = as.integer(c1cv_iv_p3155),
                        c1cv_iv_p3156 = as.integer(c1cv_iv_p3156),
                        c3iv_ed_p8587 = as.integer(c3iv_ed_p8587),
                        directorio = as.numeric(directorio),
                        secuencia_p = as.numeric(secuencia_p),
                        orden = as.numeric(orden),
                        segmento = as.numeric(segmento),
                        estrato2020 = as.numeric(estrato2020),
                        mpio = as.numeric(mpio),
                        id_individuo = as.factor(id_individuo),
                        id_hogar = as.factor(id_hogar),
                        c4ch_age_p6040 = as.numeric(c4ch_age_p6040),
                        c3iv_hac_perhog = as.numeric(c3iv_hac_perhog),
                        c3iv_hac_p5000 = as.numeric(c3iv_hac_p5000),
                        c3iv_hac_p5010 = as.numeric(c3iv_hac_p5010),
                        p1_departamento = as.numeric(p1_departamento),
                        p1_municipio = as.numeric(p1_municipio))
ecv_final_2 %<>% mutate(across(where(is.integer),as.factor))


#---- Pruebas de imputación - RF ----

# Imputación de datos

# Distribución de datos faltantes (MAR / MCAR / MNAR)
# https://ehsanx.github.io/EpiMethods/missingdata6.html
## Test de Little
### H0: Los datos faltantes son MCAR
### H1: Los datos faltantes no son MCAR

# Random Forest (RF)
## Establecimiento de iteraciones (pruebas = 5)
obb_error <- matrix(data = NA,nrow = 5,ncol = 4)
for (i in 1:5) {
  obb_error[i,1] <- i
  imp_ecv <- missForest(xmis = ecv_final_2,maxiter = i,ntree = 150)
  obb_error[i,4] <- imp_ecv$OOBerror[1]
  obb_error[i,2] <- imp_ecv$OOBerror[2]
  for (j in 1:4){
    obb_error[j+1,3] <- (obb_error[j+1,2] - obb_error[j,2])*100 / obb_error[j+1,2]
  }
}
obb_error_df <- data.frame(IT = obb_error[,1],NRMSE = obb_error[,4],
                           PFC = obb_error[,2], DIFF = obb_error[,3])

## Resultados
### PFC - NRMSE
ggplot(data = obb_error_df,mapping = aes(x = IT,y = PFC)) + 
  geom_point(pch = 8) + geom_line() + scale_x_continuous(breaks = 1:5) +
  theme_classic() + labs(x = "Iteraciones", y = "PFC",
                         title = "Proportion of Falsely Classified",
                         subtitle = "Arboles = 150")
### Variación porcentual PFC
ggplot(data = obb_error_df,mapping = aes(x = IT,y = abs(DIFF))) + 
  geom_point(pch = 8) + geom_line() + scale_x_continuous(breaks = 1:5) +
  theme_classic() + labs(x = "Iteraciones - N. De arboles (I*100)", 
                         y = "V. Porcentual - Valor absoluto")

### Optimo: 4 iteraciones con 150 arboles 
imp_ecv <- missForest(xmis = ecv_final_2,ntree = 150,maxiter = 4)
ecv_final_3rf <- imp_ecv$ximp
ecv_sector_t <- ecv_final %>% select(id_individuo,sector_empleo)
ecv_final_3rf <- left_join(x = ecv_final_3rf,y = ecv_sector_t)

#---- Resultados de imputación - RF ----

# P8587: Nivel educativo
P8587_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c3iv_ed_p8587)),4))
P8587_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P8587_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3rf$c3iv_ed_p8587)),4))
P8587_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P8587 <- full_join(x = P8587_ecvorg,y = P8587_ecvimp)
P8587 %<>% mutate(org = factor(org,levels = c(1,0)))

## Histograma
hist_p8587_rf <- ggplot(data = P8587,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") + 
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución nivel educativo - ECV",
       subtitle = "Random forest (RF)")

# P1927: Medición evaluativa
P1927_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c5pi_meva_p1927)),4))
P1927_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1927_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3rf$c5pi_meva_p1927)),4))
P1927_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1927 <- full_join(x = P1927_ecvorg,y = P1927_ecvimp)
P1927 %<>% mutate(org = factor(org,levels = c(1,0)))

## Histograma
hist_p1927_rf <- ggplot(data = P1927,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") + 
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución medición evaluativa - ECV",
       subtitle = "Random forest (RF)")

# P1901,P1903 & P1904 - Medición afectiva
## P1901
P1901_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c5pi_mafe_p1901)),4))
P1901_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1901_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3rf$c5pi_mafe_p1901)),4))
P1901_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1901 <- full_join(x = P1901_ecvorg,y = P1901_ecvimp)
P1901 %<>% mutate(org = factor(org,levels = c(1,0)))
### Histograma
hist_p1901_rf <- ggplot(data = P1901,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") + 
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución medición afectiva (Felicidad) - ECV",
       subtitle = "Random forest (RF)")

## P1903
P1903_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c5pi_mafe_p1903)),4))
P1903_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1903_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3rf$c5pi_mafe_p1903)),4))
P1903_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1903 <- full_join(x = P1903_ecvorg,y = P1903_ecvimp)
P1903 %<>% mutate(org = factor(org,levels = c(1,0)))
### Histograma
hist_p1903_rf <- ggplot(data = P1903,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") + 
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución medición afectiva (Preocupación) - ECV",
       subtitle = "Random forest (RF)")

## P1904
P1904_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c5pi_mafe_p1904)),4))
P1904_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1904_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3rf$c5pi_mafe_p1904)),4))
P1904_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1904 <- full_join(x = P1904_ecvorg,y = P1904_ecvimp)
P1904 %<>% mutate(org = factor(org,levels = c(1,0)))
### Histograma
hist_p1904_rf <- ggplot(data = P1904,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") + 
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución medición afectiva (Tristeza) - ECV",
       subtitle = "Random forest (RF)")

## P1905
P1905_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c5pi_meud_p1905)),4))
P1905_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1905_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3rf$c5pi_meud_p1905)),4))
P1905_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1905 <- full_join(x = P1905_ecvorg,y = P1905_ecvimp)
P1905 %<>% mutate(org = factor(org,levels = c(1,0)))
### Histograma
hist_p1905_rf <- ggplot(data = P1905,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") +
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución medición eudaimónica - ECV",
       subtitle = "Random forest (RF)")

## P1898
P1898_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c6pm_pins_p1898)),4))
P1898_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1898_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3rf$c6pm_pins_p1898)),4))
P1898_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1898 <- full_join(x = P1898_ecvorg,y = P1898_ecvimp)
P1898 %<>% mutate(org = factor(org,levels = c(1,0)))
### Histograma
hist_p1898_rf <- ggplot(data = P1898,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") +
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución percepción de inseguridad - ECV",
       subtitle = "Random forest (RF)")

lista_hist_rf <- list(hist_p8587_rf,hist_p1927_rf,hist_p1901_rf,hist_p1903_rf,hist_p1904_rf,
                      hist_p1905_rf,hist_p1898_rf)

ggarrange(plotlist = lista_hist_rf,common.legend = T)

#---- Pruebas de imputación - MiceRanger ----

vars_imp <- c("c5pi_meva_p1927","c5pi_mafe_p1901","c5pi_mafe_p1903","c5pi_mafe_p1904",
              "c5pi_meud_p1905","c6pm_pins_p1898","c3iv_ed_p8587")

imp_mr <- miceRanger(data = ecv_final_2,m = 3,maxiter = 6,
                     vars = vars_imp,min.node.size = 10,
                     num.trees = 300,valueSelector = "meanMatch")

ecv_final_3mr <- completeData(imp_mr, datasets = 3)[[1]]
ecv_secemp <- ecv_final %>% select(id_individuo,sector_empleo)
ecv_final_3mr <- left_join(x = ecv_final_3mr,y = ecv_secemp)
rm(ecv_secemp)

#---- Resultados de imputación - MiceRanger ----

# P8587: Nivel educativo
P8587_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c3iv_ed_p8587)),4))
P8587_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P8587_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3mr$c3iv_ed_p8587)),4))
P8587_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P8587 <- full_join(x = P8587_ecvorg,y = P8587_ecvimp)
P8587 %<>% mutate(org = factor(org,levels = c(1,0)))

## Histograma
hist_p8587_mr <- ggplot(data = P8587,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") + 
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución nivel educativo - ECV",
       subtitle = "MICE + Random forest (RF)")

# P1927: Medición evaluativa
P1927_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c5pi_meva_p1927)),4))
P1927_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1927_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3mr$c5pi_meva_p1927)),4))
P1927_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1927 <- full_join(x = P1927_ecvorg,y = P1927_ecvimp)
P1927 %<>% mutate(org = factor(org,levels = c(1,0)))

## Histograma
hist_p1927_mr <- ggplot(data = P1927,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") + 
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución medición evaluativa - ECV",
       subtitle = "MICE + Random forest (RF)")

# P1901,P1903 & P1904 - Medición afectiva
## P1901
P1901_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c5pi_mafe_p1901)),4))
P1901_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1901_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3mr$c5pi_mafe_p1901)),4))
P1901_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1901 <- full_join(x = P1901_ecvorg,y = P1901_ecvimp)
P1901 %<>% mutate(org = factor(org,levels = c(1,0)))
### Histograma
hist_p1901_mr <- ggplot(data = P1901,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") + 
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución medición afectiva (Felicidad) - ECV",
       subtitle = "MICE + Random forest (RF)")

## P1903
P1903_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c5pi_mafe_p1903)),4))
P1903_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1903_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3mr$c5pi_mafe_p1903)),4))
P1903_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1903 <- full_join(x = P1903_ecvorg,y = P1903_ecvimp)
P1903 %<>% mutate(org = factor(org,levels = c(1,0)))
### Histograma
hist_p1903_mr <- ggplot(data = P1903,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") + 
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución medición afectiva (Preocupación) - ECV",
       subtitle = "MICE + Random forest (RF)")

## P1904
P1904_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c5pi_mafe_p1904)),4))
P1904_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1904_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3mr$c5pi_mafe_p1904)),4))
P1904_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1904 <- full_join(x = P1904_ecvorg,y = P1904_ecvimp)
P1904 %<>% mutate(org = factor(org,levels = c(1,0)))
### Histograma
hist_p1904_mr <- ggplot(data = P1904,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") + 
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución medición afectiva (Tristeza) - ECV",
       subtitle = "MICE + Random forest (RF)")

## P1905
P1905_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c5pi_meud_p1905)),4))
P1905_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1905_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3mr$c5pi_meud_p1905)),4))
P1905_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1905 <- full_join(x = P1905_ecvorg,y = P1905_ecvimp)
P1905 %<>% mutate(org = factor(org,levels = c(1,0)))
### Histograma
hist_p1905_mr <- ggplot(data = P1905,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") +
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución medición eudaimónica - ECV",
       subtitle = "MICE + Random forest (RF)")

## P1898
P1898_ecvorg <- as.data.frame(round(prop.table(x = table(ecv_final$c6pm_pins_p1898)),4))
P1898_ecvorg %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 1)
P1898_ecvimp <- as.data.frame(round(prop.table(x = table(ecv_final_3mr$c6pm_pins_p1898)),4))
P1898_ecvimp %<>% rename("cat" = Var1,"ecv" = Freq) %>% mutate(org = 0)
P1898 <- full_join(x = P1898_ecvorg,y = P1898_ecvimp)
P1898 %<>% mutate(org = factor(org,levels = c(1,0)))
### Histograma
hist_p1898_mr <- ggplot(data = P1898,mapping = aes(x = cat,y = ecv,fill = org)) + 
  geom_bar(stat = "identity",position = "dodge",col="black") +
  scale_fill_manual(values = c("0" = "darkblue","1" = "gold"),labels = c("Original","Imputado")) + 
  theme_classic() + scale_y_continuous(labels = scales::percent,n.breaks = 10) + 
  labs(fill = "Tipo de dato",x = "Categoría",y = "Porcentaje",
       title = "Distribución percepción de inseguridad - ECV",
       subtitle = "MICE + Random forest (RF)")

lista_hist_mr <- list(hist_p8587_mr,hist_p1927_mr,hist_p1901_mr,hist_p1903_mr,hist_p1904_mr,
                      hist_p1905_mr,hist_p1898_mr)

ggarrange(plotlist = lista_hist_mr,common.legend = T)

#---- Creación de componentes - Sin imputación ----

# Datos faltantes
ecv_na <- miss_var_summary(data = ecv_final)
names(ecv_na) <- c("Variable","N","%N")
datasummary_df(data = ecv_na,align = "lcc",fmt = 1,
               title = "Datos faltantes - ECV")

# Componente 1: Calidad interna de la vivienda
ecv_c1 <- ecv_final %>% select(id_individuo,starts_with("c1"))
ecv_c1a <- ecv_c1
str(ecv_c1)
rownames(ecv_c1) <- ecv_c1$id_individuo
ecv_c1 %<>% select(-id_individuo)
ecv_c1 %<>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c1,align = "lccc",
                 title = "Componente 1: Calidad interna de la vivienda")

# Componente 2: Calidad del entorno
ecv_c2 <- ecv_final %>% select(id_individuo,starts_with("c2"))
ecv_c2a <- ecv_c2
str(ecv_c2)
rownames(ecv_c2) <- ecv_c2$id_individuo
ecv_c2 %<>% select(-id_individuo)
ecv_c2 %<>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c2,align = "lccc",
                 title = "Componente 2: Calidad del entorno")

# Componente 3: Ingresos y estilo de vida
ecv_c3 <- ecv_final %>% select(id_individuo,starts_with("c3"))
ecv_c3a <- ecv_c3
str(ecv_c3)
rownames(ecv_c3) <- ecv_c3$id_individuo
ecv_c3 %<>% select(-id_individuo) %>% 
  mutate(c3iv_hac_perhog = as.numeric(c3iv_hac_perhog),
         c3iv_hac_p5000 = as.numeric(c3iv_hac_p5000),
         c3iv_hac_p5010 = as.numeric(c3iv_hac_p5010),
         c3iv_ed_p8587 = factor(x = c3iv_ed_p8587,levels = 1:13))
datasummary_skim(data = ecv_c3)

# Componente 4: Características del hogar
ecv_c4 <- ecv_final %>% select(id_individuo,starts_with("c4"))
ecv_c4a <- ecv_c4
str(ecv_c4)
rownames(ecv_c4) <- ecv_c4$id_individuo
ecv_c4 %<>% select(-id_individuo) %>% 
  mutate(c4ch_sn_p6020 = as.factor(c4ch_sn_p6020),
         c4ch_pjh_p6051 = as.factor(c4ch_pjh_p6051),
         c4ch_age_gen = as.factor(c4ch_age_gen),
         c4ch_sjh = as.factor(c4ch_sjh))
datasummary_skim(data = ecv_c4)
ecv_c4p1 <- ecv_c4 %>% select(-c4ch_age_p6040)

# Componente 5: Percepción individual
ecv_c5 <- ecv_final %>% select(id_individuo,starts_with("c5"))
ecv_c5a <- ecv_c5
str(ecv_c5)
rownames(ecv_c5) <- ecv_c5$id_individuo 
ecv_c5 %<>% select(-id_individuo) %>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c5,title = "Componente 5: Percepción sentimental")

# Componente 6: Percepción material
ecv_c6 <- ecv_final %>% select(id_individuo,starts_with("c6"))
ecv_c6a <- ecv_c6
str(ecv_c6)
rownames(ecv_c6) <- ecv_c6$id_individuo 
ecv_c6 %<>% select(-id_individuo) %>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c6,title = "Componente 6: Percepción material")

#---- Creación de componentes - RF ----

# Datos faltantes
ecv_na <- miss_var_summary(data = ecv_final_3rf)
names(ecv_na) <- c("Variable","N","%N")
datasummary_df(data = ecv_na,align = "lcc",fmt = 1,
               title = "Datos faltantes - ECV")

# Componente 1: Calidad interna de la vivienda
ecv_c1 <- ecv_final_3rf %>% select(id_individuo,starts_with("c1"))
ecv_c1a <- ecv_c1
str(ecv_c1)
rownames(ecv_c1) <- ecv_c1$id_individuo
ecv_c1 %<>% select(-id_individuo)
ecv_c1 %<>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c1,align = "lccc",
                 title = "Componente 1: Calidad interna de la vivienda")

# Componente 2: Calidad del entorno
ecv_c2 <- ecv_final_3rf %>% select(id_individuo,starts_with("c2"))
ecv_c2a <- ecv_c2
str(ecv_c2)
rownames(ecv_c2) <- ecv_c2$id_individuo
ecv_c2 %<>% select(-id_individuo)
ecv_c2 %<>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c2,align = "lccc",
                 title = "Componente 2: Calidad del entorno")

# Componente 3: Ingresos y estilo de vida
ecv_c3 <- ecv_final_3rf %>% select(id_individuo,starts_with("c3"))
ecv_c3a <- ecv_c3
str(ecv_c3)
rownames(ecv_c3) <- ecv_c3$id_individuo
ecv_c3 %<>% select(-id_individuo) %>% 
  mutate(c3iv_hac_perhog = as.numeric(c3iv_hac_perhog),
         c3iv_hac_p5000 = as.numeric(c3iv_hac_p5000),
         c3iv_hac_p5010 = as.numeric(c3iv_hac_p5010),
         c3iv_ed_p8587 = factor(x = c3iv_ed_p8587,levels = 1:13))
datasummary_skim(data = ecv_c3)

# Componente 4: Características del hogar
ecv_c4 <- ecv_final_3rf %>% select(id_individuo,starts_with("c4"))
ecv_c4a <- ecv_c4
str(ecv_c4)
rownames(ecv_c4) <- ecv_c4$id_individuo
ecv_c4 %<>% select(-id_individuo) %>% 
  mutate(c4ch_sn_p6020 = as.factor(c4ch_sn_p6020),
         c4ch_pjh_p6051 = as.factor(c4ch_pjh_p6051),
         c4ch_age_gen = as.factor(c4ch_age_gen),
         c4ch_sjh = as.factor(c4ch_sjh))
datasummary_skim(data = ecv_c4)
ecv_c4p1 <- ecv_c4 %>% select(-c4ch_age_p6040)

# Componente 5: Percepción individual
ecv_c5 <- ecv_final_3rf %>% select(id_individuo,starts_with("c5"))
ecv_c5a <- ecv_c5
str(ecv_c5)
rownames(ecv_c5) <- ecv_c5$id_individuo 
ecv_c5 %<>% select(-id_individuo) %>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c5,title = "Componente 5: Percepción sentimental")

# Componente 6: Percepción material
ecv_c6 <- ecv_final_3rf %>% select(id_individuo,starts_with("c6"))
ecv_c6a <- ecv_c6
str(ecv_c6)
rownames(ecv_c6) <- ecv_c6$id_individuo 
ecv_c6 %<>% select(-id_individuo) %>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c6,title = "Componente 6: Percepción material")

#---- Creación de componentes - MiceRanger ----

# Datos faltantes
ecv_na <- miss_var_summary(data = ecv_final_3mr)
names(ecv_na) <- c("Variable","N","%N")
datasummary_df(data = ecv_na,align = "lcc",fmt = 1,
               title = "Datos faltantes - ECV")

# Componente 1: Calidad interna de la vivienda
ecv_c1 <- ecv_final_3mr %>% select(id_individuo,starts_with("c1"))
ecv_c1a <- ecv_c1
str(ecv_c1)
rownames(ecv_c1) <- ecv_c1$id_individuo
ecv_c1 %<>% select(-id_individuo)
ecv_c1 %<>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c1,align = "lccc",
                 title = "Componente 1: Calidad interna de la vivienda")

# Componente 2: Calidad del entorno
ecv_c2 <- ecv_final_3mr %>% select(id_individuo,starts_with("c2"))
ecv_c2a <- ecv_c2
str(ecv_c2)
rownames(ecv_c2) <- ecv_c2$id_individuo
ecv_c2 %<>% select(-id_individuo)
ecv_c2 %<>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c2,align = "lccc",
                 title = "Componente 2: Calidad del entorno")

# Componente 3: Ingresos y estilo de vida
ecv_c3 <- ecv_final_3mr %>% select(id_individuo,starts_with("c3"))
ecv_c3a <- ecv_c3
str(ecv_c3)
rownames(ecv_c3) <- ecv_c3$id_individuo
ecv_c3 %<>% select(-id_individuo) %>% 
  mutate(c3iv_hac_perhog = as.numeric(c3iv_hac_perhog),
         c3iv_hac_p5000 = as.numeric(c3iv_hac_p5000),
         c3iv_hac_p5010 = as.numeric(c3iv_hac_p5010),
         c3iv_ed_p8587 = factor(x = c3iv_ed_p8587,levels = 1:13))
datasummary_skim(data = ecv_c3)

# Componente 4: Características del hogar
ecv_c4 <- ecv_final_3mr %>% select(id_individuo,starts_with("c4"))
ecv_c4a <- ecv_c4
str(ecv_c4)
rownames(ecv_c4) <- ecv_c4$id_individuo
ecv_c4 %<>% select(-id_individuo) %>% 
  mutate(c4ch_sn_p6020 = as.factor(c4ch_sn_p6020),
         c4ch_pjh_p6051 = as.factor(c4ch_pjh_p6051),
         c4ch_age_gen = as.factor(c4ch_age_gen),
         c4ch_sjh = as.factor(c4ch_sjh))
datasummary_skim(data = ecv_c4)
ecv_c4p1 <- ecv_c4 %>% select(-c4ch_age_p6040)

# Componente 5: Percepción individual
ecv_c5 <- ecv_final_3mr %>% select(id_individuo,starts_with("c5"))
ecv_c5a <- ecv_c5
str(ecv_c5)
rownames(ecv_c5) <- ecv_c5$id_individuo 
ecv_c5 %<>% select(-id_individuo) %>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c5,title = "Componente 5: Percepción sentimental")

# Componente 6: Percepción material
ecv_c6 <- ecv_final_3mr %>% select(id_individuo,starts_with("c6"))
ecv_c6a <- ecv_c6
str(ecv_c6)
rownames(ecv_c6) <- ecv_c6$id_individuo 
ecv_c6 %<>% select(-id_individuo) %>% mutate(across(where(is.numeric), factor))
datasummary_skim(data = ecv_c6,title = "Componente 6: Percepción material")

#---- Insumos PCA parcial ----

# Componente 1: Calidad interna de la vivienda
str(ecv_c1)
niv_c1 <- c("nominal","nominal","ordinal","ordinal","nominal","nominal",
            "nominal","ordinal","ordinal","ordinal","ordinal")

# Componente 2: Calidad del entorno
ecv_c2[1:ncol(ecv_c2)] <- lapply(ecv_c2[1:ncol(ecv_c2)], function(x){
  x <- factor(x = x,levels = c(4,3,2,1))
})

# Componente 3: Ingresos y estilo de vida
ecv_c3 %<>% select(-c(c3iv_hac_perhog,c3iv_hac_p5000,c3iv_hac_p5010,
                      c3iv_ing_inghogar,c3iv_ing_ingpercap,c3iv_ing_ingugasto))
str(ecv_c3)
niv_c3 <- c("ordinal","metric","metric","metric","metric","metric")

# Componente 4: Características del hogar
str(ecv_c4p1)
niv_c4 <- c("nominal","nominal","ordinal","nominal")

# Componente 5: Percepción sentimental
str(ecv_c5)
ecv_c5 %<>% mutate(c5pi_meva_p1927 = factor(x = c5pi_meva_p1927,levels = 10:0),
                   c5pi_mafe_p1901 = factor(x = c5pi_mafe_p1901,levels = 10:0),
                   c5pi_mafe_p1903 = factor(x = c5pi_mafe_p1903,levels = 10:0),
                   c5pi_mafe_p1904 = factor(x = c5pi_mafe_p1904,levels = 10:0),
                   c5pi_meud_p1905 = factor(x = c5pi_meud_p1905,levels = 10:0))

# Componente 6: Percepción material

ecv_c6 %<>% mutate(c6pm_pins_p1898 = factor(x = c6pm_pins_p1898,levels = 10:0),
                   c6pm_psf_p9090 = factor(x = c6pm_psf_p9090,levels = 3:1),
                   c6pm_app_p5230 = factor(x = c6pm_app_p5230,levels = c(2,1)),
                   c6pm_pins_p9010 = factor(x = c6pm_pins_p9010,levels = c(2,1)))

#---- PCA's parciales ----

# Componente 1: Calidad interna de la vivienda
catpca_c1 <- princals(data = ecv_c1,ndim = 3,levels = niv_c1)
summary(catpca_c1)
rcatpca_c1 <- data.frame("catpca_c1" = catpca_c1$objectscores[,1])
rcatpca_c1 %<>% mutate(id_individuo = rownames(rcatpca_c1))
rownames(rcatpca_c1) <- 1:nrow(rcatpca_c1)
ecv_c1a <- left_join(x = rcatpca_c1,y = ecv_c1a)

# Componente 2: Calidad del entorno
catpca_c2 <- princals(data = ecv_c2,ndim = 3,ordinal = T)
summary(catpca_c2)
rcatpca_c2 <- data.frame("catpca_c2" = catpca_c2$objectscores[,1]*-1)
rcatpca_c2 %<>% mutate(id_individuo = rownames(rcatpca_c2))
rownames(rcatpca_c2) <- 1:nrow(rcatpca_c2)
ecv_c2a <- left_join(x = rcatpca_c2,y = ecv_c2a)

# Componente 3: Ingresos y estilo de vida
catpca_c3 <- princals(data = ecv_c3,ndim = 3,levels = niv_c3)
summary(catpca_c3)
rcatpca_c3 <- data.frame("catpca_c3" = catpca_c3$objectscores[,1])*-1
rcatpca_c3 %<>% mutate(id_individuo = rownames(rcatpca_c3))
rownames(rcatpca_c3) <- 1:nrow(rcatpca_c3)
ecv_c3a <- left_join(x = rcatpca_c3,y = ecv_c3a)

# Componente 4: Características del hogar
catpca_c4 <- princals(data = ecv_c4p1,ndim = 3,levels = niv_c4)
summary(catpca_c4)
rcatpca_c4 <- data.frame("catpca_c4" = catpca_c4$objectscores[,1])#*-1
rcatpca_c4 %<>% mutate(id_individuo = rownames(rcatpca_c4))
rownames(rcatpca_c4) <- 1:nrow(rcatpca_c4)
ecv_c4a <- left_join(x = rcatpca_c4,y = ecv_c4a)

# Componente 5: Percepción individual
catpca_c5 <- princals(data = ecv_c5,ndim = 3,ordinal = T)#[,-c(7:9)]
summary(catpca_c5)
rcatpca_c5 <- data.frame("catpca_c5" = catpca_c5$objectscores[,1])#*-1
rcatpca_c5 %<>% mutate(id_individuo = rownames(rcatpca_c5))
rownames(rcatpca_c5) <- 1:nrow(rcatpca_c5)
ecv_c5a <- left_join(x = rcatpca_c5,y = ecv_c5a)

# Componente 6: Percepción material
catpca_c6 <- princals(data = ecv_c6,ndim = 3,ordinal = T)#[,-c(7:9)]
summary(catpca_c6)
rcatpca_c6 <- data.frame("catpca_c6" = catpca_c6$objectscores[,1]*-1)
rcatpca_c6 %<>% mutate(id_individuo = rownames(rcatpca_c6))
rownames(rcatpca_c6) <- 1:nrow(rcatpca_c6)
ecv_c6a <- left_join(x = rcatpca_c6,y = ecv_c6a)

#---- Adición de componentes a ECV ----

ecv_final <- left_join(x = ecv_final,y = ecv_c1a)
ecv_final <- left_join(x = ecv_final,y = ecv_c2a)
ecv_final <- left_join(x = ecv_final,y = ecv_c3a)
ecv_final <- left_join(x = ecv_final,y = ecv_c4a)
ecv_final <- left_join(x = ecv_final,y = ecv_c5a)
ecv_final <- left_join(x = ecv_final,y = ecv_c6a)

# DF's Intermedios
rm(ecv_c1,ecv_c2,ecv_c3,ecv_c4,ecv_c5,ecv_c6,
   ecv_c1a,ecv_c2a,ecv_c3a,ecv_c4a,ecv_c4p1,ecv_c5a,ecv_c6a,
   rcatpca_c1,rcatpca_c2,rcatpca_c3,rcatpca_c4,rcatpca_c5,rcatpca_c6)

#---- Índice Integral de bienestar (IIB) ----

# Ordenar variables por componente
ecv_final %<>% relocate(directorio,secuencia_p,orden,id_hogar,id_individuo,
                        segmento,estrato2020,fex_c,p1_departamento,p1_municipio,
                        mpio,sector_empleo,contains("c1"),contains("c2"),
                        contains("c3"),contains("c4"),contains("c5"),contains("c6"))

# Selección de componentes parciales
ecv_final_pca <- ecv_final %>% select(id_individuo,contains("catpca"))
rownames(ecv_final_pca) <- ecv_final_pca$id_individuo
ecv_final_pca %<>% select(-id_individuo)

# Estimación de PCA
pca_iib <- prcomp(x = ecv_final_pca,center = T,scale.=T)
summary(pca_iib)

# Gráfico de sedimentación 
fviz_eig(pca_iib,addlabels = TRUE,ylim = c(0, 50),barfill = "steelblue",
         barcolor = "steelblue",main = "Gráfico de sedimentación",
         xlab = "Componentes Principales",ylab = "Varianza Explicada (%)")

# Circulo de correlación

fviz_pca_var(pca_iib,col.var = "contrib",
             gradient.cols = c("darkgreen","gold","sienna2","red"),
             repel = TRUE,
             title = "Círculo de Correlación")

# Gráfico de individuos

fviz_pca_ind(pca_iib,col.ind = "cos2",
             gradient.cols = c("darkgreen","gold","sienna2","red"),
             geom = "point",alpha.ind = 0.3,title = "Mapa de Individuos",
             xlab = "PC1",ylab = "PC2")

#---- Diseño de encuesta ----

## Personas
de_per_dismue <- ecv_final %>% 
  as_survey_design(ids = segmento,strata = estrato2020,
                   weights = fex_c,nest = T)
## Hogares
de_hog_dismue <- ecv_final %>% filter(orden==1) %>% 
  as_survey_design(ids = segmento,strata = estrato2020,
                   weights = fex_c,nest = T)
## Viviendas
de_viv_dismue <- ecv_final %>% 
  distinct(directorio, .keep_all = TRUE) %>% 
  as_survey_design(ids = segmento,strata = estrato2020,
                   weights = fex_c,nest = T)

#---- Estimaciones ----

## Personas
de_per_dismue %>% summarise(t_per = survey_total()/1000)
## Hogares
de_hog_dismue %>% summarise(t_hog = survey_total()/1000)
## Viviendas
de_viv_dismue %>% summarise(t_viv = survey_total()/1000)

