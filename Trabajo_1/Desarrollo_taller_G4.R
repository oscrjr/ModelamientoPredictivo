library(tidyverse)
library(glmnet)
library(ggplot2)
library(broom)
library(MASS)
library(ggcorrplot)
library(rio)
#BY MARCO ANTONIO CID RAMIREZ 
# Carga de Datos

library(readr)
MOD_MONTO_FRAUDE <- read_delim("MODELAMIENTO_MONTO_FRAUDE.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)
MOD_MONTO_FRAUDE_FECHAS = transform(MOD_MONTO_FRAUDE,FECHA_INICIAL = as.Date(FECHA_INICIAL,format="%d/%m/%Y"),FECHA_DETECCION = as.Date(FECHA_DETECCION,format="%d/%m/%Y"))
str(MOD_MONTO_FRAUDE_FECHAS)

#1.-SELECCION DE LAS MUESTRAS.
nrow(MOD_MONTO_FRAUDE_FECHAS)# numero de filas
set.seed(3700)# seleccion de semilla
##seleccion de filas 
id_entrena <- sample(1:nrow(MOD_MONTO_FRAUDE_FECHAS), size = 0.7*nrow(MOD_MONTO_FRAUDE_FECHAS), replace = FALSE)
entrena <- MOD_MONTO_FRAUDE_FECHAS[id_entrena,]
valida <- MOD_MONTO_FRAUDE_FECHAS[-id_entrena,]

#2.- ANÁLISIS DESCRIPTIVO
##Analisis de la variable a explicar Monto_Fraude--> Densidad e histo + box
###Graf 1 --> Densidad 
entrena %>% ggplot(aes(x = MONTO_FRAUDE)) +
  geom_density(fill = "gray")
###Graf 2 --> Histo + boxplot
hist(entrena$MONTO_FRAUDE, ylab = 'Frecuencia',xlab = 'Monto Fraude', 
     main = 'Histograma + Boxplot', col = rgb(1,0,0,alpha = 0.5), las =1)
par(new = TRUE)
boxplot(entrena$MONTO_FRAUDE, horizontal = TRUE,axes = FALSE, lwd = 2, col = rgb(0,0,0,alpha = 0.2))

###Calculo de datos Atipicos de la variable cuantitativa monto fraude
info.montfrad = boxplot(entrena$MONTO_FRAUDE, plot = FALSE) #datos atipicos monto fraude
nda = sum(!is.na(info.montfrad$out))
ndaprop = (sum(!is.na(info.montfrad$out))/nrow(entrena))*100

##Analisis de Variables Nominal "tipo producto y Tipo cliente"
barplot(table(entrena$TIPO_PRODUCTO), horiz =FALSE,las = 1, main = 'Tipo Producto', ylab = 'Tipo')
barplot(table(entrena$FLAG_CLIENTE_EMPRESA), horiz =FALSE,las = 1, main = 'Tipo Cliente', ylab = 'Tipo')

PP = table(entrena$TIPO_PRODUCTO) #calculo de cantidad asociada por categoria 
prop.table(PP)*100 #lo mimos que la linea anterior pero en terminos porcentuales
CC = table(entrena$FLAG_CLIENTE_EMPRESA)
prop.table(CC)*100

##Analisis de Variables discretas "Nº Operaciones y NºFraudes anteriores"
hist(entrena$N_OPERACIONES, ylab = 'Frecuencia',xlab = 'Nº Operaciones', 
     main = 'Histograma', col = rgb(1,0,0,alpha = 0.5), las =1,xlim = c(0,96), breaks = 100)

hist(entrena$N_FRAUDES_ANTERIORES, ylab = 'Frecuencia',xlab = 'Nº Fraudes ant', 
     main = 'Histograma', col = rgb(1,0,0,alpha = 0.5), las =1,xlim = c(0,10), breaks = 100)


#3.-CREACION Y TRASNFORMACION DE VARIABLES
## creacion de variable BASICAS
entrena$DAY_DETECTION = as.numeric(entrena$FECHA_DETECCION - entrena$FECHA_INICIAL)
entrena$N_FRAUDES_DIA = round(entrena$N_OPERACIONES/entrena$DAY_DETECTION,)
entrena$MONTO_DETECTION_DIA = entrena$MONTO_FRAUDE/entrena$DAY_DETECTION
entrena$MONTO_PROM_OPERACION =  entrena$MONTO_FRAUDE/entrena$N_OPERACIONES
#view(entrena)
## creacion de data frame
df_tp = entrena %>%
  dplyr::select(TIPO_PRODUCTO,MONTO_FRAUDE) %>%
  dplyr::group_by(TIPO_PRODUCTO) %>%
  dplyr::summarise(x1_tp = mean(MONTO_FRAUDE)) %>%
  dplyr::select(TIPO_PRODUCTO,x1_tp) #montos promedio de fraude por tipo de producto base entrenamiento

df_tc = entrena %>%
  dplyr::select(FLAG_CLIENTE_EMPRESA,MONTO_FRAUDE) %>%
  dplyr::group_by(FLAG_CLIENTE_EMPRESA) %>%
  dplyr::summarise(x2_tc = mean(MONTO_FRAUDE)) %>%
  dplyr::select(FLAG_CLIENTE_EMPRESA,x2_tc) #montos promedio de fraude por tipo de cliente base entrenamiento

df_nop = entrena %>%
  dplyr::select(N_OPERACIONES,MONTO_FRAUDE) %>%
  dplyr::group_by(N_OPERACIONES) %>%
  dplyr::summarise(x3_nop = mean(MONTO_FRAUDE)) %>%
  dplyr::select(N_OPERACIONES,x3_nop)


df_nop_ant = entrena %>%
  dplyr::select(N_FRAUDES_ANTERIORES,MONTO_FRAUDE) %>%
  dplyr::group_by(N_FRAUDES_ANTERIORES) %>%
  dplyr::summarise(x4_nf_ant = mean(MONTO_FRAUDE)) %>%
  dplyr::select(N_FRAUDES_ANTERIORES,x4_nf_ant)

df_day_det = entrena %>%
  dplyr::select(DAY_DETECTION,MONTO_FRAUDE) %>%
  dplyr::group_by(DAY_DETECTION) %>%
  dplyr::summarise(x5_day_det = mean(MONTO_FRAUDE)) %>%
  dplyr::select(DAY_DETECTION,x5_day_det)

df_nfra_day = entrena %>%
  dplyr::select(N_FRAUDES_DIA,MONTO_FRAUDE) %>%
  dplyr::group_by(N_FRAUDES_DIA) %>%
  dplyr::summarise(x6_nfra_day = mean(MONTO_FRAUDE)) %>%
  dplyr::select(N_FRAUDES_DIA,x6_nfra_day)

#view(df_nfra_day)
## creacion de data frame x grupos para numero de operaciones
df_nop_g0 = entrena %>%
  dplyr::select(N_OPERACIONES) %>%
  dplyr::filter(N_OPERACIONES == 1)%>%
  dplyr::group_by(N_OPERACIONES) %>%
  dplyr::summarise(GRUPO_OP = "GRUPO0_OP")


df_nop_g1 = entrena %>%
  dplyr::select(N_OPERACIONES) %>%
  dplyr::filter(N_OPERACIONES > 1 & N_OPERACIONES <= 5)%>%
  dplyr::group_by(N_OPERACIONES) %>%
  dplyr::summarise(GRUPO_OP = "GRUPO1_OP")


df_nop_g2 = entrena %>%
  dplyr::select(N_OPERACIONES) %>%
  dplyr::filter(N_OPERACIONES >= 6 & N_OPERACIONES <= 10)%>%
  dplyr::group_by(N_OPERACIONES) %>%
  dplyr::summarise(GRUPO_OP = "GRUPO2_OP") %>%
  dplyr::select(N_OPERACIONES,GRUPO_OP)

df_nop_g3 = entrena %>%
  dplyr::select(N_OPERACIONES) %>%
  dplyr::filter(N_OPERACIONES >= 11)%>%
  dplyr::group_by(N_OPERACIONES) %>%
  dplyr::summarise(GRUPO_OP = "GRUPO3_OP") %>%
  dplyr::select(N_OPERACIONES,GRUPO_OP)

df_nop_gx = bind_rows(df_nop_g0, df_nop_g1, df_nop_g2,df_nop_g3,.id = NULL) # tabla segmento de numero de operaciones
#view(df_nop_gx)


## creacion de data frame x grupos para numero de operaciones anteriores

df_nop_ant_g1 = entrena %>%
  dplyr::select(N_FRAUDES_ANTERIORES) %>%
  dplyr::filter(N_FRAUDES_ANTERIORES == 8 | N_FRAUDES_ANTERIORES == 1 | N_FRAUDES_ANTERIORES >= 4 & N_FRAUDES_ANTERIORES <= 6)%>%
  dplyr::group_by(N_FRAUDES_ANTERIORES) %>%
  dplyr::summarise(GRUPO_NF = "GRUPO1_NF") %>%
  dplyr::select(N_FRAUDES_ANTERIORES,GRUPO_NF)

df_nop_ant_g2 = entrena %>%
  dplyr::select(N_FRAUDES_ANTERIORES) %>%
  dplyr::filter(N_FRAUDES_ANTERIORES == 2 | N_FRAUDES_ANTERIORES == 3 | N_FRAUDES_ANTERIORES == 9)%>%
  dplyr::group_by(N_FRAUDES_ANTERIORES) %>%
  dplyr::summarise(GRUPO_NF = "GRUPO2_NF") %>%
  dplyr::select(N_FRAUDES_ANTERIORES,GRUPO_NF)

df_nop_ant_g3 = entrena %>%
  dplyr::select(N_FRAUDES_ANTERIORES) %>%
  dplyr::filter(N_FRAUDES_ANTERIORES == 7 | N_FRAUDES_ANTERIORES == 10)%>%
  dplyr::group_by(N_FRAUDES_ANTERIORES) %>%
  dplyr::summarise(GRUPO_NF = "GRUPO3_NF") %>%
  dplyr::select(N_FRAUDES_ANTERIORES,GRUPO_NF)

df_nop_ant_g4 = entrena %>%
  dplyr::select(N_FRAUDES_ANTERIORES) %>%
  dplyr::filter(N_FRAUDES_ANTERIORES == 0)%>%
  dplyr::group_by(N_FRAUDES_ANTERIORES) %>%
  dplyr::summarise(GRUPO_NF = "GRUPO4_NF") %>%
  dplyr::select(N_FRAUDES_ANTERIORES,GRUPO_NF)

df_nop_ant_gx = bind_rows(df_nop_ant_g1, df_nop_ant_g2,df_nop_ant_g3,df_nop_ant_g4,.id = NULL)
#view(df_nop_ant_gx)

## creacion de data frame x grupos para numero de operaciones contra el monto unitario x operacion
df_nop_uni_g1 = entrena %>%
  dplyr::select(N_OPERACIONES) %>%
  dplyr::filter(N_OPERACIONES == 1)%>%
  dplyr::group_by(N_OPERACIONES) %>%
  dplyr::summarise(GRUPO_OP_UNI = "GRUPO1_OP_UNI")


df_nop_uni_g2 = entrena %>%
  dplyr::select(N_OPERACIONES) %>%
  dplyr::filter(N_OPERACIONES != 1 & N_OPERACIONES != 13 & N_OPERACIONES != 24 & N_OPERACIONES != 25 & N_OPERACIONES != 35 & N_OPERACIONES != 11 )%>% 
  dplyr::group_by(N_OPERACIONES) %>%
  dplyr::summarise(GRUPO_OP_UNI = "GRUPO2_OP_UNI") %>%
  dplyr::select(N_OPERACIONES,GRUPO_OP_UNI)

df_nop_uni_g3 = entrena %>%
  dplyr::select(N_OPERACIONES) %>%
  dplyr::filter(N_OPERACIONES == 11 | N_OPERACIONES == 13| N_OPERACIONES == 24| N_OPERACIONES == 25| N_OPERACIONES == 35)%>%
  dplyr::group_by(N_OPERACIONES) %>%
  dplyr::summarise(GRUPO_OP_UNI = "GRUPO3_OP_UNI") %>%
  dplyr::select(N_OPERACIONES,GRUPO_OP_UNI)

df_nop_uni_gx = bind_rows(df_nop_uni_g1, df_nop_uni_g2,df_nop_uni_g3,.id = NULL) 
#view(df_nop_uni_gx)

## creacion de data frame x grupos para numero de fraudes por dia 
df_nfra_day_g1 = entrena %>%
  dplyr::select(N_FRAUDES_DIA) %>%
  dplyr::filter(N_FRAUDES_DIA <= 7)%>%
  dplyr::group_by(N_FRAUDES_DIA) %>%
  dplyr::summarise(GRUPO_FRA_DAY = "GRUPO1_FRA_DAY")


df_nfra_day_g2 = entrena %>%
  dplyr::select(N_FRAUDES_DIA) %>%
  dplyr::filter(N_FRAUDES_DIA > 7 )%>% 
  dplyr::group_by(N_FRAUDES_DIA) %>%
  dplyr::summarise(GRUPO_FRA_DAY = "GRUPO2_FRA_DAY")


df_nfra_day_gx = bind_rows(df_nfra_day_g1, df_nfra_day_g2,.id = NULL) 
#view(df_nfra_day_gx)


# tabla de entrenamiento consolidada 1 con segmentaciones
entrena_a = dplyr::inner_join(entrena, df_nop_gx, by = c("N_OPERACIONES" = "N_OPERACIONES"))
entrena_b = dplyr::inner_join(entrena_a, df_nop_ant_gx, by = c("N_FRAUDES_ANTERIORES" = "N_FRAUDES_ANTERIORES")) 
entrena_c = dplyr::inner_join(entrena_b, df_nop_uni_gx, by = c("N_OPERACIONES" = "N_OPERACIONES"))
entrena_seg = dplyr::inner_join(entrena_c, df_nfra_day_gx, by = c("N_FRAUDES_DIA" = "N_FRAUDES_DIA"))
#view(entrena_seg)

# calculos de data frames con montos promedios y montos promedios unitarios para una posterior dicotomizacion 

df_nop_mont = entrena_seg %>%
  dplyr::select(GRUPO_OP,MONTO_FRAUDE) %>%
  dplyr::group_by(GRUPO_OP) %>%
  dplyr::summarise(var_nop = mean(MONTO_FRAUDE)) %>%
  dplyr::select(GRUPO_OP,var_nop) #montos promedio de fraude por segmento de numero de operaciones base entrenamiento

df_nop_ant_mont = entrena_seg %>%
  dplyr::select(GRUPO_NF,MONTO_FRAUDE) %>%
  dplyr::group_by(GRUPO_NF) %>%
  dplyr::summarise(var_nop_ant = mean(MONTO_FRAUDE)) %>%
  dplyr::select(GRUPO_NF,var_nop_ant) #montos promedio de fraude por segmento de numero de fraudes pasados base entrenamiento

df_nop_mont_uni = entrena_seg %>%
  dplyr::select(GRUPO_OP_UNI,MONTO_PROM_OPERACION) %>%
  dplyr::group_by(GRUPO_OP_UNI) %>%
  dplyr::summarise(var_nop_uni = mean(MONTO_PROM_OPERACION)) %>%
  dplyr::select(GRUPO_OP_UNI,var_nop_uni)

df_dif_mont_day = entrena_seg %>%
  dplyr::select(DAY_DETECTION,MONTO_DETECTION_DIA) %>%
  dplyr::group_by(DAY_DETECTION) %>%
  dplyr::summarise(var_dif_day = mean(MONTO_DETECTION_DIA)) %>%
  dplyr::select(DAY_DETECTION,var_dif_day)

df_fra_mont_day = entrena_seg %>%
  dplyr::select(GRUPO_FRA_DAY,MONTO_DETECTION_DIA) %>%
  dplyr::group_by(GRUPO_FRA_DAY) %>%
  dplyr::summarise(var_nfra_day = mean(MONTO_DETECTION_DIA)) %>%
  dplyr::select(GRUPO_FRA_DAY,var_nfra_day)

### DICOTOMIZACION ENTRENAMIENTO
entrena_df = dplyr::inner_join(entrena_seg, df_nop_mont, by = c("GRUPO_OP" = "GRUPO_OP"))
entrena_df = dplyr::inner_join(entrena_df, df_nop_ant_mont, by = c("GRUPO_NF" = "GRUPO_NF"))
entrena_df = dplyr::inner_join(entrena_df, df_nop_mont_uni, by = c("GRUPO_OP_UNI" = "GRUPO_OP_UNI"))
entrena_df = dplyr::inner_join(entrena_df, df_dif_mont_day, by = c("DAY_DETECTION" = "DAY_DETECTION"))
entrena_df = dplyr::inner_join(entrena_df, df_tp, by = c("TIPO_PRODUCTO" = "TIPO_PRODUCTO"))
entrena_df = dplyr::inner_join(entrena_df, df_tc, by = c("FLAG_CLIENTE_EMPRESA" = "FLAG_CLIENTE_EMPRESA"))
entrena_df = dplyr::inner_join(entrena_df, df_fra_mont_day, by = c("GRUPO_FRA_DAY" = "GRUPO_FRA_DAY"))
#view(entrena_df)

#4.-MODELAMIENTO.
## modelo completo en bruto 

formula = MONTO_FRAUDE ~ N_OPERACIONES + FLAG_CLIENTE_EMPRESA + N_FRAUDES_ANTERIORES      
mco_bruto = lm(formula, data = entrena_seg)
summary(mco_bruto)

## modelo completo dicotomizado 
#str(entrena_df)
formula_dic = MONTO_FRAUDE ~ var_nop + var_nop_ant + x2_tc + var_nfra_day 
mco_dico = lm(formula_dic, data = entrena_df)
summary(mco_dico)

## modelo completo dicotomizado stepwise
mod_min = lm(MONTO_FRAUDE ~ 1, data = entrena_df)
formula_dic_wise = MONTO_FRAUDE ~ var_nop + var_nop_ant + x2_tc + var_nop_uni + var_dif_day + x1_tp + DAY_DETECTION + N_FRAUDES_ANTERIORES + N_OPERACIONES + N_FRAUDES_DIA + FLAG_CLIENTE_EMPRESA + var_nfra_day 
mco_dico_wise = lm(formula_dic_wise, data = entrena_df)

modelo_step_wise = step(mod_min, direction = "forward", scope = formula(mco_dico_wise))
summary(modelo_step_wise)

## modelo logaritmico
#transformacion de variables 
entrena_df$MONTO_FRAUDE_log = log(entrena_df$MONTO_FRAUDE)
entrena_df$N_OPERACIONES_log = log(entrena_df$N_OPERACIONES)
entrena_df$DAY_DETECTION_log = log(entrena_df$DAY_DETECTION)
entrena_df$MONTO_DETECTION_DIA_log = log(entrena_df$MONTO_DETECTION_DIA)
entrena_df$MONTO_PROM_OPERACION_log = log(entrena_df$MONTO_PROM_OPERACION)
entrena_df$var_nop_log = log(entrena_df$var_nop)
entrena_df$var_nop_ant_log = log(entrena_df$var_nop_ant)
entrena_df$var_nop_uni_log = log(entrena_df$var_nop_uni)
entrena_df$var_dif_day_log = log(entrena_df$var_dif_day)
entrena_df$x1_tp_log = log(entrena_df$x1_tp)
entrena_df$x2_tc_log = log(entrena_df$x2_tc)
entrena_df$var_nfra_day_log = log(entrena_df$var_nfra_day)

formula_log = MONTO_FRAUDE_log ~ var_nop_log +var_nop_ant_log + x2_tc_log  + var_nfra_day_log + N_OPERACIONES_log + var_dif_day_log + var_nop_uni_log
mco_bruto_log = lm(formula_log, data = entrena_df)
summary(mco_bruto_log)

#5.-SELECCION DE MODELO.
## Metricas de entrenamiento
### calculo de metricas modelo bruto
train_pred_bruto = entrena_seg %>% mutate(pred_modelo_monto_fraude = predict(mco_bruto ,entrena_seg),
                                    error_modelo_monto_fraude = MONTO_FRAUDE - pred_modelo_monto_fraude)

metricas_train_bruto = train_pred_bruto %>% 
  summarise(Modelo = "bruto", ECM = mean(error_modelo_monto_fraude^2), RECM = sqrt(ECM),
            MAE = mean(abs(error_modelo_monto_fraude)), MAPE = mean(abs(error_modelo_monto_fraude/MONTO_FRAUDE)))

### calculo de metricas modelo dicotomizado
train_pred_dico = entrena_df %>% mutate(pred_mod_dic_monto_fraude = predict(mco_dico ,entrena_df),
                                     error_mod_dic_monto_fraude = MONTO_FRAUDE - pred_mod_dic_monto_fraude)

metricas_train_dico = train_pred_dico %>% 
  summarise(Modelo = "dico", ECM = mean(error_mod_dic_monto_fraude^2), RECM = sqrt(ECM),
            MAE = mean(abs(error_mod_dic_monto_fraude)), MAPE = mean(abs(error_mod_dic_monto_fraude/MONTO_FRAUDE)))

### calculo de metricas modelo stepwise
train_pred_wise = entrena_df %>% mutate(pred_mod_wise_monto_fraude = predict(modelo_step_wise ,entrena_df),
                                     error_mod_wise_monto_fraude = MONTO_FRAUDE - pred_mod_wise_monto_fraude)

metricas_train_wise = train_pred_wise %>% 
  summarise(Modelo = "wise", ECM = mean(error_mod_wise_monto_fraude^2), RECM = sqrt(ECM),
            MAE = mean(abs(error_mod_wise_monto_fraude)), MAPE = mean(abs(error_mod_wise_monto_fraude/MONTO_FRAUDE)))

### calculo de metricas modelo logaritmico
train_pred_bruto_log = entrena_df %>% mutate(pred_modelo_monto_fraude_log = exp(predict(mco_bruto_log ,entrena_df)),
                                              error_modelo_monto_fraude_log = exp(MONTO_FRAUDE_log) - pred_modelo_monto_fraude_log)

metricas_train_bruto_log = train_pred_bruto_log %>% 
  summarise(Modelo = "bruto_log", ECM = mean(error_modelo_monto_fraude_log^2), RECM = sqrt(ECM),
            MAE = mean(abs(error_modelo_monto_fraude_log)), MAPE = mean(abs(error_modelo_monto_fraude_log/exp(MONTO_FRAUDE_log))))




#### Resumen de metricas de entrenamiento
metricas_entrenamiento_MCO = bind_rows(metricas_train_bruto,metricas_train_dico,metricas_train_wise,metricas_train_bruto_log) 

metricas_entrenamiento_MCO

#export(train_pred_bruto_log,'salida1.xlsx')
#-------------------------------------------------------------------------------------------------------------------
# TRABAJO SOBRE LA BASE DE DATOS DE VALIDACION
### CREACION DE VARIABLES BASICAS
valida$DAY_DETECTION = as.numeric(valida$FECHA_DETECCION - valida$FECHA_INICIAL)
valida$N_FRAUDES_DIA = round(valida$N_OPERACIONES/valida$DAY_DETECTION,)
### SEGMENTACION DE VARIABLES 
valida_a = dplyr::inner_join(valida, df_nop_gx, by = c("N_OPERACIONES" = "N_OPERACIONES"))
valida_b = dplyr::inner_join(valida_a, df_nop_ant_gx, by = c("N_FRAUDES_ANTERIORES" = "N_FRAUDES_ANTERIORES")) 
valida_c = dplyr::inner_join(valida_b, df_nop_uni_gx, by = c("N_OPERACIONES" = "N_OPERACIONES"))
valida_seg = dplyr::inner_join(valida_c, df_nfra_day_gx, by = c("N_FRAUDES_DIA" = "N_FRAUDES_DIA"))
#view(valida_seg)
### DICOTOMIZACION VALIDACION
valida_df = dplyr::inner_join(valida_seg, df_nop_mont, by = c("GRUPO_OP" = "GRUPO_OP"))
valida_df = dplyr::inner_join(valida_df, df_nop_ant_mont, by = c("GRUPO_NF" = "GRUPO_NF"))
valida_df = dplyr::inner_join(valida_df, df_nop_mont_uni, by = c("GRUPO_OP_UNI" = "GRUPO_OP_UNI"))
valida_df = dplyr::inner_join(valida_df, df_dif_mont_day, by = c("DAY_DETECTION" = "DAY_DETECTION"))
valida_df = dplyr::inner_join(valida_df, df_tp, by = c("TIPO_PRODUCTO" = "TIPO_PRODUCTO"))
valida_df = dplyr::inner_join(valida_df, df_tc, by = c("FLAG_CLIENTE_EMPRESA" = "FLAG_CLIENTE_EMPRESA"))
valida_df = dplyr::inner_join(valida_df, df_fra_mont_day, by = c("GRUPO_FRA_DAY" = "GRUPO_FRA_DAY"))
#view(valida_df)
### TRANSFORMACION DE VARIABLES 
valida_df$MONTO_FRAUDE_log = log(valida_df$MONTO_FRAUDE)
valida_df$N_OPERACIONES_log = log(valida_df$N_OPERACIONES)
valida_df$DAY_DETECTION_log = log(valida_df$DAY_DETECTION)
valida_df$var_nop_log = log(valida_df$var_nop)
valida_df$var_nop_ant_log = log(valida_df$var_nop_ant)
valida_df$var_nop_uni_log = log(valida_df$var_nop_uni)
valida_df$var_dif_day_log = log(valida_df$var_dif_day)
valida_df$x1_tp_log = log(valida_df$x1_tp)
valida_df$x2_tc_log = log(valida_df$x2_tc)
valida_df$var_nfra_day_log = log(valida_df$var_nfra_day)
#view(valida_df)

#5.-SELECCION DE MODELO.
## Metricas de entrenamiento
### calculo de metricas modelo bruto
vali_pred_bruto = valida_seg %>% mutate(pred_modelo_monto_fraude = predict(mco_bruto ,valida_seg),
                                          error_modelo_monto_fraude = MONTO_FRAUDE - pred_modelo_monto_fraude)

metricas_vali_bruto = vali_pred_bruto %>% 
  summarise(Modelo = "bruto_val", ECM = mean(error_modelo_monto_fraude^2), RECM = sqrt(ECM),
            MAE = mean(abs(error_modelo_monto_fraude)), MAPE = mean(abs(error_modelo_monto_fraude/MONTO_FRAUDE)))

### calculo de metricas modelo dicotomizado
vali_pred_dico = valida_df %>% mutate(pred_mod_dic_monto_fraude = predict(mco_dico ,valida_df),
                                        error_mod_dic_monto_fraude = MONTO_FRAUDE - pred_mod_dic_monto_fraude)

metricas_vali_dico = vali_pred_dico %>% 
  summarise(Modelo = "dico_val", ECM = mean(error_mod_dic_monto_fraude^2), RECM = sqrt(ECM),
            MAE = mean(abs(error_mod_dic_monto_fraude)), MAPE = mean(abs(error_mod_dic_monto_fraude/MONTO_FRAUDE)))

### calculo de metricas modelo stepwise
vali_pred_wise = valida_df %>% mutate(pred_mod_wise_monto_fraude = predict(modelo_step_wise ,valida_df),
                                        error_mod_wise_monto_fraude = MONTO_FRAUDE - pred_mod_wise_monto_fraude)

metricas_vali_wise = vali_pred_wise %>% 
  summarise(Modelo = "wise_val", ECM = mean(error_mod_wise_monto_fraude^2), RECM = sqrt(ECM),
            MAE = mean(abs(error_mod_wise_monto_fraude)), MAPE = mean(abs(error_mod_wise_monto_fraude/MONTO_FRAUDE)))

### calculo de metricas modelo logaritmico
vali_pred_bruto_log = valida_df %>% mutate(pred_modelo_monto_fraude_log = exp(predict(mco_bruto_log ,valida_df)),
                                             error_modelo_monto_fraude_log = exp(MONTO_FRAUDE_log) - pred_modelo_monto_fraude_log)

metricas_vali_bruto_log = vali_pred_bruto_log %>% 
  summarise(Modelo = "bruto_log", ECM = mean(error_modelo_monto_fraude_log^2), RECM = sqrt(ECM),
            MAE = mean(abs(error_modelo_monto_fraude_log)), MAPE = mean(abs(error_modelo_monto_fraude_log/exp(MONTO_FRAUDE_log))))




#### Resumen de metricas de entrenamiento
metricas_validaciono_MCO = bind_rows(metricas_vali_bruto,metricas_vali_dico,metricas_vali_wise,metricas_vali_bruto_log) 
metricas_entrenamiento_MCO
metricas_validaciono_MCO

# ANALISIS DESCRIPTIVO
str(entrena_df)
summary(entrena_df)
##Analisis de Correlaciones 
entrena_df %>% 
  dplyr::select(MONTO_FRAUDE, TIPO_PRODUCTO, FLAG_CLIENTE_EMPRESA, N_OPERACIONES, N_FRAUDES_ANTERIORES) %>% 
  plot()

entrena_df %>% 
  dplyr::select(MONTO_FRAUDE, FLAG_CLIENTE_EMPRESA,N_OPERACIONES,N_FRAUDES_ANTERIORES) %>% 
  cor %>% 
  ggcorrplot(lab = TRUE)

#VERSION LOGARITMICA
entrena_df %>% 
  dplyr::select(MONTO_FRAUDE_log,var_nop_log,var_nop_ant_log,var_nop_uni_log,var_dif_day_log,x1_tp_log,x2_tc_log,var_nfra_day_log) %>% 
  cor %>% 
  ggcorrplot(lab = TRUE)

View(entrena_df)


boxplot(entrena_df$MONTO_FRAUDE ~ entrena_df$TIPO_PRODUCTO, col = c('green','orange')) 
boxplot(entrena_df$MONTO_FRAUDE ~ entrena_df$FLAG_CLIENTE_EMPRESA, col = c('skyblue','yellow'))
boxplot(entrena_df$MONTO_FRAUDE ~ entrena_df$GRUPO_OP, col = c('skyblue','yellow','green','orange'))
boxplot(entrena_df$MONTO_FRAUDE ~ entrena_df$GRUPO_NF, col = c('skyblue','yellow','green','orange'))
boxplot(entrena_df$MONTO_FRAUDE ~ entrena_df$GRUPO_OP_UNI, col = c('skyblue','yellow','green'))
boxplot(entrena_df$MONTO_FRAUDE ~ entrena_df$GRUPO_FRA_DAY, col = c('skyblue','yellow'))
boxplot(entrena_df$MONTO_FRAUDE ~ entrena_df$DAY_DETECTION, col = c('skyblue','yellow','green','orange','blue','red'))
