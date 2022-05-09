str(entrena)
sum <- summary(entrena)


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
barplot(table(entrena$TIPO_PRODUCTO), horiz =TRUE,las = 1, main = 'Tipo Producto', ylab = 'Tipo')
barplot(table(entrena$FLAG_CLIENTE_EMPRESA), horiz =TRUE,las = 1, main = 'Tipo Cliente', ylab = 'Tipo')

PP = table(entrena$TIPO_PRODUCTO) #calculo de cantidad asociada por categoria 
prop.table(PP)*100 #lo mimos que la linea anterior pero en terminos porcentuales
CC = table(entrena$FLAG_CLIENTE_EMPRESA)
prop.table(CC)*100

##Analisis de Variables discretas "Nº Operaciones y NºFraudes anteriores"
hist(entrena$N_OPERACIONES, ylab = 'Frecuencia',xlab = 'Nº Operaciones', 
     main = 'Histograma', col = rgb(1,0,0,alpha = 0.5), las =1,xlim = c(0,96), breaks = 100)

hist(entrena$N_FRAUDES_ANTERIORES, ylab = 'Frecuencia',xlab = 'Nº Fraudes ant', 
     main = 'Histograma', col = rgb(1,0,0,alpha = 0.5), las =1,xlim = c(0,10), breaks = 10)
