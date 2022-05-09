set.seed(3700)# seleccion de semilla
##seleccion de filas 
id_entrena <- sample(1:nrow(MOD_MONTO_FRAUDE_FECHAS),
	size = 0.7*nrow(MOD_MONTO_FRAUDE_FECHAS), replace = FALSE)
entrena <- MOD_MONTO_FRAUDE_FECHAS[id_entrena,]
valida <- MOD_MONTO_FRAUDE_FECHAS[-id_entrena,]