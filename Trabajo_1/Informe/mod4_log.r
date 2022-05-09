formula_log = MONTO_FRAUDE_log ~ 
	var_nop_log +var_nop_ant_log + x2_tc_log  + var_nfra_day_log
	+ N_OPERACIONES_log + var_dif_day_log + var_nop_uni_log
	
mco_bruto_log = lm(formula_log, data = entrena_df)
summary(mco_bruto_log)