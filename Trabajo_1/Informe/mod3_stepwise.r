mod_min = lm(MONTO_FRAUDE ~ 1, data = entrena_df)
formula_dic_wise = MONTO_FRAUDE ~ 
	var_nop + var_nop_ant + x2_tc + var_nop_uni + var_dif_day 
	+ x1_tp + DAY_DETECTION + N_FRAUDES_ANTERIORES + N_OPERACIONES 
	+ N_FRAUDES_DIA + FLAG_CLIENTE_EMPRESA + var_nfra_day 
mco_dico_wise = lm(formula_dic_wise, data = entrena_df)

modelo_step_wise = step(mod_min, direction = "forward", 
	scope = formula(mco_dico_wise))
summary(modelo_step_wise)