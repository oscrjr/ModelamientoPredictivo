formula_dic = MONTO_FRAUDE ~ var_nop + var_nop_ant 
	+ x2_tc + var_nfra_day 
mco_dico = lm(formula_dic, data = entrena_df)
summary(mco_dico)