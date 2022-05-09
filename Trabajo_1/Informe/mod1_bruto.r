formula = MONTO_FRAUDE ~ 
	N_OPERACIONES + FLAG_CLIENTE_EMPRESA + N_FRAUDES_ANTERIORES      
mco_bruto = lm(formula, data = train)
summary(mco_bruto)