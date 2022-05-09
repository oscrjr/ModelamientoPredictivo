# Generacion variable en base a promedios (para numero de operaciones)
df10 = pd.DataFrame(round(train.groupby("SEG_OP")
	.agg({"MONTO_FRAUDE": "mean"})),)

# Generacion variable en base a promedios (para TIPO_PRODUCTO)
df11 = pd.DataFrame(round(train.groupby("TIPO_PRODUCTO ")
	.agg({"MONTO_FRAUDE": "mean"})),)

# Generacion variable en base a promedios (para FLAG_CLIENTE_EMPRESA)
df12 = pd.DataFrame(round(train.groupby("FLAG_CLIENTE_EMPRESA")
	.agg({"MONTO_FRAUDE": "mean"})),)

# Generacion variable en base a promedios (para dias_fraude)
df13 = pd.DataFrame(round(train.groupby("dias_fraude")
	.agg({"MONTO_FRAUDE": "mean"})),)