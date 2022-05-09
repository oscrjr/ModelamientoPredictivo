df["FECHA_INICIAL"] = pd.to_datetime(df['FECHA_INICIAL'],
	format='%d/%m/%Y', infer_datetime_format = True)
df["FECHA_DETECCION"] = pd.to_datetime(df['FECHA_DETECCION'], 
	format='%d/%m/%Y', infer_datetime_format = True)