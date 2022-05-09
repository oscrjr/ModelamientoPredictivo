# Creacion variable "Dias de Fraude"
df["dias_fraude"] = df.FECHA_DETECCION - df.FECHA_INICIAL
df["dias_fraude"] = df.dias_fraude.dt.days

# Verificacion de que no existan dias negativos
df[df["FECHA_DETECCION"] < df["FECHA_INICIAL"]]

# Verificacion de cantidad de filas duplicadas
duplicate_rows_df = df[df.duplicated()]
print("number of duplicate rows: ", duplicate_rows_df.shape)

# Verificacion datos faltantes
print(df.isnull().sum())