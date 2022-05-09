train = pd.merge(train, df10, on='SEG_OP')
train = pd.merge(train, df11, on='TIPO_PRODUCTO ')
train = pd.merge(train, df12, on='FLAG_CLIENTE_EMPRESA')
train = pd.merge(train, df13, on='dias_fraude')

# Changing columns name with index number
train.columns.values[13] = "X4_Var"
train.columns.values[12] = "X3_Var"
train.columns.values[11] = "X2_Var"
train.columns.values[10] = "X1_Var"
train.columns.values[1] = "MONTO_FRAUDE"