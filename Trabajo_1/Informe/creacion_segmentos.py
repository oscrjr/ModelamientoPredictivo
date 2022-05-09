conditions = [
    (train.N_OPERACIONES <= 5),
    ((train.N_OPERACIONES <= 11) & (train.N_OPERACIONES >= 6)),
    ((train.N_OPERACIONES > 11)),
]

choices = ['Grupo_1','Grupo_2','Grupo_3']
train['SEG_OP'] = np.select(conditions, choices, default=np.nan)
train.head()