#=====================================
#
# Prep SOE query extracted data - TREATMENT
#
#=====================================

#-------------------------------------------------------------
# Carregando dados
#-------------------------------------------------------------

cat(paste0("Loading SOE data...\n"))

path_to_read <-  paste0("./data/processed/",settings$dataref_soe_query_extraction,"/") # definição do caminho do arquivo que desejo ler
load(paste0(path_to_read,"dt_soe.rds"))

dt <- dt_soe

#-------------------------------------------------------------
# Removendo duplicatas de requerimentos
#-------------------------------------------------------------

# Não necessário para esta etapa, espaço reservado para futuras extrações



#-------------------------------------------------------------
# Atualizar UGRHI-Gerência-Divisão
#-------------------------------------------------------------

cat(paste0("Loading UGRHI-Gerência-Divisão data...\n"))
load("./data/processed/dt_ugrhi_gerencia_divisao.rds")

dt <- merge(dt[,-c("GERENCIAS","NOME_GERENCIAS","DIVISÃO")] 
            ,dt_ugrhi_gerencia_divisao[,c("UGRHI","GERÊNCIA","DIVISÃO")]
            , by = c("UGRHI"), all.x = TRUE)

head(dt)


#-------------------------------------------------------------
# Variáveis de apoio para selecionar usos vigentes
#-------------------------------------------------------------

# Definindo TIPO_REQUERIMENTO_LEGADO

dt[, TIPO_REQUERIMENTO_LEGADO := fcase(
  
  # REGRA 1: Se TIPO_REQUERIMENTO NÃO estiver vazio, mantém o valor original.
  # !is.na verifica se não é nulo; != "" verifica se não é um texto vazio.
  !is.na(TIPO_REQUERIMENTO) & TIPO_REQUERIMENTO != "", TIPO_REQUERIMENTO,
  
  # REGRA 2: Se estava vazio e TIPO_REQUERIMENTO_SOE for "Dispensa de Outorga"
  TIPO_REQUERIMENTO_SOE == "Dispensa de Outorga", "DISPENSA DE OUTORGA",
  
  # REGRA 3: Se estava vazio e TIPO_REQUERIMENTO_SOE for "Cadastro de Usos"
  TIPO_REQUERIMENTO_SOE == "Cadastro de Usos", "CADASTRO DE USOS",
  
  # REGRA PADRÃO (O último "SE" da sua fórmula): 
  # Se nenhuma das condições acima for atendida, define como "Legado"
  default = "Legado"
)
]

# Definindo Dummy de Tipo de Requerimento (D_TIPO_REQ) : Se for "Desistência", vira 1. Caso contrário (default), vira 0.)

dt[, D_TIPO_REQ := fcase(
  TIPO_REQUERIMENTO_LEGADO == "Desistência", 1L, # O 'L' diz ao R que é um número Inteiro
  default = 0L
)]


# Definindo Dummy de Possui validade (D_Possui_validade) : Se possui validade, vira 1. Caso contrário (default), vira 0.

lista_TIPO_REQUERIMENTO_LEGADO_sem_validade <- c("DISPENSA DE OUTORGA"
                                                 , "CADASTRO DE USOS")

dt[, D_Possui_validade := fcase(
  
  # Condição: Se o tipo estiver na lista acima, o resultado é 0
  TIPO_REQUERIMENTO_LEGADO %in% lista_TIPO_REQUERIMENTO_LEGADO_sem_validade, 0L,
  
  # Caso padrão: Se não estiver na lista, o resultado é 1
  default = 1L
)]


# Criando a coluna Última_palavra_situação_uso
dt[, Última_palavra_situação_uso := trimws(sub(".* ", "", SITUAÇÃO_USO))]

#check <- dt[,c("SITUAÇÃO_USO","Última_palavra_situação_uso")]


# Criando a Dummy de Situação Deferida (D_situação_deferida)

lista_situação_deferida <- c("legado"
                             , "deferida"
                             , "deferido"
                             , "cadastrado")


dt[, D_situação_deferida := fcase(
  
  # CONDIÇÃO 1: Se a última palavra estiver na lista_situação_deferida
  tolower(Última_palavra_situação_uso) %in% lista_situação_deferida | 
    
    # CONDIÇÃO 2: Ou se a célula estiver vazia (NA ou texto vazio "")
    is.na(Última_palavra_situação_uso) | Última_palavra_situação_uso == "", 1L,
  
  # CASO PADRÃO (Default): Se não atender a nenhuma regra acima, vira 0
  default = 0L
)]


#-------------------------------------------------------------
# Salvando dados
#-------------------------------------------------------------

dt_soe <- dt

path_to_save <- paste0("./data/processed/",settings$dataref_soe_query_extraction,"/")
save(dt_soe, file = paste0(path_to_save,"dt_soe.rds"))

#---------
# Clean
#---------

keep_objects <- c("settings")
remove_non_functions(keep_objects)

