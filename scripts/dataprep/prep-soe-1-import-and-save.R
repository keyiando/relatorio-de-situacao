#=====================================
#
# Prep SOE query extracted data
#
#=====================================

#-------------------------------------------------------------
# Importando dados do SOE (dados extraídos pela query da DSO)
#-------------------------------------------------------------

cat(paste0("Importing SOE's dso-query extraction data from ",settings$dataref_soe_query_extraction,".\n"))

path_to_read <-  paste0("./data/raw/soe/query-dso/",settings$dataref_soe_query_extraction,"/") # definição do caminho do arquivo que desejo ler

dt <- readxl::read_excel( # Lendo o arquivo xlsx. A tabela importada agora é o objeto "dt" no Environment.
  paste0(path_to_read,"Query_Ajustada_soe (1).xlsx"),
  guess_max = 10^5) # O padrão é 1000. Definido para 10^5 para que o R veja plo menos metade do arquivo pra adivinhar o formato (caso contrário, as linhas com NA são lidas como logi)

#save(dt, file = paste0(path_to_read,"Query_Ajustada_soe (1).rds"))
#load(paste0(path_to_read,"Query_Ajustada_soe (1).rds")) 

#-------------------------------------------------------------
# Convertendo dataframe para datatable (para uso com o pacote data.table)
#-------------------------------------------------------------

setDT(dt)

#-------------------------------------------------------------
# Explorando algumas informações do dt
#-------------------------------------------------------------

# Número de linhas e colunas dos dados, nome das colunas e formato das colunas
 
cat(paste0("  dt com ",nrow(dt)," linhas e ",ncol(dt)," colunas.\n"))

head(dt) # Imprime as 5 primeiras linhas de cada coluna do dt
colnames(dt) # Imprime os nomes das colunas do dt

str(dt) # Ou, de forma alterativa, esse comando mostra o tipo de cada coluna (chr para texto, num para número)


# Quantidade de NAs por coluna

na_count <- dt[, lapply(.SD, function(x) sum(is.na(x)))] # .SD percorre todas as colunas e sum(is.na(x)) conta os vazios

cat("Contagem de NAs por coluna:\n\n")
print(t(na_count))  # Transformamos em um formato de lista vertical para facilitar a leitura 


# Contagem de SOLICITAÇÃO_REQUERIMENTO únicos

if( length(unique(dt[,SOLICITAÇÃO_REQUERIMENTO])) == nrow(dt) ){
  cat(paste0("Quantidade de SOLICITAÇÃO_REQUERIMENTO únicos: ",length(unique(dt[,SOLICITAÇÃO_REQUERIMENTO])),"\n"))
  cat(paste0("Quantidade de linhas de dt: ",nrow(dt),"\n"))
  cat(paste0("--> Não há necessidade de remover duplicatas\n"))
} else{
  cat(paste0("Quantidade de SOLICITAÇÃO_REQUERIMENTO únicos: ",length(unique(dt[,SOLICITAÇÃO_REQUERIMENTO])),"\n"))
  cat(paste0("Quantidade de linhas de dt: ",nrow(dt),"\n"))
  cat(paste0("--> Verificar duplicatas de SOLICITAÇÃO_REQUERIMENTO\n"))
}


#-------------------------------------------------------------
# Ajustando formato dos dados: texto para numérico
#-------------------------------------------------------------

cols_to_convert <- c(
  "LATITUDE_SIRGAS2000"
  ,"LONGITUDE_SIRGAS2000"
)

# 1. Removemos a vírgula e colocamos ponto
# 2. Transformamos em numérico

dt[, (cols_to_convert) := lapply(.SD, function(x) {
  as.numeric(gsub(",", ".", trimws(as.character(x))))
}), .SDcols = cols_to_convert]

# dt$LATITUDE_SIRGAS2000 <- as.numeric(gsub(",", ".", dt$LATITUDE_SIRGAS2000)) # example without lapply

# Verificando se a quantidade de NAs permanece a mesma após a transformação

na_count_check <- dt[, lapply(.SD, function(x) sum(is.na(x)))] 
comparison <- rbind(na_count,na_count_check)
head(comparison[,..cols_to_convert]) # Both lines must be the same (1: before transformation, 2: after transformation)

rm(comparison,na_count_check) # remove temp data

#-------------------------------------------------------------
# Ajustando formato dos dados: conversão de colunas para formato de data
#-------------------------------------------------------------

# 1. Conversão do formato de data do excel para formato de data do R

cols_to_convert <- c(
  "DATA_ENTRADA_REQUERIMENTO"
#  ,"DATA_EMISSÃO_PARECER_TÉCNICO"
  ,"DATA_PUBLICAÇÃO"
  ,"DATA_VENCIMENTO_PORTARIA"
# ,"GERAÇÃO_BOLETO"
# ,"VENCIMENTO_BOLETO"
  ,"PAGAMENTO_BOLETO"
  ,"DATA_INÍCIO_PARECER_TÉCNICO"
  ,"DATA_EXTRAÇÃO"
)

excel_date_origin <- "1899-12-30" # This is the most common for Windows Excel
dt[, (cols_to_convert) := lapply(.SD, as.Date, origin = excel_date_origin), .SDcols = cols_to_convert]

# dt[, dt_venc := as.Date(DATA_VENCIMENTO_PORTARIA, origin = excel_date_origin)] # example without lapply

# Verificando se a quantidade de NAs permanece a mesma após a transformação

na_count_check <- dt[, lapply(.SD, function(x) sum(is.na(x)))] # Just to check if tje quantity of NAs is the same before and after transformation
comparison <- rbind(na_count,na_count_check)
head(comparison[,..cols_to_convert]) # Both lines must be the same (1: before transformation, 2: after transformation)

rm(comparison,na_count_check) # remove temp data

# 2. Conversão do formato de texto do excel para formato de data do R

# Listamos as colunas que contêm datas no formato Dia/Mês/Ano (DD/MM/YYYY)
cols_to_convert <- c(
  "DATA_EMISSÃO_PARECER_TÉCNICO"
  ,"GERAÇÃO_BOLETO"
  ,"VENCIMENTO_BOLETO"
)

dt[, (cols_to_convert) := lapply(.SD, dmy), .SDcols = cols_to_convert]

# Verificando se a quantidade de NAs permanece a mesma após a transformação

na_count_check <- dt[, lapply(.SD, function(x) sum(is.na(x)))] # Just to check if tje quantity of NAs is the same before and after transformation
comparison <- rbind(na_count,na_count_check)
head(comparison[,..cols_to_convert]) # Both lines must be the same (1: before transformation, 2: after transformation)

rm(comparison,na_count_check) # remove temp data

# add ANO_VENCIMENTO_PORTARIA
#dt[, ANO_VENCIMENTO_PORTARIA := lubridate::year(DATA_VENCIMENTO_PORTARIA) ]


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

