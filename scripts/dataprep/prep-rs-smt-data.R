#=====================================
#
# Prep RS SMT data
#
#=====================================

#-------------------------------------------------------------
# Importando dados
#-------------------------------------------------------------

cat("Importing  data.\n")

path_to_read <-  ("./data/raw/spaguas/") # definição do caminho do arquivo que desejo ler

dt <- readxl::read_excel( 
  paste0(path_to_read,"UGRHI-Gerencia-Divisao.xlsx")
  )

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


# Contagem de UGRHI únicos

if( length(unique(dt[,UGRHI])) == nrow(dt) ){
  cat(paste0("Quantidade de UGRHI únicos: ",length(unique(dt[,UGRHI])),"\n"))
  cat(paste0("Quantidade de linhas de dt: ",nrow(dt),"\n"))
  cat(paste0("--> Não há necessidade de remover/verificar duplicatas\n"))
} else{
  cat(paste0("Quantidade de UGRHI únicos: ",length(unique(dt[,UGRHI])),"\n"))
  cat(paste0("Quantidade de linhas de dt: ",nrow(dt),"\n"))
  cat(paste0("--> Verificar duplicatas de SOLICITAÇÃO_REQUERIMENTO\n"))
}

#-------------------------------------------------------------
# Salvando dados
#-------------------------------------------------------------

dt_ugrhi_gerencia_divisao <- dt

path_to_save <- ("./data/processed/")
save(dt_ugrhi_gerencia_divisao, file = paste0(path_to_save,"dt_ugrhi_gerencia_divisao.rds"))

#---------
# Clean
#---------

keep_objects <- c("settings")
remove_non_functions(keep_objects)

