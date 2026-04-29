#=====================================
#
# Prep RS SMT data
#
#=====================================

#-------------------------------------------------------------
# Importando dados
#-------------------------------------------------------------

cat("Importing  data.\n")

path_to_read <-  ("./data/raw/fabh-smt/rs/2020/") # definição do caminho do arquivo que desejo ler

shp_dt <- st_read(
  dsn = paste0(path_to_read,"outorga_superficial_DAEE_2020_vz.shp"), 
  options = "ENCODING=WINDOWS-1252",
  quiet = TRUE
)


#-------------------------------------------------------------
# Convertendo dataframe para datatable (para uso com o pacote data.table)
#-------------------------------------------------------------

dt <- shp_dt
setDT(dt) # convertendo para data.table
dt[,geometry := NULL]

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
cat(paste0("Quantidade de UGRHI únicos: ",length(unique(dt[,cod_ugrhi])),"\n"))

#-------------------------------------------------------------
# Salvando dados
#-------------------------------------------------------------

dt_rs2020_sup <- dt

path_to_save <- ("./data/processed/rs/")
save(dt_rs2020_sup, file = paste0(path_to_save,"dt_rs2020_sup.rds"))

#---------
# Clean
#---------

keep_objects <- c("settings")
remove_non_functions(keep_objects)

