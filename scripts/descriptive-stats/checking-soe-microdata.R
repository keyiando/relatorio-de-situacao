#=====================================
#
# Checking data
#
#=====================================

#-------------------------------------------------------------
# Carregando dados
#-------------------------------------------------------------

cat(paste0("Loading SOE data...\n"))

path_to_read <-  paste0("./data/processed/",settings$dataref_soe_query_extraction,"/") # definição do caminho do arquivo que desejo ler
load(paste0(path_to_read,"dt_soe.rds"))

dt <- dt_soe

cat(paste0("Loading rs data...\n"))

path_to_read <-  paste0("./data/processed/rs/") # definição do caminho do arquivo que desejo ler
load(paste0(path_to_read,"dt_rs2020_sup.rds"))

#-------------
# SMT Verif1 # From SOE legado (Estella)
#-------------

requerimentos_to_check <- c("2000L102003242PT"
                            ,"2000L101941296YW")

check1 <- dt[SOLICITAÇÃO_REQUERIMENTO %in% requerimentos_to_check,]

check1_t <- cbind(colnames(check),transpose(check))

colnames(dt)

write_xlsx(check1, path = paste0("./descriptive-stats/ParaVerificar_20260728_1.xlsx"))

#-------------
# SMT Verif2 # From FCHE_DEM_2019 (RS 2020) (100 usos com maiores vazões do SMT de cap superficial na base FCHE_DEM_2019 -> 57 processos únicos)
#-------------

dt_temp <- dt_rs2020_sup

dt_temp <- dt_temp[ cod_ugrhi == 10 & Referencia == "FCHE_DEM_2019",] # filtros: selecionando somente usos da UGHRI10 e ref FCHE 2019
dt_temp <- dt_temp[order(-`vz.m3s`)] # ordenar na ordem decrescente de vz m3s
dt_temp[, Autos.Requ := as.character(as.numeric(sub(",", ".", Autos.Requ)))] # limpeza do campo processo nesses casos (,0)

proc_top100 <- unique(dt_temp[1:100, .(Autos.Requ)]) # 57 processos únicos no top 100

# Encontrando os 57 processos no dt-soe
dt_fche19_top100 <- merge(proc_top100, dt, by.x = c("Autos.Requ"), by.y = c("PROCESSO"), all.x = TRUE)

length(unique(dt_fche19_top100[,Autos.Requ])) # 57 processos 

write_xlsx(dt_fche19_top100, path = paste0("./descriptive-stats/ParaVerificar_20260728_2.xlsx"))

# Somando as vazões dos os 57 processos no dt-soe

proc_top100_vetor <- proc_top100[[1]]

dt_temp1 <- dt_temp[Autos.Requ %in% proc_top100_vetor]

print(sum(dt_temp1[,vz.m3s]))


