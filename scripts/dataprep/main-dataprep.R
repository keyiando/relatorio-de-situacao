#=====================================
#
#  Prep Data - Main script
#
#=====================================

#---------------------------------------------
#
# Packages and settings
#
#---------------------------------------------

# Run and Load General functions
source("scripts/general-functions.R") 

# Install packages

options(download.file.method = "wininet") # Sometimes R tries to use a modern connection method that the firewall hates. This tells R to use the standard Windows internet settings instead.

#  List of required packages 
required_packages <- c("data.table" # optimized package to manage data
                        ,"dplyr" # to manipulate data in the cleaning process
                        ,"stringr" # to manipulate string data 
                        ,"lubridate" # for managing dates
                        ,"zoo"  # for managing dates
                        ,"readxl" # for reading xlsx (faster than openxlsx)
                       ,"openxlsx" # for reading but mostly writing xlsx with complex formatation
                       ,"writexl" # for writing xlsx
                       ,"ggplot2" # for nice graph plots
                       #,"foreign" # to read .dbf files
                       ,"sf"
                        ) # 

#  Uses the fucntion created at general-functions.R and install the requeired ones (if not already installed)
check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

# Just for gglot2 in case it doesn't install correctly 
#install.packages("ggplot2", 
#                 repos = "http://cran.rstudio.com/", 
#                 type = "win.binary")

sessionInfo() # checking session

settings <- readRDS("./settings.rds") # Load settings

# Load and run Prepdata functions
source("scripts/dataprep/prep-dataprep-functions.R")

#-------------------------------
#
# Per data category
#
#-------------------------------

# 1. UGHRI-Gerência-Divisão
#source("scripts/dataprep/prep-ugrhi-gerencia-divisao.R")
load("./data/processed/dt_ugrhi_gerencia_divisao.rds") 

# 2. SOE data
#source("scripts/dataprep/prep-soe-1-import-and-save.R")
load("./data/processed/dt_soe.rds") #

#source("scripts/dataprep/prep-soe-2-treatment.R")


# 3. RS SMT data
#source("scripts/dataprep/prep-rs-smt-data.R")









#-------------------------------
#
#  Aggregating uses by user
#
#-------------------------------

# classe de valor de boleto

dt1_por_boleto[ VALOR_BOLETO <= 100, VALOR_BOLETO_CLASSE := "até 100" ]
dt1_por_boleto[ VALOR_BOLETO > 100 & VALOR_BOLETO <= 550, VALOR_BOLETO_CLASSE := "até 550" ]
dt1_por_boleto[ VALOR_BOLETO > 500 & VALOR_BOLETO <= 1000, VALOR_BOLETO_CLASSE := "até 1000" ]
dt1_por_boleto[ VALOR_BOLETO <= 100, VALOR_BOLETO_CLASSE := "até 100" ]



pt1_temp  <- dt1_por_boleto[, .(
  obs = .N
),
by = .(VALOR_BOLETO)]

#-------------------------------
# Reference dt is dt_soe_rural (dt per requirement)
# gen dt per user 
#-------------------------------

cat("Reading dt_soe_rural.\n")

load("./data/processed/dt_soe_rural.rds") #

setDT(dt_soe_rural)

cat(paste0("dt_soe_rural com ",nrow(dt_soe_rural)," observações e ",round(sum(dt_soe_rural[,vazao_normalizada_m3s], na.rm = TRUE),2)," vazao_normalizada_m3s"))

add_line_and_save_cleaning_descriptives("dt_soe_rural"
                                        ,"initial nrow (dt_soe_rural)"
                                        ,nrow(dt_soe_rural))


pt_tipo_uso <- dt_soe_rural[, .(q_uses = .N,
                        vazao_normalizada_m3s = sum(vazao_normalizada_m3s, na.rm = TRUE)),
                    by = .(TIPO_USO)]

pt_FINALIDADE_CORRETO <- dt_soe_rural[, .(q_uses = .N,
                                vazao_normalizada_m3s = sum(vazao_normalizada_m3s, na.rm = TRUE)),
                            by = .(FINALIDADE_CORRETO)]

# Clean unnecessary columns

columns_to_keep <- c("NOME_USUÁRIO_REQUERENTE"
                     ,"NÚMERO_CPF"
                     ,"NÚMERO_CNPJ"
                     ,"UGRHI"
                     ,"NOME_UGRHI"
                     ,"TIPO_USO"
                     ,"vazao_normalizada_m3s"
)

dt_temp <- dt_soe_rural[,..columns_to_keep]

# Gen vazao_normalizada_m3dia
dt_temp[, vazao_normalizada_m3dia :=  vazao_normalizada_m3s * 24*60*60]

# Aggregating by user

dt_temp1 <- dt_temp[, .(q_uses = .N,
                        vazao_normalizada_m3s = sum(vazao_normalizada_m3s, na.rm = TRUE),
                        vazao_normalizada_m3dia = sum(vazao_normalizada_m3dia, na.rm = TRUE)),
                    by = .(NOME_USUÁRIO_REQUERENTE
                           ,NÚMERO_CPF
                           ,NÚMERO_CNPJ
                           ,UGRHI
                           ,NOME_UGRHI
                           ,TIPO_USO)]

cat(paste0("Após aggregar por usuários : dt_temp1 com ",nrow(dt_temp1)," observações e ",round(sum(dt_temp1[,vazao_normalizada_m3s], na.rm = TRUE),2)," vazao_normalizada_m3s"))

add_line_and_save_cleaning_descriptives("dt_temp1"
                                        ,"initial nrow (dt_temp1)"
                                        ,nrow(dt_temp1))

# Add pf_pj
dt_temp1[ !is.na(NÚMERO_CPF) & is.na(NÚMERO_CNPJ) , pf_pj:=  "pf"]
dt_temp1[ !is.na(NÚMERO_CNPJ) & is.na(NÚMERO_CPF) , pf_pj:=  "pj"]

#------
# Save 
#------

dt_soe_rural_users <- dt_temp1
save(dt_soe_rural_users, file = "./data/processed/dt_soe_rural_users.rds")

#-------------------------------
#
#  Descriptive Stats from dt_soe_rural_users
#
#-------------------------------

cat(paste0("DT per user: dt_soe_rural_users com ",nrow(dt_soe_rural_users)," users e ",round(sum(dt_soe_rural_users[,vazao_normalizada_m3s], na.rm = TRUE),2)," m3/s"))

add_line_and_save_cleaning_descriptives("dt_soe_rural_users"
                                        ,"users"
                                        ,nrow(dt_soe_rural_users))

add_line_and_save_cleaning_descriptives("dt_soe_rural_users"
                                        ,"uses"
                                        ,sum(dt_soe_rural_users[,q_uses]))

add_line_and_save_cleaning_descriptives("dt_soe_rural_users"
                                        ,"vazao_normalizada_m3s"
                                        ,round(sum(dt_soe_rural_users[,vazao_normalizada_m3s], na.rm = TRUE),2))

add_line_and_save_cleaning_descriptives("dt_soe_rural_users"
                                        ,"vazao_normalizada_hm3dia"
                                        ,round(sum(dt_soe_rural_users[,vazao_normalizada_m3dia], na.rm = TRUE)/10^6,2))







graph1 <- ggplot(dt_soe_rural_users[vazao_normalizada_m3dia < 10^4,], aes(x = vazao_normalizada_m3dia)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Gráfico de Densidade de uma Distribuição Normal",
    x = "Q cap (m3/dia)",
    y = "Densidade"
  ) +
  theme_minimal()

graph2 <- ggplot(dt_temp1[pf_pj == "pf" & vazao_normalizada_m3dia < 10^4,], aes(x = vazao_normalizada_m3dia)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Gráfico de Densidade de uma Distribuição Normal",
    x = "Q cap (m3/dia)",
    y = "Densidade"
  ) +
  theme_minimal()


#---------
# Final name and stats
#---------

# rename
dt_soe_rural_users <- dt_temp2

# stats
dt_soe_rural_users_pf_pj <- dt_soe_rural_users[, .(q_users = .N,
                                                   q_uses = sum(q_uses, na.rm = TRUE),
                                                   sum_Q_m3dia = sum(vazao_normalizada_m3dia, na.rm = TRUE),
                                                   avg_Q_m3dia = mean(vazao_normalizada_m3dia, na.rm = TRUE),
                                                   med_Q_m3dia = median(vazao_normalizada_m3dia, na.rm = TRUE),
                                                   sum_Q_m3s = sum(vazao_normalizada_m3s, na.rm = TRUE),
                                                   avg_Q_m3s = mean(vazao_normalizada_m3s, na.rm = TRUE),
                                                   med_Q_m3s = median(vazao_normalizada_m3s, na.rm = TRUE)),
                                               by = .(pf_pj)]


write_xlsx(dt_soe_rural_users_pf_pj, path = paste0("./descriptive-stats/dt_soe_rural_users_pf_pj_2.xlsx"))

dt_soe_rural_users_TIPO_USO <- dt_soe_rural_users[, .(q_users = .N,
                                                      q_uses = sum(q_uses, na.rm = TRUE),
                                                      sum_Q_m3dia = sum(vazao_normalizada_m3dia, na.rm = TRUE),
                                                      avg_Q_m3dia = mean(vazao_normalizada_m3dia, na.rm = TRUE),
                                                      med_Q_m3dia = median(vazao_normalizada_m3dia, na.rm = TRUE),
                                                      sum_Q_m3s = sum(vazao_normalizada_m3s, na.rm = TRUE),
                                                      avg_Q_m3s = mean(vazao_normalizada_m3s, na.rm = TRUE),
                                                      med_Q_m3s = median(vazao_normalizada_m3s, na.rm = TRUE)),
                                                  by = .(TIPO_USO)]

write_xlsx(dt_soe_rural_users_TIPO_USO, path = paste0("./descriptive-stats/dt_soe_rural_users_TIPO_USO_2.xlsx"))

dt_soe_rural_users_UGHRI <- dt_soe_rural_users[, .(q_users = .N,
                                                   q_uses = sum(q_uses, na.rm = TRUE),
                                                   sum_Q_m3dia = sum(vazao_normalizada_m3dia, na.rm = TRUE),
                                                   avg_Q_m3dia = mean(vazao_normalizada_m3dia, na.rm = TRUE),
                                                   med_Q_m3dia = median(vazao_normalizada_m3dia, na.rm = TRUE),
                                                   sum_Q_m3s = sum(vazao_normalizada_m3s, na.rm = TRUE),
                                                   avg_Q_m3s = mean(vazao_normalizada_m3s, na.rm = TRUE),
                                                   med_Q_m3s = median(vazao_normalizada_m3s, na.rm = TRUE)),
                                               by = .(UGRHI,NOME_UGRHI)]

write_xlsx(dt_soe_rural_users_UGHRI, path = paste0("./descriptive-stats/dt_soe_rural_users_UGHRI_2.xlsx"))

#---------
# Save
#---------

save(dt_soe_rural_users, file = "./data/processed/dt_soe_rural_users.rds")


#---------
# Clean
#---------

keep_objects <- c("settings")
remove_non_functions(keep_objects)
