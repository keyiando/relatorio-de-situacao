#===============================
# Prep data Functions
#===============================

#-------------------
# Convert columns
#-------------------

### Convert column to numeric
change_column_to_numeric <- function(dt, column) {
  
  if (!is.numeric(dt[[column]])) {
    dt[, (column) := as.numeric(get(column))]
    if (any(is.na(dt[[column]]))) {
      warning(paste("Coluna", column, "contém valores NA após a conversão."))
    }
  } else {
    cat(paste("Coluna", column, "já é numérica.\n"))
  }
  return(dt)
}

# Usage:
#dt <- change_column_to_numeric(dt, "A")

convert_comma_numeric <- function(x) {
  as.numeric(gsub(",", ".", x))
}
# usage:
#if (is.character(dt[["remdezr"]])) {
#  dt[, remdezr := sapply(remdezr, convert_comma_numeric)]
#}


#---------------------------------------
# Check if all needed columns are in DT
#---------------------------------------

check_needed_columns_in_dt <- function(dinput, columns_needed) {
  
  missing_columns <- setdiff(columns_needed, names(dinput))
  
  if (length(missing_columns) > 0) {
    stop(paste("Error: Columns missing in input data.table:",
               paste(missing_columns, collapse = ", ")))
  } else {
    cat("     Input data.table has all needed columns.\n")
  }
}

# Usage:
# columns_needed <- c("cpf_m", "dtadmissao", "diadesl", "ocup2002","horascontr", "mesdeslig", "remmedr")
# check_needed_columns_in_dt(dtinput, columns_needed)

#------------------------------------------------
# Add line in cleaning_descriptives.csv and save
#------------------------------------------------

add_line_and_save_cleaning_descriptives <- function(database_name_i,metric_i,value_i){
  
  # check if the file exists and read or create a new one
  
  if (file.exists("./descriptive-stats/cleaning_descriptives.csv")) {
    
    dt <- fread("./descriptive-stats/cleaning_descriptives.csv",
                colClasses = c("character","character","character","numeric"))
    
    
  } else {
    
    dt <- data.table(
      date = as.character(Sys.Date()),
      database_name = NA,
      metric = NA,
      value = NA)
    
  }
  
  # check if the metric already exists to update value or create a new line
  
  if (nrow(dt[database_name == database_name_i & metric == metric_i,]) != 0) {
    
    cat("    updating value in cleaning_descriptives\n")
    
    dt[ database_name == database_name_i & metric == metric_i, 
        `:=`
        (date = as.character(Sys.Date()),
          value = value_i)
    ]
    
  } else {
    
    cat("    adding new line in cleaning_descriptives\n")
    
    new_line <- data.table(
      date = as.character(Sys.Date()),
      database_name = database_name_i,
      metric = metric_i,
      value = value_i)
    
    dt <- rbind(dt,new_line)
    
  }
  
  fwrite(dt, 
         paste0("./descriptive-stats/cleaning_descriptives.csv"),
         sep = ";"
  )
  
}


#---------------------------------------
# calcula_rend_anual_e_produtividade_por_vinculo_rais (para arquivo de parâmetros)
#---------------------------------------

calcula_rend_anual_e_produtividade_por_vinculo_rais <- function(dt,year_i){
  
  year_char <- as.character(year_i)
  
  # Check if we have all columns needed
  columns_needed <- c("ano","cpf_m", "dtadmissao", "diadesl", "ocup2002", "horascontr", "mesdeslig", "remmedr")
  check_needed_columns_in_dt(dt,columns_needed)
  
  # Transformando a variável de rendimento médio e número de horas contratadas em numéricas,
  
  dt[
    ,
    `:=` (
      remmedr = as.numeric(str_replace(remmedr, ",", "\\.")),
      horascontr = as.numeric(horascontr)
    )
  ]
  
  # Removendo os indivíduos que possuem renda média nula
  
  dt <- dt[remmedr > 0]
  
  # Gerando as variáveis derivadas ---------------------------------------------------------
  
  # Transformando em numéricas as variáveis de dia e mês de desligamento, ajustando a 
  # variável de dia de desligamento e gerando as variáveis finais de data de admissão e de
  # desligamento
  
  dt[
    ,
    `:=` (
      diadesl = as.numeric(diadesl),
      mesdeslig = as.numeric(mesdeslig)
    )
  ][
    ,
    diadesl := case_when(
      is.na(diadesl) & mesdeslig %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31,
      is.na(diadesl) & mesdeslig %in% c(4, 6, 9, 11) ~ 30,
      is.na(diadesl) & mesdeslig == 2 ~ 28,
      mesdeslig == 0 ~ 0,
      T ~ diadesl
    )
  ][
    ,
    `:=` (
      dtadmissao = dmy(dtadmissao),
      dtdeslig = dmy(paste(diadesl, mesdeslig, year_i, sep = "-"))
    )
  ]
  
  # Gerando a variável que indica se o vínculo estava ativo em qq momento de um determinado mês
  
  dt[
    ,
    `:=` (
      indemprego1 = fifelse(dtadmissao <= dmy(paste0("31-01-",year_char)) & dtdeslig >= dmy(paste0("01-01-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego2 = fifelse(dtadmissao <= dmy(paste0("28-02-",year_char)) & dtdeslig >= dmy(paste0("01-02-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego3 = fifelse(dtadmissao <= dmy(paste0("31-03-",year_char)) & dtdeslig >= dmy(paste0("01-03-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego4 = fifelse(dtadmissao <= dmy(paste0("30-04-",year_char)) & dtdeslig >= dmy(paste0("01-04-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego5 = fifelse(dtadmissao <= dmy(paste0("31-05-",year_char)) & dtdeslig >= dmy(paste0("01-05-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego6 = fifelse(dtadmissao <= dmy(paste0("30-06-",year_char)) & dtdeslig >= dmy(paste0("01-06-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego7 = fifelse(dtadmissao <= dmy(paste0("31-07-",year_char)) & dtdeslig >= dmy(paste0("01-07-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego8 = fifelse(dtadmissao <= dmy(paste0("31-08-",year_char)) & dtdeslig >= dmy(paste0("01-08-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego9 = fifelse(dtadmissao <= dmy(paste0("30-09-",year_char)) & dtdeslig >= dmy(paste0("01-09-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego10 = fifelse(dtadmissao <= dmy(paste0("31-10-",year_char)) & dtdeslig >= dmy(paste0("01-10-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego11 = fifelse(dtadmissao <= dmy(paste0("30-11-",year_char)) & dtdeslig >= dmy(paste0("01-11-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego12 = fifelse(dtadmissao <= dmy(paste0("31-12-",year_char)) & dtdeslig >= dmy(paste0("31-12-",year_char)) | is.na(dtdeslig), 1, 0)
    )
  ]
  
  # Gerando o vetor com o nome das variáveis indicadoras de emprego ativo no mês
  
  vindemprego <- paste0("indemprego", 1:12)
  
  # Gerando a variável de rendimento anual (ainda a nível de vínculo)
  
  dt[, rend_anual := rowSums(.SD, na.rm = TRUE) * remmedr, .SDcols = vindemprego]
  
  # Ajustando a variável de horas contratadas
  
  dt[, horascontr_ajustada := horascontr * 52 / 12]
  
  # Gerando a variável de produtividade (ainda a nível de vínculo) 
  
  dt[
    ,
    produtividade := fifelse(horascontr >= 8, remmedr / horascontr_ajustada, 0)
  ]
  
  # pre-treated dt
  
  dt <- dt[,c("ano","cpf_m","ocup2002","produtividade","rend_anual")]
  
  return(dt)
}


#---------------------------------------
# calcula_rend_anual_produtividade_por_vinculo_rais_dataprep_to_sedap (para arquivos rais semi tratados to sedap)
#---------------------------------------

calcula_rend_anual_produtividade_por_vinculo_rais_dataprep_to_sedap <- function(dt,year_i){
  
  year_char <- as.character(year_i)
  
  # Check if we have all columns needed
  columns_needed <- c("cpf", "dtadmissao", "diadesl", "mesdeslig", "remmedr", "horascontr","ocup2002","naturjur")
  check_needed_columns_in_dt(dt,columns_needed)
  
  # Removendo os indivíduos que possuem renda média nula
  
  dt <- dt[remmedr > 0]
  cat(paste0("     ",nrow(dt)," rows after removing non positive remmedr.\n"))
  
  # Gerando as variáveis derivadas ---------------------------------------------------------
  
  # Ajustando a variável de dia de desligamento e gerando as variáveis finais de data de admissão e de
  # desligamento
  
  dt[
    ,
    diadesl := case_when(
      is.na(diadesl) & mesdeslig %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31,
      is.na(diadesl) & mesdeslig %in% c(4, 6, 9, 11) ~ 30,
      is.na(diadesl) & mesdeslig == 2 ~ 28,
      mesdeslig == 0 ~ 0,
      T ~ diadesl
    )
  ][
    ,
    `:=` (
      dtadmissao = dmy(dtadmissao),
      dtdeslig = dmy(paste(diadesl, mesdeslig, year_i, sep = "-"))
    )
  ]
  
  # Gerando a variável que indica se o vínculo estava ativo em qq momento de um determinado mês
  
  dt[
    ,
    `:=` (
      indemprego1 = fifelse(dtadmissao <= dmy(paste0("31-01-",year_char)) & dtdeslig >= dmy(paste0("01-01-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego2 = fifelse(dtadmissao <= dmy(paste0("28-02-",year_char)) & dtdeslig >= dmy(paste0("01-02-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego3 = fifelse(dtadmissao <= dmy(paste0("31-03-",year_char)) & dtdeslig >= dmy(paste0("01-03-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego4 = fifelse(dtadmissao <= dmy(paste0("30-04-",year_char)) & dtdeslig >= dmy(paste0("01-04-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego5 = fifelse(dtadmissao <= dmy(paste0("31-05-",year_char)) & dtdeslig >= dmy(paste0("01-05-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego6 = fifelse(dtadmissao <= dmy(paste0("30-06-",year_char)) & dtdeslig >= dmy(paste0("01-06-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego7 = fifelse(dtadmissao <= dmy(paste0("31-07-",year_char)) & dtdeslig >= dmy(paste0("01-07-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego8 = fifelse(dtadmissao <= dmy(paste0("31-08-",year_char)) & dtdeslig >= dmy(paste0("01-08-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego9 = fifelse(dtadmissao <= dmy(paste0("30-09-",year_char)) & dtdeslig >= dmy(paste0("01-09-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego10 = fifelse(dtadmissao <= dmy(paste0("31-10-",year_char)) & dtdeslig >= dmy(paste0("01-10-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego11 = fifelse(dtadmissao <= dmy(paste0("30-11-",year_char)) & dtdeslig >= dmy(paste0("01-11-",year_char)) | is.na(dtdeslig), 1, 0),
      indemprego12 = fifelse(dtadmissao <= dmy(paste0("31-12-",year_char)) & dtdeslig >= dmy(paste0("31-12-",year_char)) | is.na(dtdeslig), 1, 0)
    )
  ]
  
  # Gerando o vetor com o nome das variáveis indicadoras de emprego ativo no mês
  
  vindemprego <- paste0("indemprego", 1:12)
  
  # Gerando a variável de rendimento anual (ainda a nível de vínculo)
  
  dt[, rend_anual := rowSums(.SD, na.rm = TRUE) * remmedr, .SDcols = vindemprego]
  
  # Ajustando a variável de horas contratadas
  
  dt[, horascontr_ajustada := horascontr * 52 / 12]
  
  # Gerando a variável de produtividade (ainda a nível de vínculo) 
  
  dt[, dtadmissao_auxiliar := as.numeric(dtadmissao)]
  
  dt[
    ,
    produtividade := fifelse(horascontr >= 8, remmedr / horascontr_ajustada, 0)
  ][
    ,
    max_produtividade := fifelse(produtividade == max(produtividade), 1, 0),
    by = cpf
  ][
    ,
    max_dtadmissao := fifelse(dtadmissao_auxiliar == max(dtadmissao_auxiliar), 1, 0),
    by = .(cpf, max_produtividade)
  ][
    ,
    max_produtividade := max_produtividade * max_dtadmissao # identificador do vínculo de maior produtividade e com contratação mais recente
  ]
  
  # pre-treated dt
  
  dt <- dt[,c("ano","cpf", "rend_anual","produtividade","ocup2002","naturjur","max_produtividade",
              "indemprego1","indemprego2","indemprego3","indemprego4","indemprego5","indemprego6",
              "indemprego7","indemprego8","indemprego9","indemprego10","indemprego11","indemprego12")]
  
  return(dt)
}

