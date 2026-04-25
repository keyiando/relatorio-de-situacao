#=====================================
#
#  Configurações iniciais (Settings) 
#
#=====================================

# Limpa todos os objetos armazenados na memória (Global Environment)
# O comando rm (remove) garante que começamos o script "do zero"

rm(list = ls())

# Chama o Garbage Collector (gc) para libertar memória RAM não utilizada pelo sistema

gc()

# Criamos uma lista (list) chamada 'settings' para centralizar variáveis importantes. 
# Isso facilita a manutenção do código se algo mudar no futuro.

settings <- list(
  
  project_path = "D:/Keyi/OneDrive/Documentos/SPAguas/DGC/Projetos/relatorio-de-situacao" # Caminho da pasta principal onde o projeto está guardado
# project_path = "C:/Users/kaussami/OneDrive - PRODESP/Documentos/DGC/Projetos/relatorio-de-situacao" # 
  , dataref_soe_query_extraction = c("2026-04-10") # Data da extração de dados (query)
  )

#  , dataref_soe_query_extraction = c("2026-04-17") # Data da extração de dados (query)


# Define o diretório de trabalho (Working Directory) do R para a pasta do projeto
# O setwd faz com que o R "olhe" diretamente para esta pasta ao ler ou salvar arquivos

setwd(settings$project_path)

# Exibe uma mensagem explicativa no console para o usuário
cat(
  "\n", # Pula uma linha para organizar o console
  "-----------------------------------------------------------\n",
  " DIRETÓRIO DE TRABALHO DEFINIDO\n",
  "-----------------------------------------------------------\n",
  " Todos os arquivos serão lidos ou salvos na pasta:\n",
  " ", getwd(), "\n", # getwd() mostra o caminho atual para confirmar
  "-----------------------------------------------------------\n"
)

# Salva a lista de configurações num arquivo de formato nativo do R (.rds)
# Isso permite carregar as mesmas definições noutros scripts futuramente

saveRDS(settings, "./settings.rds")

# Desativa a notação científica (ex: 1e+05 vira 100000)
# O scipen=100 força o R a mostrar números por extenso, facilitando a leitura

options(scipen=100) 

# Limpa novamente o ambiente para garantir que apenas o que foi configurado permaneça
rm(list = ls())






