# Analysis of OFX files

# removes scientific notation
options(scipen=999)

# lists csv files
csv_list <- list.files(pattern = "*.csv", path = getwd(), recursive = TRUE)


colClasses <- c('integer','string','string','string','string','string','factor','double','factor',
                'date', 'date', 'string','string','date', 'double',  'string', 'factor','string')

# loads csv into data frames
library(data.table)
dt = do.call(rbind, lapply(csv_list, fread, colClasses = colClasses))

# cast date columns to date
dt$stat.start_date <- as.Date(dt$stat.start_date)
dt$stat.end_date <- as.Date(dt$stat.end_date)
dt$t.date <- as.Date(dt$t.date)

dt[, t.database := strftime(t.date, "%Y-%m-01") ]
dt[, t.class := 'sem categoria']


classify_transaction <- function(dt){
        
        dt[, t.class := 'sem categoria']
        
        dt[ str_detect(t.memo, "Vivo Celular"), t.class := "Telefone/Internet/TV"]
        
        dt[ str_detect(t.memo, "Saque"), t.class := "Gastos em Dinheiro"]
        
        dt[ str_detect(t.memo, "Resgate Poupança"), t.class := "Resgates"]
        
        dt[ str_detect(t.memo, "CYNTHIA"), t.class := "Saúde"]
        dt[ str_detect(t.memo, "PASSAREDO"), t.class := "Passagem Aérea"]
        dt[ str_detect(t.memo, "TAM SITE"), t.class := "Passagem Aérea"]
        dt[ str_detect(t.memo, "MULTIPLUS"), t.class := "Passagem Aérea"]
        dt[ str_detect(t.memo, "GOL TRAN"), t.class := "Passagem Aérea"]
        dt[ str_detect(t.memo, "AF INTERNET"), t.class := "Passagem Aérea"]
        dt[ str_detect(t.memo, "TAXA DE EMBARQUE"), t.class := "Passagem Aérea"]
        dt[ str_detect(t.memo, "AZUL LINHAS"), t.class := "Passagem Aérea"]
        
        dt[ str_detect(t.memo, "HOTEL"), t.class := "Viagens"]
        dt[ str_detect(t.memo, "VIAÇÃO"), t.class := "Viagens"]
        
        dt[ str_detect(t.memo, "SMAFF ASA NOR"), t.class := "Carro"]

        dt[ str_detect(t.memo, "SPOTIFY"), t.class := "Utilidades"]
        
        dt[ str_detect(t.memo, "VILA ROMANA"), t.class := "Compras"]
        dt[ str_detect(t.memo, "DULAR"), t.class := "Compras"]
        dt[ str_detect(t.memo, "EDITORA ANTROPOSOFICA"), t.class := "Compras"]
        dt[ str_detect(t.memo, "VAR.CAMBIAL"), t.class := "Compras"]
        dt[ str_detect(t.memo, "TOK"), t.class := "Compras"]
        dt[ str_detect(t.memo, "ZELO-BRASILIA"), t.class := "Compras"]
        
        dt[ str_detect(t.memo, "TOSCANELLO"), t.class := "Alimentação"]
        dt[ str_detect(t.memo, "VERDURAO"), t.class := "Alimentação"]
        
        dt[ str_detect(t.memo, "TOUJOURS BISTROT"), t.class := "Restaurantes"]
        dt[ str_detect(t.memo, "GREENS"), t.class := "Restaurantes"]
        dt[ str_detect(t.memo, "TICIANA WERNER"), t.class := "Restaurantes"]
        dt[ str_detect(t.memo, "RESTAURANTE"), t.class := "Restaurantes"]
        dt[ str_detect(t.memo, "REST "), t.class := "Restaurantes"]
        dt[ str_detect(t.memo, "REST."), t.class := "Restaurantes"]
        
        dt[ str_detect(t.memo, "Transferência"), t.class := "Transferências"]
        dt[ str_detect(t.memo, "TED"), t.class := "Transferências"]
        dt[ str_detect(t.memo, "Emissão de DOC"), t.class := "Transferências"]
        
        dt[ str_detect(t.memo, "Tesouro Direto-Compra"), t.class := "Investimento"]
        
        dt[ str_detect(t.memo, "Pagto cartão crédito"), t.class := "Cartão Crédito"]
        
        regex_transferencia <- "Transferência on line[ -]+([0-9]{2}[/][0-9]{2})[ ]+([0-9]*)[ ]*([0-9]*[-][0-9a-zA-Z])[ ]+([a-zA-Z ]+)"
        
        dt
}

df <- classify_transaction(dt)

dt[ t.class == 'sem categoria', .(t.memo, t.class)]