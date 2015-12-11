# Analysis of OFX files

library(data.table)
library(stringr)

# removes scientific notation
options(scipen=999)

# setting wd
csv_basepath = "~/Documents/SpiderOak/Financeiro/ofx/"

# lists csv files
csv_list <- paste( csv_basepath, list.files(pattern = "*.csv", path = csv_basepath, recursive = TRUE), sep = '')

colClasses <- c('integer','string','string','string','string','string','factor','double','factor',
                'date', 'date', 'string','string','date', 'double',  'string', 'factor','string')

# loads csv into data frames
dt = do.call(rbind, lapply(csv_list, fread, colClasses = colClasses))

# loads classification file
classes = read.csv("classes.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

# cast date columns to date
dt$stat.start_date <- as.Date(dt$stat.start_date)
dt$stat.end_date <- as.Date(dt$stat.end_date)
dt$t.date <- as.Date(dt$t.date)

dt[, t.month := as.Date(strftime(t.date, "%Y-%m-01")) ]
dt[, t.class := 'sem categoria']

classify_transaction_textual <- function(dt, classes){
        
        classes <- classes[classes$match_type == "textual",]
        for (i in 1:nrow(classes)){
                dt[ str_detect(t.memo, classes[i,4]), t.class := classes[i,2]]
                dt[ str_detect(t.memo, classes[i,4]), t.subclass := classes[i,3]]
        }
        
        dt
}

classify_transaction_amount <- function(dt, classes){
        
        classes <- classes[classes$match_type == "amount",]
        for (i in 1:nrow(classes)){
                dt[ str_detect(t.memo, classes[i,4]) & t.amount == classes[i,5], 
                    t.class := classes[i,2]]
                dt[ str_detect(t.memo, classes[i,4]) & t.amount == classes[i,5], 
                    t.subclass := classes[i,3]]
        }
        
        dt
}

dt <- classify_transaction_textual(dt, classes)
dt <- classify_transaction_amount(dt, classes)

dt[ t.class == 'sem categoria', .(t.month, t.memo, t.class, t.amount)][order(t.month)]