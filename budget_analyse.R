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

plot_expenses_by_class <- function(dt, in_year, in_currency){
        
        # sum by class
        sum_by_year_class <- dt[ order(year(t.month)), sum(t.amount), 
                                 by = .(year(t.month), stat.currency,t.class)]
        
        # filter the year, currency and get expenses '< 0'
        d <- sum_by_year_class[year == in_year 
                               & stat.currency == in_currency 
                               & V1 < 0 ][order(-V1)]
        
        # filter investments 
        d <- d[t.class != "Investimento"]
        
        # signal
        if ( max(d$V1) < 0 ) {
                slices <- d$V1 * -1        
        } else {
                slices <- d$V1
        }
        
        lbls <- d$t.class

        pct <- round(slices/sum(slices)*100)
        lbls <- paste(lbls, pct) # add percents to labels 
        lbls <- paste(lbls,"%",sep="") # ad % to labels 
        
        total <- sum(d$V1) * -1
        
        main <- paste("Gastos por Categoria em ", in_year, ": (",  in_currency, ") ",
                      format(total, digits=10, nsmall=2, decimal.mark=",", big.mark="."),
                      sep = "")
        
        collors = rainbow(length(lbls))
        pie(slices, labels = lbls, main = main, col = collors)
        #legend("bottom", lbls, fill = collors)
}

plot_expenses_by_subclass <- function(dt, in_year, in_currency, in_class){
        
        # sum by subclass
        sum_by_year_class <- dt[ order(year(t.month)), sum(t.amount), 
                                 by = .( year(t.month), stat.currency,t.class, t.subclass)]
        
        # filter the year, currency, class and get expenses '< 0'
        d <- sum_by_year_class[year == in_year 
                               & stat.currency == in_currency 
                               & t.class == in_class 
                                ][order(-V1)]
        
        # signal
        if ( max(d$V1) < 0 ) {
                slices <- d$V1 * -1
                total <- sum(d$V1) * -1
        } else {
                slices <- d$V1
                total <- sum(d$V1)
        }
        
        lbls <- d$t.subclass
        
        pct <- round(slices/sum(slices)*100)
        lbls <- paste(lbls, pct) # add percents to labels 
        lbls <- paste(lbls,"%",sep="") # ad % to labels 
        
        main <- paste("Gastos Categoria ", in_class, " em ", in_year, ": (",  in_currency, ") ",
                      format(total, digits=10, nsmall=2, decimal.mark=",", big.mark="."),
                      sep = "")
        
        pie(slices, labels = lbls, main = main, col=rainbow(length(lbls)))
}


classify_all_transactions <- function(dt){
        # classfies generic classes
        dt[, t.class := 'sem categoria']
        dt[ str_detect(t.memo, "Cheque Compensado"), t.class := 'Outros']
        dt[ str_detect(t.memo, "Cheque Compensado"), t.subclass := "Pagamentos"]
        dt[ str_detect(t.memo, "Pagamento de Título"), t.class := 'Outros']
        dt[ str_detect(t.memo, "Pagamento de Título"), t.subclass := 'Pagamentos']
        dt[ str_detect(t.memo, "Pagto via Auto-Atend"), t.class := 'Outros']
        dt[ str_detect(t.memo, "Pagto via Auto-Atend"), t.subclass := 'Pagamentos']
        dt[ str_detect(t.memo, "Transferência"), t.class := 'Outros']
        dt[ str_detect(t.memo, "Transferência"), t.subclass := 'Transferencias']

        # classifies expends/incomes
        dt <- classify_transaction_textual(dt, classes)
        dt <- classify_transaction_amount(dt, classes)

        # reclassify "others" incomes
        #dt[t.class == "Outros" & t.amount > 0, t.class := "Entradas"][ t.subclass := "Transferencias"]
}


#dt[ t.class == 'sem categoria' & t.date > '2015-01-01', .(t.month, t.memo, t.class, t.amount)][order(t.month)]

dt <- classify_all_transactions(dt)

plot_expenses_by_class(dt, 2015, "BRL")
dt[year(t.date) == 2015 & t.class == "Outros", .(t.memo, t.amount, t.date, t.subclass)][order(t.date)]