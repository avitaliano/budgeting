# Analysis of OFX files

library(data.table)
library(stringr)

# removes scientific notation
options(scipen=999)

# setting csv base dir
csv_basepath = "~/Documents/SpiderOak/Financeiro/ofx/"

load_ofx_flat <- function(basedir){
        
    # lists csv files
    csv_list <- paste( csv_basepath, list.files(pattern = "*.csv", path = csv_basepath, recursive = TRUE), sep = '')

    colClasses <- c('integer', 'string', 'string', 'string', 'string', 'string', 'factor', 'double', 'factor',
                    'date', 'date', 'string', 'string', 'date', 'double',  'string', 'factor', 'string')

    # loads csv into data frames
    dt = do.call(rbind, lapply(csv_list, fread, colClasses = colClasses))

    # cast date columns to date
    dt$stat.start_date <- as.Date(dt$stat.start_date)
    dt$stat.end_date <- as.Date(dt$stat.end_date)
    dt$t.date <- as.Date(dt$t.date)

    dt[, t.month := as.Date(strftime(t.date, "%Y-%m-01")) ]
    dt[, year := year(t.date) ]

    dt
}

load_classes <- function(classes_file){

    # tests if classes file exists
    if ( file.exists(classes_file)){
        # loads classification file
        classes = fread(classes_file, header = TRUE, sep = ";", stringsAsFactors = FALSE)
    } else{
        # creates empty classes file
        warning(paste("file ", classes_file, " does not exist. creating one."))
        classes <- data.table(match_type = character(), class = character(),
                              subclass = character(), match_expression = character(), match_amount= double())
    }

    classes
}

update_classes <- function(classes, in_match_type, in_class, in_subclass, in_expression, in_amount = NA){
    new_classes <- rbind(classes, data.table(match_type=in_match_type,
                                             class=in_class, subclass = in_subclass,
                                             match_expression=in_expression,
                                             match_amount=in_amount))
    new_classes[order(match_type, class, subclass, match_expression)]
}

write_classes <- function(classes, filename){
    write.table(classes, filename, sep = ';', row.names = FALSE)
}

classify_transaction_textual <- function(dt, classes){

    classes <- classes[match_type == "textual",]
    for (i in 1:nrow(classes)){
        dt[ str_detect(t.memo, classes[i,match_expression]), t.class := classes[i,class]]
        dt[ str_detect(t.memo, classes[i,match_expression]), t.subclass := classes[i,subclass]]
    }

    dt
}

classify_transaction_amount <- function(dt, classes){
    classes <- classes[classes$match_type == "amount",]
    for (i in 1:nrow(classes)){
        dt[ str_detect(t.memo, classes[i,match_expression]) & t.amount == classes[i,match_amount], 
           t.class := classes[i,class]]
        dt[ str_detect(t.memo, classes[i,match_expression]) & t.amount == classes[i,match_amount], 
           t.subclass := classes[i,subclass]]
    }

    dt
}

classify_all_transactions <- function(dt, classes){
    # classfies generic classes
    dt[, t.class := 'sem categoria']

    # classifies payment methods with generic payment classes - Bank BB
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

}

sum_expenses_by_class <- function(dt, in_year, in_currency = "BRL"){
        # sum by class and year
        sum <- dt[ year == in_year & stat.currency == in_currency, 
                   sum(t.amount),
                   by = .(year, stat.currency, t.class)]
        
        # total income = class "Entradas"
        total_income <- sum[t.class =="Entradas"][,V1]
        sum[, perc := V1 / total_income * -1]
        
        # returns
        sum[order(-V1)]
}

sum_expenses_by_subclass <- function(dt, in_year, in_class, in_currency = "BRL"){
        # sum by class and year
        sum <- dt[ year == in_year & t.class == in_class & stat.currency == in_currency, 
                   sum(t.amount),
                   by = .(year, stat.currency, t.subclass)]
        
        total <- sum[, sum(V1)]
        sum[, perc := V1 / total]
        sum
}

preview_by_class <- function(dt,in_year, in_class){
        dt[year(t.month) == in_year & t.class == in_class,
           .(t.subclass,t.date,t.memo,t.amount)]
}

plot_expenses_by_class <- function(dt, in_year, in_currency = "BRL", filter_investment = TRUE){

    # sum by class, filtering by year and currency
    sum <- sum_expenses_by_class(dt, in_year)

    # filter currency and income class (entradas)'
    sum <- sum[stat.currency == in_currency & t.class != "Entradas"][order(-V1)]

    # filter investments
    if ( filter_investment == TRUE){
            sum <- sum[ t.class != "Investimento"][order(-V1)]
    }
    
    # data
    slices <- abs(sum[,V1])
    lbls <- sum[,t.class]

    total <- sum(slices)
    
    pct <- round( slices / total * 100 )
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 

    main <- paste("Gastos por categoria (", in_year, "): ",  in_currency, " ",
                  format(total, digits=10, nsmall=2, decimal.mark=",", big.mark="."),
                  sep = "")

    collors = rainbow(length(lbls))
    pie(slices, labels = lbls, main = main, col = collors)
    #legend("bottom", lbls, fill = collors)
}

plot_expenses_by_subclass <- function(dt, in_year, in_class, in_currency = "BRL"){

    # sum by subclass
    sum <- sum_expenses_by_subclass(dt, in_year, in_class, in_currency)

    # filter the year, currency, class and get expenses '< 0'
    sum <- sum[order(-V1)]

    # signal
    slices <- abs(sum[,V1])
    lbls <- sum[,t.subclass]

    total <- sum(slices)
    
    pct <- round( slices / total * 100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 

    main <- paste("Gastos Categoria ", in_class, " em ", in_year, ": (",  in_currency, ") ",
                  format(total, digits=10, nsmall=2, decimal.mark=",", big.mark="."),
                  sep = "")

    pie(slices, labels = lbls, main = main, col=rainbow(length(lbls)))
}

# this function append recursive expenses to the dt
# the expenses are indicated in the classes.csv file as "monthly"match type
append_monthly_expenses <- function(dt, classes){

        # to-do: not implemented
    classes <- classes[match_type == "monthly"]
    
}

# init procedures
classes <- load_classes("classes.csv")
dt <- load_ofx_flat(csv_basepath)
classify_all_transactions(dt, classes)

plot_expenses_by_class(dt, 2015, "BRL", TRUE)
