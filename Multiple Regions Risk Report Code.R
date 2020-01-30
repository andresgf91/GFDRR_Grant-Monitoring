data_date <- date_data_udpated

as.of.date <- date_data_udpated

all_regions <- c("AFR","SAR","LCR","MNA","ECA","EAP","GLOBAL")
current_trustee_subset <- c("TF072236","TF072584", "TF072129","TF071630")

#for (i in all_regions) {

input <- "ALL"
#report_region <- ifelse(input$risk_region=="ALL",all_regions,input$risk_region)
report_region <- all_regions
#trustee_subset <- input$risk_trustee
trustee_subset <- current_trustee_subset
wb <- createWorkbook()
#CODE------------------

for (j in 1:length(report_region)){

data <- report_grants %>% filter(#`Child Fund Status` %in% input$risk_fund_status,
                                  Trustee  %in% trustee_subset,
                                 `Region Name` == report_region[j])

#data <- grants
region <- report_region[j]

message(paste("Preparing report for",region))


funding_sources <- trustee_subset %>%
  unique() %>%
  paste(sep="",collapse= "; ")

df <- data %>% filter(`Grant Amount`>0)#,!is.na(`Activation Date`))
df <- df %>%  select(`Closing FY`,
                     Trustee,
                     `Trustee Fund Name`,
                     `Child Fund`,
                     `Child Fund Name`,
                     `Child Fund Status`,
                     `Project ID`,
                     `Execution Type`,
                     `Child Fund TTL Name`,
                     `Managing Unit Name`,
                     `Lead GP/Global Themes`,
                     Country,
                     `Activation Date`,
                     `Closing Date`,
                     `Grant Amount`,
                     `Cumulative Disbursements`,
                     `PO Commitments`,
                     `Uncommitted Balance`,
                     `Months to Closing Date`) 

df <- df %>% filter(`Uncommitted Balance`>0)

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#df$`Child Fund Months Active` <-  elapsed_months(data_date,df$`Activation Date`)

df$`Uncommitted Balance (Percent)` <- df$`Uncommitted Balance`/df$`Grant Amount` 

df$`Required Monthly Disbursement Rate` <- (df$`Uncommitted Balance` /df$`Grant Amount`)/
  (ifelse(df$`Months to Closing Date`>=1,df$`Months to Closing Date`,1))


compute_risk_level <- function (x){
  
  risk_level <- ifelse(abs(x) < .03,"Low Risk",
                       ifelse(abs(x) < .05,"Medium Risk",
                              ifelse(abs(x) < .10,"High Risk",
                                     "Very High Risk")))
  
  return(risk_level)
}

df$`Disbursement Risk Level` <- compute_risk_level(df$`Required Monthly Disbursement Rate`)

df <- df %>% arrange(abs(as.numeric(`Required Monthly Disbursement Rate`)))

df$`Grace Period` <- ifelse(df$`Months to Closing Date`<0,
                            "Yes",
                            "No")


df$grace_period_divider <- ifelse((4 + df$`Months to Closing Date`)>=1,
                                  (4 + df$`Months to Closing Date`),
                                  1)
df$`Required Monthly Disbursement Rate` <- ifelse(df$`Months to Closing Date`<0,
                                                  (df$`Uncommitted Balance (Percent)`/df$grace_period_divider),
                                                  df$`Required Monthly Disbursement Rate`)

df <- df %>% select(-grace_period_divider)

df$funding_sources <- paste0(df$`Trustee Fund Name`," (",df$Trustee,")")

# names_trustee_subset <- left_join(as.data.frame(trustee_subset),
#   recode_trustee.2,by=c("trustee_subset"="Trustee")) %>% 
# mutate(name_number = paste0(`Trustee Fund Name`," (",trustee_subset,")"))

trustee_subset_names_number <- sort(unique(df$funding_sources)) %>%
  paste(sep="",collapse="; ")

df <- df %>% select(-funding_sources)

df$`Activation Date` <- as.Date.POSIXct(df$`Activation Date`)

df$`Months to Closing Date`<- round(df$`Months to Closing Date`,digits = 0)

options("openxlsx.dateFormat", "mm/dd/yyyy")

report_title <- paste("Disbursement Risk for",report_region[j],"region as of",as.of.date)
funding_sources <- paste("Funding Sources:",trustee_subset_names_number)

addWorksheet(wb, report_region[j])
mergeCells(wb,j,cols = 2:8,rows = 1)
mergeCells(wb,j,cols = 2:8,rows = 2)

writeData(wb,j,
          report_title,
          startRow = 1,
          startCol = 2)

writeData(wb,j,
          funding_sources,
          startRow = 2,
          startCol = 2)

writeDataTable(wb,j, df, startRow = 4, startCol = 2)


low_risk <- createStyle(fgFill ="#86F9B7",halign = "left")
medium_risk <- createStyle(fgFill ="#FFE285",halign = "left")
high_risk <- createStyle(fgFill ="#FD8D75",halign = "left")
very_high_risk <- createStyle(fgFill ="#F17979",halign = "left")

low_risk_rows <- which(df$`Disbursement Risk Level`=="Low Risk")
medium_risk_rows <- which(df$`Disbursement Risk Level`=="Medium Risk")
high_risk_rows <- which(df$`Disbursement Risk Level`=="High Risk")
very_high_risk_rows <- which(df$`Disbursement Risk Level`=="Very High Risk")


for (i in low_risk_rows){
  addStyle(wb,j,rows=i+4,cols=1:length(df) + 1,style = low_risk)
}

for (i in medium_risk_rows){
  addStyle(wb,j,rows=i+4,cols=1:length(df) + 1,style = medium_risk)
}

for (i in high_risk_rows){
  addStyle(wb,j,rows=i+4,cols=1:length(df) + 1,style = high_risk)
}

for (i in very_high_risk_rows){
  addStyle(wb,j,rows=i+4,cols=1:length(df) + 1,style = very_high_risk)
}

header_style <- createStyle(borderColour = getOption("openxlsx.borderColour", "black"),
                            borderStyle = getOption("openxlsx.borderStyle", "thick"),
                            halign = 'center',
                            valign = 'center',
                            textDecoration = NULL,
                            wrapText = TRUE)

dollar_format <- createStyle(numFmt = "ACCOUNTING")
percent_format <- createStyle(numFmt = "0.0%")
date_format <- createStyle(numFmt = "mm/dd/yyyy")
num_format <- createStyle(numFmt = "NUMBER")

addStyle(wb,j,rows=4,cols=2:(length(df)+1),style = header_style)

number_format_range <- 5:(nrow(df)+5)


addStyle(wb,j,rows=number_format_range,cols=14,style = date_format,stack = TRUE)
addStyle(wb,j,rows=number_format_range,cols=15,style = date_format,stack = TRUE) 
addStyle(wb,j,rows=number_format_range,cols=19,style = dollar_format,stack = TRUE)
addStyle(wb,j,rows=number_format_range,cols=16,style = dollar_format,stack = TRUE)
addStyle(wb,j,rows=number_format_range,cols=17,style = dollar_format,stack = TRUE)
addStyle(wb,j,rows=number_format_range,cols=18,style = dollar_format,stack = TRUE)
#addStyle(wb,j,rows=number_format_range,cols=20,style = num_format,stack = TRUE)
addStyle(wb,j,rows=number_format_range,cols=21,style = percent_format,stack = TRUE)
addStyle(wb,j,rows=number_format_range,cols=22,style = percent_format,stack = TRUE)

setColWidths(wb,j, cols=2:length(df)+1, widths = "auto")
setColWidths(wb,j, cols=6, widths = 90) #child fund name
setColWidths(wb,j, cols=9, widths = 9)
setColWidths(wb,j, cols=11, widths = 11)
setColWidths(wb,j, cols=13, widths = 25)
setColWidths(wb,j, cols=17, widths = 15)
setColWidths(wb,j, cols=19, widths = 14)
setColWidths(wb,j, cols=20, widths = 15)
setColWidths(wb,j, cols=21, widths = 19)
setColWidths(wb,j, cols=22, widths = 15)
setColWidths(wb,j, cols=23, widths = 15)


}

if(input =="ALL"){
    
    data <- report_grants %>% filter(#`Child Fund Status` %in% input$risk_fund_status,
      Trustee  %in% trustee_subset)
    
    #data <- grants
    region <- 'ALL regions'
    
    message(paste("Preparing report for",region))
    
    
    funding_sources <- trustee_subset %>%
      unique() %>%
      paste(sep="",collapse= "; ")
    
    df <- data %>% filter(`Grant Amount`>0)#,!is.na(`Activation Date`))
    df <- df %>%  select(`Closing FY`,
                         Trustee,
                         `Trustee Fund Name`,
                         `Child Fund`,
                         `Child Fund Name`,
                         `Child Fund Status`,
                         `Project ID`,
                         `Execution Type`,
                         `Child Fund TTL Name`,
                         `Managing Unit Name`,
                         `Lead GP/Global Themes`,
                         Country,
                         `Activation Date`,
                         `Closing Date`,
                         `Grant Amount`,
                         `Cumulative Disbursements`,
                         `PO Commitments`,
                         `Uncommitted Balance`,
                         `Months to Closing Date`) 
    
    df <- df %>% filter(`Uncommitted Balance`>0)
    
    elapsed_months <- function(end_date, start_date) {
      ed <- as.POSIXlt(end_date)
      sd <- as.POSIXlt(start_date)
      12 * (ed$year - sd$year) + (ed$mon - sd$mon)
    }
    
    #df$`Child Fund Months Active` <-  elapsed_months(data_date,df$`Activation Date`)
    
    df$`Uncommitted Balance (Percent)` <- df$`Uncommitted Balance`/df$`Grant Amount` 
    
    df$`Required Monthly Disbursement Rate` <- (df$`Uncommitted Balance` /df$`Grant Amount`)/
      (ifelse(df$`Months to Closing Date`>=1,df$`Months to Closing Date`,1))
    
    
    compute_risk_level <- function (x){
      
      risk_level <- ifelse(abs(x) < .03,"Low Risk",
                           ifelse(abs(x) < .05,"Medium Risk",
                                  ifelse(abs(x) < .10,"High Risk",
                                         "Very High Risk")))
      
      return(risk_level)
    }
    
    df$`Disbursement Risk Level` <- compute_risk_level(df$`Required Monthly Disbursement Rate`)
    
    df <- df %>% arrange(abs(as.numeric(`Required Monthly Disbursement Rate`)))
    
    df$`Grace Period` <- ifelse(df$`Months to Closing Date`<0,
                                "Yes",
                                "No")
    
    
    df$grace_period_divider <- ifelse((4 + df$`Months to Closing Date`)>=1,
                                      (4 + df$`Months to Closing Date`),
                                      1)
    df$`Required Monthly Disbursement Rate` <- ifelse(df$`Months to Closing Date`<0,
                                                      (df$`Uncommitted Balance (Percent)`/df$grace_period_divider),
                                                      df$`Required Monthly Disbursement Rate`)
    
    df <- df %>% select(-grace_period_divider)
    
    df$funding_sources <- paste0(df$`Trustee Fund Name`," (",df$Trustee,")")
    
    # names_trustee_subset <- left_join(as.data.frame(trustee_subset),
    #   recode_trustee.2,by=c("trustee_subset"="Trustee")) %>% 
    # mutate(name_number = paste0(`Trustee Fund Name`," (",trustee_subset,")"))
    
    trustee_subset_names_number <- sort(unique(df$funding_sources)) %>%
      paste(sep="",collapse="; ")
    
    df <- df %>% select(-funding_sources)
    
    df$`Activation Date` <- as.Date.POSIXct(df$`Activation Date`)
    
    df$`Months to Closing Date`<- round(df$`Months to Closing Date`,digits = 0)
    
    options("openxlsx.dateFormat", "mm/dd/yyyy")
    
    report_title <- paste("Disbursement Risk for",region,"region(s) as of",as.of.date)
    funding_sources <- paste("Funding Sources:",trustee_subset_names_number)
    
    addWorksheet(wb, region)
    mergeCells(wb,8,cols = 2:8,rows = 1)
    mergeCells(wb,8,cols = 2:8,rows = 2)
    
    writeData(wb,8,
              report_title,
              startRow = 1,
              startCol = 2)
    
    writeData(wb,8,
              funding_sources,
              startRow = 2,
              startCol = 2)
    
    writeDataTable(wb,8, df, startRow = 4, startCol = 2)
    
    
    low_risk <- createStyle(fgFill ="#86F9B7",halign = "left")
    medium_risk <- createStyle(fgFill ="#FFE285",halign = "left")
    high_risk <- createStyle(fgFill ="#FD8D75",halign = "left")
    very_high_risk <- createStyle(fgFill ="#F17979",halign = "left")
    
    low_risk_rows <- which(df$`Disbursement Risk Level`=="Low Risk")
    medium_risk_rows <- which(df$`Disbursement Risk Level`=="Medium Risk")
    high_risk_rows <- which(df$`Disbursement Risk Level`=="High Risk")
    very_high_risk_rows <- which(df$`Disbursement Risk Level`=="Very High Risk")
    
    
    for (i in low_risk_rows){
      addStyle(wb,8,rows=i+4,cols=1:length(df) + 1,style = low_risk)
    }
    
    for (i in medium_risk_rows){
      addStyle(wb,8,rows=i+4,cols=1:length(df) + 1,style = medium_risk)
    }
    
    for (i in high_risk_rows){
      addStyle(wb,8,rows=i+4,cols=1:length(df) + 1,style = high_risk)
    }
    
    for (i in very_high_risk_rows){
      addStyle(wb,8,rows=i+4,cols=1:length(df) + 1,style = very_high_risk)
    }
    
    header_style <- createStyle(borderColour = getOption("openxlsx.borderColour", "black"),
                                borderStyle = getOption("openxlsx.borderStyle", "thick"),
                                halign = 'center',
                                valign = 'center',
                                textDecoration = NULL,
                                wrapText = TRUE)
    
    dollar_format <- createStyle(numFmt = "ACCOUNTING")
    percent_format <- createStyle(numFmt = "0.0%")
    date_format <- createStyle(numFmt = "mm/dd/yyyy")
    num_format <- createStyle(numFmt = "NUMBER")
    
    addStyle(wb,8,rows=4,cols=2:(length(df)+1),style = header_style)
    
    number_format_range <- 5:(nrow(df)+5)
    
    
    addStyle(wb,8,rows=number_format_range,cols=14,style = date_format,stack = TRUE)
    addStyle(wb,8,rows=number_format_range,cols=15,style = date_format,stack = TRUE) 
    addStyle(wb,8,rows=number_format_range,cols=19,style = dollar_format,stack = TRUE)
    addStyle(wb,8,rows=number_format_range,cols=16,style = dollar_format,stack = TRUE)
    addStyle(wb,8,rows=number_format_range,cols=17,style = dollar_format,stack = TRUE)
    addStyle(wb,8,rows=number_format_range,cols=18,style = dollar_format,stack = TRUE)
    #addStyle(wb,8,rows=number_format_range,cols=20,style = num_format,stack = TRUE)
    addStyle(wb,8,rows=number_format_range,cols=21,style = percent_format,stack = TRUE)
    addStyle(wb,8,rows=number_format_range,cols=22,style = percent_format,stack = TRUE)
    
    setColWidths(wb,8, cols=2:length(df)+1, widths = "auto")
    setColWidths(wb,8, cols=6, widths = 90) #child fund name
    setColWidths(wb,8, cols=9, widths = 9)
    setColWidths(wb,8, cols=11, widths = 11)
    setColWidths(wb,8, cols=13, widths = 25)
    setColWidths(wb,8, cols=17, widths = 15)
    setColWidths(wb,8, cols=19, widths = 14)
    setColWidths(wb,8, cols=20, widths = 15)
    setColWidths(wb,8, cols=21, widths = 19)
    setColWidths(wb,8, cols=22, widths = 15)
    setColWidths(wb,8, cols=23, widths = 15)
    
  }
  

#openxlsx::openXL(wb)


