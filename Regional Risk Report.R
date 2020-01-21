library(openxlsx)
library(dplyr)
library(readxl)

grants_file <- "GFDRR Raw Data 1_13_20.xlsx"

as.of.date <- "January 13, 2020"
data_date <- as.Date("2020-01-13")

all_regions <- c("AFR","SAR","LCR","MNA","ECA","EAP","Global")

#for (i in all_regions) {
  

report_region <- "AFR"
trustee_subset <- c("TF072236","TF072584", "TF072129","TF071630") 

#CODE------------------
grants <- read_xlsx(grants_file,2)
grants <- grants %>% filter(`Child Fund Status` %in% c("ACTV","PEND"),
                            Trustee %in% trustee_subset,
                            `Region Name` %in% report_region)

recode_trustee.2 <- read_xlsx("GFDRR Trustee Names.xlsx")

grants <- left_join(grants,recode_trustee.2,by = c("Trustee"="Trustee"))

      data <- grants
      region <- report_region
      funding_sources <- data$`Trustee Fund Name`  %>%
        unique() %>%
        paste(sep="",collapse= "; ")
      wb <- createWorkbook()
      df <- data %>% filter(`Grant Amount`>0,!is.na(`Activation Date`))
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
                           `Months to Closing Date`) %>%
        mutate("Uncommitted Balance" =
                 `Grant Amount` - (`Cumulative Disbursements` +`PO Commitments`)
        )
      
      df <- df %>% filter(`Uncommitted Balance`>0)
      
      elapsed_months <- function(end_date, start_date) {
        ed <- as.POSIXlt(end_date)
        sd <- as.POSIXlt(start_date)
        12 * (ed$year - sd$year) + (ed$mon - sd$mon)
      }
      
      #df$`Child Fund Months Active` <-  elapsed_months(data_date,df$`Activation Date`)
      
      df$`Months to Closing Date 2` <- as.numeric(round(
        (difftime(
          strptime(df$`Closing Date`, format = "%Y-%m-%d"),
          strptime(data_date, format = "%Y-%m-%d"),
          units="days")
         )/30,digits = 1
        ))
      
      df$`Uncommitted Balance (Percent)` <- df$`Uncommitted Balance`/df$`Grant Amount` 
      
      df$`Required Monthly Disbursement Rate` <- (df$`Uncommitted Balance` /df$`Grant Amount`)/
        (ifelse(df$`Months to Closing Date 2`>=1,df$`Months to Closing Date 2`,1))
     
      
      compute_risk_level <- function (x){
        
        risk_level <- ifelse(abs(x) < .03,"Low Risk",
                             ifelse(abs(x) < .05,"Medium Risk",
                                    ifelse(abs(x) < .10,"High Risk",
                                           "Very High Risk")))
        
        return(risk_level)
      }
      
      df$`Disbursement Risk Level` <- compute_risk_level(df$`Required Monthly Disbursement Rate`)
      
      df <- df %>% arrange(abs(as.numeric(`Required Monthly Disbursement Rate`)))
      
      df$`Grace Period` <- ifelse(df$`Months to Closing Date 2`<0,
                                  "Yes",
                                  "No")
     
    
      
     df <- df %>% select(-`Months to Closing Date`) %>% 
       dplyr::rename("Months to Closing Date" = `Months to Closing Date 2`)
     
     df$grace_period_divider <- ifelse((4 + df$`Months to Closing Date`)>=1,
                                       (4 + df$`Months to Closing Date`),
                                       1)
     df$`Required Monthly Disbursement Rate` <- ifelse(df$`Months to Closing Date`<0,
                                                       (df$`Uncommitted Balance (Percent)`/df$grace_period_divider),
                                                       df$`Required Monthly Disbursement Rate`)
     
     df <- df %>% select(-grace_period_divider)
      
      names_trustee_subset <- left_join(as.data.frame(trustee_subset),
                                        recode_trustee.2,by=c("trustee_subset"="Trustee")) %>% 
        mutate(name_number = paste0(`Trustee Fund Name`," (",trustee_subset,")"))
      
      trustee_subset_names_number <- names_trustee_subset$name_number %>%
        paste(sep="",collapse="; ")
      
      df$`Activation Date` <- as.Date.POSIXct(df$`Activation Date`)
      
      options("openxlsx.dateFormat", "mm/dd/yyyy")
      wb <- createWorkbook()
      report_title <- paste("Disbursement Risk for",report_region,"Region - as of",as.of.date)
      funding_sources <- paste("Funding Sources:",trustee_subset_names_number)
      
      addWorksheet(wb, "Disbursement Risk")
      mergeCells(wb,1,cols = 2:8,rows = 1)
      mergeCells(wb,1,cols = 2:8,rows = 2)
      
      writeData(wb, 1,
                report_title,
                startRow = 1,
                startCol = 2)
      
      writeData(wb, 1,
                funding_sources,
                startRow = 2,
                startCol = 2)
      
      writeDataTable(wb, 1, df, startRow = 4, startCol = 2)
      
      
      low_risk <- createStyle(fgFill ="#86F9B7",halign = "left")
      medium_risk <- createStyle(fgFill ="#FFE285",halign = "left")
      high_risk <- createStyle(fgFill ="#FD8D75",halign = "left")
      very_high_risk <- createStyle(fgFill ="#F17979",halign = "left")
      
      low_risk_rows <- which(df$`Disbursement Risk Level`=="Low Risk")
      medium_risk_rows <- which(df$`Disbursement Risk Level`=="Medium Risk")
      high_risk_rows <- which(df$`Disbursement Risk Level`=="High Risk")
      very_high_risk_rows <- which(df$`Disbursement Risk Level`=="Very High Risk")
      
      
      for (i in low_risk_rows){
        addStyle(wb,1,rows=i+4,cols=1:length(df) + 1,style = low_risk)
      }
      
      for (i in medium_risk_rows){
        addStyle(wb,1,rows=i+4,cols=1:length(df) + 1,style = medium_risk)
      }
      
      for (i in high_risk_rows){
        addStyle(wb,1,rows=i+4,cols=1:length(df) + 1,style = high_risk)
      }
      
      for (i in very_high_risk_rows){
        addStyle(wb,1,rows=i+4,cols=1:length(df) + 1,style = very_high_risk)
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
      
      addStyle(wb,1,rows=4,cols=2:(length(df)+1),style = header_style)
      
      number_format_range <- 5:(nrow(df)+5)
      
      
      addStyle(wb,1,rows=number_format_range,cols=14,style = date_format,stack = TRUE)
      addStyle(wb,1,rows=number_format_range,cols=15,style = dollar_format,stack = TRUE)
      addStyle(wb,1,rows=number_format_range,cols=16,style = dollar_format,stack = TRUE)
      addStyle(wb,1,rows=number_format_range,cols=17,style = dollar_format,stack = TRUE)
      addStyle(wb,1,rows=number_format_range,cols=18,style = dollar_format,stack = TRUE)
      addStyle(wb,1,rows=number_format_range,cols=20,style = percent_format,stack = TRUE)
      addStyle(wb,1,rows=number_format_range,cols=21,style = percent_format,stack = TRUE)
      addStyle(wb,1,rows=number_format_range,cols=22,style = date_format,stack = TRUE)
      
      setColWidths(wb, 1, cols=2:length(df)+1, widths = "auto")
      setColWidths(wb, 1, cols=7, widths = 90) #child fund name
      setColWidths(wb, 1, cols=9, widths = 9)
      setColWidths(wb, 1, cols=11, widths = 11)
      setColWidths(wb, 1, cols=13, widths = 25)
      setColWidths(wb, 1, cols=17, widths = 15)
      setColWidths(wb, 1, cols=19, widths = 14)
      setColWidths(wb, 1, cols=20, widths = 15)
      setColWidths(wb, 1, cols=21, widths = 19)
      setColWidths(wb, 1, cols=22, widths = 15)
      setColWidths(wb, 1, cols=23, widths = 15)
     
      ## opens a temp version
#-----SAVE FILE----------------------------------------      
      
      file.name <- paste0("~/Akiko_Sonia/Risk Reports/",report_region,"_risk_V1.xlsx")
     # saveWorkbook(wb,file = file.name,overwrite = TRUE)
openXL(wb)
#}