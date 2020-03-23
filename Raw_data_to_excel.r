require(openxlsx)

all_grants <- all_grants %>%
  dplyr::rename("Available Balance (Uncommitted)" = `Available Balance USD`) %>% 
  dplyr::mutate("Available Balance (Uncommitted)" =
                  `Grant Amount USD` - (`Disbursements USD` + `Commitments USD`)) %>% 
  dplyr::mutate("Real Transfers in" = `Transfer-in USD` - `Transfers-out in USD`) %>% 
  dplyr::mutate("Not Yet Transferred" = `Grant Amount USD` - `Real Transfers in`) %>% 
  select(-c("Transfer-in USD","Transfers-out in USD"))

all_grants <- all_grants %>%
  mutate(`Fund Country Region Name`= Region) %>% 
  select(-Region) %>% 
  rename("Region Name"=`Fund Country Region Name`,
         "Cumulative Disbursements USD" = `Disbursements USD`)


active_data <- report_grants

active_data <- active_data  %>%
  rename(
    "Window #" = `Window Number`,
    "Trustee #" = Trustee,
    "Child Fund #" = `Child Fund`,
    "Lead GP/Global Theme" = `Lead GP/Global Themes`,
    "Managing Unit" = `Managing Unit Name`,
    "2020 Disbursements" = `2020 Disbursement USD`,
    "Available Balance (Uncommitted)" = `Uncommitted Balance`,
    "Required Monthly Disbursement Rate =(Remaining/GrantAmount)/MonthClosing" =
      required_disbursement_rate
  ) %>% 
  dplyr::mutate("Real Transfers in" = `Transfer-in USD` - `Transfers-out in USD`) %>% 
  dplyr::mutate("Not Yet Transferred" = `Grant Amount` - `Real Transfers in`) 


active_grants <- active_data %>%
  select(
    `Closing FY`,
    `Window #`,
    `Window Name`,
    `Trustee #`,
    `Child Fund #`,
    `Project ID`,
    `Child Fund Name`,
    `Execution Type`,
    `Child Fund Status`,
    `Child Fund TTL Name`,
    `TTL Unit`,
    `Managing Unit`,
    `Lead GP/Global Theme`,
    VPU,
    `Region Name`,
    Country,
    `Activation Date`,
    `Closing Date`,
    `Grant Amount`,
    `Cumulative Disbursements`,
    `2020 Disbursements`,
    `PO Commitments`,
    `Real Transfers in`,
    `Not Yet Transferred`,
    `Available Balance (Uncommitted)`,
    `Months to Closing Date`,
    `Required Monthly Disbursement Rate =(Remaining/GrantAmount)/MonthClosing`
  )

active_grants <- active_grants %>%
  rename("Required Monthly Disbursement Rate" = 
           `Required Monthly Disbursement Rate =(Remaining/GrantAmount)/MonthClosing`) %>% 
  mutate(`Available Balance (Uncommitted)`= `Grant Amount` - 
           (`Cumulative Disbursements` + `PO Commitments`)
         ) %>% 
  mutate(`Required Monthly Disbursement Rate` =
           (`Available Balance (Uncommitted)`/ `Grant Amount`)/`Months to Closing Date`
         )


PMA_grants_excel <- active_grants %>% filter(`Child Fund #` %in% PMA_grants$Fund)

active_grants <- active_grants %>% 
  filter(!(`Child Fund #` %in% report_grants$`Child Fund`[report_grants$PMA=='yes']))


compute_risk_level <- function (x){
  
  risk_level <- ifelse(x < .025,"Low Risk",
                       ifelse(x < .055,"Medium Risk",
                              ifelse(x < .105,"High Risk",
                                     "Very High Risk")))

  
  return(risk_level)
}


active_grants$`Disbursement Risk Level` <- compute_risk_level(as.numeric(active_grants$`Required Monthly Disbursement Rate`))


active_grants$`Disbursement Risk Level` <- ifelse(active_grants$`Months to Closing Date`<0,
                                                "Closed (Grace Period)",
                                                active_grants$`Disbursement Risk Level`)


active_grants$`Disbursement Risk Level` <- factor(active_grants$`Disbursement Risk Level` ,
                                           levels = c( "Closed (Grace Period)",
                                                       "Very High Risk",
                                                       "High Risk",
                                                       "Medium Risk",
                                                       "Low Risk"))

active_grants <- active_grants %>% arrange(`Disbursement Risk Level`)


trustee <- trustee %>% select(`Donor Name`,Fund,`Fund Name`,`Fund TTL Name`,`TF End Disb Date`) 


names(trustee) <- c("Donor",
                    "Trustee",
                    "Trustee Fund Name",
                    "Trustee Fund TTL Name",
                    "End Disbursement Date")

trustee <- trustee %>% arrange(Donor)


#create excel workbook
wb <- createWorkbook()

#addtables 
addWorksheet(wb, "All Grants")
writeDataTable(wb,"All Grants", all_grants,
               withFilter = TRUE,tableStyle = "None")

addWorksheet(wb, "Active Grants")
writeDataTable(wb,"Active Grants", active_grants,startRow = 8,
               withFilter = TRUE,tableStyle = "TableStyleLight9")

addWorksheet(wb, "PMA Grants")
writeDataTable(wb,"PMA Grants", PMA_grants_excel,
               withFilter = TRUE,tableStyle = "TableStyleLight12")

addWorksheet(wb, "GFDRR TFs")
writeDataTable(wb,"GFDRR TFs", trustee,
               withFilter = TRUE,tableStyle = "TableStyleLight10")



#FORMATTING 
low_risk <- createStyle(fgFill ="#86F9B7",halign = "left")
medium_risk <- createStyle(fgFill ="#FFE285",halign = "left")
high_risk <- createStyle(fgFill ="#FD8D75",halign = "left")
very_high_risk <- createStyle(fgFill ="#F17979",halign = "left")
grace_period_style <- createStyle(fgFill ="#B7B7B7",halign = "left")

low_risk_rows <- which(active_grants$`Disbursement Risk Level`=="Low Risk")
medium_risk_rows <- which(active_grants$`Disbursement Risk Level`=="Medium Risk")
high_risk_rows <- which(active_grants$`Disbursement Risk Level`=="High Risk")
very_high_risk_rows <- which(active_grants$`Disbursement Risk Level`=="Very High Risk")
grace_period_rows <- which(active_grants$`Disbursement Risk Level`=="Closed (Grace Period)")


RISK.df.row <- 8

for (i in low_risk_rows){
  addStyle(wb,2,rows=i+RISK.df.row,cols=1:length(active_grants),style = low_risk)
}

for (i in medium_risk_rows){
  addStyle(wb,2,rows=i+RISK.df.row,cols=1:length(active_grants),style = medium_risk)
}

for (i in high_risk_rows){
  addStyle(wb,2,rows=i+RISK.df.row,cols=1:length(active_grants),style = high_risk)
}

for (i in very_high_risk_rows){
  addStyle(wb,2,rows=i+RISK.df.row,cols=1:length(active_grants),style = very_high_risk)
}

for (i in grace_period_rows){
  addStyle(wb,2,rows=i+RISK.df.row,cols=1:length(active_grants),style = grace_period_style)
}


legend_text <- c("Closed. These grants are under the grace period.",
                 "Very High Risk (>10% / month disbursement required)",
                 "High Risk ( > 5%-10% / month disbursement required)",
                 "Medium Risk (3 - 5% / month disbursement required)",
                 "Low Risk (<  3% / month disbursement required)")

sheet_header <- paste("GFDRR Active Grant List as of",date_data_udpated)


writeData(wb,2,sheet_header,startCol = 1,startRow = 1)
writeData(wb,2,"Disbursement Risk Legend",startCol = 6,startRow = 1)
writeData(wb,2,legend_text,startCol = 7,startRow = 2)

dollar_format <- createStyle(numFmt = "ACCOUNTING")
percent_format <- createStyle(numFmt = "0%")
date_format <- createStyle(numFmt = "mm/dd/yyyy")
num_format <- createStyle(numFmt = "NUMBER")
wrapped_text <- createStyle(wrapText = TRUE,valign = "top")
black_and_bold <- createStyle(fontColour = "#000000",textDecoration = 'bold')



addStyle(wb,2,rows=2,cols = 6, style = grace_period_style)
addStyle(wb,2,rows=3,cols = 6, style = very_high_risk)
addStyle(wb,2,rows=4,cols = 6, style = high_risk)
addStyle(wb,2,rows=5,cols = 6, style = medium_risk)
addStyle(wb,2,rows=6,cols = 6, style = low_risk)


all_grants_row_range <- 1:nrow(all_grants)+1
addStyle(wb,1,rows=all_grants_row_range,cols=25,style=date_format)   #activation date
addStyle(wb,1,rows=all_grants_row_range,cols=26,style=date_format)   #closing date
addStyle(wb,1,rows=all_grants_row_range,cols=27,style=dollar_format) #grant amount
addStyle(wb,1,rows=all_grants_row_range,cols=28,style=dollar_format) #cumulative disbursements
addStyle(wb,1,rows=all_grants_row_range,cols=29,style=dollar_format) #2020 disbursements
addStyle(wb,1,rows=all_grants_row_range,cols=30,style=dollar_format) #commitments
addStyle(wb,1,rows=all_grants_row_range,cols=31,style=dollar_format) #available balance
addStyle(wb,1,rows=all_grants_row_range,cols=32,style=dollar_format) #real- transfer-in
addStyle(wb,1,rows=all_grants_row_range,cols=33,style=dollar_format) #Not yet Transferred


active_grants_row_range <- 8:nrow(active_grants)+1
addStyle(wb,2,rows=active_grants_row_range,cols=17,style=date_format,stack = TRUE)   #activation date
addStyle(wb,2,rows=active_grants_row_range,cols=18,style=date_format,stack = TRUE)   #closing date
addStyle(wb,2,rows=active_grants_row_range,cols=19,style=dollar_format,stack = TRUE) #grant amount
addStyle(wb,2,rows=active_grants_row_range,cols=20,style=dollar_format,stack = TRUE) #cumulative disbursements
addStyle(wb,2,rows=active_grants_row_range,cols=22,style=dollar_format,stack = TRUE) #2020 disbursements
addStyle(wb,2,rows=active_grants_row_range,cols=22,style=dollar_format,stack = TRUE) #commitments
addStyle(wb,2,rows=active_grants_row_range,cols=23,style=dollar_format,stack = TRUE) #real- transfer-in
addStyle(wb,2,rows=active_grants_row_range,cols=24,style=dollar_format,stack = TRUE) #Not yet Transferred
addStyle(wb,2,rows=active_grants_row_range,cols=25,style=dollar_format,stack = TRUE) #available balance
addStyle(wb,2,rows=active_grants_row_range,cols=27,style=percent_format,stack = TRUE) #required monthly disbursement rate

addStyle(wb,2,rows = 8,cols = 1:ncol(active_grants),style = wrapped_text,stack = TRUE)
setColWidths(wb,2,cols = 7,widths = 30)
setColWidths(wb,2, cols = 10,widths = 12)
setColWidths(wb,2,cols = 17:25,widths = 15)
setColWidths(wb,2,cols = 26,widths = 12)
setColWidths(wb,2,cols = 27,widths = 12)
setColWidths(wb,2,cols = 28,widths = 20)



PMA_grants_row_range <- 1:nrow(PMA_grants_excel)+1
addStyle(wb,3,rows=PMA_grants_row_range,cols=17,style=date_format,stack = TRUE)   #activation date
addStyle(wb,3,rows=PMA_grants_row_range,cols=18,style=date_format,stack = TRUE)   #closing date
addStyle(wb,3,rows=PMA_grants_row_range,cols=19,style=dollar_format,stack = TRUE) #grant amount
addStyle(wb,3,rows=PMA_grants_row_range,cols=20,style=dollar_format,stack = TRUE) #cumulative disbursements
addStyle(wb,3,rows=PMA_grants_row_range,cols=21,style=dollar_format,stack = TRUE) #2020 disbursements
addStyle(wb,3,rows=PMA_grants_row_range,cols=22,style=dollar_format,stack = TRUE) #commitments
addStyle(wb,3,rows=PMA_grants_row_range,cols=23,style=dollar_format,stack = TRUE) #real- transfer-in
addStyle(wb,3,rows=PMA_grants_row_range,cols=24,style=dollar_format,stack = TRUE) #Not yet Transferred
addStyle(wb,3,rows=PMA_grants_row_range,cols=25,style=dollar_format,stack = TRUE) #available balance
addStyle(wb,3,rows=PMA_grants_row_range,cols=27,style=percent_format,stack = TRUE)
addStyle(wb,3,rows = 1,cols = 1:ncol(PMA_grants_excel),style = wrapped_text,stack = TRUE)


addStyle(wb,4,rows=1:nrow(trustee)+1,cols=5,style=date_format,stack = TRUE)
setColWidths(wb,4, cols = 1:ncol(trustee), widths = "auto")


