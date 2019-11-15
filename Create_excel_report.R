
generate_risk_report <- function (data,region) {

require(openxlsx)
  
wb <- createWorkbook()
df <- data %>% filter(`Fund Status`=="ACTV",`Grant Amount USD`>0)
df <- df %>%  select(Trustee,
                     temp.name,
                     Fund,
                     `Fund Name`,
                     `Project ID`,
                     `DF Execution Type`,
                     `Fund TTL Name`,
                     Region,
                     Country,
                     `Grant Amount USD`,
                     `Disbursements USD`,
                     `Commitments USD`,
                     months_to_end_disbursement,
                     unnacounted_amount,
                     percent_unaccounted,
                     burn_rate,
                     required_disbursement_rate,
                     disbursement_risk_level)

report_title <- paste("Disbursement Risk Report for",region,"Region")

addWorksheet(wb, "Disbursement Risk Report")
mergeCells(wb,1,c(2,3,4),1)

writeData(wb, 1,
          report_title,
          startRow = 1,
          startCol = 2)

writeDataTable(wb, 1, df, startRow = 3, startCol = 2)


low_risk <- createStyle(fgFill ="#2ECC71")
medium_risk <- createStyle(fgFill ="#FFC300")
high_risk <- createStyle(fgFill ="#FF5733")
very_high_risk <- createStyle(fgFill ="#C70039")

low_risk_rows <- which(df$disbursement_risk_level=="Low Risk") 
medium_risk_rows <- which(df$disbursement_risk_level=="Medium Risk")
high_risk_rows <- which(df$disbursement_risk_level=="High Risk")
very_high_risk_rows <- which(df$disbursement_risk_level=="Very High Risk")


for (i in low_risk_rows){
addStyle(wb,1,rows=i+3,cols=1:length(df)+1,style = low_risk)
}

for (i in medium_risk_rows){
  addStyle(wb,1,rows=i+3,cols=1:length(df)+1,style = medium_risk)
}

for (i in high_risk_rows){
  addStyle(wb,1,rows=i+3,cols=1:length(df)+1,style = high_risk)
}

for (i in very_high_risk_rows){
  addStyle(wb,1,rows=i+3,cols=1:length(df)+1,style = very_high_risk)
}

header_style <- createStyle(borderColour = getOption("openxlsx.borderColour", "black"),
            borderStyle = getOption("openxlsx.borderStyle", "thick"),
            halign = 'center', valign = 'center', textDecoration = NULL,
            wrapText = TRUE)

addStyle(wb,1,rows=3,cols=2:length(df)+1,style = header_style)

## opens a temp version
openXL(wb)

}
