library(openxlsx)
library(dplyr)
library(stringi)

## DATA CRUNCH (Pre-processing) --------

# Tab.1. Data Crunh --------

current_trustee_subset <- c("TF072236","TF072584", "TF072129","TF071630")
current_trustee_subset <- active_trustee %>%
  filter(Fund %in% c("TF072236","TF072584", "TF072129","TF071630")) %>%
  select(temp.name) 

as.of.date <- date_data_udpated

data <- report_grants %>% filter(PMA=='no')

region <- 'ALL regions'

message(paste("Preparing report for",region))


wb <- createWorkbook()

funding_sources <- active_trustee$temp.name %>% #changeeeeee!
  unique() %>%
  paste(sep = "",collapse= "; ")

current_trustee_subset_collapsed <- sort(current_trustee_subset$temp.name)%>% #changeeeeee!
  unique() %>%
  paste(sep = "",collapse= "; ")


df <- data %>%  select(`Closing FY`,`Trustee #`, 
                       `Child Fund #`,
                       `Child Fund Name`,
                       `Child Fund Status`,
                       `Project ID`,
                       `Execution Type`,
                       `Child Fund TTL Name`,
                       `Child Fund TTL Name`,
                       `Managing Unit`,
                       `Lead GP/Global Theme`,
                       Country,
                       `Activation Date`,
                       `Closing Date`,
                       `Grant Amount`,
                       `Cumulative Disbursements`,
                       `PO Commitments`,
                       `Remaining Available Balance`,
                       `Months to Closing Date`,
                       `Region Name`,
                       `Required Monthly Disbursement Rate =(Remaining/GrantAmount)/MonthClosing`)


elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#df$`Child Fund Months Active` <-  elapsed_months(data_date,df$`Activation Date`)

df$`Uncommitted Balance (Percent)` <- df$`Remaining Available Balance`/df$`Grant Amount`

df <- df %>% rename("Required Monthly Disbursement Rate" =
                      `Required Monthly Disbursement Rate =(Remaining/GrantAmount)/MonthClosing`)



df$`Required Monthly Disbursement Rate` <- ifelse(df$`Required Monthly Disbursement Rate`=='Closed',
                                                  999,
                                                  as.numeric(df$`Required Monthly Disbursement Rate`))


compute_risk_level <- function (x){
  
  risk_level <- ifelse(x == 999, "Closed (Grace Period)",
                       ifelse(x < .025,"Low Risk",
                              ifelse(x < .055,"Medium Risk",
                                     ifelse(x < .105,"High Risk",
                                            "Very High Risk"))))
  
  return(risk_level)
}



df$`Disbursement Risk Level` <- compute_risk_level(as.numeric(df$`Required Monthly Disbursement Rate`))


df$`Grace Period` <- ifelse(df$`Disbursement Risk Level`=="Closed (Grace Period)",
                            "Yes",
                            "No")

#if (!is.null(df$`Child Fund`=='TF0B2168')){

#df$`Grace Period`[df$`Child Fund`=='TF0B2168'] <- "Yes"}

#df$`Disbursement Risk Level`[459] <- "Closed (Grace Period)"

df <- df %>%
  arrange(match(`Disbursement Risk Level`, c("Closed (Grace Period)","Very High Risk","High Risk","Medium Risk", "Low Risk" )))


#df <- df %>% arrange(abs(as.numeric(`Required Monthly Disbursement Rate`)))

df <- left_join(df,recode_trustee,
                by = c("Trustee #"="Trustee"))

df$funding_sources <- paste0(df$`Trustee Fund Name`," (",df$`Trustee #`,")")

# names_trustee_subset <- left_join(as.data.frame(trustee_subset),
#   recode_trustee.2,by=c("trustee_subset"="Trustee")) %>%
# mutate(name_number = paste0(`Trustee Fund Name`," (",trustee_subset,")"))

trustee_subset_names_number <- sort(unique(df$funding_sources)) %>%
  paste(sep="",collapse="; ")

df <- df %>% select(-funding_sources)

#df$`Activation Date` <- as.Date.POSIXct(df$`Activation Date`)

#df$`Months to Closing Date`<- round(df$`Months to Closing Date`,digits = 0)

options("openxlsx.dateFormat", "mm/dd/yyyy")

df <- df %>% rename("Uncommitted Balance" = `Remaining Available Balance`)
df <- df %>% mutate(GPURL_binary = ifelse(`Lead GP/Global Theme`=="Urban, Resilience and Land",
                                          "GPURL",
                                          "Non-GPURL"))

df$GPURL_binary[is.na(df$GPURL_binary)] <- "Non-GPURL"

sum_df_all <- df %>%
  summarise("# Grants" = n(),
            "$ Amount" = sum(`Grant Amount`),
            "Balance" = sum(`Uncommitted Balance`)) %>%
  mutate("percent" = Balance/`$ Amount`)

#Changeeeeee
sum_df_all$by_8_2020 <- df %>%
  filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name) %>%
  select(`Uncommitted Balance`) %>%
  sum()

sum_df_GPURL <- df %>%
  filter(GPURL_binary=="GPURL") %>% 
  summarise("# Grants" = n(),
            "$ Amount" = sum(`Grant Amount`),
            "Balance" = sum(`Uncommitted Balance`))%>%
  mutate("percent"= Balance/`$ Amount`)

sum_df_GPURL$by_8_2020 <- df %>% filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name,
                                        GPURL_binary=="GPURL") %>%
  select(`Uncommitted Balance`) %>% sum()

sum_df_non_GPURL <- df %>%
  filter(GPURL_binary=="Non-GPURL") %>% 
  summarise("# Grants" = n(),
            "$ Amount" = sum(`Grant Amount`),
            "Balance" = sum(`Uncommitted Balance`))%>%
  mutate("percent"= Balance/`$ Amount`) 

sum_df_non_GPURL$by_8_2020 <- df %>%
  filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name,
         GPURL_binary=="Non-GPURL") %>%
  select(`Uncommitted Balance`) %>% sum()


sum_display_df <- data.frame("Summary" = c("Grant Count",
                                           "Total $ (Million)",
                                           "Total $ (Uncommitted)",
                                           "Percent Uncommitted",
                                           paste("Total $ (Uncommitted) under",current_trustee_subset_collapsed)), #CHANGEEE
                             "GPURL" = unname(unlist(as.list(sum_df_GPURL))),
                             "Non-GPURL" = unname(unlist(as.list(sum_df_non_GPURL))),
                             "Combined Total" = unname(unlist(as.list(sum_df_all))))

names(sum_display_df) <- c("Summary","GPURL","Non-GPURL","Combined Total")



risk_summary <- df %>% group_by(`Disbursement Risk Level`) %>% 
  summarise("Grant Count" = n(),
            "Total Grant Amount" = sum(`Grant Amount`),
            "Total Uncommitted" = sum(`Uncommitted Balance`))

risk_summary[6,] <-  c("Total Combined",
                       sum(risk_summary$`Grant Count`),
                       sum(risk_summary$`Total Grant Amount`),
                       sum(risk_summary$`Total Uncommitted`))

T_risk_summary <- risk_summary %>% data.table::transpose()
colnames(T_risk_summary) <- T_risk_summary[1,]
T_risk_summary$Summary <- colnames(risk_summary)
T_risk_summary <- T_risk_summary[-1,]



T_risk_summary <- T_risk_summary %>% select(Summary,
                                            `Very High Risk`,
                                            `High Risk`,
                                            `Medium Risk`,
                                            `Low Risk`,
                                            `Closed (Grace Period)`,
                                            `Total Combined`)


T_risk_summary <- T_risk_summary %>% mutate(`Very High Risk`=as.numeric(`Very High Risk`),
                                            `High Risk`=as.numeric(`High Risk`),
                                            `Medium Risk`=as.numeric(`Medium Risk`),
                                            `Low Risk`=as.numeric(`Low Risk`),
                                            `Closed (Grace Period)`=as.numeric(`Closed (Grace Period)`),
                                            `Total Combined`=as.numeric( `Total Combined`))

legend_text <- c("Closed. These grants are under the grace period.",
                 "Very High Risk (>10% / month disbursement required)",
                 "High Risk ( > 5%-10% / month disbursement required)",
                 "Medium Risk (3 - 5% / month disbursement required)",
                 "Low Risk (<  3% / month disbursement required)")

report_title <- paste("Disbursement Risk for",region,"as of",as.of.date)
tab_title <- "GFDRR ACTIVE GRANTS"

tab.names <- c("GPURL by Risk","Risk Summary")
funding_sources <- paste("Funding Sources:",trustee_subset_names_number)


# Tab.2.Data Crunch---------------
all_regions <- sort(unique(df$`Region Name`))

list_dfs <- list()

dfs_to_produce <- c("ALL",all_regions)

summary_risk_tab_2 <- function(temp.region="ALL"){
  
  if (temp.region=="ALL"){
    temp.region <- all_regions}
  
  GPURL <- df %>% 
    filter(`Region Name` %in% temp.region,
           GPURL_binary=="GPURL") %>% 
    group_by(`Disbursement Risk Level`) %>% 
    summarise("Grant Count" = n(),
              "Total Grant Amount" = sum(`Grant Amount`),
              "Total Uncommitted" = sum(`Uncommitted Balance`))
  
  GPURL.2 <- df %>%
    filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name,
           GPURL_binary=="GPURL",
           `Region Name` %in% temp.region) %>%
    group_by(`Disbursement Risk Level`) %>% #,`Trustee Fund Name`) %>% 
    summarise("Uncommitted to Implement by 8/2020"=sum(`Uncommitted Balance`)) 
  
  GPURL <- full_join(GPURL,GPURL.2)
  
  NON.GPURL <- df %>% 
    filter(`Region Name` %in% temp.region,
           GPURL_binary== "Non-GPURL") %>% 
    group_by(`Disbursement Risk Level`) %>% 
    summarise("Grant Count" = n(),
              "Total Grant Amount" = sum(`Grant Amount`),
              "Total Uncommitted" = sum(`Uncommitted Balance`))
  
  NON.GPURL.2 <- df %>%
    filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name,
           GPURL_binary=="Non-GPURL",
           `Region Name` %in% temp.region) %>%
    group_by(`Disbursement Risk Level`) %>% 
    summarise("Uncommitted to Implement by 8/2020"=sum(`Uncommitted Balance`))
  
  NON.GPURL <- full_join(NON.GPURL,NON.GPURL.2)
  
  output <- full_join(GPURL,NON.GPURL,by="Disbursement Risk Level",suffix = c(" (GPURL)", " (Non-GPURL)"))
  
  total_row <- c("Total",
                 sum(output$`Grant Count (GPURL)`,na.rm = TRUE),
                 sum(output$`Total Grant Amount (GPURL)`,na.rm = TRUE),
                 sum(output$`Total Uncommitted (GPURL)`,na.rm = TRUE),
                 sum(output$`Uncommitted to Implement by 8/2020 (GPURL)`,na.rm = TRUE),
                 sum(output$`Grant Count (Non-GPURL)`,na.rm = TRUE),
                 sum(output$`Total Grant Amount (Non-GPURL)`,na.rm = TRUE),
                 sum(output$`Total Uncommitted (Non-GPURL)`,na.rm = TRUE),
                 sum(output$`Uncommitted to Implement by 8/2020 (Non-GPURL)`,na.rm = TRUE))
  
  output <- rbind(output, total_row)
  for (j in unique(df$`Disbursement Risk Level`)) {
    
    if (!(j %in% output$`Disbursement Risk Level`)){
      
      new_row <- c(j,rep(NA,8)) 
      output <- rbind(output, new_row)
      
    }
  }
  
  
  output$`Disbursement Risk Level` <- factor(output$`Disbursement Risk Level` ,
                                             levels = c( "Very High Risk",
                                                         "High Risk",
                                                         "Medium Risk",
                                                         "Low Risk",
                                                         "Closed (Grace Period)",
                                                         "Total"))
  
  output <- output %>% arrange(`Disbursement Risk Level`)
  
  output <- output %>% mutate(`Grant Count (GPURL)`=as.numeric(`Grant Count (GPURL)`),
                              `Total Grant Amount (GPURL)`=as.numeric(`Total Grant Amount (GPURL)`),
                              `Total Uncommitted (GPURL)`=as.numeric(`Total Uncommitted (GPURL)`),
                              `Uncommitted to Implement by 8/2020 (GPURL)`= as.numeric(`Uncommitted to Implement by 8/2020 (GPURL)`),
                              `Grant Count (Non-GPURL)`= as.numeric(`Grant Count (Non-GPURL)`),
                              `Total Grant Amount (Non-GPURL)`= as.numeric(`Total Grant Amount (Non-GPURL)`),
                              `Total Uncommitted (Non-GPURL)`= as.numeric(`Total Uncommitted (Non-GPURL)`),
                              `Uncommitted to Implement by 8/2020 (Non-GPURL)`= as.numeric(`Uncommitted to Implement by 8/2020 (Non-GPURL)`))
  
  
  
  
  
  output
}

for (i in 1:length(dfs_to_produce)){
  list_dfs[[dfs_to_produce[i]]] <- summary_risk_tab_2(temp.region = dfs_to_produce[i])
}




## TAB 1. EXCEL ------------------------------------------
addWorksheet(wb, tab.names[1])

writeData(wb,1,
          tab_title,
          startRow = 1,
          startCol = 1)

writeDataTable(wb,1,x = sum_display_df,startCol = 1,startRow = 3,
               withFilter = F,firstColumn = TRUE)

writeDataTable(wb,1,T_risk_summary,startCol = 7, startRow = 3,withFilter = F,
               tableStyle ="TableStyleLight16",firstColumn = TRUE,bandedRows = F)

writeData(wb,1,legend_text,16,3)


RISK.df.row <- 10

writeDataTable(wb,1,df,
               startRow = RISK.df.row,
               startCol = 1)


low_risk <- createStyle(fgFill ="#86F9B7",halign = "left")
medium_risk <- createStyle(fgFill ="#FFE285",halign = "left")
high_risk <- createStyle(fgFill ="#FD8D75",halign = "left")
very_high_risk <- createStyle(fgFill ="#F17979",halign = "left")
grace_period_style <- createStyle(fgFill ="#B7B7B7",halign = "left")

low_risk_rows <- which(df$`Disbursement Risk Level`=="Low Risk")
medium_risk_rows <- which(df$`Disbursement Risk Level`=="Medium Risk")
high_risk_rows <- which(df$`Disbursement Risk Level`=="High Risk")
very_high_risk_rows <- which(df$`Disbursement Risk Level`=="Very High Risk")
grace_period_rows <- which(df$`Disbursement Risk Level`=="Closed (Grace Period)")

for (i in low_risk_rows){
  addStyle(wb,1,rows=i+RISK.df.row,cols=1:length(df),style = low_risk)
}

for (i in medium_risk_rows){
  addStyle(wb,1,rows=i+RISK.df.row,cols=1:length(df),style = medium_risk)
}

for (i in high_risk_rows){
  addStyle(wb,1,rows=i+RISK.df.row,cols=1:length(df),style = high_risk)
}

for (i in very_high_risk_rows){
  addStyle(wb,1,rows=i+RISK.df.row,cols=1:length(df),style = very_high_risk)
}

for (i in grace_period_rows){
  addStyle(wb,1,rows=i+RISK.df.row,cols=1:length(df),style = grace_period_style)
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
wrapped_text <- createStyle(wrapText = TRUE,valign = "top")
black_and_bold <- createStyle(fontColour = "#000000",textDecoration = 'bold')
addStyle(wb,1,rows=RISK.df.row,cols=1:(length(df)),style = header_style)

number_format_range <- RISK.df.row:(nrow(df)+RISK.df.row)

addStyle(wb,1,rows=number_format_range,cols=12,style = date_format,stack = TRUE)
addStyle(wb,1,rows=number_format_range,cols=13,style = date_format,stack = TRUE)
addStyle(wb,1,rows=number_format_range,cols=14,style = dollar_format,stack = TRUE)
addStyle(wb,1,rows=number_format_range,cols=15,style = dollar_format,stack = TRUE)
addStyle(wb,1,rows=number_format_range,cols=16,style = dollar_format,stack = TRUE)
addStyle(wb,1,rows=number_format_range,cols=17,style = dollar_format,stack = TRUE)
#addStyle(wb,1,rows=number_format_range,cols=18,style = num_format,stack = TRUE)
addStyle(wb,1,rows=number_format_range,cols=20,style = percent_format,stack = TRUE)
#addStyle(wb,1,rows=number_format_range,cols=22,style = percent_format,stack = TRUE)
#addStyle(wb,1,rows=number_format_range,cols=23,style = percent_format,stack = TRUE)

setColWidths(wb,1, cols=1:length(df), widths = "auto")
setColWidths(wb,1, cols=2, widths = 15)
setColWidths(wb,1, cols=1, widths = 25)
addStyle(wb,1,rows = 8,cols = 1,style = wrapped_text,stack = TRUE)
addStyle(wb,1,rows = 10,cols = 1:length(df),style = wrapped_text,stack = TRUE)
setColWidths(wb,1, cols=3, widths = 17)
setColWidths(wb,1, cols=5, widths = 20) #child fund name
setColWidths(wb,1, cols=6, widths = 9)
setColWidths(wb,1, cols=9, widths = 13)
setColWidths(wb,1, cols=10, widths = 10)
setColWidths(wb,1, cols=11, widths = 13)
setColWidths(wb,1, cols=12, widths = 15)
setColWidths(wb,1, cols=13, widths = 15)
setColWidths(wb,1, cols=15, widths = 14)
setColWidths(wb,1, cols=16, widths = 40)
setColWidths(wb,1, cols=17, widths = 15)
setColWidths(wb,1, cols=19, widths = 14)
setColWidths(wb,1, cols=20, widths = 15)
setColWidths(wb,1, cols=21, widths = 19)
setColWidths(wb,1, cols=22, widths = 15)
setColWidths(wb,1, cols=23, widths = 15)



#ADDITIONAL COLOR COSMETICS

addStyle(wb,1,rows=3,cols = 8, style = very_high_risk)
addStyle(wb,1,rows=4,cols = 15, style = very_high_risk)
addStyle(wb,1,rows=3,cols = 9, style = high_risk)
addStyle(wb,1,rows=5,cols = 15, style = high_risk)
addStyle(wb,1,rows=3,cols = 10, style = medium_risk)
addStyle(wb,1,rows=6,cols = 15, style = medium_risk)
addStyle(wb,1,rows=3,cols = 11, style = low_risk)
addStyle(wb,1,rows=7,cols = 15, style = low_risk)
addStyle(wb,1,rows=3,cols = 12, style = grace_period_style)
addStyle(wb,1,rows=3,cols = 15, style = grace_period_style)

addStyle(wb,1,dollar_format,5,2:4,stack = T)
addStyle(wb,1,dollar_format,6,2:4,stack = T)
addStyle(wb,1,percent_format,7,2:4,stack = T)
addStyle(wb,1,dollar_format,8,2:4,stack = T)

addStyle(wb,1,dollar_format,5,8:13,stack = T)
addStyle(wb,1,dollar_format,6,8:13,stack = T)



##TAB 2. EXCEL---------------------------------
addWorksheet(wb, tab.names[2])

writeData(wb,2,"Summary of GFDRR Active Grants by Risk Category",startCol = 1,startRow = 1)

writeData(wb,2,names(list_dfs[1]),startCol = 1,startRow = 3)
writeDataTable(wb,2,list_dfs[[1]],startCol = 1,startRow = 5,
               withFilter = F,firstColumn = T,
               tableStyle = "TableStyleLight1")

writeData(wb,2,names(list_dfs[2]),startCol = 1,startRow = 13)
writeDataTable(wb,2,list_dfs[[2]],startCol = 1,startRow = 15,
               withFilter = F,firstColumn = T,
               tableStyle = "TableStyleLight1")


writeData(wb,2,names(list_dfs[3]),startCol = 1,startRow = 23)
writeDataTable(wb,2,list_dfs[[3]],startCol = 1,startRow = 25,
               withFilter = F,firstColumn = T,
               tableStyle = "TableStyleLight1")


writeData(wb,2,names(list_dfs[4]),startCol = 1,startRow = 33)
writeDataTable(wb,2,list_dfs[[4]],startCol = 1,startRow = 35,
               withFilter = F,firstColumn = T,
               tableStyle = "TableStyleLight1")

writeData(wb,2,names(list_dfs[5]),startCol = 11,startRow = 3)
writeDataTable(wb,2,list_dfs[[5]],startCol = 11,startRow = 5,
               withFilter = F,firstColumn = T,
               tableStyle = "TableStyleLight1")


writeData(wb,2,names(list_dfs[6]),startCol = 11,startRow = 13)
writeDataTable(wb,2,list_dfs[[6]],startCol = 11,startRow = 15,
               withFilter = F,firstColumn = T,
               tableStyle = "TableStyleLight1")


writeData(wb,2,names(list_dfs[7]),startCol = 11,startRow = 23)
writeDataTable(wb,2,list_dfs[[7]],startCol = 11,startRow = 25,
               withFilter = F,firstColumn = T,
               tableStyle = "TableStyleLight1")


writeData(wb,2,names(list_dfs[8]),startCol = 11,startRow = 33)
writeDataTable(wb,2,list_dfs[[8]],startCol = 11,startRow = 35,
               withFilter = F,firstColumn = T,
               tableStyle = "TableStyleLight1")


addStyle(wb,2,dollar_format,rows = 6:42,cols = 3,stack = TRUE)
addStyle(wb,2,dollar_format,rows = 6:42,cols = 4,stack = TRUE)
addStyle(wb,2,dollar_format,rows = 6:42,cols = 5,stack = TRUE)
addStyle(wb,2,dollar_format,rows = 6:42,cols = 7,stack = TRUE)
addStyle(wb,2,dollar_format,rows = 6:42,cols = 8,stack = TRUE)
addStyle(wb,2,dollar_format,rows = 6:42,cols = 9,stack = TRUE)

addStyle(wb,2,dollar_format,rows = 6:42,cols = 13,stack = TRUE)
addStyle(wb,2,dollar_format,rows = 6:42,cols = 14,stack = TRUE)
addStyle(wb,2,dollar_format,rows = 6:42,cols = 15,stack = TRUE)
addStyle(wb,2,dollar_format,rows = 6:42,cols = 17,stack = TRUE)
addStyle(wb,2,dollar_format,rows = 6:42,cols = 18,stack = TRUE)
addStyle(wb,2,dollar_format,rows = 6:42,cols = 19,stack = TRUE)

setColWidths(wb,2,cols=c(3,4,5,7,8,9,13,14,15,17,18,19),widths = 16)
setColWidths(wb,2,cols=c(1,11),widths = 17)
addStyle(wb,2,very_high_risk,rows = c(6,16,26,36),cols = 1)
addStyle(wb,2,very_high_risk,rows = c(6,16,26,36),cols = 11)
addStyle(wb,2,high_risk,rows = c(7,17,27,37),cols = 1)
addStyle(wb,2,high_risk,rows = c(7,17,27,37),cols = 11)
addStyle(wb,2,medium_risk,rows = c(8,18,28,38),cols = 1)
addStyle(wb,2,medium_risk,rows = c(8,18,28,38),cols = 11)
addStyle(wb,2,low_risk,rows = c(9,19,29,39),cols = 1)
addStyle(wb,2,low_risk,rows = c(9,19,29,39),cols = 11)
addStyle(wb,2,grace_period_style,rows = c(10,20,30,40),cols = 1)
addStyle(wb,2,grace_period_style,rows = c(10,20,30,40),cols = 11)


writeData(wb,2,"GPURL",startCol = 2,startRow = (4))
writeData(wb,2,"GPURL",startCol = 2,startRow = (14))
writeData(wb,2,"GPURL",startCol = 2,startRow = (24))
writeData(wb,2,"GPURL",startCol = 2,startRow = (34))
writeData(wb,2,"GPURL",startCol = 12,startRow = (4))
writeData(wb,2,"GPURL",startCol = 12,startRow = (14))
writeData(wb,2,"GPURL",startCol = 12,startRow = (24))
writeData(wb,2,"GPURL",startCol = 12,startRow = (34))


writeData(wb,2,"Non-GPURL",startCol = 6,startRow = (4))
writeData(wb,2,"Non-GPURL",startCol = 6,startRow = (14))
writeData(wb,2,"Non-GPURL",startCol = 6,startRow = (24))
writeData(wb,2,"Non-GPURL",startCol = 6,startRow = (34))
writeData(wb,2,"Non-GPURL",startCol = 16,startRow = (4))
writeData(wb,2,"Non-GPURL",startCol = 16,startRow = (14))
writeData(wb,2,"Non-GPURL",startCol = 16,startRow = (24))
writeData(wb,2,"Non-GPURL",startCol = 16,startRow = (34))


mergeCells(wb,2,cols = 1:9,rows = 1)

mergeCells(wb,2,cols = 2:5,rows = 4)
mergeCells(wb,2,cols = 6:9,rows = 4)
mergeCells(wb,2,cols = 12:15,rows = 4)
mergeCells(wb,2,cols = 16:19,rows = 4)

mergeCells(wb,2,cols = 2:5,rows = 14)
mergeCells(wb,2,cols = 6:9,rows = 14)
mergeCells(wb,2,cols = 12:15,rows = 14)
mergeCells(wb,2,cols = 16:19,rows = 14)

mergeCells(wb,2,cols = 2:5,rows = 24)
mergeCells(wb,2,cols = 6:9,rows = 24)
mergeCells(wb,2,cols = 12:15,rows = 24)
mergeCells(wb,2,cols = 16:19,rows = 24)

mergeCells(wb,2,cols = 2:5,rows = 34)
mergeCells(wb,2,cols = 6:9,rows = 34)
mergeCells(wb,2,cols = 12:15,rows = 34)
mergeCells(wb,2,cols = 16:19,rows = 34)

gp_style <- createStyle(fgFill = "#EAE8E8",halign = "center",textDecoration = 'bold')

addStyle(wb,2,gp_style,stack = TRUE,rows = c(4,14,24,34),cols = 2)
addStyle(wb,2,gp_style,stack = TRUE,rows = c(4,14,24,34),cols = 6)
addStyle(wb,2,gp_style,stack = TRUE,rows = c(4,14,24,34),cols = 12)
addStyle(wb,2,gp_style,stack = TRUE,rows = c(4,14,24,34),cols = 16)

addStyle(wb,2,black_and_bold,rows = c(3,13,23,33),cols=1,stack = T)
addStyle(wb,2,black_and_bold,rows = c(3,13,23,33),cols=11,stack = T)


for (f in c(5,15,25,35)){
  addStyle(wb,2,wrapped_text,rows=f,cols=1:20)}

for (i in all_regions){
  temp_df <- df %>%
    filter(`Region Name` == i,
           !is.na(`Lead GP/Global Theme`)
    )
  
  sum_df_all <- temp_df %>%
    summarise("# Grants" = n(),
              "$ Amount" = sum(`Grant Amount`),
              "Balance" = sum(`Uncommitted Balance`)) %>%
    mutate("percent" = Balance/`$ Amount`)
  
  sum_df_all$uncommitted_8020 <- temp_df %>%
    filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name) %>% 
    select(`Uncommitted Balance`) %>% sum()
  
  sum_df_all$PO_8020 <- temp_df %>%
    filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name) %>% 
    select(`PO Commitments`) %>% sum()
  
  
  
  sum_df_GPURL <-  temp_df %>%
    filter(GPURL_binary=="GPURL") %>% 
    summarise("# Grants" = n(),
              "$ Amount" = sum(`Grant Amount`),
              "Balance" = sum(`Uncommitted Balance`))%>%
    mutate("percent"= Balance/`$ Amount`)
  
  sum_df_GPURL$uncommitted_8020 <- temp_df %>%
    filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name,
           GPURL_binary=="GPURL") %>% 
    select(`Uncommitted Balance`) %>% sum()
  
  sum_df_GPURL$PO_8020 <- temp_df %>%
    filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name,
           GPURL_binary=="GPURL") %>% 
    select(`PO Commitments`) %>% sum()
  
  
  sum_df_non_GPURL <- temp_df %>%
    filter(GPURL_binary=="Non-GPURL") %>% 
    summarise("# Grants" = n(),
              "$ Amount" = sum(`Grant Amount`),
              "Balance" = sum(`Uncommitted Balance`))%>%
    mutate("percent"= Balance/`$ Amount`) 
  
  sum_df_non_GPURL$uncommitted_8020 <- temp_df %>%
    filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name,
           GPURL_binary=="Non-GPURL") %>% 
    select(`Uncommitted Balance`) %>% sum()
  
  sum_df_non_GPURL$PO_8020 <- temp_df %>%
    filter(`Trustee Fund Name` %in% current_trustee_subset$temp.name,
           GPURL_binary=="Non-GPURL") %>% 
    select(`PO Commitments`) %>% sum()
  
  
  
  sum_display_df <- data.frame("Summary"= c("Grant Count",
                                            "Total $ (Million)",
                                            "Total Uncommitted Balance ($)",
                                            "% Uncommitted Balance ($)",
                                            "Total $ (Uncommitted) Required to Implement by 8/2020 (MDTF, Japan 1, ACP-EU)",
                                            "PO Commitments to spend by 8/2020  (MDTF, Japan 1, ACP-EU)"),
                               "GPURL" = unname(unlist(as.list(sum_df_GPURL))),
                               "Non-GPURL" = unname(unlist(as.list(sum_df_non_GPURL))),
                               "Combined Total" = unname(unlist(as.list(sum_df_all))))
  
  names(sum_display_df) <- c("Summary","GPURL","Non-GPURL"," Combined Total")
  
  
  #---------COUNTRIES DF ----------------------
  temp_df_all <- temp_df %>%
    group_by(Country) %>%
    summarise("# Grants" = n(),
              "$ Amount" = (sum(`Grant Amount`)),
              "Balance" = (sum(`Uncommitted Balance`)))
  
  temp_df_GPURL <- temp_df %>%
    filter(GPURL_binary=="GPURL") %>% 
    group_by(Country) %>%
    summarise("# Grants" = n(),
              "$ Amount" = (sum(`Grant Amount`)),
              "Balance" = (sum(`Uncommitted Balance`)))
  
  temp_df_non_GPURL <- temp_df %>%
    filter(GPURL_binary=="Non-GPURL") %>% 
    group_by(Country) %>%
    summarise("# Grants" = n(),
              "$ Amount" = (sum(`Grant Amount`)),
              "Balance" = (sum(`Uncommitted Balance`)))
  
  
  display_df_partial <- full_join(temp_df_GPURL,
                                  temp_df_non_GPURL,
                                  by="Country",
                                  suffix=c(" (GPURL)"," (Non-GPURL)"))
  
  
  display_df <- left_join(temp_df_all,display_df_partial,by="Country")
  
  display_df <- display_df %>%  rename("Country/Region"=Country)
  
  
  #---------FUNDING SOURCE DF ----------------------
  
  temp_df_all <- temp_df %>%
    group_by(`Trustee Fund Name`) %>%
    summarise("# Grants" = n(),
              "$ Amount" = (sum(`Grant Amount`)),
              "Balance" = (sum(`Uncommitted Balance`)))
  
  temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
    group_by(`Trustee Fund Name`) %>%
    summarise("# Grants" = n(),
              "$ Amount" = (sum(`Grant Amount`)),
              "Balance" = (sum(`Uncommitted Balance`)))
  
  temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
    group_by(`Trustee Fund Name`) %>%
    summarise("# Grants" = n(),
              "$ Amount" = (sum(`Grant Amount`)),
              "Balance" = (sum(`Uncommitted Balance`)))
  
  
  display_df_partial <- full_join(temp_df_GPURL,
                                  temp_df_non_GPURL,
                                  by="Trustee Fund Name",
                                  suffix=c(" (GPURL)"," (Non-GPURL)"))
  
  
  display_df_funding <- left_join(temp_df_all,display_df_partial,by="Trustee Fund Name")
  
  
  #------ CREATE EXCEL WORKBOOK AND ADD DATAFRAMES -------
  
  
  #temp_df <- reactive_df()
  report_title <- paste0("Summary of GFDRR Portfolio (as of ",report_data_date,")")
  
  addWorksheet(wb,i)
  # mergeCells(wb,1,c(2,3,4),1)
  
  i <- (which(all_regions==i))+2
  
  writeData(wb,i,
            report_title,
            startRow = 1,
            startCol = 2)
  
  writeDataTable(wb,i,  sum_display_df, startRow = 3, startCol = 2, withFilter = F)
  writeDataTable(wb,i,  display_df, startRow = 14, startCol = 2,withFilter = F)
  writeDataTable(wb,i,  display_df_funding, startRow = (17+(nrow(display_df))), startCol = 2,withFilter = F)
  
  #FORMULAS FOR TOTALS IN DISPLAY DF  -----------
  end_row.display_df <- (14+nrow(display_df))
  
  writeFormula(wb,i,x=paste0("=SUM(C14:C",end_row.display_df,")"),startCol = 3,startRow = (end_row.display_df+1))
  writeFormula(wb,i,x=paste0("=SUM(D14:D",end_row.display_df,")"),startCol = 4,startRow = (end_row.display_df+1))
  writeFormula(wb,i,x=paste0("=SUM(E14:E",end_row.display_df,")"),startCol = 5,startRow = (end_row.display_df+1))
  writeFormula(wb,i,x=paste0("=SUM(F14:F",end_row.display_df,")"),startCol = 6,startRow = (end_row.display_df+1))
  writeFormula(wb,i,x=paste0("=SUM(G14:G",end_row.display_df,")"),startCol = 7,startRow = (end_row.display_df+1))
  writeFormula(wb,i,x=paste0("=SUM(H14:H",end_row.display_df,")"),startCol = 8,startRow = (end_row.display_df+1))
  writeFormula(wb,i,x=paste0("=SUM(I14:I",end_row.display_df,")"),startCol = 9,startRow = (end_row.display_df+1))
  writeFormula(wb,i,x=paste0("=SUM(J14:J",end_row.display_df,")"),startCol = 10,startRow = (end_row.display_df+1))
  writeFormula(wb,i,x=paste0("=SUM(K14:K",end_row.display_df,")"),startCol = 11,startRow = (end_row.display_df+1))
  
  writeData(wb,i,x="Total",startCol = 2,startRow = (end_row.display_df+1))
  
  #FORMULAS FOR TOTALS IN DISPLAY DF FUNDING -----------
  start_row.funding <- (14+nrow(display_df)+4)
  end_row.funding <- (14+nrow(display_df)+3+nrow(display_df_funding))
  
  writeFormula(wb,i,x=paste0("=SUM(C",start_row.funding,":C",end_row.funding,")"),startCol = 3,startRow = (end_row.funding+1))
  writeFormula(wb,i,x=paste0("=SUM(D",start_row.funding,":D",end_row.funding,")"),startCol = 4,startRow = (end_row.funding+1))
  writeFormula(wb,i,x=paste0("=SUM(E",start_row.funding,":E",end_row.funding,")"),startCol = 5,startRow = (end_row.funding+1))
  writeFormula(wb,i,x=paste0("=SUM(F",start_row.funding,":F",end_row.funding,")"),startCol = 6,startRow = (end_row.funding+1))
  writeFormula(wb,i,x=paste0("=SUM(G",start_row.funding,":G",end_row.funding,")"),startCol = 7,startRow = (end_row.funding+1))
  writeFormula(wb,i,x=paste0("=SUM(H",start_row.funding,":H",end_row.funding,")"),startCol = 8,startRow = (end_row.funding+1))
  writeFormula(wb,i,x=paste0("=SUM(I",start_row.funding,":I",end_row.funding,")"),startCol = 9,startRow = (end_row.funding+1))
  writeFormula(wb,i,x=paste0("=SUM(J",start_row.funding,":J",end_row.funding,")"),startCol = 10,startRow = (end_row.funding+1))
  writeFormula(wb,i,x=paste0("=SUM(K",start_row.funding,":K",end_row.funding,")"),startCol = 11,startRow = (end_row.funding+1))
  
  writeData(wb,i,x="Total",startCol = 2,startRow = (end_row.funding+1))
  
  percent_format <- createStyle(numFmt = "0%")
  total_style <- createStyle(bgFill = "#5A52FA",fontColour = "#FFFFFF",textDecoration = "bold")
  
  addStyle(wb,i,dollar_format,row=5:6,cols=3)
  addStyle(wb,i,dollar_format,row=5:6,cols=4)
  addStyle(wb,i,dollar_format,row=5:6,cols=5)
  addStyle(wb,i,percent_format,rows=7,cols=3:5)
  addStyle(wb,i,dollar_format,row=5:6,cols=5)
  addStyle(wb,i,dollar_format,row=8,cols=3:5)
  addStyle(wb,i,dollar_format,row=9,cols=3:5)
  
  dollar_columns_Country_and_Trustee <- c(4,5,7,8,10,11)
  
  rows.display_df <- 14:(end_row.display_df+1)
  rows.display_df_funding <- start_row.funding:(end_row.funding+1)
  
  for (j in dollar_columns_Country_and_Trustee){
    addStyle(wb,i,dollar_format,rows=rows.display_df,cols = j)
    addStyle(wb,i,dollar_format,rows=rows.display_df_funding,cols = j)
  }
  
  addStyle(wb,i,total_style,rows=(end_row.display_df+1),cols = 2:11,stack=TRUE )
  addStyle(wb,i,total_style,rows=(end_row.funding+1),cols = 2:11,stack=TRUE)
  
  setColWidths(wb,i, cols = 1:ncol(display_df)+1, widths = "auto")
  
  setColWidths(wb,i, cols =4, widths = 15)
  setColWidths(wb,i, cols =6, widths = 8)
  setColWidths(wb,i, cols =9, widths = 8)
  addStyle(wb,1,wrapped_text,cols=1:11,rows = 14,stack = TRUE)
  
}

