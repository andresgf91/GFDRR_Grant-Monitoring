library(openxlsx)
library(dplyr)
library(readxl)

grants_file <- "Downloads/GFDRR Raw Data 1_13_20 (2).xlsx"

as.of.date <- "January 13, 2020"
report_region <- "AFR"

#CODE---------------
grants <- read_xlsx(grants_file,2)
grants <- grants %>% filter(`Child Fund Status` %in% c("ACTV","PEND"))

recode_GT <- read_xlsx("World_Bank/GFDRR/Shiny_Dashboards/Github_Code/GFDRR_Grant-Monitoring/Global Theme - Resp. Unit Mapping.xlsx")

SR_grant_details <- read_xlsx("Downloads/Trust_Fund_Breakdown_Table_TF4.1 Grant Details Report (1).xlsx",skip=3)

recode_trustee.2 <- read_xlsx("World_Bank/GFDRR/Shiny_Dashboards/Github_Code/GFDRR_Grant-Monitoring/GFDRR Trustee Names.xlsx")


#grants <- left_join(grants,SR_grant_details,by = c("Child Fund"="Grant No."))

# homeless_TFs <- grants %>% filter(is.na(`Lead GP/Global Themes`)) %>% 
#   select(`Child Fund`,`TTL Unit Name`)
# 
# #assign global theme to grant based on Grant TTL unit
# 
# homed_TFs <- left_join(homeless_TFs,recode_GT,
#                        by=c("TTL Unit Name"="Resp. Unit")) %>%
#   select(`Child Fund`,`Lead GP/Global Themes`)
# 
# grants$`Lead GP/Global Themes`[is.na(grants$`Lead GP/Global Themes`)] <-
#   homed_TFs$`Lead GP/Global Themes`[homed_TFs$`Child Fund`==grants$`Child Fund`[is.na(grants$`Lead GP/Global Themes`)]]
# 
# still_homeless <- which(is.na(grants$`Lead GP/Global Themes`))
# 
# 
# for (i in still_homeless){
#   unit <- grants$`TTL Unit Name`[i]
#   
#   GPs <- grants %>%
#     filter(`TTL Unit Name`==unit) %>%
#     select(`Lead GP/Global Themes`) %>%
#     filter(!is.na(`Lead GP/Global Themes`))
#   
#   if (nrow(GPs)>0){
#     grants$`Lead GP/Global Themes`[i] <- GPs[]}
#   else{message("There is still more homeless grants")}
#   
# }


grants <- left_join(grants,recode_trustee.2,by = c("Trustee"="Trustee"))


still_homeless <- which(is.na(grants$`Lead GP/Global Themes`))

if (length(still_homeless)==0){
 homelessness_state <-  "You have solved homelessness!"
}else {
  homelessness_state <- "You still have a homelessness problem"
}

grants <- grants  %>% filter(`Child Fund Status` %in% c("ACTV","PEND"),`Region Name`==report_region)

temp_df <-  grants %>%
  filter(!is.na(`Lead GP/Global Themes`)) %>% 
  mutate(GPURL_binary = ifelse(
    `Lead GP/Global Themes`=="Urban, Resilience and Land",
                               "GPURL",
                               "Non-GPURL")) 


temp_df$unnacounted_amount <- temp_df$`Grant Amount` -
  (temp_df$`Cumulative Disbursements` + temp_df$`PO Commitments`)

sum_df_all <- temp_df %>%
  summarise("# Grants" = n(),
            "$ Amount" = sum(`Grant Amount`),
            "Balance" = sum(unnacounted_amount)) %>%
  mutate("percent" = Balance/`$ Amount`)

sum_df_GPURL <-  temp_df %>%
  filter(GPURL_binary=="GPURL") %>% 
  summarise("# Grants" = n(),
            "$ Amount" = sum(`Grant Amount`),
            "Balance" = sum(unnacounted_amount))%>%
  mutate("percent"= Balance/`$ Amount`)

sum_df_non_GPURL <- temp_df %>%
  filter(GPURL_binary=="Non-GPURL") %>% 
  summarise("# Grants" = n(),
            "$ Amount" = sum(`Grant Amount`),
            "Balance" = sum(unnacounted_amount))%>%
  mutate("percent"= Balance/`$ Amount`) 


#cutoff_date <- as.character(temp_df$`End Disb. Date`[which.max(temp_df$`End Disb. Date`)])

sum_display_df <- data.frame("Summary"= c("Grant Count",
                                          "Total $ (Million)",
                                          "Total Uncommitted Balance ($)",
                                          "% Uncommitted Balance ($)"),
                             "GPURL"=unname(unlist(as.list(sum_df_GPURL))),
                             "Non-GPURL"=unname(unlist(as.list(sum_df_non_GPURL))),
                             "Combined Total"=unname(unlist(as.list(sum_df_all))))

names(sum_display_df) <- c("Summary","GPURL","Non-GPURL"," Combined Total")


#---------COUNTRIES DF ----------------------
temp_df_all <- temp_df %>%
  group_by(Country) %>%
  summarise("# Grants" = n(),
            "$ Amount" = (sum(`Grant Amount`)),
            "Balance" = (sum(unnacounted_amount)))

temp_df_GPURL <- temp_df %>%
  filter(GPURL_binary=="GPURL") %>% 
  group_by(Country) %>%
  summarise("# Grants" = n(),
            "$ Amount" = (sum(`Grant Amount`)),
            "Balance" = (sum(unnacounted_amount)))

temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
  group_by(Country) %>%
  summarise("# Grants" = n(),
            "$ Amount" = (sum(`Grant Amount`)),
            "Balance" = (sum(unnacounted_amount)))


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
            "Balance" = (sum(unnacounted_amount)))

temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
  group_by(`Trustee Fund Name`) %>%
  summarise("# Grants" = n(),
            "$ Amount" = (sum(`Grant Amount`)),
            "Balance" = (sum(unnacounted_amount)))

temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
  group_by(`Trustee Fund Name`) %>%
  summarise("# Grants" = n(),
            "$ Amount" = (sum(`Grant Amount`)),
            "Balance" = (sum(unnacounted_amount)))


display_df_partial <- full_join(temp_df_GPURL,
                                temp_df_non_GPURL,
                                by="Trustee Fund Name",
                                suffix=c(" (GPURL)"," (Non-GPURL)"))


display_df_funding <- left_join(temp_df_all,display_df_partial,by="Trustee Fund Name")


#------ CREATE EXCEL WORKBOOK AND ADD DATAFRAMES -------
require(openxlsx)
wb <- createWorkbook()

#temp_df <- reactive_df()
report_title <- paste0("Summary of GFDRR ",report_region, " Portfolio (as of ",as.of.date,")")

addWorksheet(wb, "Portfolio Summary")
# mergeCells(wb,1,c(2,3,4),1)

writeData(wb, 1,
          report_title,
          startRow = 1,
          startCol = 2)

writeDataTable(wb, 1,  sum_display_df, startRow = 3, startCol = 2, withFilter = F)
writeDataTable(wb, 1,  display_df, startRow = 12, startCol = 2,withFilter = F)
writeDataTable(wb, 1,  display_df_funding, startRow = (15+(nrow(display_df))), startCol = 2,withFilter = F)

#FORMULAS FOR TOTALS IN DISPLAY DF  -----------
end_row.display_df <- (12+nrow(display_df))
 
writeFormula(wb,1,x=paste0("=SUM(C12:C",end_row.display_df,")"),startCol = 3,startRow = (end_row.display_df+1))
writeFormula(wb,1,x=paste0("=SUM(D12:D",end_row.display_df,")"),startCol = 4,startRow = (end_row.display_df+1))
writeFormula(wb,1,x=paste0("=SUM(E12:E",end_row.display_df,")"),startCol = 5,startRow = (end_row.display_df+1))
writeFormula(wb,1,x=paste0("=SUM(F12:F",end_row.display_df,")"),startCol = 6,startRow = (end_row.display_df+1))
writeFormula(wb,1,x=paste0("=SUM(G12:G",end_row.display_df,")"),startCol = 7,startRow = (end_row.display_df+1))
writeFormula(wb,1,x=paste0("=SUM(H12:H",end_row.display_df,")"),startCol = 8,startRow = (end_row.display_df+1))
writeFormula(wb,1,x=paste0("=SUM(I12:I",end_row.display_df,")"),startCol = 9,startRow = (end_row.display_df+1))
writeFormula(wb,1,x=paste0("=SUM(J12:J",end_row.display_df,")"),startCol = 10,startRow = (end_row.display_df+1))
writeFormula(wb,1,x=paste0("=SUM(K12:K",end_row.display_df,")"),startCol = 11,startRow = (end_row.display_df+1))

writeData(wb,1,x="Total",startCol = 2,startRow = (end_row.display_df+1))

#FORMULAS FOR TOTALS IN DISPLAY DF FUNDING -----------
start_row.funding <- (12+nrow(display_df)+4)
end_row.funding <- (12+nrow(display_df)+3+nrow(display_df_funding))

writeFormula(wb,1,x=paste0("=SUM(C",start_row.funding,":C",end_row.funding,")"),startCol = 3,startRow = (end_row.funding+1))
writeFormula(wb,1,x=paste0("=SUM(D",start_row.funding,":D",end_row.funding,")"),startCol = 4,startRow = (end_row.funding+1))
writeFormula(wb,1,x=paste0("=SUM(E",start_row.funding,":E",end_row.funding,")"),startCol = 5,startRow = (end_row.funding+1))
writeFormula(wb,1,x=paste0("=SUM(F",start_row.funding,":F",end_row.funding,")"),startCol = 6,startRow = (end_row.funding+1))
writeFormula(wb,1,x=paste0("=SUM(G",start_row.funding,":G",end_row.funding,")"),startCol = 7,startRow = (end_row.funding+1))
writeFormula(wb,1,x=paste0("=SUM(H",start_row.funding,":H",end_row.funding,")"),startCol = 8,startRow = (end_row.funding+1))
writeFormula(wb,1,x=paste0("=SUM(I",start_row.funding,":I",end_row.funding,")"),startCol = 9,startRow = (end_row.funding+1))
writeFormula(wb,1,x=paste0("=SUM(J",start_row.funding,":J",end_row.funding,")"),startCol = 10,startRow = (end_row.funding+1))
writeFormula(wb,1,x=paste0("=SUM(K",start_row.funding,":K",end_row.funding,")"),startCol = 11,startRow = (end_row.funding+1))

writeData(wb,1,x="Total",startCol = 2,startRow = (end_row.funding+1))

dollar_format <- createStyle(numFmt = "$0,0")
percent_format <- createStyle(numFmt = "0%")
total_style <- createStyle(bgFill = "#5A52FA",fontColour = "#FFFFFF",textDecoration = "bold")

addStyle(wb,1,dollar_format,row=5:6,cols=3)
addStyle(wb,1,dollar_format,row=5:6,cols=4)
addStyle(wb,1,dollar_format,row=5:6,cols=5)
addStyle(wb,1,percent_format,rows=7,cols=3:5)



dollar_columns_Country_and_Trustee <- c(4,5,7,8,10,11)

rows.display_df <- 12:(end_row.display_df+1)
rows.display_df_funding <- start_row.funding:(end_row.funding+1)

for (i in dollar_columns_Country_and_Trustee){
  addStyle(wb,1,dollar_format,rows=rows.display_df,cols = i)
  addStyle(wb,1,dollar_format,rows=rows.display_df_funding,cols = i)
}

addStyle(wb,1,total_style,rows=(end_row.display_df+1),cols = 2:11,stack=TRUE )
addStyle(wb,1,total_style,rows=(end_row.funding+1),cols = 2:11,stack=TRUE)

setColWidths(wb, 1, cols = 1:ncol(display_df)+1, widths = "auto")


#openxlsx::openXL(wb)
###SAVE----------------
openxlsx::saveWorkbook(wb,file=paste0("World_Bank/GFDRR/Akiko/",report_region,"_V1.xlsx"),overwrite = TRUE)

message(homelessness_state)


