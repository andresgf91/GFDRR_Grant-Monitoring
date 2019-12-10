

library(readxl)
recode_GP <- read_xlsx("Trust_Fund_Breakdown_Table_TF4.1 Grant Details Report.xlsx",skip=3)


Global.Theme_Resp.Unit <- recode_GP %>%
  filter(`Grant No.` %in% grants$Fund)  %>%
  filter(`Fund Status`=="ACTIVE") %>%
  select(`Resp. Unit`,`Lead GP/Global Themes`) %>% 
  group_by(`Resp. Unit`,`Lead GP/Global Themes`) %>%
  dplyr::distinct() %>% 
  arrange(`Resp. Unit`)

write.csv(Global.Theme_Resp.Unit,file="Global Theme - Resp. Unit Mapping.csv")       


unit_managers <- recode_GP %>%
  filter(`Grant No.` %in% grants$Fund)  %>%
  filter(`Fund Status`=="ACTIVE") %>%
  select(`Grant Managing Unit`,`Practice Manager`) %>% 
  group_by(`Grant Managing Unit`,`Practice Manager`) %>%
  dplyr::distinct() %>% 
  arrange(`Grant Managing Unit`)


unit_managers <-  unit_managers[complete.cases(unit_managers), ]

write.csv(unit_managers,file="Managers - Units.csv")       

