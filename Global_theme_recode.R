

library(readxl)
recode_GP <- read_xlsx("Trust_Fund_Breakdown_Table_TF4.1 Grant Details Report.xlsx",skip=3)



unit_global.theme <- recode_GP %>%
  filter(`Fund Status`=="ACTIVE") %>%
  select(`Grant Managing Unit`,`Lead GP/Global Themes`) %>%
  group_by(`Grant Managing Unit`,`Lead GP/Global Themes`) %>%
  distinct() %>% 
  arrange(`Lead GP/Global Themes`)


View(unit_global.theme)

na.1 <- unit_global.theme %>% filter(is.na(`Lead GP/Global Themes`))
na.2 <- unit_global.theme %>% filter(!is.na(`Lead GP/Global Themes`))



# for (i in na.1$`Grant Managing Unit`) {
#   if(i in na.2$`Grant Managing Unit`){
#     unit_global.theme$`Lead GP/Global Themes`[unit_global.theme$`Grant Managing Unit`==i] <-  
#   }
# }

na.1$`Grant Managing Unit`[which(!(na.1$`Grant Managing Unit` %in% na.2$`Grant Managing Unit`))]
