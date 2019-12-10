

#create Akiko report
library(readxl)

data <- read_xlsx(path = "GFDRR Dashboard grant level 12_3_19.xlsx")


AFR <- grants %>% filter(`Fund Status`=="ACTV",Region=="AFR")

AFR$GPURL_binary <- ifelse(AFR$`Lead GP/Global Themes`=="Urban, Resilience and Land","GPURL","Non-GPURL")


temp_df <- AFR

temp_df_all <- temp_df %>%
  group_by(Country) %>%
  summarise("# Grants" = n(),
            "$ Amount" = dollar(sum(`Grant Amount USD`),accuracy = 1),
            "Balance" = dollar(sum(unnacounted_amount),accuracy = 1))

temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
  group_by(Country) %>%
  summarise("# Grants" = n(),
            "$ Amount" = dollar(sum(`Grant Amount USD`),accuracy = 1),
            "Balance" = dollar(sum(unnacounted_amount),accuracy = 1))

temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
    group_by(Country) %>%
    summarise("# Grants" = n(),
              "$ Amount" = dollar(sum(`Grant Amount USD`),accuracy = 1),
              "Balance" = dollar(sum(unnacounted_amount),accuracy = 1))


display_df_partial <- left_join(temp_df_GPURL,
                                temp_df_non_GPURL,
                                by="Country",
                                suffix=c(" (GPURL)"," (Non-GPURL)"))


display_df <- left_join(temp_df_all,display_df_partial,by="Country")


View(display_df)


sum_df_all <- temp_df %>%
  summarise("# Grants" = n(),
            "$ Amount" = sum(`Grant Amount USD`),
            "Balance" = sum(unnacounted_amount)) %>%
  mutate("percent"= Balance/`$ Amount`)%>% 
  mutate(`$ Amount`= dollar(`$ Amount`,accuracy = 1),
         `Balance`=dollar(`Balance`,accuracy = 1),
         `percent`=percent(`percent`))

sum_df_GPURL <-  temp_df %>%
  filter(GPURL_binary=="GPURL") %>% 
  summarise("# Grants" = n(),
            "$ Amount" = sum(`Grant Amount USD`),
            "Balance" = sum(unnacounted_amount))%>%
  mutate("percent"= Balance/`$ Amount`)%>% 
  mutate(`$ Amount`= dollar(`$ Amount`,accuracy = 1),
         `Balance`=dollar(`Balance`,accuracy = 1),
         `percent`=percent(`percent`))

sum_df_non_GPURL <- temp_df %>%
  filter(GPURL_binary=="Non-GPURL") %>% 
  summarise("# Grants" = n(),
            "$ Amount" = sum(`Grant Amount USD`),
            "Balance" = sum(unnacounted_amount))%>%
  mutate("percent"= Balance/`$ Amount`) %>% 
  mutate(`$ Amount`= dollar(`$ Amount`,accuracy = 1),
         `Balance`=dollar(`Balance`,accuracy = 1),
        `percent`=percent(`percent`))


sum_display_df <- data.frame("Summary"= c("Grant Count",
                                          "Total $ (Million)",
                                          "Total Uncommitted Balance ($) to Implement by 8/2020",
                                          "% Uncommitted Balance ($) to Implement by 8/2020"),
                             "GPURL"=unname(unlist(as.list(sum_df_GPURL))),
                             "Non-GPURL"=unname(unlist(as.list(sum_df_non_GPURL))),
                             "Combined Total"=unname(unlist(as.list(sum_df_all))))



