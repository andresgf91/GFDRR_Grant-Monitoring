
# PACKAGES ----------------------------------------------------------------
library(ggplot2)

# OVERVIEW TAB ------------------------------------------------------------

melted_contributions <- active_trustee %>%
  filter(`Net Paid-In Condribution in USD`!=0) %>% arrange(`Net Signed Condribution in USD`) %>% 
  select(temp.name,
         `Net Unpaid contribution in USD`,
         `Net Paid-In Condribution in USD`) %>% 
  reshape2::melt() %>%
  inner_join(active_trustee %>% arrange(`Net Signed Condribution in USD`) %>% 
               filter(`Net Paid-In Condribution in USD`!=0) %>% 
               select(temp.name,
                      `Net Unpaid contribution in USD`,
                      `Net Paid-In Condribution in USD`),.,by="temp.name")


melted_contributions <- melted_contributions %>%
  dplyr::mutate(variable=ifelse(variable=="Net Unpaid contribution in USD",
                                "Un-paid","Paid"))

trustee_contributions_GG <- ggplot(melted_contributions,
                                   aes(
                                     reorder(temp.name,
                                             value, sum),
                                     value / 1000000,
                                     fill = variable,
                                     text = paste0("Trustee: ", temp.name, "\n",
                                                   variable, ": ", dollar(value))
                                   )) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Contributions by Trustee",
       y = "Expected Contribution (USD M)",
       x = "Trustee") +
  #theme(axis.text.x = element_text(angle=90)) +
  coord_flip() +
  scale_fill_discrete(name = "Contributions", labels = c("Un-paid", "Paid")) +
  scale_fill_manual(name="Contributions",values = c("#2E2EFE", "#CED8F6", "#56B4E9")) +
  theme(
    rect = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent")
  ) 


new_df <- data.frame(Disbursed=sum(grants$`Disbursements USD`),
                     Committed=sum(grants$`Commitments USD`),
                     "Remaining"=sum(grants$unnacounted_amount)) %>% 
  reshape2::melt() %>%
  mutate(total=sum(grants$`Grant Amount USD`)) %>%
  mutate(percent=value/total)

# 
# stream <- streamgraph(grouped_gg_data %>% filter(yearquarter<20210),
#               key="fund",
#               value="amount", date="quarterr",
#               height="400px", width="800px",order = "inside-out")