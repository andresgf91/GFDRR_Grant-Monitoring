# PACKAGES ----------------------------------------------------------------
library(readxl)
library(dplyr)

options(scipen = 999)
# READ IN DATA ------------------------------------------------------------

sap <- read_xlsx('SAP Data as of June 30_2019.xlsx')
grants <- read_xlsx("GFDRR Dashoard grant level 12_2_19.xlsx")

date_data_udpated <- lubridate::as_date("19/10/08")

dash <- read_xlsx('GFDRR Dashboard - 201910.xlsx', sheet = 3, skip = 2)
#trustee <- read_xls('GFDRR_mg_unit.xls', skip=2)
trustee <- read_xlsx('GFDRR Dashboard Trustee Level 10_8_19.xlsx')
recode_trustee <- read_xlsx('recodes.xlsx',sheet=1) %>% select(-Status)
recode_region <- read_xlsx('recodes.xlsx',sheet=2)

expenses <- read.csv(file='V2_GFDRR TF Expense Details - FY18 and FY19 YTD(AutoRecovered).csv',
                     stringsAsFactors = FALSE,col.names =  c("Disbursing.Trust.Fund",
                                                             "Disbursing.Trust.Fund.Description",
                                                             "TF.Status",
                                                             "Task.team.Leader",
                                                             "Managing.Unit",
                                                             "Region",
                                                             "Global.Practice",
                                                             "Trustee",
                                                             "Trustee.Description",
                                                             "TF.Creation.date",
                                                             "TF.Closing.Date",
                                                             "Cost.Object",
                                                             "Cost.Object.Description",
                                                             "Commitment.item",
                                                             "Commitment.Item.Group",
                                                             "Disbursements.FY.2018",
                                                             "Disbursements.FY.2019...YTD.Feb.2019"))[1:6958,1:17]

expenses <- expenses %>% mutate(Disbursements.FY.2018=as.numeric(Disbursements.FY.2018),
                            Disbursements.FY.2019...YTD.Feb.2019=as.numeric(Disbursements.FY.2019...YTD.Feb.2019))

#create a new df with only active trustees and with recoded names
active_trustee <- full_join(trustee,recode_trustee,
                            by=c('Fund'='Trustee.num')) %>%
  filter(`Fund Status` %in% c("ACTV","ACTN"))

needs <- active_trustee %>%
  select(Fund,`Fund Name`,Trustee.name)

write.csv(needs,file="Needs_GFDRR_attention_1.csv")


grants %>% filter(Fund %in% active_trustee$Fund)

#add short region name to SAP data 
grants <- full_join(grants,recode_region,by=c("Fund Country Region Name"='Region_Name'))

#filter out grants that have 0 
grants <- grants %>% filter(`Grant Amount USD`>0)

#create a  new column with pseudo final TRUSTEE names to display
#this is just in case not all desired names have been provided by GFDRR
active_trustee$temp.name <- NA
for (i in 1:nrow(active_trustee)){
  if (is.na(active_trustee$Trustee.name[i])){
    active_trustee$temp.name[i] <- active_trustee$`Fund Name`[i]
  } else
  {active_trustee$temp.name[i] <- active_trustee$Trustee.name[i]}
}

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}




active_trustee$months_to_end_disbursement <- 
  elapsed_months(active_trustee$`TF End Disb Date`,today())

active_trustee$months_to_end_disbursement_static <- 
  elapsed_months(active_trustee$`TF End Disb Date`,date_data_udpated)



grants$months_to_end_disbursement <- 
  elapsed_months(grants$`Closing Date`,today())

grants$months_to_end_disbursement_static <- 
  elapsed_months(grants$`Closing Date`,date_data_udpated)


# add a column with Trustee temporary name V
grants <- full_join(grants,active_trustee %>%
                   filter(Fund %in% trustee$Fund) %>% 
                   select(temp.name,Fund),
                 by = c("Trustee"="Fund")) %>%
  filter(!is.na(Fund))

grants$closing_FY <- ifelse(month(grants$`Closing Date`) > 6,
                         year(grants$`Closing Date`) + 1,
                         year(grants$`Closing Date`))

grants$remaining_balance <- grants$`Grant Amount USD` - grants$`Disbursements USD`

grants$unnacounted_amount <- grants$`Grant Amount USD` -
  (grants$`Disbursements USD` + grants$`Commitments USD`)

grants$percent_unaccounted <- (grants$unnacounted_amount * 100 ) / grants$`Grant Amount USD`

grants$tf_age_months <- elapsed_months(today(),grants$`Activation Date`)

grants$planned_tf_duration_months <- elapsed_months(grants$`Closing Date`,
                                                 grants$`Activation Date`)

grants$time_ratio <- (grants$tf_age_months/grants$planned_tf_duration_months)


grants$disbursement_rate <- ifelse(grants$`Disbursements USD`>0,
                                           (grants$`Disbursements USD`/grants$`Grant Amount USD`),
                                           0)
grants$monthly_disbursement_rate <- ifelse(grants$`Disbursements USD`>0,
                                   (grants$`Disbursements USD`/grants$`Grant Amount USD`)/grants$tf_age_months,
                                   0)

grants$completion_gap <- grants$time_ratio - grants$disbursement_rate

grants$burn_rate <- (grants$`Disbursements USD`/ grants$tf_age_months)

grants$percent_remaining <- grants$remaining_balance/grants$`Grant Amount USD`

grants$required_disbursement_rate <- (grants$unnacounted_amount/grants$`Grant Amount USD`)/
  grants$months_to_end_disbursement

grants$required_disbursement_rate <- ifelse(grants$required_disbursement_rate==Inf,
                                      grants$percent_unaccounted/100,
                                      grants$required_disbursement_rate)

compute_risk_level <- function (x){
  
  risk_level <- ifelse(x < .03,"Low Risk",
                       ifelse(x < .05,"Medium Risk",
                       ifelse(x < .10,"High Risk",
                       "Very High Risk")))
  
  return(risk_level)
}

grants$disbursement_risk_level <- compute_risk_level(grants$required_disbursement_rate)


compute_risk_color<- function (x){
  
  risk_color <- ifelse(x<.03,"green",
                       ifelse(x<.05,"yellow",
                              ifelse(x<.10,"orange",
                                     "red")))
  
  return(risk_color)
}

grants$disbursement_risk_color <- compute_risk_color(grants$required_disbursement_rate) 

#compute adjusted transfers (accounting for transfers out)
grants$adjusted_transfer <- grants$`Transfer-in USD` - grants$`Transfers-out in USD`

#compute percentage of what was transferred that is available
grants$percent_transferred_available <- grants$`Available Balance USD`/grants$adjusted_transfer

#percent amount that is still available to be transferred
grants$funds_to_be_transferred <- grants$`Grant Amount USD` - grants$adjusted_transfer

#compute percentage of total grant that still needs to be transferred
grants$percent_left_to_transfer <- grants$funds_to_be_transferred/grants$`Grant Amount USD`

#two step process to identify PMA grants:

#step 1
grants$PMA <- ifelse(is.na(grants$`Project ID`),'yes','no')

#step 2
#remove_just-in-time from PMA 
grants$PMA <- ifelse(stringi::stri_detect(tolower(grants$`Fund Name`),
                                          fixed=tolower('Just-in-Time')),
                     'no',
                     grants$PMA)

PMA_grants <- grants %>% filter(PMA=='yes',`Fund Status`=="ACTV")

PMA_expenses <- expenses %>% filter(Disbursing.Trust.Fund %in% PMA_grants$Fund)


n_months <- PMA_grants$months_to_end_disbursement_static[which.max(PMA_grants$months_to_end_disbursement_static)]
list_monthly_resources <- rep(list(0),n_months)
list_grant_df <- rep(list(0),nrow(PMA_grants))
first_date <- as_date(date_data_udpated) %>% floor_date(unit = "months")

for (i in 1:nrow(PMA_grants)){
  
  temp_max_months <- PMA_grants$months_to_end_disbursement_static[i]
  monthly_allocation <- (PMA_grants$unnacounted_amount[i])/temp_max_months
  
  temp_df <- data_frame(fund=rep(as.character(PMA_grants$Fund[i]),temp_max_months),
                        fund_name=rep(as.character(PMA_grants$`Fund Name`[i]),temp_max_months),
                        trustee=rep(as.character(PMA_grants$Trustee[i]),temp_max_months),
                        fund_TTL=rep(as.character(PMA_grants$`Fund TTL Name`[i]),temp_max_months),
                        sub_date= first_date %m+% months(c(0:(temp_max_months-1))),
                        amount=rep(monthly_allocation,temp_max_months)
  )
  
  list_grant_df[[i]] <- temp_df
  
  for (j in 1:temp_max_months){
    list_monthly_resources[[j]] <- list_monthly_resources[[j]] + monthly_allocation
  }
  
  #print(list_monthly_resources[[1]])
}

gg_data <- do.call(rbind,list_grant_df)

data <- data.frame(month_num=1:n_months,
                   amount_available=unlist(list_monthly_resources))

data$dates <- first_date %m+% months(c(0:(nrow(data)-1)))

data$yearmonth <- paste0(year(data$dates),ifelse(month(data$dates)<10,
                                                 paste0(0,month(data$dates)),
                                                 month(data$dates)))

gg_data$yearmonth <- paste0(year(gg_data$sub_date),ifelse(month(gg_data$sub_date)<10,
                                                          paste0(0,month(gg_data$sub_date)),
                                                          month(gg_data$sub_date)))

data$yearquarter<- paste0(as.numeric(year(data$dates)),as.numeric(quarter(data$dates)))
gg_data$yearquarter<- paste0(as.numeric(year(gg_data$sub_date)),
                             as.numeric(quarter(gg_data$sub_date)))

data$quarterr <- zoo::as.yearqtr(data$dates)
gg_data$quarterr <- zoo::as.yearqtr(gg_data$sub_date)

gg_data$quarter_lubri <- lubridate::quarter(gg_data$sub_date,with_year = TRUE)

grouped_gg_data <- gg_data %>%
  mutate(quarterr=as_date(quarterr)) %>% 
  group_by(quarterr,fund_name,fund,yearquarter) %>%
  summarise(amount=sum(amount))


quarterly_total <- gg_data %>% mutate(quarterr=as_date(quarterr)) %>%
  group_by(quarterr) %>% summarise(Q_amount=sum(amount))

grouped_gg_data <- full_join(grouped_gg_data,quarterly_total,by="quarterr")

gg_df <- grouped_gg_data %>% filter(yearquarter<20210)

