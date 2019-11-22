#read dashboard `raw_data` sheet into R
current_dashboard <- read_xlsx('GFDRR Dashboard - 201905.xlsx',
                               sheet = 'Raw Data',
                               skip = 2)

awards_sheet <- read_xlsx('GFDRR Dashboard - 201905.xlsx',
                          sheet = 'FY17-19 Awards',
                          skip = 3)

#create new names for columns
new_dashboard_names <- c("Window#","Window_Name","Trustee#",
                         "Child_TF","Project_ID", "TF Name",
                         "Execution_Type","Fund_Status","TTL",
                         "Region", "Country", "Act_Date",
                         "Clos_Date","TF Amount","Total Disbursed",
                         "2019_Dis","PO Commitments", "Closing_FY",
                         "Year","Month","Year-Mo","Balance",
                         "TF Age (Months)","Planned TF Duration (Months)",
                         "Months to Closing", "Time Ratio", "Disbursement Rate",
                         "Completion_Gap","Burn_Rate_Monthly",
                         "Expected_Balance_June_2018",
                         ">3_years_old", "Past_closing_date","Slow_Disbursing",
                         "High_undisbursed balances_(>60%)",">2_years_old_<60%_disbursed",
                         "<6months_to_closing_high_balance(>60%)",
                         "On_Track_for_Disbursement_by_End_FY?","Problem_project",
                         "Regional_Focal_Point_Notes","Nothing" )

#change name of columnns
names(current_dashboard) <- new_dashboard_names

#remove empty rows and non-active grant from dataframe
current_dashboard <- current_dashboard %>% filter(!is.na(Child_TF),!is.na(Act_Date))

#convert date columns to date-format 
current_dashboard$Act_Date <- ymd(current_dashboard$Act_Date)
current_dashboard$Clos_Date <- ymd(current_dashboard$Clos_Date)
current_dashboard$Year <- year(current_dashboard$Act_Date)
current_dashboard$Month <- month(current_dashboard$Act_Date)
