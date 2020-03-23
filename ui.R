#
# This is the user-interface definition of the Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/ 
#

# LOAD PACKAGES -----------------------------------------------------------

library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(shinydashboardPlus)
library(readxl)
library(pander)
library(scales)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(shinyBS)
library(plotly)
library(streamgraph)
library(DT)
library(shinythemes)
library(shinyjs)

# BUILD USER INTERFACE ----------------------------------------------------

## Header ---------------
Header <- dashboardHeaderPlus(title=
                                #tags$a(tags$img(src='GFDRR_BW_logo.png',height='45',width='190'))
                                
                                
                                tagList(
  span(class="logo-lg",'SARA'), 
  span(class = "logo-mini","SARA "))
  ,
                          dropdownMenuOutput("date_data_updated_message"),
                         # disable = F,
                          enable_rightsidebar = TRUE,
                          rightSidebarIcon = "gears",
  left_menu =  tagList(img(class="logo-lg",src='GFDRR_BW_logo.png',height='45',width='190'))
)
## Sidebar ---------------

    
secretariat_view <-   # menuItem("Secretariat View",icon = icon("dashboard"),
                       menuItem("Portfolio Overview",
                                tabName = "overview",
                                icon = icon("dashboard"),
                                selected = T)
                       #,
info <-             #menuSubItem("PMA",
                              #  tabName = "PMA",
                              #  icon = icon("stream")),
                    menuItem("Additional Information",
                             tabName = "admin_info",
                             icon = icon("info"))

parent_trust_fund_view <-   menuItem("Parent Trust Fund",
                       tabName = "parent_tf",
                       icon = icon("funnel-dollar"),
                       selected = F)

regions_view <-  menuItem("Grant Portfolio",
                       tabName = "regions",
                       icon = icon("search-plus"),selected = F)

TTL_grant_detail <-  menuItem("TTL/Grant Detail",
                       menuSubItem("Grant View",
                                   tabName = "grant_dash",
                                   icon = icon("dashboard")),
                       menuSubItem("TTL View",
                                   tabName = "TTL_dashboard",
                                   icon = icon("stream")))

reports_tab <- menuItem("Download Reports",
                        tabName = "reports",
                        icon=icon("file-download"),
                        selected = F)


Sidebar <- dashboardSidebar(#collapsed = TRUE,
                            sidebarMenu( secretariat_view,
                                         parent_trust_fund_view,
                                         regions_view,
                                        info,
                                        reports_tab,
                                       # TTL_grant_detail,
                                        id = 'nav'))

## tab.1 (Overview)---------------
tab.1 <-  tabItem(tabName = "overview",
                    theme = shinytheme("readable"),
                     titlePanel('Global Overview'),
                    fluidRow(
                      column(
                        width = 2,
                        valueBoxOutput("total_contributions", width = NULL),
                        valueBoxOutput("total_received", width = NULL),
                        valueBoxOutput("total_unpaid", width = NULL),
                        valueBoxOutput("total_active_portfolio", width = NULL),
                        valueBoxOutput("total_uncommitted_balance", width = NULL),
                        valueBoxOutput("closing<12", width = NULL)
                      ),
                      column(
                        width = 4,
                       
                        boxPlus(
                          plotlyOutput("funding_region", height = "260px"),
                                title='Funding by Region',
                                background = "blue",
                                enable_label = T,
                                label_text = NULL,
                                width = NULL,
                                collapsible = TRUE,
                                closable = F,
                                collapsed = F),
                        boxPlus(
                          plotlyOutput("funding_GP", height = "260px"),
                                title='Funding by Global Practice',
                                background = "blue",
                                enable_label = T,
                                label_text = NULL,
                                width = NULL,
                                collapsible = TRUE,
                                closable = FALSE,
                                collapsed = F),
                        boxPlus(
                          plotlyOutput("n_grants_region", height = 260),
                          title='Grants by Region',
                          background = "blue",
                          enable_label = T,
                          label_text = NULL,
                          width = NULL,
                          collapsible = TRUE,
                          closable = F,
                          collapsed = F)
                       # plotlyOutput("funding_GP", height = 550)
                      ),
                      column(
                        width = 6,
                        boxPlus(
                          plotlyOutput("elpie",
                                             height="260px"),
                                title='Active Portfolio',
                                background = "blue",
                                enable_label = T,
                                label_text = NULL,
                                width = NULL,
                                collapsible = TRUE,
                                closable = F),
                        boxPlus(
                          plotlyOutput("plot1",
                                       height = "600px"),
                                title = "Contributions by Trustee",
                                width = NULL,
                          background = "blue",
                          enable_label = T,
                          label_text = NULL,
                          collapsible = TRUE,
                          closable = F)
                        #plotlyOutput("overview_progress_GG", height = 90)),
                      )
                    )
                           # valueBoxOutput("number_active_grants_op", width = 5),
                             # infoBoxOutput("total_remaining_balance_op", width = 7),
                           # plotlyOutput("overview_progress_GG_op"),
                             # valueBoxOutput("number_active_grants_pma", width = 5),
                             # infoBoxOutput("total_remaining_balance_pma", width = 7),
                    
                      #plotlyOutput("overview_progress_GG_pma"),
                    
                    
    )



                    
                  

# tab 1.3 (additional info)------
tab.1.3 <-  tabItem(tabName = "admin_info",
                      titlePanel('Additional Information'),
                      tabsetPanel(
                        type = 'pills',
                        tabPanel("Trustee Info",
                                 tableOutput("trustee_name_TTL")),
                        tabPanel(
                          "Donor Contributions",
                          tableOutput("donor_contributions"),
                          plotlyOutput("donor_contributions_GG")
                        ),
                        tabPanel(
                          title = "RETF grants overview",
                          fluidRow(
                            valueBoxOutput("RETF_n_grants_A"),
                            valueBoxOutput("RETF_$_grants_A")
                          ),
                          fluidPage(tabsetPanel(
                            tabPanel(title = "Trustees",
                                     plotlyOutput("RETF_trustees_A_pie", height = 450)),
                            tabPanel(title = "Regions",
                                     plotlyOutput("RETF_region_A_pie",height = 450))
                          ))
                        )
                      )
                    )

## tab.2 (Parent Trustfund View) ----------------------
tab.2 <- tabItem(tabName= "parent_tf",fluidPage(
                 titlePanel("Parent Trust Fund View"),
                           fluidRow(
                   column(width=3,
                 boxPlus(title = "", closable = F,enable_label = T,label_text = 'Parent Fund(s)',
                     textOutput('trustee_name'),
                     width=NULL,background = "navy")),
                 column(width=9,
                 boxPlus(title = "", closable = F,enable_label = T,label_text = 'Donor Agencies',
                     textOutput('trustee_contribution_agency'),
                     width = 6,background = 'navy'),
                 boxPlus(title = "", closable = F,enable_label = T,label_text = 'TTL(s)',
                     textOutput('TTL_name'),
                     width = 6,
                     background = "navy"))),
                 fluidRow(
                   column(width=3,
                 valueBoxOutput("fund_contributions", width = NULL),
                 valueBoxOutput("trustee_closing_in_months",width = NULL),
                 valueBoxOutput("trustee_active_grants",width = NULL),
                 valueBoxOutput("trustee_grants_closing_3",width = NULL)
                           ),
                 column(width=9,
                        boxPlus(
                          plotlyOutput("trustee_received_unpaid_pie", height = 260),
                          title='Contributions',
                          background = "blue",
                          enable_label = T,
                          label_text = NULL,
                          width = 6,
                          collapsible = TRUE,
                          closable = F),
                        boxPlus(
                          plotlyOutput("trustee_dis_GG", height = 260),
                          title='Disbursement Summary',
                          background = "blue",
                          enable_label = T,
                          label_text = NULL,
                          width = 6,
                          collapsible = TRUE,
                          closable = F),
                        boxPlus(
                          plotlyOutput("trustee_region_n_grants_GG", height = 260),
                          title='Grants by Region',
                          background = "blue",
                          enable_label = T,
                          label_text = NULL,
                          width = 6,
                          collapsible = TRUE,
                          closable = F),
                        boxPlus(
                          plotlyOutput("trustee_region_GG", height = 260),
                          title='Funding per Region',
                          background = "blue",
                          enable_label = T,
                          label_text = NULL,
                          width = 6,
                          collapsible = TRUE,
                          closable = F))),
                 fluidRow(
                   boxPlus(dataTableOutput("trustee_countries_DT"),
                   title='Country Summary Table',
                   background = NULL,
                   enable_label = T,
                   label_text = NULL,
                   width = NULL,
                   collapsible = TRUE,
                   closable = F,
                   collapsed = T)
                 
                 ))
)
                        
                        
                        
                              #valueBoxOutput("trustee_received",width = NULL),
                             # valueBoxOutput("trustee_unpaid",width = NULL)),
                              #infoBoxOutput("trustee_grants_amounts",width = NULL)),
  


## tab.3 (Regions View) ---------------

tab.3 <-  tabItem(tabName = "regions",
                  class = 'active',
                    titlePanel("Grant Portfolio"),
                    fluidRow(
                      column(width=3,
                             valueBoxOutput(outputId = "focal_active_grants",width = 12),
                             valueBoxOutput(outputId = "focal_active_funds", width = 12),
                             valueBoxOutput("low_risk", width = 6),
                             valueBoxOutput("medium_risk", width = 6),
                             valueBoxOutput("high_risk", width = 6),
                             valueBoxOutput("very_high_risk", width = 6),
                        valueBoxOutput(outputId = "focal_grants_closing_3", width = 6),
                             valueBoxOutput(outputId = "focal_grants_active_3_zero_dis",width = 6),
                             valueBoxOutput(outputId = "region_grants_may_need_transfer", width = 6),
                             valueBoxOutput(outputId = "region_grants_active_no_transfer",width = 6)
                             ),
                      column(width=9,fluidRow(
                        boxPlus(
                          plotlyOutput("elpie2",
                                       height="260px"),
                          title='Funds Summary',
                          background = "navy",
                          enable_label = T,
                          label_text = NULL,
                          width = 6,
                          collapsible = TRUE,
                          closable = F,
                          collapsed = F),
                        boxPlus(
                            plotlyOutput(outputId = "region_GP_GG",
                                         height="260px"),
                            title='Funding by GP/Global Theme',
                            background = "navy",
                            enable_label = T,
                            label_text = NULL,
                            width = 6,
                            collapsible = TRUE,
                            closable = F,
                            collapsed = F),
                            boxPlus(
                              plotlyOutput(outputId = "disbursement_risk_GG",
                                           height="260px"),
                              title ='Grants Disbursement Risk',
                              background = "blue",
                              enable_label = T,
                              label_text = NULL,
                              width = 12,
                              collapsible = TRUE,
                              closable = F,
                              collapsed = F),
                            
                        boxPlus(
                          plotlyOutput(outputId = "focal_region_n_grants_GG",
                                       height = "420px"),
                          title ='Active Portfolio by Trustee',
                          background = "light-blue",
                          enable_label = T,
                          label_text = NULL,
                          width = 12,
                          collapsible = TRUE,
                          closable = F,
                          collapsed = F)
                             )),
                            fluidRow(
                          
                          boxPlus(solidHeader = T,
                            DT::dataTableOutput(outputId = "region_summary_grants_table"),
                            title='Summary Table',
                            background = NULL,
                            enable_label = T,
                            label_text = NULL,
                            width = 12,
                            collapsible = TRUE,
                            closable = F,
                            collapsed = T) ,
                          boxPlus(solidHeader = T,
                                  DT::dataTableOutput(outputId = "region_countries_grants_table"),
                                  title ='Countries Summary Table',
                                  background = NULL,
                                  enable_label = T,
                                  label_text = NULL,
                                  width = 12,
                                  collapsible = TRUE,
                                  closable = F,
                                  collapsed = T) ,
                          boxPlus(solidHeader = T,
                                  DT::dataTableOutput(outputId = "region_funding_source_grants_table"),
                                  title='Funding source table',
                                  background = NULL,
                                  enable_label = T,
                                  label_text = NULL,
                                  width = 12,
                                  collapsible = TRUE,
                                  closable = F,
                                  collapsed = T)
                            )
                            
                      #,
                     #fluidRow(
                        #plotlyOutput("region_remaining_committed_disbursed", height = 100),
                       # plotlyOutput(outputId = "region_GP_GG", width = 1200,height=500)),
                           # ,
                       #     fluidRow(column(3,offset = 1,
                      #                      downloadButton("generate_risk_report",
                      #                     "Download Excel Report"))),
                          #dateInput(
                          #  "summary_table_cutoff_date",
                          #  "Grants that need to be implemented by:",
                         #   value = grants$`Closing Date`[which.max(grants$`Closing Date`)] %>%
                         #     lubridate::as_date(),
                         #   autoclose = TRUE
                        #  ),
                         
                      #    fluidRow(column(3,offset = 1,
                      #                    downloadButton("generate_full_excel_report_1",
                       #                                "Open test version excel Report"))
                      #    )
                       # ,
                       #   fluidRow(column(3,offset = 1,
                        #                  downloadButton("generate_full_excel_report_2",
                        #                               "Open test version excel Report"))
                        #  ),
                        
                          #plotlyOutput(outputId = "focal_region_n_grants_GG"),

                          #fluidRow(column(3,offset = 1,
                           #               downloadButton("generate_full_excel_report_3",
                                               #        "Open draft excel Report"))
                          ))
                  

#tabPanel(title="RETF")

## tab.4 (PMA View) ---------------

PMA.tab <- tabItem(tabName = "PMA",
                   titlePanel("Program Management and Administration"),
                             column(width=4,fluidRow(
                     infoBoxOutput(outputId = 'resources_available',width = NULL),
                     infoBoxOutput(outputId = 'PMA_grants_n',width = NULL),
                     infoBoxOutput(outputId = "current_quarter",width = NULL),
                     infoBoxOutput(outputId = "next_quarter",width = NULL),
                            progressBar(id = 'quarter_spent',
                                 value = 0,
                                 display_pct = T,
                                 title = "% Spent",size = NULL))),
                     column(width=8,
                     tabsetPanel(tabPanel(title= "PMA Quartely",
                     plotlyOutput(outputId = "PMA_chart_1")),
                     tabPanel(title= "PMA Streamgraph",
                     streamgraphOutput(outputId = "streamgraph",
                                       height="350px",
                                       width="800px")))))



## tab. 5 GRANT DASHBOARD -----------

tab.5 <- tabItem(
  tabName = "grant_dash",
    titlePanel("Grant Summary"),
    fluidRow(
      column(width = 3,
      box(width = NULL,
        textInput("child_TF_num",
                "Grant Number",
                value = NULL,
                placeholder = "Enter grant number (e.g., TF018353)")),
      box(title = 'Grant Name',
          textOutput('grant_name'),
          width = NULL),
      box(title = 'TTL Name',
          textOutput('grant_TTL'),
          width = NULL),
      box(title = 'Country',
          textOutput('grant_country'),
          width = NULL),
      box(title = 'Region',
          textOutput('grant_region'),
          width = NULL),
      box(title = 'Unit',
          textOutput('grant_unit'),
          width = NULL)),
      column(width=3,
             valueBoxOutput("single_grant_amount",width = NULL),
             valueBoxOutput("single_grant_remaining_bal",width = NULL),
             valueBoxOutput("single_grant_m_active",width = NULL),
             valueBoxOutput("single_grant_m_disrate",width = NULL),
             valueBoxOutput("single_grant_m_to_close",width = NULL),
             valueBoxOutput("single_grant_m_req_disrate",width = NULL))
      #,
     # box(
         # plotlyOutput("grant_expense_GG", height = 400)),
     # box(width = 4,collapsible = T,
       # tableOutput("expense_table"))
    )
  )




## TAB 6. TTL Dashboard ----------
tab.6 <- tabItem(
  tabName = "TTL_dashboard",
    titlePanel("TTL Summary"),
    fluidRow(
      column(width = 3,
             box(width = NULL,
                 textInput("TTL_upi",
                           "TTL UPI",
                           value = NULL,
                           placeholder = "Enter UPI")),
             box(title = 'TTL Name',
                 textOutput('TTL_name_dash'),
                 width = NULL),
             box(title = 'TTL Unit',
                 textOutput('TTL_unit_dash'),
                 width = NULL)),
      column(width=3, valueBoxOutput("TTL_grants_active",width = NULL),
             valueBoxOutput("TTL_total_grant_amount",width = NULL),
             valueBoxOutput("TTL_total_remaining_bal",width = NULL)),
      box(
        plotlyOutput("TTL_balance_GG", height = 400)),
      box(width = 4,collapsible = T,
          tableOutput("TTL_grant_table"))
    )
  )


# Tab 7. Reports Download Tab --------

tab.reports <- tabItem(
  tabName = 'reports',
  h1("Download Reports"),
  
  fluidRow(
    column(width=3,
           selectInput(inputId = "report_type",label=NULL,
                       choices= c("Summary Report",
                                  "Disbursement Risk Report",
                                  "Master Report",
                                  "Source Data"),
                       selectize=TRUE)),
    column(width=4,
           conditionalPanel(
             condition = "input.report_type == 'Summary Report'",
             boxPad(color = 'blue',
                    selectInput(inputId = "summary_fund_status",width = NULL,
                                label="Child Fund Status:",
                                choices= c("ACTV","PEND"),
                                multiple = T,
                                selectize = T,
                                selected =  c("ACTV","PEND")),
                    selectInput(inputId = "summary_region",
                                label="Region Name(s):",
                                choices= sort(unique(grants$Region)),
                                multiple = T,
                                selectize = T,
                                selected =  sort(unique(grants$Region))),
                    selectInput(inputId = "summary_trustee",
                                label="Trustee(s):",
                                choices= sort(unique(grants$temp.name)),
                                multiple = T,
                                selectize = T,
                                selected =  sort(unique(grants$temp.name)))
             )
           ),
           conditionalPanel(
             condition = "input.report_type == 'Disbursement Risk Report'",
             boxPad(color = 'blue',
                    selectInput(inputId = "risk_fund_status",width = NULL,
                                label="Child Fund Status:",
                                choices= c("ACTV","PEND"),
                                multiple = T,
                                selectize = T,
                                selected =  c("ACTV","PEND")),
                    selectInput(inputId = "risk_region",
                                label="Region Name(s):",
                                choices= c("ALL",sort(unique(grants$Region))),
                                multiple = F,
                                selectize = T,
                                selected =  "ALL"),
                    selectInput(inputId = "risk_trustee",
                                label="Trustee(s):",
                                choices= sort(unique(grants$temp.name)),
                                multiple = T,
                                selectize = T,
                                selected =  sort(unique(grants$temp.name)))
             )
           )
    ),
    conditionalPanel(condition = "input.report_type == 'Summary Report'",
                     downloadButton("Download_summary_report.xlsx",
                                    label = 'Download Report')),
    conditionalPanel(condition = "input.report_type == 'Disbursement Risk Report'",
                     downloadButton("Download_risk_report.xlsx",
                                    label = 'Download Report')),
    conditionalPanel(condition = "input.report_type == 'Master Report'",
                     downloadButton("Download_master_report.xlsx",
                                    label = 'Download Report')),
    
    conditionalPanel(condition = "input.report_type == 'Source Data'",
                     downloadButton("Download_source_data.xlsx",
                                    label = 'Download Data'))
  )
)


## BODY ---------------
Body <- dashboardBody(
tags$head(tags$style(HTML('
  .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
  width:600px;
  }
    /* body */
    .content-wrapper, .right-side {
    background-color: #FFFFFF;
    }
    
    .small-box {height: 150px}
    
    .wrapper{
    overflow-y: hidden;




}

  '))),
tabItems(tab.1,tab.3,tab.2,tab.1.3,tab.reports))
#tab.2,tab.3,PMA.tab,tab.1.3,tab.5,tab.6))

# RAY OF SUNSHINE --------------
ray.of.sunshine <- rightSidebar(
  background = "dark",
  rightSidebarTabContent(
    id= 1,
    icon="desktop",
    active=TRUE,
    title= "Tab 1",
    selectInput('select_trustee',"Selected trustee:",
                choices = sort(unique(active_trustee$temp.name)))
  ),
  rightSidebarTabContent(
    id = 2,icon="gears",
    selectInput(
      'focal_select_region',
      "Selected Region(s):",
      choices = sort(unique(grants$Region)),
      multiple = TRUE,
      selectize = TRUE,
      selected = sort(unique(grants$Region))
    ),
    selectInput(
      'focal_select_trustee',
      "Select a Trustee",
      choices = c("All" = "", sort(unique(grants$temp.name))),
      multiple = TRUE,
      selectize = TRUE,
      selected = unique(grants$temp.name),
      width = NULL
    ),
    selectInput(
      "region_BE_RE",
      "Selected Grant Exc. Types:",
      choices = unique(grants$`DF Execution Type`),
      multiple = T,
      selectize = T,
      selected = unique(grants$`DF Execution Type`)
    ),
    selectInput(
      "region_PMA_or_not",
      "Select Grant Types",
      choices = unique(grants$PMA.2),
      multiple = T,
      selectize = T,
      selected = unique(grants$PMA.2)
  )
)
)

# UI ------
ui <- dashboardPagePlus(collapse_sidebar = T,
                        useShinyjs(),
                        header = Header,
                        sidebar = Sidebar,
                        body = Body,
                        rightsidebar =  rightSidebar(
                          background = "dark",
                          uiOutput("side_bar"),
                          title = "Right Sidebar"),
                        skin = "black")


#source("server.R")
#shinyApp(ui, server)
