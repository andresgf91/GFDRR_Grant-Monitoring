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

source("data_import.R")
source('Data_processing.R')
source("Plots.R")


# BUILD USER INTERFACE ----------------------------------------------------

## Header ---------------
header <- dashboardHeaderPlus(
                          dropdownMenuOutput("date_data_updated_message"),
                          disable = F,
                          enable_rightsidebar = TRUE,
                          rightSidebarIcon = "sliders-h"
)
## Sidebar ---------------

    
secretariat_view <-    menuItem("Secretariat View",icon = icon("dashboard"),
                       menuSubItem("Overview",
                                tabName = "overview",
                                icon = icon("dashboard")),
                       #menuSubItem("PMA",
                              #  tabName = "PMA",
                              #  icon = icon("stream")),
                       menuSubItem("Other Info",
                                tabName = "admin_info",
                                icon = icon("stream")))

parent_trust_fund_view <-   menuItem("Parent Trust Fund",
                       tabName = "parent_tf",
                       icon = icon("stream"))

regions_view <-  menuItem("Regions",
                       tabName = "regions",
                       icon = icon("receipt"),selected = TRUE)

TTL_grant_detail <-  menuItem("TTL/Grant Detail",
                       menuSubItem("Grant View",
                                   tabName = "grant_dash",
                                   icon = icon("dashboard")),
                       menuSubItem("TTL View",
                                   tabName = "TTL_dashboard",
                                   icon = icon("stream")))


sidebar <- dashboardSidebar(collapsed = TRUE,
                            sidebarMenu(regions_view,
                                        secretariat_view,
                                        parent_trust_fund_view,
                                        TTL_grant_detail))

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
                        valueBoxOutput("closing<12", width = NULL)
                      ),
                      column(
                        width = 4,
                        boxPlus(
                          plotlyOutput("n_grants_region", height = 260),
                          title='Grants by Region',
                          background = "blue",
                          enable_label = T,
                          label_text = NULL,
                          width = NULL,
                          collapsible = TRUE,
                          closable = F),
                        boxPlus(plotlyOutput("funding_region", height = "260px"),
                                title='Funding by Region',
                                background = "blue",
                                enable_label = T,
                                label_text = NULL,
                                width = NULL,
                                collapsible = TRUE,
                                closable = F,
                                collapsed = TRUE),
                        boxPlus(plotlyOutput("funding_GP", height = "260px"),
                                title='Funding by Global Practice',
                                background = "blue",
                                enable_label = T,
                                label_text = NULL,
                                width = NULL,
                                collapsible = TRUE,
                                closable = FALSE,
                                collapsed = TRUE )
                       # plotlyOutput("funding_GP", height = 550)
                      ),
                      column(
                        width = 6,
                        boxPlus(
                          plotlyOutput("elpie",
                                             height="260px"),
                                title='Active Grants',
                                background = "blue",
                                enable_label = T,
                                label_text = NULL,
                                width = NULL,
                                collapsible = TRUE,
                                closable = F),
                        boxPlus(
                          plotlyOutput("plot1",height = "260px"),
                                title="Contributions by Trustee",
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



                    
                  

# tab 1.3 ------
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
tab.2 <- tabItem(tabName= "parent_tf",
                 wellPanel("Parent Trust Fund View"),
                           fluidRow(
                   column(width=3,
                 box(title="Contribution by:",
                     textOutput('trustee_contribution_agency'),
                     width=NULL
                     )),
                 column(width=5,
                 box(title = 'Trustee Fund Name',
                     textOutput('trustee_name'),
                     width = NULL),
                 box(title = 'TTL Name',
                     textOutput('TTL_name'),
                     width = NULL)),
                 column(width=4,
                        wellPanel(
                          selectInput('trustee_select_region',"Selected Regions",
                                      choices = sort(unique(grants$temp.name)),
                                      multiple = TRUE,
                                      selectize = TRUE,
                                      selected =  sort(unique(grants$temp.name)))))),
                 fluidRow(
                   tabsetPanel(
                     tabPanel(title="Overview",
                              fluidRow(
                       column(width=4,
                              valueBoxOutput("fund_contributions",width = NULL),
                              valueBoxOutput("trustee_closing_in_months",width = NULL)),
                       column(width=4,
                              valueBoxOutput("trustee_received",width = NULL),
                              valueBoxOutput("trustee_unpaid",width = NULL)),
                              #infoBoxOutput("trustee_grants_amounts",width = NULL)),
                       column(width=4,
                              valueBoxOutput("trustee_active_grants",width = NULL),
                              valueBoxOutput("trustee_grants_closing_6",width = NULL))),
                       fluidRow(
                         plotlyOutput("trustee_dis_GG",height = 100))),
                     tabPanel(
                   title="Grants per Region",
                   plotlyOutput(outputId = "trustee_region_n_grants_GG",width = 1000, height=300)),
                   tabPanel(
                     title="Funding per Region",
                     plotlyOutput(outputId = "trustee_region_GG",width = 1000,height = 300)),
                   tabPanel(title="List of Countries",
                            dataTableOutput("trustee_countries_DT")))))


## tab.3 (Regions View) ---------------

tab.3 <-  tabItem(tabName = "regions",
                  class = 'active',
                    titlePanel("Grants View"),
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
                              plotlyOutput(outputId = "disbursement_risk_GG", height="260px"),
                              title='Grants Disbursement Risk',
                              background = "navy",
                              enable_label = T,
                              label_text = NULL,
                              width = 12,
                              collapsible = TRUE,
                              closable = F,
                              collapsed = F),
                            
                        boxPlus(
                          plotlyOutput(outputId = "focal_region_n_grants_GG",
                                       height="260px"),
                          title='Active Grants by Trustee',
                          background = "navy",
                          enable_label = T,
                          label_text = NULL,
                          width = 12,
                          collapsible = TRUE,
                          closable = F,
                          collapsed = F)
                             )),
                            fluidRow(
                              
                      #--------        
                            #,
                          #    boxPlus(title='Recipient Executed Grants',
                          #      #plotlyOutput(outputId = "RETF_trustees_R_pie",
                          #                  # height="260px"),
                          #      valueBoxOutput("RETF_n_grants_R"),
                          #      valueBoxOutput("RETF_$_grants_R"),
                          #      
                          #      background = "blue",
                          #      enable_label = T,
                          #      label_text = NULL,
                          #      width = 6,
                          #      collapsible = TRUE,
                          #      closable = F,
                          #      collapsed = TRUE)
                          # )
                      
                      #--------------
                          
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
                                  title='Countries Summary Table',
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

## BODY ---------------
body <- dashboardBody(tags$head(tags$style(HTML('
  .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
  width:600px;
  }
    /* body */
    .content-wrapper, .right-side {
    background-color: #FFFFFF;
    }
    
    .small-box {height: 150px}
    
    .wrapper{
    overflow-y: hidden;}

  '))),tabItems(tab.1,tab.2,tab.3,PMA.tab,tab.1.3,tab.5,tab.6))

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
ui <- dashboardPagePlus(collapse_sidebar = TRUE,header,sidebar,body,
                        rightsidebar = ray.of.sunshine,
                        skin = "black")


#source("server.R")
#shinyApp(ui, server)
