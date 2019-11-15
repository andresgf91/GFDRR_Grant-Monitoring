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

source("data_import.R")
source('Data_processing.R')
source("Plots.R")

# BUILD USER INTERFACE ----------------------------------------------------

## Header ---------------
header <- dashboardHeader(title = "GFDRR Secretariat Draft Dashboard",
                          dropdownMenuOutput("notifications_Menu"))

## Sidebar ---------------
sidebar <- dashboardSidebar(
  sidebarMenu(menuItem("Secretariat View",
                       menuSubItem("Overview",
                                tabName = "overview",
                                icon = icon("dashboard")),
                       menuSubItem("PMA",
                                tabName = "PMA",
                                icon = icon("stream")),
                       menuSubItem("Other Info",
                                tabName = "admin_info",
                                icon = icon("stream"))),
              menuItem("Parent Trust Fund",
                       tabName = "parent_tf",
                       icon = icon("stream")),
              menuItem("Regions",
                       tabName = "regions",
                       icon = icon("receipt")),
              menuItem("TTL/Grant Detail",
                       menuSubItem("Grant View",
                                   tabName = "grant_dash",
                                   icon = icon("dashboard")),
                       menuSubItem("TTL View",
                                   tabName = "TTL_dashboard",
                                   icon = icon("stream")))))

## tab.1 (Overview)---------------
tab.1 <-  tabItem(tabName = "overview",
                  fluidPage(titlePanel('Global Overview'),
                            fluidRow(
  column(width=4,
         infoBoxOutput("total_contributions",width = NULL),
         infoBoxOutput("total_received",width = NULL),
         infoBoxOutput("total_unpaid",width = NULL)),
  column(width=4,
         valueBoxOutput("number_active_grants",width = NULL),
         infoBoxOutput("total_remaining_balance",width = NULL),
         plotlyOutput("overview_progress_GG",height = 100)),
  column(width=4,
         infoBoxOutput("closing<12",width = NULL),
         infoBoxOutput("closing<6",width = NULL))),
  fluidRow(tabsetPanel(
    tabPanel("Contributions by Trustee",
             plotlyOutput("plot1", height = 450)),
    tabPanel("Active Grants per Region",
             plotlyOutput("n_grants_region",height=450)),
    tabPanel("Active Funding per Region",
             plotlyOutput("funding_region",height=450)),
    tabPanel("Active Funding per GP",
             plotlyOutput("funding_GP",height=450))))))

# tab 1.3 ------
tab.1.3 <-  tabItem(tabName = "admin_info",
                    fluidPage(
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
                                     plotlyOutput("RETF_trustees_A_pie")),
                            tabPanel(title = "Regions",
                                     plotlyOutput("RETF_region_A_pie"))
                          ))
                        )
                      )
                    ))


## tab.2 (Trustee View) ----------------------
tab.2 <- tabItem(tabName= "parent_tf",
                 fluidPage(titlePanel("Parent Trust Fund View"),
                           fluidRow(
                   column(width=3,
                 selectInput('select_trustee',"Selected trustee:",
                             choices = sort(unique(active_trustee$temp.name))),
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
                   column(width=4,
                          infoBoxOutput("fund_contributions",width = NULL),
                          infoBoxOutput("trustee_received",width = NULL),
                          infoBoxOutput("trustee_unpaid",width = NULL)),
                 column(width=4,
                        valueBoxOutput("trustee_active_grants",width = NULL),
                        infoBoxOutput("trustee_grants_amounts",width = NULL)),
                 column(width=4,
                        infoBoxOutput("trustee_closing_in_months",width = NULL),
                        valueBoxOutput("trustee_grants_closing_6",width = NULL))),
                 fluidRow(
                 plotlyOutput("trustee_dis_GG",height = 100)),
                 fluidRow(tabsetPanel(tabPanel(
                   title="Grants per Region",
                   plotlyOutput(outputId = "trustee_region_n_grants_GG",width = 1000, height=300)),
                   tabPanel(
                     title="Funding per Region",
                     plotlyOutput(outputId = "trustee_region_GG",width = 1000,height = 300)),
                   tabPanel(title="List of Countries",
                            dataTableOutput("trustee_countries_DT"))))))


## tab.3 (Regions View) ---------------

tab.3 <-  tabItem(tabName= "regions",class='active',
                  fluidPage(titlePanel("Regional View"), fluidRow(
  column(width=2, wellPanel(selectInput('focal_select_region',"Select a Region",
                                        choices = c("Select region",sort(unique(grants$Region))),
                                        selectize = TRUE,
                                        selected="AFR"))),
  column(width=4,box(title = 'Region',
                     textOutput('focal_region_name'),
                     width = NULL)),
  column(width=4,wellPanel(selectInput('focal_select_trustee',"Select a Trustee",
                                        choices = c("All" = "",sort(unique(grants$temp.name))),
                                        multiple = TRUE,
                                       selectize = TRUE,
                                       selected = unique(grants$temp.name),
                             width = NULL))),
  column(width=2,wellPanel(selectInput("region_BE_RE","Selected Grant Excution Types",
                             choices= unique(grants$`DF Execution Type`),
                             multiple = T,
                             selectize = T,
                             selected=unique(grants$`DF Execution Type`))))),
  fluidRow(
    tabsetPanel(type='pills',tabPanel(title="Overview",
                 fluidRow(column(width=6,
                                 wellPanel(
                   infoBoxOutput(outputId = "focal_active_grants",
                                 width = NULL),
                   infoBoxOutput(outputId = "focal_active_funds",
                                 width= NULL),
                   plotlyOutput("region_remaining_committed_disbursed",height = 100))),
                   column(width=3,
                          valueBoxOutput(outputId ="focal_grants_closing_3",
                                         width=NULL),
                          valueBoxOutput(outputId ="focal_grants_active_3_zero_dis",
                                         width=NULL)),
                   column(width=3,
                          valueBoxOutput(outputId ="region_grants_may_need_transfer",
                                         width=NULL),
                          valueBoxOutput(outputId ="region_grants_active_no_transfer",
                                         width=NULL))),
                   tabsetPanel(tabPanel(
                     title="Parent Fund",
                     plotlyOutput(outputId = "focal_region_n_grants_GG",width = 1000)),
                     tabPanel(
                       title="Unit (will be GP)",
                       plotlyOutput(outputId = "region_GP_GG",width = 1200)))),
                 tabPanel(title = "Disbursement Risk",
                          fluidRow(plotOutput(outputId = "disbursement_risk_GG"),
                                          valueBoxOutput("low_risk",width = 3),
                                          valueBoxOutput("medium_risk",width = 3),
                                          valueBoxOutput("high_risk",width = 3),
                                          valueBoxOutput("very_high_risk",width = 3),
                                   fluidRow(actionButton("generate_risk_report",
                                                         "Generate Excel Risk Report")))),
                tabPanel(title="Countries",
                         DT::dataTableOutput(outputId = "region_countries_grants_table")),
                tabPanel(title="RETF grants",fluidRow(
                         valueBoxOutput("RETF_n_grants_R"),
                         valueBoxOutput("RETF_$_grants_R")),
                         fluidRow(
                         plotlyOutput("RETF_trustees_R_pie"))
                         )))))

#tabPanel(title="RETF")

## tab.4 (PMA View) ---------------

PMA.tab <- tabItem(tabName = "PMA",
                   fluidPage(titlePanel("Program Management and Administration"),
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
                                       width="800px"))))))




## tab. 5 GRANT DASHBOARD -----------

tab.5 <- tabItem(
  tabName = "grant_dash",
  fluidPage(
    titlePanel("Grant Dashboard"),
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
      column(width=2,
             valueBoxOutput("single_grant_amount",width = NULL),
             valueBoxOutput("single_grant_remaining_bal",width = NULL),
             valueBoxOutput("single_grant_m_active",width = NULL),
             valueBoxOutput("single_grant_m_disrate",width = NULL),
             valueBoxOutput("single_grant_m_to_close",width = NULL),
             valueBoxOutput("single_grant_m_req_disrate",width = NULL)),
      box(
          plotlyOutput("grant_expense_GG", height = 400)),
      box(width = 4,collapsible = T,
        tableOutput("expense_table"))
    )
  ))




## TAB 6. TTL Dashboard 
tab.6 <- tabItem(
  tabName = "TTL_dashboard",
  fluidPage(
    titlePanel("TTL Dashboard"),
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
      column(width=2, valueBoxOutput("TTL_grants_active",width = NULL),
             valueBoxOutput("TTL_total_grant_amount",width = NULL),
             valueBoxOutput("TTL_total_remaining_bal",width = NULL)),
      box(
        plotlyOutput("TTL_balance_GG", height = 400)),
      box(width = 4,collapsible = T,
          tableOutput("TTL_grant_table"))
    )
  ))

## BODY ---------------
body <- dashboardBody(tabItems(tab.1,tab.2,tab.3,PMA.tab,tab.1.3,tab.5,tab.6))

# UI ------
ui <- dashboardPage(header,sidebar,body)


#source("server.R")
#shinyApp(ui, server)
