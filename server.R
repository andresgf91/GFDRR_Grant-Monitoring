#
# This is the server logic of a Shiny web application.
# You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(shinydashboard)
library(hrbrthemes)
library(plotly)
library(ggplot2)
library(lubridate)
library(stringi)
library(RColorBrewer)
library(openxlsx)

# Define server logic required to draw a histogram
server <- shinyServer(function(input,output,session) {
  
  show_grant_button <-  function(title,id_name){HTML(paste0(title,
                                                                "<button id=",id_name,
                                                                " type=\"button\"",
                                                                "class=\"btn btn-default action-button\"",
                                                                "style=\"padding:2px; font-size:80%\">Show</button>")
  ) 
  }
  # Interactive Rigth Side-Bar 
  
  
  
  observe({
    if (req(input$nav) == "parent_tf"){
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
      output$side_bar <- renderUI({
        
        rightSidebarTabContent(
          title="Edit View",
          id= 1,
          icon="desktop",
          active=TRUE,
          checkboxGroupButtons(
            'select_trustee',
            "Selected trustee:",
            choices = sort(unique(active_trustee$temp.name[active_trustee$`Net Signed Condribution in USD`>=0])),
            selected = active_trustee$temp.name[active_trustee$`Net Signed Condribution in USD`>=0][1],
            size = "sm",
            justified = F,
            status = "primary",
            individual = T,
            width='100%',
            checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),

          checkboxGroupButtons(
            inputId = "trustee_select_region",
            label = "Filter Regions:",
            size = "sm",
            choices = sort(unique(grants$Region)), 
            justified = F,
            status = "primary",
            individual = T,
            width='100%',
            checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                            
          ),
          
          checkboxInput('trustee_remove_PMA',"Remove PMA grants")
        )
        
   
      })
      
 
    }
    if (req(input$nav) == "regions"){
      shinyjs::addClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
      shinyjs::removeClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
      output$side_bar <- renderUI({
        rightSidebarTabContent(title = "Edit View",
          id = 2,icon="gears",
          checkboxGroupButtons(
            'focal_select_region',
            label="Filter region(s):",
            choices = sort(unique(grants$Region)),
            individual = T,
            status = "primary",
            size = "sm",
            checkIcon = list(yes=icon("ok", lib = "glyphicon"))
           
          ),
          checkboxGroupButtons(
            'focal_select_trustee',
            "Selected trustee(s)",
            choices = sort(unique(grants$temp.name)),
            individual = T,
            status = "primary",
            size = "sm",
            checkIcon = list(yes=icon("ok", lib = "glyphicon"))
          ),
          checkboxGroupButtons(
            "region_BE_RE",
            "Selected grant exc. types:",
            choices = unique(grants$`DF Execution Type`),
            individual = T,
            status = "primary",
            size = "sm",
            checkIcon = list(yes=icon("ok", lib = "glyphicon"))
          ),
          checkboxGroupButtons(
            "region_PMA_or_not",
            "Selected grant types:",
            choices = unique(grants$PMA.2),
            individual = T,
            status = "primary",
            size = "sm",
            checkIcon = list(yes=icon("ok", lib = "glyphicon"))
          ),
         checkboxGroupButtons(
            "closing_fy",
            "Closing in fiscal year(s):",
            choices = sort(unique(grants$closing_FY)),
            individual = T,
            status = "primary",
            size = "sm",
            checkIcon = list(yes=icon("ok", lib = "glyphicon"))
          )
          )
      })
    }
    if (req(input$nav) == "overview"){
      shinyjs::addClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
      shinyjs::removeClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
      
      output$side_bar <- NULL
    }
    if (req(input$nav) == "admin_info"){
      shinyjs::addClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
      shinyjs::removeClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
      
      output$side_bar <- renderUI({ div() })
    }
    
    
  })
  
  message_df <- data.frame()
# TAB.1 -------------------------------------------------------------------

    output$plot1 <- renderPlotly({
      
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
        labs(y = "Expected Contribution (USD M)",
             x = "Trustee") +
        scale_fill_manual(name=NULL,values = c("#2E2EFE", "#CED8F6", "#56B4E9")) + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      m <- list(
        l = 5,
        r = 5,
        b = 5,
        t = 5,
        pad = 2
      )
      ggplotly(trustee_contributions_GG,tooltip = "text") %>%
        layout(margin=m,legend = list(x = 0.05, y = 0.95,font = list(size = 15)), xaxis = list(tickfont=list(size=14)))
      
      
      })
  
  
  output$pledged <- renderText({
    sum(active_trustee$`Net Signed Condribution in USD`) %>%
      dollar(accuracy = 1) %>% as.character()
  })
    
    output$total_contributions <- renderValueBox({
      sum(active_trustee$`Net Signed Condribution in USD`) %>%
      dollar(accuracy = 1) %>% 
      valueBox(value=tags$p(., style = "font-size: 60%;"),
               icon = icon("money-check-alt"),
               color = "navy",
               subtitle = "Total Pledged (Across Active Trustees)")
      })
    
    output$total_received <- renderValueBox({
      sum(active_trustee$`Net Paid-In Condribution in USD`) %>% 
      dollar(accuracy = 1) %>% 
      valueBox( value=tags$p(., style = "font-size: 60%;"),
                icon = icon("hand-holding-usd"),
        color = "blue", subtitle = "Total Received (Paid)")
      })
    
    output$total_unpaid <- renderValueBox({
      sum(active_trustee$`Net Unpaid contribution in USD`) %>% 
      dollar(accuracy = 1) %>% 
        valueBox(
          value= tags$p(., style = "font-size: 60%;"), icon = icon("file-invoice-dollar"),
          color = "light-blue", subtitle = "Total Pending (Un-paid)")
      
      })
    
    output$total_active_portfolio <- renderValueBox({
      sum(grants$`Grant Amount USD`) %>% 
        dollar(accuracy = 1) %>% 
        valueBox(
          value= tags$p(., style = "font-size: 60%;"), icon = icon("file-invoice-dollar"),
          color = "light-blue", subtitle = "Active Portfolio Amount")
      
    })
    
    output$total_uncommitted_balance <- renderValueBox({
      sum(grants$unnacounted_amount) %>% 
        dollar(accuracy = 1) %>% 
        valueBox(
          value= tags$p(., style = "font-size: 60%;"), icon = icon("file-invoice-dollar"),
          color = "light-blue", subtitle = "Available Balance in Active Portfolio")
      
    })

    output$elpie <- renderPlotly({
      
      temp_df <- grants %>% 
        mutate(PMA= ifelse(PMA=="yes","PMA","Operational")) %>% 
          group_by(PMA) %>%
        summarise(n_grants = n(),
                  remaining_balance = round(sum(unnacounted_amount)),
                  total_award_amount= sum(`Grant Amount USD`))
      
      total <- sum(temp_df$total_award_amount)
      
      m <- list(
        l = 15,
        r = 2,
        b = 10,
        t = 20,
        pad = 4
      )
      
      colors <- c('rgb(17,71,250)','rgb(192,207,255)')
    
      plot_ly(textposition="outside") %>%
        add_pie(title="Number of Grants",
          data = temp_df,
                labels=~PMA,
                values = ~n_grants,
                domain = list(x = c(0, 0.47),
                              y = c(0.1, 1)),
                            name = paste0("Active","\n", "Grants"),
                textinfo="value",
                            marker = list(colors=colors),
                            hole = 0.75) %>% 
        add_pie(title="Uncommitted Balance",
          data=temp_df,
                labels = ~PMA,
                values = ~remaining_balance,
                text = ~paste(dollar(remaining_balance,scale = 1/1000000,accuracy = .1),"M"),
                hoverinfo = 'label+text+name+value',
                textinfo = 'text',
                hole = 0.75,
                name = paste0("Uncommitted","\n","Balance"),
                domain = list(x = c(0.49, .96),
                              y = c(0.1, 1))) %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               margin=m)
    })
    
    output$total_remaining_balance <- renderValueBox({
      
     temp_grants <- grants %>% filter(Trustee %in% active_trustee$Fund)
      sum(temp_grants$unnacounted_amount) %>% 
      dollar(accuracy = 1)%>% 
        valueBox(value=., icon = icon("receipt"),
          color = "blue", subtitle =  "Available Balance")
      })
    
    output$`closing<12` <- renderValueBox({
      
        active_trustee %>%
        filter(months_to_end_disbursement<=12) %>% nrow() %>% 
        valueBox(
          value=.,
          icon = icon("stopwatch"),
          color="aqua",
          subtitle = show_grant_button("Trustees closing in less than 12 months ",
                                       "show_trustees_12months")
          )
    })
    
    
    observeEvent(input$show_trustees_12months, {
      data <-  active_trustee %>%
        filter(months_to_end_disbursement <= 12) %>%
        select(Fund,
               temp.name,
               `Fund TTL Name`,
               `Net Signed Condribution in USD`,
               `Net Unpaid contribution in USD`,
               `Available Balance USD`,
               months_to_end_disbursement
        ) %>%
        arrange(months_to_end_disbursement) %>%
        rename("Months Left to Disburse" = months_to_end_disbursement,
               "Short Name"= temp.name)
      
      reactive_data$download_table <- data
      
      data <- data %>% 
        mutate(`Net Signed Condribution in USD`= dollar(`Net Signed Condribution in USD`,accuracy = 1),
               `Net Unpaid contribution in USD`= dollar(`Net Unpaid contribution in USD`,accuracy = 1),
               `Available Balance USD`= dollar(`Available Balance USD`,accuracy = 1),
               `Months Left to Disburse`= round(`Months Left to Disburse`,digits = 0))
      
  
      
      showModal(modalDialog(size = 'l',
                            title = "Trustees Closing in less than 12 months",
                            renderTable(data),
                            easyClose = T,footer = downloadBttn('dload_trustees_12months')))
      
     
    })
    
    output$dload_trustees_12months <- downloadHandler(
      filename = "trustees_closing_less_12_months.xlsx",
      content = function(file) {
        write.xlsx(reactive_data$download_table,file,asTable = T)
      }) 
    
    
    shinyjs::addClass(id = "overview", class = "navbar-right")
    
    
    output$`closing<6` <- renderValueBox({
      active_trustee %>%
        filter(months_to_end_disbursement <= 6) %>% nrow() %>% 
        valueBox(
          "Trustees closing in < 6 months",
          .,
          icon = icon("stopwatch"),
          color = "red")
    })
    
    output$all_grants_amount <- renderValueBox({
    
      temp_grants <- grants %>% filter(Trustee %in% active_trustee$Trustee)
      sum(temp_grants$`Grant Amount USD`) %>% dollar() %>% 
        valueBox(
          "Amount awarded in grants",
          .,
          icon = icon("stopwatch"),
          color = "red",
          subtitle = "* All grants")
    })
    
    output$overview_progress_GG <- renderPlotly({
      
      temp_df <- grants
      
      new_df <- data.frame(Disbursed=sum(temp_df$`Disbursements USD`),
                           Committed=sum(temp_df$`Commitments USD`),
                           "Available Balance"=sum(temp_df$unnacounted_amount)) %>% 
        reshape2::melt() %>%
        mutate(total=sum(temp_df$`Grant Amount USD`)) %>%
        mutate(percent=value/total)
      
      gg <- new_df %>% ggplot(aes(x=total,
                                  y=value,
                                  fill=variable,
                                  label=percent(percent),
                                  text=paste(variable,"\n",
                                             "USD Amount:",dollar(value),"\n",
                                             "Percent of Total:",percent(percent)))) +
        geom_bar(stat='identity',position = 'stack') +
        # coord_flip() +
        theme_minimal() +
        labs(y="USD Amount")+
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        theme(#legend.direction = "horizontal",
          legend.position = 'bottom',
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm")) +
        coord_flip() +
        scale_fill_discrete(breaks=c("Available Balance","Committed","Disbursed")) +
        scale_fill_brewer(palette = "Blues") +
        geom_text(size = 3, position = position_stack(vjust = 0.5)) 
      
      plotly::ggplotly(gg, tooltip = "text") %>%
        layout(legend = list(orientation = 'h',
                             font = list(size = 10),
                             x=.2, y = -4,
                             traceorder="reversed"))
      
    })
    
    # output$number_active_grants <- renderValueBox({
    #   
    #   temp_grants <- grants %>%
    #     select(Fund) %>% 
    #     distinct() %>% nrow() %>% 
    #     valueBox(.,
    #       subtitle = "All Active Grants",
    #       icon = icon("list-ol"),
    #       color = "blue")
    # })
    # 
    #OPERATIONAL GRANTS ONLY 
    
    output$number_active_grants_op <- renderValueBox({
      
      temp_grants <- grants %>%
        filter(PMA=="no") %>% select(Fund) %>% 
        distinct() %>% nrow() %>% 
        valueBox(.,
                 subtitle = "Active Operational Grants",
                 icon = icon("list-ol"),
                 color = "green")
    })
    
    
    output$total_remaining_balance_op <- renderValueBox({
      
      temp_grants <- grants %>% filter(Trustee %in% active_trustee$Fund,PMA=="no")
      sum(temp_grants$unnacounted_amount) %>% 
        dollar()%>% 
        valueBox(
          "Uncommited Balance", ., icon = icon("receipt"),
          color = "green", subtitle = "*operational grants")
    })
    

    output$overview_progress_GG_op <- renderPlotly({
      
      temp_df <- grants %>% filter(PMA=="no")
      
      new_df <- data.frame(Disbursed=sum(temp_df$`Disbursements USD`),
                           Committed=sum(temp_df$`Commitments USD`),
                           "Available Balance"=sum(temp_df$unnacounted_amount)) %>% 
        reshape2::melt() %>%
        mutate(total=sum(temp_df$`Grant Amount USD`)) %>%
        mutate(percent=value/total)
      
      gg <- new_df %>% ggplot(aes(x=total,
                                  y=value,
                                  fill=variable,
                                  label=percent(percent),
                                  text=paste(variable,"\n",
                                             "USD Amount:",dollar(value),"\n",
                                             "Percent of Total:",percent(percent)))) +
        geom_bar(stat='identity',position = 'stack') +
        # coord_flip() +
        theme_minimal() +
        labs(y="USD Amount")+
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        theme(#legend.direction = "horizontal",
          legend.position = 'bottom',
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm")) +
        coord_flip() +
        scale_fill_discrete(breaks=c("Available Balance","Committed","Disbursed")) +
        scale_fill_brewer(palette = "Blues") +
        geom_text(size = 3, position = position_stack(vjust = 0.5)) 
      
      plotly::ggplotly(gg, tooltip = "text") %>%
        layout(legend = list(orientation = 'h',
                             font = list(size = 10),
                             x=.2, y = -4,
                             traceorder="reversed"))
    })
    
    #PMA grant number
    output$number_active_grants_pma <- renderValueBox({
      
      temp_grants <- grants %>%
        filter(PMA=="yes") %>% select(Fund) %>% 
        distinct() %>% nrow() %>% 
        valueBox(.,
                 subtitle = "Active PMA Grants",
                 icon = icon("list-ol"),
                 color = "yellow")
    })
    
    output$total_remaining_balance_pma <- renderValueBox({
      
      temp_grants <- grants %>% filter(Trustee %in% active_trustee$Fund,PMA=="yes")
      sum(temp_grants$unnacounted_amount) %>% 
        dollar()%>% 
        valueBox(
          "Available Balance", ., icon = icon("receipt"),
          color = "yellow", subtitle = "*PMA grants")
    })
    
    output$overview_progress_GG_pma <- renderPlotly({
      
      temp_df <- grants %>% filter(PMA=="yes")
      
      new_df <- data.frame(Disbursed=sum(temp_df$`Disbursements USD`),
                           Committed=sum(temp_df$`Commitments USD`),
                           "Available Balance"=sum(temp_df$unnacounted_amount)) %>% 
        reshape2::melt() %>%
        mutate(total=sum(temp_df$`Grant Amount USD`)) %>%
        mutate(percent=value/total)
      
      gg <- new_df %>% ggplot(aes(x=total,
                                  y=value,
                                  fill=variable,
                                  label=percent(percent),
                                  text=paste(variable,"\n",
                                             "USD Amount:",dollar(value),"\n",
                                             "Percent of Total:",percent(percent)))) +
        geom_bar(stat='identity',position = 'stack') +
        # coord_flip() +
        theme_minimal() +
        labs(y="USD Amount")+
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        theme(#legend.direction = "horizontal",
          legend.position = 'bottom',
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm")) +
        coord_flip() +
        scale_fill_discrete(breaks=c("Available Balance","Committed","Disbursed")) +
        scale_fill_brewer(palette = "Blues") +
        geom_text(size = 3, position = position_stack(vjust = 0.5)) 
      
      plotly::ggplotly(gg, tooltip = "text") %>%
        layout(legend = list(orientation = 'h',
                             font = list(size = 10),
                             x=.2, y = -4,
                             traceorder="reversed"))
    })
    
    
    output$n_grants_region <- renderPlotly({
      
      temp_df <- grants %>% filter(PMA=="no")
      temp_df <- temp_df %>% 
        group_by(Region) %>%
        summarise(n_grants = n(), total_award_amount = sum(`Grant Amount USD`)) 
      
     temp_df <- left_join(temp_df,dplyr::distinct(select(.data = grants,Region,region_color)))
      
     temp_df$Region <- factor(temp_df$Region,
                              levels = unique(temp_df$Region)[order(temp_df$n_grants, decreasing = TRUE)])
     
     plot_ly(data=temp_df,
              type="bar",
              marker=~list(color=region_color),
              x=~Region,
              y=~n_grants) %>% layout(yaxis = list(title = 'Number of Grants'))
      
    })
  
    
    output$funding_region <- renderPlotly({
      
      data <- grants %>%
        filter(PMA=="no") %>% 
        group_by(Region) %>% 
        summarise(n_grants = n(),
                  total_award_amount = sum(`Grant Amount USD`),
                  region_color=unique(region_color))
      
      total <- sum(data$total_award_amount)
      
      m <- list(
        l = 20,
        r = 5,
        b = 10,
        t = 10,
        pad = 2
      )
      
     data$Region <- factor(data$Region,
                               levels = unique(data$Region)[order(data$total_award_amount, decreasing = TRUE)])
      
     
     data$Percent <- data$total_award_amount/sum(data$total_award_amount)
      plot_ly(data=data,
              type="bar",
              marker=~list(color=region_color),
              x=~Region,
              y=~total_award_amount,
              hoverinfo='text',
              hovertext=~paste0(Region,"\n",dollar(total_award_amount),"\n",percent(Percent))) %>%
        layout(yaxis = list(title = 'Grant Amount'))
      
    })
    
    
    output$funding_GP <- renderPlotly({
      
      data <- grants %>% 
        group_by(`Lead GP/Global Themes`) %>% 
        summarise(n_grants = n(), total_award_amount = sum(`Grant Amount USD`))
      
      total <- sum(data$total_award_amount)
      data$percent.1 <- data$total_award_amount/total
      
      data$pie_name <- ifelse(data$percent.1 >=.01,data$`Lead GP/Global Themes`,"Other")
      
      data <- data %>% group_by(pie_name) %>%
        summarise(n_grants=sum(n_grants),
                  total_award_amount=sum(total_award_amount))
      m <- list(
        l = 5,
        r = 5,
        b = 10,
        t = 10,
        pad = 1
      )
      
      data$pie_name[data$pie_name=="Environment, Natural Resources & the Blue Economy"] <- "Environment, \n Natural Resources \n& the Blue Economy"
      
      data$pie_name[data$pie_name=="Finance, Competitiveness and Innovation"] <- "Finance, Competitiveness \n& Innovation"
      
      data$pie_name[data$pie_name=="Urban, Resilience and Land"] <- "Urban, Resilience & Land"
      
      
      
      plot_ly(data,
              labels = ~pie_name,
              values = ~total_award_amount,
              text = ~paste0(percent(total_award_amount/total,accuracy = 0.1)),
              hovertext= ~paste0(pie_name,"\n",
                                 dollar(total_award_amount),"\n",
                                 round(total_award_amount/total*100,digits=1)," % of total funding",
                                "\n","(",n_grants," grants)"),
              hoverinfo = 'text',
              textinfo = 'text',
              type = 'pie',
              rotation=110,
              domain = list(x = c(0.00, 0.97), y = c(0, 0.95)),
              textfont = list(color = '#000000', size = 9.5)) %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               margin=m,
               showlegend = T,
               legend = list(font = list(size = 9)))
      
      
    })
    
    
# TAB.1.3 -------------
    output$trustee_name_TTL <- renderTable({
      active_trustee  %>% 
        select(Fund,`Fund Name`,`Fund TTL Name`,`TF End Disb Date`,Trustee.name) %>% 
        mutate(`TF End Disb Date`= as.character(as_date(`TF End Disb Date`))) %>% 
        rename("Trustee Short Name"=Trustee.name)
    },striped = T)
    
    
    output$donor_contributions <- renderTable({
      
      
      
      active_trustee$`Donor Agency Name`[active_trustee$`Donor Name`=='Multi Donor'] <- "Multiple Donors"
      
      
      active_trustee %>% group_by(`Donor Name`,`Donor Agency Name`) %>%
        summarise("Total Signed Contributions"=sum(`Net Signed Condribution in USD`) %>% dollar(),
                  "Total Received Contributions"=sum(`Net Paid-In Condribution in USD`) %>% dollar(),
                  "Total Pending Contributions"=sum(`Net Unpaid contribution in USD`) %>% dollar())
    },striped = T)
    
    
    output$donor_contributions_GG <- renderPlotly({
      signed_df <- active_trustee %>% filter(`Net Signed Condribution in USD`>0) %>%
        group_by(`Donor Name`) %>%
        summarise("Total Signed Contributions"=sum(`Net Signed Condribution in USD`))
      
      active_trustee$`Donor Agency Name`[active_trustee$`Donor Name`=="Multi Donor"] <- "Multiple Donors"
    gg <-  active_trustee %>% filter(`Net Signed Condribution in USD`>0) %>%
        group_by(`Donor Name`,`Donor Agency Name`) %>%
        summarise(#"Total Signed Contributions"=sum(`Net Signed Condribution in USD`),
                  "Total Pending Contributions"=sum(`Net Unpaid contribution in USD`),
                  "Total Received Contributions"=sum(`Net Paid-In Condribution in USD`)) %>% 
        reshape2::melt() %>% full_join(.,signed_df,by='Donor Name') %>% 
    
      ggplot(aes(x=`Donor Name`,y=value,fill=variable,
                   text=paste(`Donor Agency Name`,"\n",
                              "Total Signed:", dollar(`Total Signed Contributions`),"\n",
                              variable,":",dollar(value)))) +
          geom_bar(stat='identity') +
        theme_classic() +
        scale_y_continuous(labels=dollar_format(prefix="$")) +
        scale_fill_discrete(name = "Contributions", labels = c("Pending", "Received")) +
        labs(y="USD Amount", title="Contributions by Donor Agency") +
      theme(rect = element_rect(fill="transparent"),
            plot.background = element_rect(fill="transparent",color=NA),
            panel.background = element_rect(fill="transparent")) 
      
      plotly::ggplotly(gg, tooltip='text')
    })
  
    
    output$RETF_n_grants_A <- renderValueBox({
      
      temp_df <- grants %>% filter(`DF Execution Type`=="RE")
      temp_df %>% nrow() %>%
        valueBox(value=.,
                 subtitle =HTML("<b>Active RETF grants</b> <button id=\"show_grants_RETF_A\" type=\"button\" class=\"btn btn-default action-button\">Show Grants</button>"),
                 color="blue")
      
    })
    
    
    observeEvent(input$show_grants_RETF_A, {
      data <- grants %>% filter(`DF Execution Type`=="RE")
      isolate(data <- data %>% filter(`Grant Amount USD` != 0) %>%
                select(Fund,
                       `Fund Name`,
                       `Grant Amount USD`,
                       temp.name,
                       `Fund TTL Name`,
                       `TTL Unit Name`,
                       tf_age_months,
                       months_to_end_disbursement) %>%
                arrange(-tf_age_months) %>%
                mutate(`Grant Amount USD` = dollar(`Grant Amount USD`)) %>%
                rename("Months Left to Disburse" = months_to_end_disbursement,
                       "Months Since Grant Activation"= tf_age_months,
                       "Trustee"= temp.name))
      
      showModal(modalDialog(size = 'l',
                            title = "BETF grants",
                            renderTable(data),
                            easyClose = TRUE))
      
    })
    
    output$`RETF_$_grants_A` <- renderValueBox({
      
      temp_df <- grants %>% filter(`DF Execution Type`=="RE")
      sum(temp_df$`Grant Amount USD`) %>% dollar() %>% 
        valueBox(value=.,
                 subtitle = "Active RETF funds amount",
                 color="blue")
      
    })
    
    
    output$RETF_trustees_A_pie <- renderPlotly({
      
      remove_num <- function(x){
        word <- x
        letter <- stri_sub(x,5)
        if(letter %in% c("1","2","3","4","5","6","7","8","9")){
          return(stri_sub(x,1,4))} else{
            return(stri_sub(word))
          }
      }
      
      temp_df <- grants %>% filter(`DF Execution Type`=="RE")
      
      #temp_df$aggregate_unit <- sapply(temp_df$`TTL Unit Name`, function(x) remove_num(x)) %>% as.vector()
      data <- temp_df %>% 
        group_by(temp.name) %>% 
        summarise(n_grants = n(), total_award_amount = sum(`Grant Amount USD`))
      
      total <- sum(data$total_award_amount)
      m <- list(
        l = 40,
        r = 20,
        b = 70,
        t = 50,
        pad = 4
      )
      plot_ly(data, labels = ~temp.name, values = ~total_award_amount, type = 'pie',
              text=~paste0(round(total_award_amount/total*100,digits=1)," %",
                           "\n","(",n_grants," grants)"),
              hoverinfo="label+value+text",
              textinfo='text') %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),margin=m,
               title = "RETF Funding per Trustee")
      
      
    })
    
    output$RETF_region_A_pie <- renderPlotly({
      
      temp_df <- grants %>% filter(`DF Execution Type`=="RE")
      
      m <- list(
        l = 100,
        r = 40,
        b = 70,
        t = 50,
        pad = 4
      )
      
      data <- temp_df %>% 
        group_by(Region) %>% 
        summarise(n_grants = n(), total_award_amount = sum(`Grant Amount USD`), r_color=unique(region_color))
      
      total <- sum(data$total_award_amount)
      
      plot_ly(data, labels = ~Region, values = ~total_award_amount, type = 'pie',
              text=~paste0(round(total_award_amount/total*100,digits=1)," %",
                           "\n","(",n_grants," grants)"),
              hoverinfo="label+value+text",
              textinfo='text',
              marker=~list(colors=r_color)) %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               title = "RETF Funding per Region",margin=m)
      
      
    })
    
# TAB.2 -------------------------------------------------------------------

    reactive_active_trustee <- reactive({
    
      active_trustee %>%
        filter(temp.name %in% input$select_trustee)
    })
    
    reactive_grants_trustee <- reactive({
      
      data <- grants %>%
        filter(temp.name %in% input$select_trustee)
      
      if(!is.null(input$trustee_select_region)){
      
      data <- data %>% 
        filter(Region %in% all_fun(input$trustee_select_region,Region))
        
        }
      
     if(input$trustee_remove_PMA==TRUE){
       data <- data %>% filter(PMA=='no')
     }
      else{
      data}
      
    })
    
    observeEvent(input$select_trustee, {
      #subset the data by trustee selected
      temp_grants <- grants %>% filter(temp.name %in% input$select_trustee)

      updateCheckboxGroupButtons(session,inputId =  "trustee_select_region",
                        label = 'Filter Regions:',
                        choices = sort(unique(temp_grants$Region)),
                        status = "primary",
                        size='sm',
                        checkIcon = list(yes = icon("ok", lib = "glyphicon")))
                                         #,
                                         #no = icon("remove", lib = "glyphicon")))


      })

    output$trustee_contribution_agency <- renderText({
      temp_active_trustee <- reactive_active_trustee()
      
      paste0(unique(temp_active_trustee$`Donor Agency Name`), collapse = "; ")
      })
      
      
      output$TTL_name <- renderText({
        temp_active_trustee <- reactive_active_trustee()

          paste0(unique(temp_active_trustee$`Fund TTL Name`),collapse = "; ")
      
      })
      
      output$trustee_name <- renderText({
        
        temp_active_trustee <- reactive_active_trustee()
  
        paste0(paste0(temp_active_trustee$temp.name," (",temp_active_trustee$Fund,")"),collapse ="; " )
      })
  
      output$fund_balance <- renderValueBox({
        
        temp_active_trustee <- reactive_active_trustee()
        
        s_title <- ifelse(nrow(temp_active_trustee)>1,
                          "Sum of Balance Across Selected Parent Funds",
                          "Parent Fund Balance")
        
        sum(temp_active_trustee$`Available Balance USD`,
            temp_active_trustee$`Net Unpaid contribution in USD`) %>%
          dollar %>% 
          valueBox(subtitle = s_title,value=tags$p(., style = "font-size: 85%;"),color = "blue",icon = icon("money-check-alt"))
      })
      
      
      output$trustee_received_unpaid_pie <- renderPlotly({
        
        temp_df <- reactive_active_trustee()
        temp_df <- temp_df %>% 
          summarise("Available" = round(sum(`Available Balance USD`)),
                    "Un-paid" = round(sum(`Net Unpaid contribution in USD`))) %>%
          reshape2::melt()
        
        temp_df$Percent <- temp_df$value/sum(temp_df$value)
        
        total <- sum(temp_df$value)
        
        m <- list(
          l = 15,
          r = 5,
          b = 10,
          t = 15,
          pad = 4
        )
        
        colors <- c('rgb(17,71,250)',#,'rgb(192,192,192)',
                    'rgb(192,207,255)')
        
        plot_ly() %>%
          add_pie(
            data = temp_df,
            title="Balance",
            labels=~variable,
            values = ~value,
            textinfo= 'text',
            text=  ~paste0(dollar(value),"\n",percent(Percent)),
            hoverinfo="label+text",
            marker = list(colors=colors),
            hole = 0.75,
            rotation=75) %>% 
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 margin=m,
                 legend = list(orientation = 'h',font = list(size = 9), x=0.25, y=-0.01))
      })
    

      output$trustee_grants_amounts <- renderValueBox({
        
        temp_grants <- reactive_grants_trustee()
        sum(temp_grants$`Grant Amount USD`) %>% dollar %>% 
          valueBox(subtitle = "Total Awarded Amount",value=.)
      })
      
      output$trustee_dis_GG <- renderPlotly({
    
          
          temp_df <- reactive_grants_trustee()
          temp_df <- temp_df %>% 
            summarise(Disbursed = sum(`Disbursements USD`),
                      Committed = sum(`Commitments USD`),
                      Uncommitted = sum(unnacounted_amount)) %>%
            reshape2::melt()
          
          temp_df$percentage <- temp_df$value/sum(temp_df$value)
          
          total <- sum(temp_df$value)
          
          m <- list(
            l = 15,
            r = 5,
            b = 5,
            t = 15,
            pad = 4
          )
          
          colors <- c('rgb(17,71,250)',
                      'rgb(192,207,255)',
                      'rgb(192,192,192)')
          
          plot_ly() %>%
            add_pie(
              data = temp_df,
              title="Active Funds",
              labels=~variable,
              values = ~value,
              textinfo= ~percent(percentage,accuracy = 0.1),
              hoverinfo='text',
              #text=  ~paste0(dollar(value),"\n",percent(percentage)),
              hovertext=~paste0(variable,"\n",dollar(value),"\n",percent(percentage)),
              marker = list(colors=colors),
              hole = 0.75,
              rotation=75) %>% 
            layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   margin=m,
                   legend = list(orientation = 'h',font = list(size = 9), x=0.1, y=-0.1))
        })
      
      output$trustee_active_grants <- renderValueBox({
        temp_grants <- reactive_grants_trustee()
        temp_grants %>% select(Fund)%>% dplyr::distinct() %>% nrow() %>% 
          valueBox(subtitle = show_grant_button("Grants in active portfolio",
                                                "show_active_grants_trustee"),
                   value=.,
                   color = "light-blue")
      })
      
      observeEvent(input$show_active_grants_trustee, {
        temp_grants <- reactive_grants_trustee()
        
        isolate(data <- temp_grants %>%
                  select(Trustee,
                         Fund,
                         `Fund Name`,`Fund TTL Name`,`Grant Amount USD`,
                         unnacounted_amount,months_to_end_disbursement)  %>% 
                  rename("Uncommitted Balance" = unnacounted_amount,
                         "Child Fund" = Fund,
                         "TTL Name"=`Fund TTL Name`,
                         "Months to Closing Date"= months_to_end_disbursement)
                )
        
        reactive_data$download_table <- data
        
        
        data <- data %>% 
          mutate(`Grant Amount USD` = dollar(`Grant Amount USD`),
                 `Uncommitted Balance` = dollar(`Uncommitted Balance`))
                  
        showModal(modalDialog(size = 'l',
                              title = "Active Portfolio",
                              renderTable(data),
                              easyClose = TRUE,
                              footer = downloadBttn('download_active_grants_trustee')))
        
      })
      
      
      output$download_active_grants_trustee <- downloadHandler(
        filename = "parent_view_active_grants.xlsx",
        content = function(file) {
          write.xlsx(reactive_data$download_table,file)
        }) 
      
      output$trustee_grants_closing_3 <- renderValueBox({
        
        temp_grants <- reactive_grants_trustee()
        temp_grants %>%
          filter(months_to_end_disbursement <= 3) %>% nrow() %>% 
          valueBox(subtitle = show_grant_button("Grants closing in less than 6 months",
                                                "show_grants_closing_3"),
                   value=.,
                   color = 'light-blue')
        
      })
      
      observeEvent(input$show_grants_closing_3, {
        temp_grants <- reactive_grants_trustee()
        
        isolate(data <- temp_grants %>% #filter(`Fund Status` == "ACTV", `Grant Amount USD` != 0) %>%
                  filter(months_to_end_disbursement <= 3) %>%
                  select(Fund,
                         `Fund Name`,
                         `Fund TTL Name`,
                         `Grant Amount USD`,
                         percent_unaccounted,
                         months_to_end_disbursement,
                         Region) %>%
                  arrange(-percent_unaccounted) %>% 
                  rename("Months Left to Disburse" = months_to_end_disbursement,
                         "Uncommitted bal. % of Total" = percent_unaccounted)
        )
      reactive_data$download_table <- data %>%
        mutate(`Uncommitted bal. % of Total` = round((`Uncommitted bal. % of Total`/100),2))
        
       data <- data  %>% 
          mutate(`Uncommitted bal. % of Total` = percent((`Uncommitted bal. % of Total`/100)),
                 `Grant Amount USD` = dollar(`Grant Amount USD`))
        
        showModal(modalDialog(size = 'l',
                              title = "Active Grants closing in less than 3 months",
                              renderTable(data),
                              easyClose = TRUE,
                              footer = downloadBttn('dload_trustee_closing_3')))
        
      })
      
      output$dload_trustee_closing_3 <-
        downloadHandler(
          filename = "Trustees closing less than 3 months.xlsx",
          content = function(file) {
            write.xlsx(reactive_data$download_table,file)
          }) 
      

      output$trustee_closing_in_months <- renderValueBox({
        temp_active_trustee <- reactive_active_trustee()
        
        
        display_text <- paste0(temp_active_trustee$months_to_end_disbursement, collapse = " | ")
          
          
          
          valueBox(subtitle = "Months to fund closing date:",
                   value=display_text,
                   icon = icon("stopwatch"),
                   color = "blue")
      })
      
      output$trustee_region_n_grants_GG <- renderPlotly({
        
       
        
        temp_df <- reactive_grants_trustee()
  
        temp_df <- temp_df %>%
          group_by(Region) %>%
          summarise(n_grants = n(), total_award_amount = sum(`Grant Amount USD`)) 
        
        temp_df <- left_join(temp_df,dplyr::distinct(select(.data = grants,Region,region_color)))
        
        
        temp_df$Region <- factor(temp_df$Region,
                                 levels = unique(temp_df$Region)[order(temp_df$n_grants, decreasing = TRUE)])
        
        plot_ly(data=temp_df,
                type="bar",
                marker=~list(color=region_color),
                x=~Region,
                y=~n_grants,
                hovertext=~paste0(Region,"\n",n_grants," active grants"),
                hoverinfo="text") %>%
          layout(yaxis = list(title = 'Number of Grants'))
        
        
        
      })
      
      output$trustee_region_GG <- renderPlotly({
        
        data <- reactive_grants_trustee()
        
        data <- data %>%
          group_by(Region) %>% 
          summarise(n_grants = n(),
                    total_award_amount = sum(`Grant Amount USD`),
                    region_color=unique(region_color))
        
        total <- sum(data$total_award_amount)
        
        m <- list(
          l = 20,
          r = 5,
          b = 10,
          t = 10,
          pad = 2
        )
        
        data$Region <- factor(data$Region,
                              levels = unique(data$Region)[order(data$total_award_amount, decreasing = TRUE)])
        
        
        data$percentage <- data$total_award_amount/sum(data$total_award_amount)
        plot_ly(data=data,
                type="bar",
                marker=~list(color=region_color),
                x=~Region,
                y=~total_award_amount,
                hoverinfo='text',
                hovertext=~paste0(Region,"\n",dollar(total_award_amount),"\n",percent(percentage))) %>%
          layout(yaxis = list(title = 'Grant Amount'))
        
      })
      
      output$trustee_countries_DT <- renderDataTable({
        
        temp_df <- reactive_grants_trustee()
        temp_df <- temp_df %>%
          group_by(Country) %>%
          summarise("Grant Count" = n(),
                    "Total Grant Amount" = dollar(sum(`Grant Amount USD`)),
                    "Uncommitted Amount" = dollar(sum(unnacounted_amount)),
                    "Percent Uncommitted" = percent((sum(unnacounted_amount))/sum(`Grant Amount USD`),accuracy=1)
                    )
        
        
    
        
        DT::datatable(temp_df,options = list(
          "pageLength" = 10))
       
      })
    
# TAB.3 REGIONS VIEW -------------------------------------------------------------------
    
    reactive_df <- reactive({
      
      data <- grants 
      
      if(!is.null(input$focal_select_region)){
        data <- data %>% filter(Region %in% input$focal_select_region)
      }
      
      if(!is.null(input$focal_select_trustee)){
        data <- data %>% filter(temp.name %in% input$focal_select_trustee)
      }
     
      if(!is.null(input$region_BE_RE)){
        data <- data %>% filter(`DF Execution Type` %in% input$region_BE_RE)
      }
      
      if(!is.null(input$region_PMA_or_not)){
        data <- data %>% filter(PMA.2 %in% input$region_PMA_or_not)
      }
      if(!is.null(input$closing_fy)){
        data <- data %>% filter(closing_FY %in% input$closing_fy)
      }
      
      data
    })
      
    reactive_df_2 <- reactive({
        
      data <- grants 
      
      if(!is.null(input$focal_select_region)){
        data <- data %>% filter(Region %in% input$focal_select_region)
      }
      
      if(!is.null(input$focal_select_trustee)){
        data <- data %>% filter(temp.name %in% input$focal_select_trustee)
      }
          
      data %>% filter(`DF Execution Type` =="RE")
      })
      
    reactive_summary <- reactive({
      
      data <- grants 
      
      if(!is.null(input$focal_select_region)){
        data <- data %>% filter(Region %in% input$focal_select_region)
      }
      
      if(!is.null(input$focal_select_trustee)){
        data <- data %>% filter(temp.name %in% input$focal_select_trustee)
      }
      
      if(!is.null(input$region_BE_RE)){
        data <- data %>% filter(`DF Execution Type` %in% input$region_BE_RE)
      }
      
      if(!is.null(input$region_PMA_or_not)){
        data <- data %>% filter(PMA.2 %in% input$region_PMA_or_not)
      }
      
      data

      })
      
    reactive_country_regions <- reactive({
        
      
      data <- grants 
      
      if(!is.null(input$focal_select_region)){
        data <- data %>% filter(Region %in% input$focal_select_region)
      }
      
      if(!is.null(input$focal_select_trustee)){
        data <- data %>% filter(temp.name %in% input$focal_select_trustee)
      }
      
      if(!is.null(input$region_BE_RE)){
        data <- data %>% filter(`DF Execution Type` %in% input$region_BE_RE)
      }
      
      data
      })
    
    reactive_data <- reactiveValues(focal_grants = grants,
                                    percent_df = NA,
                                    region_grants = grants,
                                    download_table=NA)
  
    observeEvent(input$focal_select_region,{
     df <- grants %>%
       filter(Region %in% input$focal_select_region) 
    
        updateCheckboxGroupButtons(
          session,
          "focal_select_trustee",
          choices = sort(unique(df$temp.name)),
          status = "primary",
          size='sm',
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                       )

   },ignoreNULL = T)
    
    #select all trustees if no region has been selected
    observe({
      if(is.null(input$focal_select_region)){
  
        updateCheckboxGroupButtons(
        session,
        "focal_select_trustee",
        choices = sort(unique(grants$temp.name)),
        status = "primary",
        size='sm',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    }
      
    })
    
    
# 
#     observeEvent(input$focal_select_trustee,{
# 
#       if (!is.null(input$focal_select_trustee)){
#       temp.df <- reactive_df()
#       data$focal_grants <- reactive_df()
# 
#       data$percent_df <- temp.df %>%
#         select(`Disbursements USD`,`Grant Amount USD`) %>%
#         summarise(total_dis = sum(`Disbursements USD`),
#                   total_awarded= sum(`Grant Amount USD`))
# 
#       updateProgressBar(session = session,
#                         id = 'focal_percent_active_disbursed',
#                         value = data$percent_df$total_dis[[1]],
#                         total = data$percent_df$total_awarded[[1]],
#                         status = 'info') }
# 
#       attention_needed <- data$focal_grants  %>%
#         filter(percent_unaccounted >=35,
#                months_to_end_disbursement<=6) %>%
#         nrow()
# 
#       if(attention_needed>0){
# 
#         message_list <- paste(attention_needed,
#                            "Grant(s) are closing in less than 6 months and still have more than 35% available balance")
# 
#         output$notifications_Menu <- renderMenu({
#           dropdownMenu(type ="messages",
#                        messageItem(message = message_list,
#                                    from = "System"))
#         })
#       } else {output$notifications_Menu <- NULL }
#       
#     })
    
    output$date_data_updated_message <- renderMenu({
      dropdownMenu(type="notifications",
                   notificationItem(text = paste("Data updated as of:", report_data_date),
                                    icon=icon("calendar")),
                   icon = icon("calendar"))
    })

    #subset the data by trustee selected
      
    #Display name of selected Region
      output$focal_region_name <- renderText({
        
        data <- reactive_df()
        data$`Fund Country Region Name`[1]})
  
      output$focal_active_grants <- renderValueBox({
        
        data <- reactive_df()
        data %>% nrow() %>%
        valueBox(subtitle = "Active Grants",
                value = tags$p(., style = "font-size: 85%;"),
                color = 'navy',
                icon=icon("list-ol"))

      })
      
      
      output$elpie2 <- renderPlotly({
        
        temp_df <- reactive_df()
        temp_df <- temp_df %>% 
          summarise("Disbursed" = sum(`Disbursements USD`),
                    "Uncommitted" = round(sum(unnacounted_amount)),
                    "Committed"= sum(`Commitments USD`)) %>% reshape2::melt()
        
        temp_df$percentage <- temp_df$value/sum(temp_df$value)
        
        total <- sum(temp_df$value)
        
        m <- list(
          l = 0,
          r = 0,
          b = 10,
          t = 10,
          pad = 4
        )
        
        colors <- c('rgb(17,71,250)','rgb(192,192,192)','rgb(192,207,255)')
        
        plot_ly(textposition="outside") %>%
          add_pie(#title="Disbursement Summary",
                  data = temp_df,
                  labels=~variable,
                  values = ~value,
                  textinfo= ~percentage,
                  hoverinfo= ~paste0(variable,"\n",dollar(value),percent(percentage)),
                  marker = list(colors=colors),
                  hole = 0.75) %>% 
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 margin=m,
                 legend = list(orientation = 'h',font = list(size = 9), x=0.25, y=-0.01))
      })
      

      output$focal_active_funds <- renderValueBox({
        temp_df <- reactive_df()
        
       temp_df %>%
          select(`Grant Amount USD`)
       valor <- sum(temp_df$`Grant Amount USD`) %>%
          as.numeric() %>%
          dollar()
         valueBox(subtitle = "Total Funding",
                   value =tags$p(valor, style = "font-size: 65%;"),
                   color = 'navy')
      })
      


      output$focal_region_n_grants_GG <- renderPlotly({
        
        temp_df <- reactive_df()
        
        temp_df$temp.name[temp_df$temp.name=="Japan Program Phase I SDTF"] <-"Japan Phase I SDTF"
        
        temp_df$temp.name[temp_df$temp.name=="Japan Program Phase II SDTF"] <- "Japan Phase II SDTF"
        
        gg <- temp_df %>% 
          group_by(temp.name) %>%
          summarise(n_grants = n(), total_award_amount = sum(`Grant Amount USD`)) %>%
          ggplot(aes(x=reorder(temp.name,n_grants),y=n_grants,
                     text=paste(temp.name,
                                "\n",
                                "Number of Grants:",
                                n_grants,
                                "\n",
                                "Total Awards Amount:",
                                dollar(total_award_amount)))) +
          geom_col(fill='royalblue') +
          theme_classic() +
         # coord_flip() +
          labs(x="Trustee Name", y="Number of Grants") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,size=10))
        
        m <- list(
          l = 5,
          r = 5,
          b = 0,
          t = 0
        )
        

       plotly::ggplotly(gg, tooltip = "text") %>% layout(margin=m)

      })
      
      output$region_GP_GG <- renderPlotly({
        
        temp_df <- reactive_df() 
        
        
        
        data <- temp_df %>% 
          group_by(`Lead GP/Global Themes`) %>% 
          summarise(n_grants = n(), total_award_amount = sum(`Grant Amount USD`))
        
        total <- sum(data$total_award_amount)
        data$percent.1 <- data$total_award_amount/total
        
        data$pie_name <- ifelse(data$percent.1 >=.01,data$`Lead GP/Global Themes`,"Other")
        
        data <- data %>% group_by(pie_name) %>%
          summarise(n_grants=sum(n_grants),
                    total_award_amount=sum(total_award_amount))
        m <- list(
          l = 5,
          r = 5,
          b = 15,
          t = 15,
          pad = 2
        )
        
        data$pie_name[data$pie_name=="Environment, Natural Resources & the Blue Economy"] <- "Environment, Natural Resources \n& the Blue Economy"
        
        data$pie_name[data$pie_name=="Finance, Competitiveness and Innovation"] <- "Finance, Competitiveness \n& Innovation"
        
        
        
        
        
        plot_ly(data,
                labels = ~pie_name,
                values = ~total_award_amount,
                text = ~paste0(percent(total_award_amount/total,accuracy = 0.1)),
                hovertext= ~paste0(pie_name,"\n",
                                   dollar(total_award_amount),"\n",
                                   round(total_award_amount/total*100,digits=1)," % of total funding",
                                   "\n","(",n_grants," grants)"),
                hoverinfo = 'text',
                textinfo = 'text',
                type = 'pie',
                rotation=75,
                domain = list(x = c(0.105, 0.98), y = c(0, 0.90))) %>%
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 margin=m,
                 showlegend = T,
                 legend = list(font = list(size = 8)))
        
        
        
         # 
         # data <- temp_df %>% 
         #  group_by(`Lead GP/Global Themes`) %>% 
         #  summarise(n_grants = n(), total_award_amount = sum(`Grant Amount USD`))
         # 
         
         # total <- sum(data$total_award_amount)
         # data$percent.1 <- data$total_award_amount/total
         # 
         # data$pie_name <- ifelse(data$percent.1 >=.01,data$`Lead GP/Global Themes`,"Other")
         # 
         # data <- data %>% group_by(pie_name) %>%
         #   summarise(n_grants=sum(n_grants),
         #             total_award_amount=sum(total_award_amount))
         # 
         # 
         # m <- list(
         #   l = 10,
         #   r = 10,
         #   b = 10,
         #   t = 10,
         #   pad = 4
         # )
         # 
         # plot_ly(data,
         #         labels = ~pie_name,
         #         values = ~total_award_amount,
         #         text = ~paste0(percent(total_award_amount/total,accuracy = .1),
         #                        "\n","(",n_grants," grants)",
         #                        "\n", dollar(total_award_amount)),
         #         hoverinfo = 'label+text',
         #         textinfo = 'percent',
         #         type = 'pie',
         #         showlegend = FALSE,
         #         rotation=50) %>%
         #   layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         #          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         #          margin=m)
        
      })
      
      output$region_countries_grants_table <- DT::renderDataTable({
        
        temp_df <- reactive_df()
        temp_df_all <- temp_df %>%
          group_by(Country) %>%
          summarise("# Grants" = n(),
                    "$ Amount" = dollar(sum(`Grant Amount USD`)),
                    "Balance" = dollar(sum(unnacounted_amount)))
        
        temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
          group_by(Country) %>%
          summarise("# Grants" = n(),
                    "$ Amount" = dollar(sum(`Grant Amount USD`)),
                    "Balance" = dollar(sum(unnacounted_amount)))
        
        temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
          group_by(Country) %>%
          summarise("# Grants" = n(),
                    "$ Amount" = dollar(sum(`Grant Amount USD`)),
                    "Balance" = dollar(sum(unnacounted_amount)))
        
        
        display_df_partial <- full_join(temp_df_GPURL,
                                        temp_df_non_GPURL,
                                        by="Country",
                                        suffix=c(" (GPURL)"," (Non-GPURL)"))
        
        
        display_df <- left_join(temp_df_all,display_df_partial,by="Country")
        
        
        sketch <- htmltools::withTags(table(
          class = 'display',
          thead(
            tr(th(colspan = 1, ''),
               th(colspan = 1, ''),
               th(colspan = 2, 'All'),
               th(colspan = 1, ''),
               th(colspan = 2, 'GPURL'),
               th(colspan = 1, ''),
               th(colspan = 2, 'Non-GPURL')
            ),
            tr(lapply(c("Trustee",rep(c("# Grants","$ Amount","Balance"),3)), th)
            )
          )
        ))
        
        DT::datatable( data = display_df,
                       options = list( 
                         dom = "lfrtip",
                         paging=TRUE
                       ),
                       container=sketch,
                       rownames=FALSE # end of options
                       
        )
        }) # 
        
      output$region_funding_source_grants_table <- DT::renderDataTable({
        
        temp_df <- reactive_country_regions()
        temp_df_all <- temp_df %>%
          group_by(temp.name) %>%
          summarise("# Grants" = n(),
                    "$ Amount" = dollar(sum(`Grant Amount USD`)),
                    "Balance" = dollar(sum(unnacounted_amount)))
        
        temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
          group_by(temp.name) %>%
          summarise("# Grants" = n(),
                    "$ Amount" = dollar(sum(`Grant Amount USD`)),
                    "Balance" = dollar(sum(unnacounted_amount)))
        
        temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
          group_by(temp.name) %>%
          summarise("# Grants" = n(),
                    "$ Amount" = dollar(sum(`Grant Amount USD`)),
                    "Balance" = dollar(sum(unnacounted_amount)))
        
        
        display_df_partial <- full_join(temp_df_GPURL,
                                        temp_df_non_GPURL,
                                        by="temp.name",
                                        suffix=c(" (GPURL)"," (Non-GPURL)"))
        
        
        display_df_funding <- left_join(temp_df_all,display_df_partial,by="temp.name")
        
        sketch <- htmltools::withTags(table(
          class = 'display',
          thead(
            tr(th(colspan = 1, ''),
               th(colspan = 1, ''),
              th( colspan = 2, 'All'),
              th(colspan = 1, ''),
              th(colspan = 2, 'GPURL'),
              th(colspan = 1, ''),
               th(colspan = 2, 'Non-GPURL')
            ),
            tr(lapply(c("Trustee",rep(c("# Grants","$ Amount","Balance"),3)), th)
            )
          )
        ))
        
        
        DT::datatable( data = display_df_funding,
                       options = list( 
                         dom = "lfrtip",
                         paging=TRUE
                         ),
                       container=sketch,
                       rownames=FALSE # end of options
                       
        )
      })
      
      output$region_summary_grants_table <- DT::renderDataTable({
        
        temp_df <- reactive_summary()
        
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
        
        
        #cutoff_date <- as.character(input$summary_table_cutoff_date)
        
        sum_display_df <- data.frame("Summary"= c("Grant Count",
                                                  "Total $ (Million)",
                                                  paste0("Total Uncommitted Balance ($)"),
                                                  paste0("% Uncommitted Balance ($)")),
                                     "GPURL"=unname(unlist(as.list(sum_df_GPURL))),
                                     "Non-GPURL"=unname(unlist(as.list(sum_df_non_GPURL))),
                                     "Combined Total"=unname(unlist(as.list(sum_df_all))))
        
        
        

        DT::datatable( data = sum_display_df, autoHideNavigation = T,
                       options = list( 
                         dom = "rt",
                         paging=TRUE,
                         buttons = 
                           list("copy",
                                list(
                                  extend = "collection",
                                  buttons = c("csv", "excel", "pdf"),
                                  text = "Download Displayed Table"))
                         
                         # end of buttons customization
                          # end of lengthMenu customization
                         , pageLength = 5
                         
                         
                       )# end of options
                       
        )
      })
      
      #-----------generate_full_excel_report_1----------      
      output$generate_full_excel_report_1 <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          openxlsx::saveWorkbook({
            temp_df <- reactive_summary()
            
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
            
            
            cutoff_date <- as.character(input$summary_table_cutoff_date)
            
            sum_display_df <- data.frame("Summary"= c("Grant Count",
                                                      "Total $ (Million)",
                                                      paste0("Total Uncommitted Balance ($) to Implement by ",cutoff_date),
                                                      paste0("% Uncommitted Balance ($) to Implement by ",cutoff_date)),
                                         "GPURL"=unname(unlist(as.list(sum_df_GPURL))),
                                         "Non-GPURL"=unname(unlist(as.list(sum_df_non_GPURL))),
                                         "Combined Total"=unname(unlist(as.list(sum_df_all))))
            
            
            #---------COUNTRIES DF ----------------------
            temp_df <- reactive_df()
            temp_df_all <- temp_df %>%
              group_by(Country) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
              group_by(Country) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
              group_by(Country) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            
            display_df_partial <- full_join(temp_df_GPURL,
                                            temp_df_non_GPURL,
                                            by="Country",
                                            suffix=c(" (GPURL)"," (Non-GPURL)"))
            
            
            display_df <- left_join(temp_df_all,display_df_partial,by="Country")
            
            
            #---------FUNDING SOURCE DF ----------------------
            
            temp_df <- reactive_country_regions()
            temp_df_all <- temp_df %>%
              group_by(temp.name) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
              group_by(temp.name) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
              group_by(temp.name) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            
            display_df_partial <- full_join(temp_df_GPURL,
                                            temp_df_non_GPURL,
                                            by="temp.name",
                                            suffix=c(" (GPURL)"," (Non-GPURL)"))
            
            
            display_df_funding <- left_join(temp_df_all,display_df_partial,by="temp.name")
            
            #------ CREATE EXCEL WORKBOOK AND ADD DATAFRAMES -------
            require(openxlsx)
            wb <- createWorkbook()
            
            #temp_df <- reactive_df()
            report_title <- paste("Test Version -- Summary Report for","unique(temp_df$Region)","Region(s)")
            
            addWorksheet(wb, "Portfolio Summary")
            # mergeCells(wb,1,c(2,3,4),1)
            
            writeData(wb, 1,
                      report_title,
                      startRow = 1,
                      startCol = 2)
            
            writeDataTable(wb, 1,  sum_display_df, startRow = 3, startCol = 2, withFilter = F)
            writeDataTable(wb, 1,  display_df, startRow = 5+(nrow(sum_display_df)+2), startCol = 2,withFilter = F)
            writeDataTable(wb, 1,  display_df_funding, startRow = 15 + nrow(display_df), startCol = 2,withFilter = F)
            
            
            setColWidths(wb, 1, cols = 1:ncol(display_df)+1, widths = "auto")
            # header_style <- createStyle(borderColour = getOption("openxlsx.borderColour", "black"),
            #                             borderStyle = getOption("openxlsx.borderStyle", "thick"),
            #                             halign = 'center',
            #                             valign = 'center',
            #                             textDecoration = NULL,
            #                             wrapText = TRUE)
            # 
            # addStyle(wb,1,rows=3,cols=2:length(sum_display_df)+1,style = header_style)
            # 
            ## opens a temp version
            #openXL(wb)
            wb
            
          },file,overwrite = TRUE)
        })
      
      
      #-----------generate_full_excel_report_2----------      
      output$generate_full_excel_report_2 <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          openxlsx::saveWorkbook({
            temp_df <- reactive_summary()
            
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
            
            
            cutoff_date <- as.character(input$summary_table_cutoff_date)
            
            sum_display_df <- data.frame("Summary"= c("Grant Count",
                                                      "Total $ (Million)",
                                                      paste0("Total Uncommitted Balance ($) to Implement by ",cutoff_date),
                                                      paste0("% Uncommitted Balance ($) to Implement by ",cutoff_date)),
                                         "GPURL"=unname(unlist(as.list(sum_df_GPURL))),
                                         "Non-GPURL"=unname(unlist(as.list(sum_df_non_GPURL))),
                                         "Combined Total"=unname(unlist(as.list(sum_df_all))))
            
            
            #---------COUNTRIES DF ----------------------
            temp_df <- reactive_df()
            temp_df_all <- temp_df %>%
              group_by(Country) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
              group_by(Country) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
              group_by(Country) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            
            display_df_partial <- full_join(temp_df_GPURL,
                                            temp_df_non_GPURL,
                                            by="Country",
                                            suffix=c(" (GPURL)"," (Non-GPURL)"))
            
            
            display_df <- left_join(temp_df_all,display_df_partial,by="Country")
            
            
            #---------FUNDING SOURCE DF ----------------------
            
            temp_df <- reactive_country_regions()
            temp_df_all <- temp_df %>%
              group_by(temp.name) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
              group_by(temp.name) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
              group_by(temp.name) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            
            display_df_partial <- full_join(temp_df_GPURL,
                                            temp_df_non_GPURL,
                                            by="temp.name",
                                            suffix=c(" (GPURL)"," (Non-GPURL)"))
            
            
            display_df_funding <- left_join(temp_df_all,display_df_partial,by="temp.name")
            
            #------ CREATE EXCEL WORKBOOK AND ADD DATAFRAMES -------
            require(openxlsx)
            wb <- createWorkbook()
            
            #temp_df <- reactive_df()
            report_title <- paste("Test Version -- Summary Report for","unique(temp_df$Region)","Region(s)")
            
            addWorksheet(wb, "Portfolio Summary")
            # mergeCells(wb,1,c(2,3,4),1)
            
            writeData(wb, 1,
                      report_title,
                      startRow = 1,
                      startCol = 2)
            
            writeDataTable(wb, 1,  sum_display_df, startRow = 3, startCol = 2, withFilter = F)
            writeDataTable(wb, 1,  display_df, startRow = 5+(nrow(sum_display_df)+2), startCol = 2,withFilter = F)
            writeDataTable(wb, 1,  display_df_funding, startRow = 15 + nrow(display_df), startCol = 2,withFilter = F)
            
            
            setColWidths(wb, 1, cols = 1:ncol(display_df)+1, widths = "auto")
            # header_style <- createStyle(borderColour = getOption("openxlsx.borderColour", "black"),
            #                             borderStyle = getOption("openxlsx.borderStyle", "thick"),
            #                             halign = 'center',
            #                             valign = 'center',
            #                             textDecoration = NULL,
            #                             wrapText = TRUE)
            # 
            # addStyle(wb,1,rows=3,cols=2:length(sum_display_df)+1,style = header_style)
            # 
            ## opens a temp version
            #openXL(wb)
            wb
            
          },file,overwrite = TRUE)
        })
      
      
      #-----------generate_full_excel_report_3----------      
      output$generate_full_excel_report_3 <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            openxlsx::saveWorkbook({
            temp_df <- reactive_summary()
            
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
            
            
            cutoff_date <- as.character(input$summary_table_cutoff_date)
            
            sum_display_df <- data.frame("Summary"= c("Grant Count",
                                                      "Total $ (Million)",
                                                      paste0("Total Uncommitted Balance ($) to Implement by ",cutoff_date),
                                                      paste0("% Uncommitted Balance ($) to Implement by ",cutoff_date)),
                                         "GPURL"=unname(unlist(as.list(sum_df_GPURL))),
                                         "Non-GPURL"=unname(unlist(as.list(sum_df_non_GPURL))),
                                         "Combined Total"=unname(unlist(as.list(sum_df_all))))
            
            
            #---------COUNTRIES DF ----------------------
            temp_df <- reactive_df()
            temp_df_all <- temp_df %>%
              group_by(Country) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
              group_by(Country) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
              group_by(Country) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            
            display_df_partial <- full_join(temp_df_GPURL,
                                            temp_df_non_GPURL,
                                            by="Country",
                                            suffix=c(" (GPURL)"," (Non-GPURL)"))
            
            
            display_df <- left_join(temp_df_all,display_df_partial,by="Country")
            
            
            #---------FUNDING SOURCE DF ----------------------
            
            temp_df <- reactive_country_regions()
            temp_df_all <- temp_df %>%
              group_by(temp.name) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_GPURL <-  temp_df %>% filter(GPURL_binary=="GPURL") %>% 
              group_by(temp.name) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            temp_df_non_GPURL <- temp_df %>% filter(GPURL_binary=="Non-GPURL") %>% 
              group_by(temp.name) %>%
              summarise("# Grants" = n(),
                        "$ Amount" = dollar(sum(`Grant Amount USD`)),
                        "Balance" = dollar(sum(unnacounted_amount)))
            
            
            display_df_partial <- full_join(temp_df_GPURL,
                                            temp_df_non_GPURL,
                                            by="temp.name",
                                            suffix=c(" (GPURL)"," (Non-GPURL)"))
            
            
            display_df_funding <- left_join(temp_df_all,display_df_partial,by="temp.name")
            
            #------ CREATE EXCEL WORKBOOK AND ADD DATAFRAMES -------
            require(openxlsx)
            wb <- createWorkbook()
            
            #temp_df <- reactive_df()
            report_title <- paste("Test Version -- Summary Report for","unique(temp_df$Region)","Region(s)")
            
            addWorksheet(wb, "Portfolio Summary")
            # mergeCells(wb,1,c(2,3,4),1)
            
            writeData(wb, 1,
                      report_title,
                      startRow = 1,
                      startCol = 2)
            
            writeDataTable(wb, 1,  sum_display_df, startRow = 3, startCol = 2, withFilter = F)
            writeDataTable(wb, 1,  display_df, startRow = 5+(nrow(sum_display_df)+2), startCol = 2,withFilter = F)
            writeDataTable(wb, 1,  display_df_funding, startRow = 15 + nrow(display_df), startCol = 2,withFilter = F)
            
            
            setColWidths(wb, 1, cols = 1:ncol(display_df)+1, widths = "auto")
            # header_style <- createStyle(borderColour = getOption("openxlsx.borderColour", "black"),
            #                             borderStyle = getOption("openxlsx.borderStyle", "thick"),
            #                             halign = 'center',
            #                             valign = 'center',
            #                             textDecoration = NULL,
            #                             wrapText = TRUE)
            # 
            # addStyle(wb,1,rows=3,cols=2:length(sum_display_df)+1,style = header_style)
            # 
            ## opens a temp version
            #openXL(wb)
            wb
            
          },file,overwrite = TRUE)
        })
      
      

      output$focal_grants_active_3_zero_dis <- renderValueBox({
        
        temp_df <- reactive_df()
        valor <- temp_df %>% filter(tf_age_months >= 3,
                           percent_unaccounted==100) %>%
          nrow()
          valueBox(
            value = tags$p(valor, style = "font-size: 85%;"),
            color='light-blue',
            subtitle = show_grant_button("3+ mos active, no commitments ",
                                         "show_region_grants_no_discom")
                   )

      })
      

      
 
      
      output$focal_grants_closing_3 <- renderValueBox({
        
        temp_df <- reactive_df()
        valor <- temp_df %>% filter(months_to_end_disbursement >= 3) %>%
          nrow()
          valueBox(value = tags$p(valor, style = "font-size: 85%;"),
                   subtitle = show_grant_button("Closing in less than 3 months ",
                                                "show_region_grants_closing_3"),
                   color = "light-blue")

      })
      
      output$region_grants_may_need_transfer <- renderValueBox({
        
        temp_df <- reactive_df()
        valor <- temp_df %>% filter(funds_to_be_transferred > 1,percent_transferred_available<.3) %>% nrow()
          valueBox(value =tags$p(valor, style = "font-size: 85%;"),
                   subtitle = show_grant_button(title = "May require funds transferred",
                                                id_name = "show_grants_need_transfer"),
                   color = 'light-blue')

      })
      output$region_grants_active_no_transfer <- renderValueBox({
        
        temp_df <- reactive_df()
        valor <- temp_df %>%
          filter(`Transfer-in USD`==0,
                 `Fund Status`=="ACTV") %>%
          nrow() 
          valueBox(value = tags$p(valor, style = "font-size: 85%;"),
                   subtitle = show_grant_button("Active, no initial transfer ",
                                                "show_grants_no_first_transfer"),
                   color = 'light-blue')
        
      })
      output$disbursement_risk_GG <- renderPlotly({
        data <- reactive_df()

        #plot_title <- paste("Disbursement Risk Overview for",list(input$focal_select_region),"Region(s)")
        
        #plot_title <- stri_replace(plot_title,fixed="c","")

        data$plot_risk_name <- factor(
          data$disbursement_risk_level,levels = c("Low Risk",
                                                            "Medium Risk",
                                                            "High Risk",
      
                                                                                                                "Very High Risk"))
       gg <-  data %>%
         group_by(disbursement_risk_level) %>%
         mutate(n_grants=n()) %>% 
          filter(!is.na(disbursement_risk_level)) %>%
          ggplot(aes(x=plot_risk_name,
                     fill=disbursement_risk_level,
                     text = paste0(disbursement_risk_level,"\n",n_grants))) +
          geom_bar(stat='count') +
          scale_fill_manual("legend", values = c("Very High Risk" = "#C70039",
                                                 "High Risk" = "#FF5733",
                                                 "Medium Risk" = "#FFC300",
                                                 "Low Risk"= "#2ECC71")) +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(x="Risk Level",
               y="Number of Grants")
       
       ggplotly(gg,tooltip='text')

      })

     output$very_high_risk <- renderValueBox({

       temp_df <- reactive_df()
       temp_df %>% filter(disbursement_risk_level=='Very High Risk') %>% nrow() %>%
         valueBox(value=.,
                  subtitle = show_grant_button("Very High Risk ",
                                               "show_grants_VHR"),
                  color="blue")

     })

     output$high_risk <- renderValueBox({

       temp_df <- reactive_df()
       temp_df %>% filter(disbursement_risk_level=='High Risk') %>% nrow() %>%
         valueBox(value=.,
                  subtitle = show_grant_button("High Risk ",
                                               "show_grants_HR"),
                  color="blue")

     })

     output$medium_risk <- renderValueBox({
       temp_df <- reactive_df()
       temp_df %>% filter(disbursement_risk_level=='Medium Risk') %>% nrow() %>%
         valueBox(value=.,
                  subtitle =show_grant_button("Medium Risk ",
                                              "show_grants_MR"),
                  color="blue")

     })

     output$low_risk <- renderValueBox({
       temp_df <- reactive_df()
       temp_df %>% filter(disbursement_risk_level=='Low Risk') %>% nrow() %>%
         valueBox(value=.,
                  subtitle = show_grant_button("Low Risk ",
                                               "show_grants_LR"),
                  color="blue")

     })
     
     

    output$generate_risk_report <-  downloadHandler(
                   filename = function() {
                     paste("Disbursement Risk_",date_data_udpated, ".xlsx", sep="")
                   },
                   content = function(file) {
                     openxlsx::saveWorkbook({

      require(openxlsx)
      data <- reactive_df()
      region <- paste(unlist(input$focal_select_region), sep="", collapse="; ")
      funding_sources <- data$temp.name %>%
        unique() %>%
        paste(sep="",collapse="; ")
      wb <- createWorkbook()
      df <- data %>% filter(`Fund Status`=="ACTV",
                            `Grant Amount USD`>0,
                            PMA=="no")
      df <- df %>%  select(Trustee,
                           temp.name,
                           Fund,
                           `Fund Name`,
                           `Project ID`,
                           `DF Execution Type`,
                           `Fund TTL Name`,
                           Region,
                           Country,
                           `Grant Amount USD`,
                           `Disbursements USD`,
                           `Commitments USD`,
                           months_to_end_disbursement,
                           unnacounted_amount,
                           percent_unaccounted,
                           monthly_disbursement_rate,
                           required_disbursement_rate,
                           disbursement_risk_level) %>% 
        mutate(monthly_disbursement_rate = percent(monthly_disbursement_rate),
              `Grant Amount USD`=dollar(`Grant Amount USD`),
              `Disbursements USD`=dollar(`Disbursements USD`),
              `Commitments USD`=dollar(`Commitments USD`),
              unnacounted_amount=dollar(unnacounted_amount),
              required_disbursement_rate = percent(required_disbursement_rate),
              percent_unaccounted=percent(percent_unaccounted/100)) %>% 
        rename("Trustee Name"= temp.name,
               "Uncommitted Balance" = unnacounted_amount,
               "Percent Uncommitted" = percent_unaccounted,
               "Avg. Monthly Disbursment Rate" = monthly_disbursement_rate,
               "Requirre Monthly Disbursment Rate" = required_disbursement_rate,
               "Disbursement Risk Level"=disbursement_risk_level,
               "Months to Closing Date"= months_to_end_disbursement)

      report_title <- paste("Disbursement Risk for",region,"Region(s)")
      funding_sources <- paste("Funding Sources:",funding_sources)

      addWorksheet(wb, "Disbursement Risk")
      mergeCells(wb,1,2:8,1)
      mergeCells(wb,1,2:8,2)

      writeData(wb, 1,
                report_title,
                startRow = 1,
                startCol = 2)
      
      writeData(wb, 1,
                funding_sources,
                startRow = 2,
                startCol = 2)

      writeDataTable(wb, 1, df, startRow = 4, startCol = 2)


      low_risk <- createStyle(fgFill ="#2ECC71")
      medium_risk <- createStyle(fgFill ="#FFC300")
      high_risk <- createStyle(fgFill ="#FF5733")
      very_high_risk <- createStyle(fgFill ="#C70039")

      low_risk_rows <- which(df$`Disbursement Risk Level`=="Low Risk")
      medium_risk_rows <- which(df$`Disbursement Risk Level`=="Medium Risk")
      high_risk_rows <- which(df$`Disbursement Risk Level`=="High Risk")
      very_high_risk_rows <- which(df$`Disbursement Risk Level`=="Very High Risk")


      for (i in low_risk_rows){
        addStyle(wb,1,rows=i+4,cols=1:length(df)+1,style = low_risk)
      }

      for (i in medium_risk_rows){
        addStyle(wb,1,rows=i+4,cols=1:length(df)+1,style = medium_risk)
      }

      for (i in high_risk_rows){
        addStyle(wb,1,rows=i+4,cols=1:length(df)+1,style = high_risk)
      }

      for (i in very_high_risk_rows){
        addStyle(wb,1,rows=i+4,cols=1:length(df)+1,style = very_high_risk)
      }

      header_style <- createStyle(borderColour = getOption("openxlsx.borderColour", "black"),
                                  borderStyle = getOption("openxlsx.borderStyle", "thick"),
                                  halign = 'center',
                                  valign = 'center',
                                  textDecoration = NULL,
                                  wrapText = TRUE)

      addStyle(wb,1,rows=4,cols=2:length(df)+1,style = header_style)
      
      setColWidths(wb, 1, cols=2:length(df)+1, widths = "auto")
      setColWidths(wb, 1, cols=5, widths = 100)

      ## opens a temp version
      wb
      },file,overwrite = TRUE)
                     }
    )

    observeEvent(input$show_grants_need_transfer, {
      temp_df <- reactive_df()
      
      isolate(data <- temp_df %>%
                filter(funds_to_be_transferred > 1,
                       percent_transferred_available<.3) %>%
                select(Fund,
                       `Fund Name`,
                       `Grant Amount USD`,
                       `Fund TTL Name`,
                       percent_unaccounted,
                       months_to_end_disbursement_static,
                       percent_transferred_available,
                       funds_to_be_transferred,
                       percent_left_to_transfer) %>%
                arrange(percent_transferred_available) %>%
                rename("Months to Closing Date" = months_to_end_disbursement_static,
                       "Percent Uncommitted" = percent_unaccounted,
                       "Percent of grant not yet transferred" = percent_left_to_transfer,
                       "Amount not yet trasnferred"=funds_to_be_transferred,
                       "Percent of funds transferred available"=percent_transferred_available))
      
      reactive_data$download_table <- data %>%
        mutate(`Percent Uncommitted`=round(`Percent Uncommitted`/100,3),
               `Percent of grant not yet transferred` = round(`Percent of grant not yet transferred`,3),
               `Percent of funds transferred available` = round(`Percent of funds transferred available`,3))
        
      data <- data %>% 
        mutate(`Percent Uncommitted` = percent((`Percent Uncommitted`/100),accuracy = .1),
               `Grant Amount USD` = dollar(`Grant Amount USD`),
               `Percent of grant not yet transferred` = percent(`Percent of grant not yet transferred`),
               `Percent of funds transferred available` = percent(`Percent of funds transferred available`,accuracy = .1),
               `Amount not yet trasnferred`=dollar(`Amount not yet trasnferred`),
               `Months to Closing Date`=as.character(`Months to Closing Date`))
      
      showModal(modalDialog(size = 'l',
                            title = "Grants that may require a transfer",
                            renderTable(data),
                            easyClose = TRUE,
                footer = list(modalButton(label = 'Close'),downloadBttn('dload_need_transfer'))))
      
    })
    
    output$dload_need_transfer <- downloadHandler(
      filename = "Grants that may need funds transferred.xlsx",
      content = function(file) {
        write.xlsx(reactive_data$download_table,file)
      }) 
    
    #display list of grants that are actuive but have not received first transfer yet
    observeEvent(input$show_grants_no_first_transfer, {
      temp_df <- reactive_df()
      
      isolate(data <- temp_df %>%
                filter(`Transfer-in USD` == 0,`Fund Status`=="ACTV") %>%
                select(Fund,
                       `Fund Name`,
                       `Grant Amount USD`,
                       `Fund TTL Name`,
                       tf_age_months,
                       months_to_end_disbursement_static,
                       funds_to_be_transferred,
                       percent_left_to_transfer) %>%
                arrange(-tf_age_months) %>%
                rename("Months to Closing Date" = months_to_end_disbursement_static,
                       "Number of months grant has been active" = tf_age_months,
                       "Percent of grant not yet transferred" = percent_left_to_transfer,
                       "Amount not yet transferred"= funds_to_be_transferred))
      
      reactive_data$download_table <- data %>%
        mutate(`Percent of grant not yet transferred`= round(`Percent of grant not yet transferred`,3))
      
    data <- data %>%
      mutate(`Grant Amount USD` = dollar(`Grant Amount USD`),
             `Percent of grant not yet transferred`= percent(`Percent of grant not yet transferred`),
             `Amount not yet transferred` = dollar(`Amount not yet transferred`),
             `Number of months grant has been active` = as.character(`Number of months grant has been active`),
             `Months to Closing Date` = as.character(`Months to Closing Date`))
                
      
      showModal(modalDialog(size = 'l',
                            title = "Active Grants Without Initial Transfer",
                            renderTable(data),
                            easyClose = TRUE, footer = downloadBttn('dload_no_initial_transfer')))
      
    })
    
    output$dload_no_initial_transfer <- downloadHandler(
      filename = "Active grants without initial transfer.xlsx",
      content = function(file) {
        write.xlsx(reactive_data$download_table,file)
      }) 
    
    observeEvent(input$show_grants_VHR, {
      data <- reactive_df()
      isolate(data <- data %>% filter(disbursement_risk_level == 'Very High Risk',
                                      `Grant Amount USD` != 0) %>%
                select(Fund,
                       `Fund Name`,
                       `Grant Amount USD`,
                       `Fund TTL Name`,
                       `Grant Amount USD`,
                       percent_unaccounted,
                       months_to_end_disbursement,
                       required_disbursement_rate) %>%
                arrange(-required_disbursement_rate)  %>%
                rename("Months Left to Disburse" = months_to_end_disbursement,
                       "Percent Uncommitted" = percent_unaccounted,
                       "Required Monthly Disbursement Rate" = required_disbursement_rate))
      
      
      reactive_data$download_table <- data %>%
        mutate(`Percent Uncommitted`=round(`Percentage Uncommitted`/100,3),
               `Required Monthly Disbursement Rate` = round(`Required Monthly Disbursement Rate`,3))
      
      data <- data %>%
        mutate(`Percent Uncommitted` = percent((`Percent Uncommitted`/100)),
               `Grant Amount USD` = dollar(`Grant Amount USD`),
               `Required Monthly Disbursement Rate` = percent(`Required Monthly Disbursement Rate`),
               `Months Left to Disburse`= as.character(`Months Left to Disburse`))

      showModal(modalDialog(size = 'l',
                            title = "Very High Risk Grants",
                            renderTable(data),
                            easyClose = TRUE,footer = downloadBttn('dload_VHR')))

    })
    
    output$dload_VHR <- downloadHandler(
      filename = "very_high_risk_grants.xlsx",
      content = function(file) {
        write.xlsx(reactive_data$download_table,file)
      }) 
    
    
    
    observeEvent(input$show_grants_HR, {
      
      data <- reactive_df()
      isolate(data <- data %>% filter(disbursement_risk_level == 'High Risk',
                                      `Grant Amount USD` != 0) %>%
                select(Fund,
                       `Fund Name`,
                       `Grant Amount USD`,
                       `Fund TTL Name`,
                       `Grant Amount USD`,
                       percent_unaccounted,
                       months_to_end_disbursement,
                       required_disbursement_rate) %>%
                arrange(-required_disbursement_rate)  %>%
                rename("Months Left to Disburse" = months_to_end_disbursement,
                       "Percent Uncommitted" = percent_unaccounted,
                       "Required Monthly Disbursement Rate" = required_disbursement_rate))
      
      
      reactive_data$download_table <- data %>%
        mutate(`Percent Uncommitted`=round(`Percent Uncommitted`/100,3),
               `Required Monthly Disbursement Rate` = round(`Required Monthly Disbursement Rate`,3))
      
      data <- data %>%
        mutate(`Percent Uncommitted` = percent((`Percent Uncommitted`/100)),
               `Grant Amount USD` = dollar(`Grant Amount USD`),
               `Required Monthly Disbursement Rate` = percent(`Required Monthly Disbursement Rate`),
               `Months Left to Disburse`= as.character(`Months Left to Disburse`))
      
      showModal(modalDialog(size = 'l',
                            title = "High Risk Grants",
                            renderTable(data),
                            easyClose = TRUE,footer = downloadBttn("dload_HR")))
    

    })
    output$dload_HR <- downloadHandler(
      filename = "high_risk_grants.xlsx",
      content = function(file) {
        write.xlsx(reactive_data$download_table,file)
      }) 
    
    observeEvent(input$show_grants_MR, {
      data <- reactive_df()
      isolate(data <- data %>% filter(disbursement_risk_level == 'Medium Risk',
                                      `Grant Amount USD` != 0) %>%
                select(Fund,
                       `Fund Name`,
                       `Grant Amount USD`,
                       `Fund TTL Name`,
                       `Grant Amount USD`,
                       percent_unaccounted,
                       months_to_end_disbursement,
                       required_disbursement_rate) %>%
                arrange(-required_disbursement_rate)  %>%
                rename("Months Left to Disburse" = months_to_end_disbursement,
                       "Percent Uncommitted" = percent_unaccounted,
                       "Required Monthly Disbursement Rate" = required_disbursement_rate))
      
      
      reactive_data$download_table <- data %>%
        mutate(`Percent Uncommitted`=round(`Percent Uncommitted`/100,3),
               `Required Monthly Disbursement Rate` = round(`Required Monthly Disbursement Rate`,3))
      
      data <- data %>%
        mutate(`Percent Uncommitted` = percent((`Percent Uncommitted`/100)),
               `Grant Amount USD` = dollar(`Grant Amount USD`),
               `Required Monthly Disbursement Rate` = percent(`Required Monthly Disbursement Rate`),
               `Months Left to Disburse`= as.character(`Months Left to Disburse`))

      showModal(modalDialog(size = 'l',
                            title = "Medium Risk Grants",
                            renderTable(data),
                            easyClose = TRUE,
                            footer = downloadBttn("dload_MR")))

    })
    
    output$dload_MR <- downloadHandler(
      filename = "medium_risk_grants.xlsx",
      content = function(file) {
        write.xlsx(reactive_data$download_table,file)
      }) 
    
    observeEvent(input$show_grants_LR, {
      data <- reactive_df()
      isolate(data <- data %>% filter(disbursement_risk_level == 'Low Risk',
                                     `Grant Amount USD` != 0) %>%
                select(Fund,
                       `Fund Name`,
                       `Grant Amount USD`,
                       `Fund TTL Name`,
                       `Grant Amount USD`,
                       percent_unaccounted,
                       months_to_end_disbursement,
                       required_disbursement_rate) %>%
                arrange(-required_disbursement_rate)  %>%
                rename("Months Left to Disburse" = months_to_end_disbursement,
                       "Percent Uncommitted" = percent_unaccounted,
                       "Required Monthly Disbursement Rate" = required_disbursement_rate))
      
      
      reactive_data$download_table <- data %>%
        mutate(`Percent Uncommitted`=round(`Percent Uncommitted`/100,3),
               `Required Monthly Disbursement Rate` = round(`Required Monthly Disbursement Rate`,3))
      
      data <- data %>%
        mutate(`Percent Uncommitted` = percent((`Percent Uncommitted`/100)),
               `Grant Amount USD` = dollar(`Grant Amount USD`),
               `Required Monthly Disbursement Rate` = percent(`Required Monthly Disbursement Rate`),
               `Months Left to Disburse`= as.character(`Months Left to Disburse`))
      
      showModal(modalDialog(size = 'l',
                            title = "Low Risk Grants",
                            renderTable(data),
                            easyClose = TRUE,
                            footer = downloadBttn('dload_LR')))

    })
    
    output$dload_LR <- downloadHandler(
      filename = "low_risk_grants.xlsx",
      content = function(file) {
        write.xlsx(reactive_data$download_table,file)
      }) 
    
    
    observeEvent(input$show_region_grants_closing_3, {
      data <- reactive_df()
      isolate(data <- data %>% filter(`Grant Amount USD` != 0,
                                      months_to_end_disbursement <= 3) %>%
                select(Fund,
                       `Fund Name`,
                       `Grant Amount USD`,
                       `Fund TTL Name`,
                       `Grant Amount USD`,
                       percent_unaccounted,
                       months_to_end_disbursement,
                       required_disbursement_rate) %>%
                arrange(-percent_unaccounted))
      
      reactive_data$download_table <- data %>%  
        rename("Months Left to Disburse" = months_to_end_disbursement,
               "Percent available" = percent_unaccounted,
               "Required Monthly Disbursement Rate" = required_disbursement_rate) %>% 
        mutate(`Percent available`=round(`Percent available`/100,3),
               `Required Monthly Disbursement Rate`=round(`Required Monthly Disbursement Rate`,3))
    
                
    data <- data %>% 
                mutate(percent_unaccounted = percent((percent_unaccounted/100)),
                       `Grant Amount USD` = dollar(`Grant Amount USD`),
                       required_disbursement_rate = percent(required_disbursement_rate)) %>%
                rename("Months Left to Disburse" = months_to_end_disbursement,
                       "Percent available" = percent_unaccounted,
                       "Required Monthly Disbursement Rate" = required_disbursement_rate)
      
      showModal(modalDialog(size = 'l',
                            title = "Grants Closing in 3 Months or Less",
                            renderTable(data),
                            easyClose = TRUE,footer = downloadBttn("dload_grants_close_3")))
      
    })
    
    output$dload_grants_close_3 <- downloadHandler(
      filename = "Grants closing in less than 3 months.xlsx",
      content = function(file) {
        write.xlsx(reactive_data$download_table,file)
      }) 
    
    observeEvent(input$show_region_grants_no_discom, {
      data <- reactive_df()
      isolate(data <- data %>% filter(`Grant Amount USD` != 0,
                                      tf_age_months >= 3,
                                      percent_unaccounted==100) %>%
                select(Fund,
                       `Fund Name`,
                       `Grant Amount USD`,
                       `Fund TTL Name`,
                       percent_unaccounted,
                       tf_age_months,
                       months_to_end_disbursement_static,
                       required_disbursement_rate) %>%
                arrange(-tf_age_months))
      
      reactive_data$download_table <- data %>%
        rename("Months to Closing Date" = months_to_end_disbursement_static,
               "Percent uncommitted" = percent_unaccounted,
               "Required Monthly Disbursement Rate" = required_disbursement_rate,
               "Months Since Grant Activation" = tf_age_months) %>% 
        mutate(`Percent uncommitted`= round(`Percent uncommitted`/100,3),
               `Required Monthly Disbursement Rate`=round(`Required Monthly Disbursement Rate`,3))
      
      data <- data %>%
                mutate(percent_unaccounted = percent((percent_unaccounted/100)),
                       `Grant Amount USD` = dollar(`Grant Amount USD`),
                       required_disbursement_rate = percent(required_disbursement_rate),
                       tf_age_months = as.character(tf_age_months),
                       months_to_end_disbursement_static=as.character(months_to_end_disbursement_static)) %>%
                rename("Months to Closing Date" = months_to_end_disbursement_static,
                       "Percent uncommitted" = percent_unaccounted,
                       "Required Monthly Disbursement Rate" = required_disbursement_rate,
                       "Months Since Grant Activation"=tf_age_months)
      
      showModal(modalDialog(size = 'l',
                            title = "Grants Closing in 6 Months or Less",
                            renderTable(data),
                            easyClose = TRUE,footer = downloadBttn('dload_zero_dis')))
      
    })
    
    output$dload_zero_dis <- downloadHandler(
      filename = "Active more than 3 months no committments.xlsx",
      content = function(file) {
        write.xlsx(reactive_data$download_table,file)
      }) 
    
   
    output$RETF_n_grants_R <- renderValueBox({
      
      temp_df <- reactive_df_2()
      temp_df %>% nrow() %>%
        valueBox(value=.,
                 subtitle =HTML("<b>Active RETF grants</b> <button id=\"show_grants_RETF_R\" type=\"button\" class=\"btn btn-default action-button\">Show Grants</button>"),
                 color="blue")
      
    })
    
  
    observeEvent(input$show_grants_RETF_R, {
      data <- reactive_df_2()
      isolate(data <- data %>% filter(`Grant Amount USD` != 0) %>%
                select(Fund,
                       `Fund Name`,
                       `Grant Amount USD`,
                       temp.name,
                       `Fund TTL Name`,
                       `TTL Unit Name`,
                       tf_age_months,
                       months_to_end_disbursement) %>%
                arrange(-tf_age_months) %>%
                mutate(`Grant Amount USD` = dollar(`Grant Amount USD`)) %>%
                rename("Months Left to Disburse" = months_to_end_disbursement,
                       "Months Since Grant Activation"= tf_age_months,
                       "Trustee"= temp.name))
      
      showModal(modalDialog(size = 'l',
                            title = "BETF grants",
                            renderTable(data),
                            easyClose = TRUE))
      
    })
    
    output$`RETF_$_grants_R` <- renderValueBox({
      
      temp_df <- reactive_df_2()
      sum(temp_df$`Grant Amount USD`) %>% dollar() %>% 
        valueBox(value=.,
                 subtitle = "Active RETF funds amount",
                 color="blue")
      
    })
    
    output$RETF_trustees_R_pie <- renderPlotly({
      
      temp_df <- reactive_df_2() 
      
      data <- temp_df %>% 
        group_by(`Lead GP/Global Themes`) %>% 
        summarise(n_grants = n(), total_award_amount = sum(`Grant Amount USD`))
      
      
      total <- sum(data$total_award_amount)
      data$percent.1 <- data$total_award_amount/total
      
      data$pie_name <- ifelse(data$percent.1 >=.01,data$`Lead GP/Global Themes`,"Other")
      
      data <- data %>% group_by(pie_name) %>%
        summarise(n_grants=sum(n_grants),
                  total_award_amount=sum(total_award_amount))
      m <- list(
        l = 40,
        r = 20,
        b = 40,
        t = 90,
        pad = 4
      )
      
      
      
      plot_ly(data,
              labels = ~pie_name,
              values = ~total_award_amount,
              text = ~paste0(percent(total_award_amount/total),"\n","(",n_grants," grants)"),
              hoverinfo = 'label+value+text',
              textinfo = 'text',
              type = 'pie') %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               margin=m,
               title="RETF Funding (Costum Selection)")
      
      
    })
    
 
#final closing brackets    
    
    
# TAB.4 PMA -------------------------------------
    output$resources_available <- renderValueBox({
      sum(PMA_grants$unnacounted_amount) %>%
        dollar() %>% 
      valueBox("PMA Unnacounted Balance",value=.,color = 'green')
      
    })
  
    output$PMA_chart_1 <- renderPlotly({
      
      colourCount = length(unique(grouped_gg_data$fund))
      getPalette = colorRampPalette(brewer.pal(7, "Set2"),bias=2)
      
      gg <- ggplot(gg_df,aes(x=factor(quarterr),
                             y=amount,
                             fill=fund,
                             text=paste(as.character(zoo::as.yearqtr(gg_df$quarterr)),"\n",
                                        "Total Av. Quarter:",dollar(Q_amount),"\n",
                                        "PMA TF Name:",fund_name,"\n",
                                        "PMA TF Number:",fund,"\n",
                                        "PMA TF Av. Quarter:",dollar(amount)))) +
        geom_bar(stat="identity") +
        theme_classic() +
        scale_fill_manual(values =  getPalette(colourCount))+
        scale_x_discrete(labels=c(unique(as.character(zoo::as.yearqtr(gg_df$quarterr)))))+
        scale_y_continuous(labels=dollar_format(prefix="$"),
                           breaks =c(25,50,75,100,125,150,175,200,225,250,275,300)*10000) +
        labs(x="Quarter", y="Available USD Amount") + theme(legend.position = 'none')
      
      plotly::ggplotly(gg,tooltip='text')
      
      
      
    })
    
    
    output$streamgraph <- renderStreamgraph({
      
      streamgraph(data = gg_df,
                  key = "fund_name",
                  value ="amount",
                  date = 'quarterr',
                  order="inside-out",
                  offset="zero") %>% sg_legend(show=TRUE, label="PMA Grant Names:") 
    
      })
    
    
    
    output$current_quarter <- renderValueBox({
      current_Q_date <- zoo::as.yearqtr(date_data_udpated) %>% as_date()
      
      current_Q_amount <- grouped_gg_data[grouped_gg_data$quarterr==current_Q_date,] [1,6] %>%
        as.numeric()
      
      valueBox("Available in Current Quarter",
              value = dollar(current_Q_amount),icon = icon("wallet")) })
      
      output$next_quarter <- renderValueBox({
        current_Q_date <- zoo::as.yearqtr(date_data_udpated) %>% as_date() 
        
        next_Q_date <- current_Q_date %m+% months(3) %>% as_date()
        
        current_Q_amount <- grouped_gg_data[grouped_gg_data$quarterr==current_Q_date,] [1,6] %>%
          as.numeric()
        next_Q_amount <- grouped_gg_data[grouped_gg_data$quarterr==next_Q_date,] [1,6] %>%
          as.numeric()
        
        arrow_icon <- ifelse(current_Q_amount==next_Q_amount,"arrow-right",
                       ifelse(current_Q_amount<next_Q_amount,"arrow-up","arrow-down"))
        
        valueBox("Available in next Quarter",
                value = dollar(next_Q_amount),icon = icon(as.character(arrow_icon)))
      
      })
      
        output$PMA_grants_n <- renderValueBox({
          
          PMA_grants %>% filter(`Fund Status`=="ACTV") %>% nrow() %>% 
          valueBox("Active PMA Grants",
                  value = .,icon = icon("list-ol"),
                  subtitle = HTML("<button id=\"show_PMA_grants\" type=\"button\" class=\"btn btn-default action-button\">Show Grants</button>"))
          
    })
    
        observeEvent(input$show_PMA_grants, {
          data <- PMA_grants
         data <- data %>% filter(`Grant Amount USD` != 0,
                                          `Fund Status`=="ACTV") %>%
                    select(Fund,
                           `Fund Name`,
                           `Grant Amount USD`,
                           `Fund TTL Name`,
                           Region,
                           percent_unaccounted,
                           tf_age_months,
                           months_to_end_disbursement
                           ) %>%
                    arrange(months_to_end_disbursement) %>%
                    mutate(percent_unaccounted = percent((percent_unaccounted/100)),
                           `Grant Amount USD` = dollar(`Grant Amount USD`)) %>%
                    rename("Months Left to Disburse" = months_to_end_disbursement,
                           "Percent Available" = percent_unaccounted,
                           "Months Since Grant Activation"= tf_age_months)
          
          showModal(modalDialog(size = 'l',
                                title = "Active PMA Grants",
                                renderTable(data),
                                easyClose = T))
          
        })
        
        

        
    
        
        
        
# TAB.5 GRANT DASHBOARD ---------
        
        reactive_grant <- reactive({
          grants %>%
            filter(Fund==input$child_TF_num) 
        })
        
        reactive_grant_expense <- reactive({
          data_2 %>%
            filter(child_TF==input$child_TF_num) 
        })
        
        output$grant_name <- renderText({
          
          grant <- reactive_grant()
          
          isolate(if(!is.null(input$child_TF_num)){
            text <-  grant$`Fund Name` %>% as.character()
          } else {
            text <- NULL
          })
          text 
        })
        
        
        
        output$grant_TTL <- renderText({
          
          grant <- reactive_grant()
          
          isolate(if(!is.null(input$child_TF_num)){
              text <-  ifelse(is.na(grant$`Co-TTL1 Name`),
                              grant$`Fund TTL Name`,
                              ifelse(grant$`Fund TTL Name` != grant$`Co-TTL1 Name`,
                                     paste(grant$`Fund TTL Name`,"&",grant$`Co-TTL1 Name`),
                                     grant$`Fund TTL Name`))
                          
          } else {
            text <- NULL
          })
          text %>% as.character()
        })
        
        
        
        
        output$grant_country <- renderText({
          
          grant <- reactive_grant()
          
          isolate(if(!is.null(input$child_TF_num)){
            text <-  grant$Country
            
          } else {
            text <- NULL
          })
        
          text %>% as.character()
        })
        
        
        
        
        output$grant_region <- renderText({
          
          grant <- reactive_grant()
          
          isolate(if(!is.null(input$child_TF_num)){
            text <-  grant$`Fund Country Region Name`
            
          } else {
            text <- NULL
          })
          
          text <- ifelse(text=="OTHER","Global",text)
          text %>% as.character()
        })
        
        output$grant_unit <- renderText({
          
          grant <- reactive_grant()
          
          isolate(if(!is.null(input$child_TF_num)){
            text <-  grant$`TTL Unit Name`
            
          } else {
            text <- NULL
          })
          text %>% as.character()
        })
        
        output$single_grant_amount <- renderValueBox({
          grant <- reactive_grant()
          grant$`Grant Amount USD` %>%  dollar(accuracy = 1) %>% 
            valueBox(value=.,subtitle = "Grant Amount",color = "green")
          
          
        })
        
        output$single_grant_remaining_bal <- renderValueBox({
          grant <- reactive_grant()
          grant$unnacounted_amount %>% dollar(accuracy = 1) %>% 
            valueBox(value=.,subtitle = "Available Balance",color = "green")
          
        })
        
        
        
        
        output$single_grant_m_active <- renderValueBox({
          grant <- reactive_grant()
          grant$tf_age_months %>% as.numeric() %>% 
            valueBox(value=.,subtitle = "Months since activation",color = "yellow")
          
          
        })
        
        
        output$single_grant_m_disrate <- renderValueBox({
          grant <- reactive_grant()
          grant$monthly_disbursement_rate %>% percent()%>% 
            valueBox(value=.,subtitle = "Monthly Disbursement Rate",color = "yellow")
          
          
        })
        
        
        output$single_grant_m_to_close<- renderValueBox({
          grant <- reactive_grant()
          grant$months_to_end_disbursement %>% as.numeric() %>% 
          valueBox(value=.,subtitle = "Months to end disbursement",color = "orange")
          
          
        })
        
        output$single_grant_m_req_disrate<- renderValueBox({
          grant <- reactive_grant()
          grant$required_disbursement_rate %>% percent() %>% 
            valueBox(value=.,subtitle = "Monthly required disbursement rate",color = "orange")
          
          
        })
        
        
        output$grant_expense_GG <- renderPlotly({
          TTL_spending_df <- reactive_grant_expense()
          
          gg <-
            ggplot(TTL_spending_df,
                   aes(reorder(item_group, -total_disbursed), total_disbursed,
                       fill=item_group)) +
            geom_col() +
            theme_classic() +
            labs(x = "Expense Category", y = "Amount Disbursed") +
            theme(axis.text.x = element_blank()) +
            scale_y_continuous(labels=dollar_format(prefix="$")) +
            labs(fill="Expense Category")
            
            
          
          ggplotly(gg)
        })
        
        
        output$expense_table <- renderTable({
          df <- reactive_grant_expense()
          grant_amount <- reactive_grant()
          grant_amount <- grant_amount$`Grant Amount USD`
          df  %>%
            group_by(item_group) %>%
            summarise('total_dis' = sum(total_disbursed)) %>%
            arrange(-total_dis) %>%
            mutate(
              "percent_of_total" = percent(total_dis /grant_amount),
              'total_dis' = dollar(total_dis,accuracy = 1))  %>%
            rename(
              "Expense Category" = item_group,
              "Disbursed to date" = total_dis,
              "% of Grant" = percent_of_total
            )
        })
        
## TTL DASHABOARD ----------------
        reactive_TTL <- reactive({
          grants %>%
            filter(as.numeric(`Project TTL UPI`)==as.numeric(input$TTL_upi))
        })
        
        reactive_grant_expense <- reactive({
          
          TTL <- reactive_TTL()
          ttl_name <- TTL$`Project TTL UPI`[1]
          data_2 %>%
            filter(TTL==ttl_name) 
        })
        
        output$TTL_name_dash <- renderText({
          
         TTL <- reactive_TTL()
          
          isolate(if(!is.null(input$TTL_upi)){
            text <-  TTL$`Fund TTL Name` %>% unique() %>% as.character()
          } else {
            text <- NULL
          })
          text 
        })
        
        output$TTL_unit_dash <- renderText({

          TTL <- reactive_TTL()

          isolate(if(!is.null(input$TTL_upi)){
            text <-  unique(TTL$`TTL Unit Name`)

          } else {
            text <- NULL
          })

          text %>% as.character()
        })


        output$TTL_total_grant_amount <- renderValueBox({

          TTL <- reactive_TTL()

          isolate(if(!is.null(input$TTL_upi)){
            sum(TTL$`Grant Amount USD`) %>%
              dollar(accuracy = 1) %>%
              valueBox(value=.,subtitle = "Total Grant Amount",color = 'green')

          } else {
            NULL
          })
        })
      
        
        output$TTL_grants_active <- renderValueBox({
          
          TTL <- reactive_TTL()
          TTL <- dplyr::distinct(TTL)
          isolate(if(!is.null(input$TTL_upi)){
              valueBox(value=nrow(TTL),subtitle = "Number of Active Grants")
            
          } else {
            NULL
          })
        })

        output$TTL_total_remaining_bal <- renderValueBox({
          
          TTL <- reactive_TTL()
          TTL <- dplyr::distinct(TTL)
          isolate(if(!is.null(input$TTL_upi)){
            valueBox(value=dollar(sum(TTL$unnacounted_amount),accuracy = 1),subtitle = "Total Available Balance")
            
          } else {
            NULL
          })
        })

  
        #-----------DOWNLOAD SUMMARY REPORT----------      
        output$Download_summary_report.xlsx <- downloadHandler(
          filename = function() {
            paste("Summary Report",".xlsx", sep="")},
          content = function(file) {
            withProgress(message='Analyzing data and creating report, this may take a minute or two...',{
            openxlsx::saveWorkbook({
              
              
              #CODE---------------
            
              temp_df <- report_grants %>%
                filter(`Child Fund Status` %in% input$summary_fund_status,
                       `Region Name` %in% input$summary_region,
                       `Trustee Fund Name` %in% input$summary_trustee,
                       !is.na(`Lead GP/Global Themes`)
                )
              
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
              wb <- createWorkbook()
              
              #temp_df <- reactive_df()
              report_title <- paste0("Summary of GFDRR Portfolio (as of ",report_data_date,")")
              
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
              
              wb
              
            },file,overwrite = TRUE)})
          })
        
        
    
        
        #-----------DOWNLOAD DISBURSEMENT RISK REPORT----------      
        output$Download_risk_report.xlsx <- 
          downloadHandler(
            filename = function() {
              paste("Risk Report_",input$risk_region,"_region",".xlsx", sep="")
            },
            content = function(file) {
              
               withProgress(message='Analyzing data and creating report, this may take a minute or two...',{
              openxlsx::saveWorkbook({
                
                data_date <- date_data_udpated
                as.of.date <- date_data_udpated
                
                all_regions <- c("AFR","SAR","LCR","MNA","ECA","EAP","GLOBAL")
                current_trustee_subset <- c("TF072236","TF072584", "TF072129","TF071630")
                
                #for (i in all_regions) {
                
                
                report_region <- {if(input$risk_region=="ALL"){all_regions} else {input$risk_region}}
                print(report_region)
                #report_region <- all_regions
                trustee_subset <- input$risk_trustee
                #trustee_subset <- current_trustee_subset
                wb <- createWorkbook()
                #CODE------------------
                
                for (j in 1:length(report_region)){
                  
                  data <- report_grants %>%
                    filter(`Child Fund Status` %in% input$risk_fund_status,
                           `Trustee Fund Name`  %in% trustee_subset,
                    `Region Name` == report_region[j])
                  
                  #data <- grants
                  region <- report_region[j]
                  
                  message(paste("Preparing report for",region))
                  
                  
                  funding_sources <- trustee_subset %>%
                    unique() %>%
                    paste(sep="",collapse= "; ")
                  
                  df <- data %>% filter(`Grant Amount`>0)#,!is.na(`Activation Date`))
                  df <- df %>%  select(`Closing FY`,
                                       Trustee,
                                       `Trustee Fund Name`,
                                       `Child Fund`,
                                       `Child Fund Name`,
                                       `Child Fund Status`,
                                       `Project ID`,
                                       `Execution Type`,
                                       `Child Fund TTL Name`,
                                       `Managing Unit Name`,
                                       `Lead GP/Global Themes`,
                                       Country,
                                       `Activation Date`,
                                       `Closing Date`,
                                       `Grant Amount`,
                                       `Cumulative Disbursements`,
                                       `PO Commitments`,
                                       `Uncommitted Balance`,
                                       `Months to Closing Date`) 
                  
                  df <- df %>% filter(`Uncommitted Balance`>0)
                  
                  elapsed_months <- function(end_date, start_date) {
                    ed <- as.POSIXlt(end_date)
                    sd <- as.POSIXlt(start_date)
                    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
                  }
                  
                  #df$`Child Fund Months Active` <-  elapsed_months(data_date,df$`Activation Date`)
                  
                  df$`Uncommitted Balance (Percent)` <- df$`Uncommitted Balance`/df$`Grant Amount` 
                  
                  df$`Required Monthly Disbursement Rate` <- (df$`Uncommitted Balance` /df$`Grant Amount`)/
                    (ifelse(df$`Months to Closing Date`>=1,df$`Months to Closing Date`,1))
                  
                  
                  compute_risk_level <- function (x){
                    
                    risk_level <- ifelse(abs(x) < .03,"Low Risk",
                                         ifelse(abs(x) < .05,"Medium Risk",
                                                ifelse(abs(x) < .10,"High Risk",
                                                       "Very High Risk")))
                    
                    return(risk_level)
                  }
                  
                  df$`Disbursement Risk Level` <- compute_risk_level(df$`Required Monthly Disbursement Rate`)
                  
                  df <- df %>% arrange(abs(as.numeric(`Required Monthly Disbursement Rate`)))
                  
                  df$`Grace Period` <- ifelse(df$`Months to Closing Date`<0,
                                              "Yes",
                                              "No")
                  
                  
                  df$grace_period_divider <- ifelse((4 + df$`Months to Closing Date`)>=1,
                                                    (4 + df$`Months to Closing Date`),
                                                    1)
                  df$`Required Monthly Disbursement Rate` <- ifelse(df$`Months to Closing Date`<0,
                                                                    (df$`Uncommitted Balance (Percent)`/df$grace_period_divider),
                                                                    df$`Required Monthly Disbursement Rate`)
                  
                  df <- df %>% select(-grace_period_divider)
                  
                  df$funding_sources <- paste0(df$`Trustee Fund Name`," (",df$Trustee,")")
                  
                  # names_trustee_subset <- left_join(as.data.frame(trustee_subset),
                  #   recode_trustee.2,by=c("trustee_subset"="Trustee")) %>% 
                  # mutate(name_number = paste0(`Trustee Fund Name`," (",trustee_subset,")"))
                  
                  trustee_subset_names_number <- sort(unique(df$funding_sources)) %>%
                    paste(sep="",collapse="; ")
                  
                  df <- df %>% select(-funding_sources)
                  
                  df$`Activation Date` <- as.Date.POSIXct(df$`Activation Date`)
                  
                  df$`Months to Closing Date`<- round(df$`Months to Closing Date`,digits = 0)
                  
                  options("openxlsx.dateFormat", "mm/dd/yyyy")
                  
                  report_title <- paste("Disbursement Risk for",report_region[j],"region as of",as.of.date)
                  funding_sources <- paste("Funding Sources:",trustee_subset_names_number)
                  
                  addWorksheet(wb, report_region[j])
                  mergeCells(wb,j,cols = 2:8,rows = 1)
                  mergeCells(wb,j,cols = 2:8,rows = 2)
                  
                  writeData(wb,j,
                            report_title,
                            startRow = 1,
                            startCol = 2)
                  
                  writeData(wb,j,
                            funding_sources,
                            startRow = 2,
                            startCol = 2)
                  
                  writeDataTable(wb,j, df, startRow = 4, startCol = 2)
                  
                  
                  low_risk <- createStyle(fgFill ="#86F9B7",halign = "left")
                  medium_risk <- createStyle(fgFill ="#FFE285",halign = "left")
                  high_risk <- createStyle(fgFill ="#FD8D75",halign = "left")
                  very_high_risk <- createStyle(fgFill ="#F17979",halign = "left")
                  
                  low_risk_rows <- which(df$`Disbursement Risk Level`=="Low Risk")
                  medium_risk_rows <- which(df$`Disbursement Risk Level`=="Medium Risk")
                  high_risk_rows <- which(df$`Disbursement Risk Level`=="High Risk")
                  very_high_risk_rows <- which(df$`Disbursement Risk Level`=="Very High Risk")
                  
                  
                  for (i in low_risk_rows){
                    addStyle(wb,j,rows=i+4,cols=1:length(df) + 1,style = low_risk)
                  }
                  
                  for (i in medium_risk_rows){
                    addStyle(wb,j,rows=i+4,cols=1:length(df) + 1,style = medium_risk)
                  }
                  
                  for (i in high_risk_rows){
                    addStyle(wb,j,rows=i+4,cols=1:length(df) + 1,style = high_risk)
                  }
                  
                  for (i in very_high_risk_rows){
                    addStyle(wb,j,rows=i+4,cols=1:length(df) + 1,style = very_high_risk)
                  }
                  
                  header_style <- createStyle(borderColour = getOption("openxlsx.borderColour", "black"),
                                              borderStyle = getOption("openxlsx.borderStyle", "thick"),
                                              halign = 'center',
                                              valign = 'center',
                                              textDecoration = NULL,
                                              wrapText = TRUE)
                  
                  dollar_format <- createStyle(numFmt = "ACCOUNTING")
                  percent_format <- createStyle(numFmt = "0.0%")
                  date_format <- createStyle(numFmt = "mm/dd/yyyy")
                  num_format <- createStyle(numFmt = "NUMBER")
                  
                  addStyle(wb,j,rows=4,cols=2:(length(df)+1),style = header_style)
                  
                  number_format_range <- 5:(nrow(df)+5)
                  
                  
                  addStyle(wb,j,rows=number_format_range,cols=14,style = date_format,stack = TRUE)
                  addStyle(wb,j,rows=number_format_range,cols=15,style = date_format,stack = TRUE) 
                  addStyle(wb,j,rows=number_format_range,cols=19,style = dollar_format,stack = TRUE)
                  addStyle(wb,j,rows=number_format_range,cols=16,style = dollar_format,stack = TRUE)
                  addStyle(wb,j,rows=number_format_range,cols=17,style = dollar_format,stack = TRUE)
                  addStyle(wb,j,rows=number_format_range,cols=18,style = dollar_format,stack = TRUE)
                  #addStyle(wb,j,rows=number_format_range,cols=20,style = num_format,stack = TRUE)
                  addStyle(wb,j,rows=number_format_range,cols=21,style = percent_format,stack = TRUE)
                  addStyle(wb,j,rows=number_format_range,cols=22,style = percent_format,stack = TRUE)
                  
                  setColWidths(wb,j, cols=2:length(df)+1, widths = "auto")
                  setColWidths(wb,j, cols=6, widths = 90) #child fund name
                  setColWidths(wb,j, cols=9, widths = 9)
                  setColWidths(wb,j, cols=11, widths = 11)
                  setColWidths(wb,j, cols=13, widths = 25)
                  setColWidths(wb,j, cols=17, widths = 15)
                  setColWidths(wb,j, cols=19, widths = 14)
                  setColWidths(wb,j, cols=20, widths = 15)
                  setColWidths(wb,j, cols=21, widths = 19)
                  setColWidths(wb,j, cols=22, widths = 15)
                  setColWidths(wb,j, cols=23, widths = 15)
                  
                  
                }
              #---------
                if(input$risk_region =="ALL"){

                  data <- report_grants %>% filter(`Child Fund Status` %in% input$risk_fund_status,
                                                   `Trustee Fund Name` %in% trustee_subset)

                  #data <- grants
                  region <- 'ALL regions'

                  message(paste("Preparing report for",region))


                  funding_sources <- trustee_subset %>%
                    unique() %>%
                    paste(sep="",collapse= "; ")

                  df <- data %>% filter(`Grant Amount`>0)#,!is.na(`Activation Date`))
                  df <- df %>%  select(`Closing FY`,
                                        Trustee,
                                       `Trustee Fund Name`,
                                       `Child Fund`,
                                       `Child Fund Name`,
                                       `Child Fund Status`,
                                       `Project ID`,
                                       `Execution Type`,
                                       `Child Fund TTL Name`,
                                       `Managing Unit Name`,
                                       `Lead GP/Global Themes`,
                                        Country,
                                       `Activation Date`,
                                       `Closing Date`,
                                       `Grant Amount`,
                                       `Cumulative Disbursements`,
                                       `PO Commitments`,
                                       `Uncommitted Balance`,
                                       `Months to Closing Date`)

                  df <- df %>% filter(`Uncommitted Balance`>0)

                  elapsed_months <- function(end_date, start_date) {
                    ed <- as.POSIXlt(end_date)
                    sd <- as.POSIXlt(start_date)
                    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
                  }

                  #df$`Child Fund Months Active` <-  elapsed_months(data_date,df$`Activation Date`)

                  df$`Uncommitted Balance (Percent)` <- df$`Uncommitted Balance`/df$`Grant Amount`

                  df$`Required Monthly Disbursement Rate` <- (df$`Uncommitted Balance` /df$`Grant Amount`)/
                    (ifelse(df$`Months to Closing Date`>=1,df$`Months to Closing Date`,1))


                  compute_risk_level <- function (x){

                    risk_level <- ifelse(abs(x) < .03,"Low Risk",
                                         ifelse(abs(x) < .05,"Medium Risk",
                                                ifelse(abs(x) < .10,"High Risk",
                                                       "Very High Risk")))

                    return(risk_level)
                  }

                  df$`Disbursement Risk Level` <- compute_risk_level(df$`Required Monthly Disbursement Rate`)

                  df <- df %>% arrange(abs(as.numeric(`Required Monthly Disbursement Rate`)))

                  df$`Grace Period` <- ifelse(df$`Months to Closing Date`<0,
                                              "Yes",
                                              "No")


                  df$grace_period_divider <- ifelse((4 + df$`Months to Closing Date`)>=1,
                                                    (4 + df$`Months to Closing Date`),
                                                    1)
                  df$`Required Monthly Disbursement Rate` <- ifelse(df$`Months to Closing Date`<0,
                                                                    (df$`Uncommitted Balance (Percent)`/df$grace_period_divider),
                                                                    df$`Required Monthly Disbursement Rate`)

                  df <- df %>% select(-grace_period_divider)

                  df$funding_sources <- paste0(df$`Trustee Fund Name`," (",df$Trustee,")")

                  # names_trustee_subset <- left_join(as.data.frame(trustee_subset),
                  #   recode_trustee.2,by=c("trustee_subset"="Trustee")) %>%
                  # mutate(name_number = paste0(`Trustee Fund Name`," (",trustee_subset,")"))

                  trustee_subset_names_number <- sort(unique(df$funding_sources)) %>%
                    paste(sep="",collapse="; ")

                  df <- df %>% select(-funding_sources)

                  df$`Activation Date` <- as.Date.POSIXct(df$`Activation Date`)

                  df$`Months to Closing Date`<- round(df$`Months to Closing Date`,digits = 0)

                  options("openxlsx.dateFormat", "mm/dd/yyyy")

                  report_title <- paste("Disbursement Risk for",region,"as of",as.of.date)
                  funding_sources <- paste("Funding Sources:",trustee_subset_names_number)

                  addWorksheet(wb, region)
                  mergeCells(wb,8,cols = 2:8,rows = 1)
                  mergeCells(wb,8,cols = 2:8,rows = 2)

                  writeData(wb,8,
                            report_title,
                            startRow = 1,
                            startCol = 2)

                  writeData(wb,8,
                            funding_sources,
                            startRow = 2,
                            startCol = 2)

                  writeDataTable(wb,8, df, startRow = 4, startCol = 2)


                  low_risk <- createStyle(fgFill ="#86F9B7",halign = "left")
                  medium_risk <- createStyle(fgFill ="#FFE285",halign = "left")
                  high_risk <- createStyle(fgFill ="#FD8D75",halign = "left")
                  very_high_risk <- createStyle(fgFill ="#F17979",halign = "left")

                  low_risk_rows <- which(df$`Disbursement Risk Level`=="Low Risk")
                  medium_risk_rows <- which(df$`Disbursement Risk Level`=="Medium Risk")
                  high_risk_rows <- which(df$`Disbursement Risk Level`=="High Risk")
                  very_high_risk_rows <- which(df$`Disbursement Risk Level`=="Very High Risk")


                  for (i in low_risk_rows){
                    addStyle(wb,8,rows=i+4,cols=1:length(df) + 1,style = low_risk)
                  }

                  for (i in medium_risk_rows){
                    addStyle(wb,8,rows=i+4,cols=1:length(df) + 1,style = medium_risk)
                  }

                  for (i in high_risk_rows){
                    addStyle(wb,8,rows=i+4,cols=1:length(df) + 1,style = high_risk)
                  }

                  for (i in very_high_risk_rows){
                    addStyle(wb,8,rows=i+4,cols=1:length(df) + 1,style = very_high_risk)
                  }

                  header_style <- createStyle(borderColour = getOption("openxlsx.borderColour", "black"),
                                              borderStyle = getOption("openxlsx.borderStyle", "thick"),
                                              halign = 'center',
                                              valign = 'center',
                                              textDecoration = NULL,
                                              wrapText = TRUE)

                  dollar_format <- createStyle(numFmt = "ACCOUNTING")
                  percent_format <- createStyle(numFmt = "0.0%")
                  date_format <- createStyle(numFmt = "mm/dd/yyyy")
                  num_format <- createStyle(numFmt = "NUMBER")

                  addStyle(wb,8,rows=4,cols=2:(length(df)+1),style = header_style)

                  number_format_range <- 5:(nrow(df)+5)


                  addStyle(wb,8,rows=number_format_range,cols=14,style = date_format,stack = TRUE)
                  addStyle(wb,8,rows=number_format_range,cols=15,style = date_format,stack = TRUE)
                  addStyle(wb,8,rows=number_format_range,cols=19,style = dollar_format,stack = TRUE)
                  addStyle(wb,8,rows=number_format_range,cols=16,style = dollar_format,stack = TRUE)
                  addStyle(wb,8,rows=number_format_range,cols=17,style = dollar_format,stack = TRUE)
                  addStyle(wb,8,rows=number_format_range,cols=18,style = dollar_format,stack = TRUE)
                  #addStyle(wb,8,rows=number_format_range,cols=20,style = num_format,stack = TRUE)
                  addStyle(wb,8,rows=number_format_range,cols=21,style = percent_format,stack = TRUE)
                  addStyle(wb,8,rows=number_format_range,cols=22,style = percent_format,stack = TRUE)

                  setColWidths(wb,8, cols=2:length(df)+1, widths = "auto")
                  setColWidths(wb,8, cols=6, widths = 90) #child fund name
                  setColWidths(wb,8, cols=9, widths = 9)
                  setColWidths(wb,8, cols=11, widths = 11)
                  setColWidths(wb,8, cols=13, widths = 25)
                  setColWidths(wb,8, cols=17, widths = 15)
                  setColWidths(wb,8, cols=19, widths = 14)
                  setColWidths(wb,8, cols=20, widths = 15)
                  setColWidths(wb,8, cols=21, widths = 19)
                  setColWidths(wb,8, cols=22, widths = 15)
                  setColWidths(wb,8, cols=23, widths = 15)

                }

                wb
                
              },file,overwrite = TRUE) })
              
         

            })
        
   ### DOWNLOAD MASTER REPORT    -------------  
        
        output$Download_master_report.xlsx <- 
         downloadHandler(
            filename = function() {
              paste("Master Report_as_of_",date_data_udpated,".xlsx", sep="")
            },
            content = function(file) {
              
              withProgress(message='Analyzing data and creating report',
                           detail="this may take a minute or two",{
                             openxlsx::saveWorkbook({
                
                source("Sameh_Report_jaime_data.R")
                wb
                
          },file,overwrite = TRUE)})
          })
        

        output$Download_source_data.xlsx <- 
          downloadHandler(
            filename = function() {
              paste("Source_Data_as_of_",date_data_udpated,".xlsx", sep="")
            },
            content = function(file) {
              
              withProgress(message='Compiling and Downloading Data',
                           detail="this may take a minute or two",{
                             openxlsx::saveWorkbook({
                               
                               source("Raw_data_to_excel.r")
                               wb
                               
                             },file,overwrite = TRUE)})
            })
        
        
        
        
        
        
        
}) #END OF ALL CODE (SERVER FUNCTION)




