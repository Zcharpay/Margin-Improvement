library(shiny)
library(shinydashboard)
library(DT)

source("datawrangling_experiment2.R")

ui <- dashboardPage(
  dashboardHeader(title = "Margin Improvement"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Detail View", tabName = "detailview", icon = icon("th")),
      menuItem("Volume Balance",tabName = "volbal", icon = icon("balance-scale"))
    ),
    sliderInput("dashslider_date", "Date Filter:", min=as.Date("2017-01-01"), max=as.Date("2019-12-01"),
                value=c(as.Date("2017-01-01"),as.Date("2019-12-01")), step = NULL, timeFormat = "%b-%y")
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title="Gross Margin Per Month", collapsible = TRUE,
                    plotOutput("GMpermonth", height = 450,click = "plot_click_GMpermonth")
                    ),
                box(title = "Gross Margin Cumulative", collapsible = TRUE,
                    plotOutput("GMpermonthcum",height = 450,click = "plot_click_GMpermonthcum")
                    )
              ),
              fluidRow(
                tabBox(
                  title = "Summary of Selected Time Period (US$M)",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "dashtable_tabs", width = 9,
                  # height = "250px",
                  tabPanel("Summary",
                           fluidRow(
                             column(width=12,
                                    div("This table summarises Kwinana's margin for the selected time period, versus promise, in US$M",
                                        style="background-color: #ece7f2;")
                                    )
                           ),
                           # h5("This table summarises Kwinana's margin for the selected time period, in US$M"),
                           br(),
                           tableOutput("dashtable_summary_table")
                           ),
                  tabPanel("Past Performance",
                           fluidRow(
                             column(width=12,
                                    div("Actual results from start of selected time period to date, by category:",
                                        style="background-color: #ece7f2;font-weight: bold;")
                             )
                           ),
                           # strong("Actual results from start of selected time period to date, by category:"),
                           DT::dataTableOutput("dashtable_past_table"),
                           br(), br(),
                           fluidRow(
                             column(width=12,
                                    div("Detailed view of results. Select rows in the table above to filter categories:",
                                        style="background-color: #ece7f2;font-weight: bold;")
                             )
                           ),
                           # strong("Detailed view of results. Select rows in the table above to filter categories:"),
                           br(),
                           DT::dataTableOutput("dashtable_past_table_details")
                           ),
                  tabPanel("Future",
                           fluidRow(
                           column(width=3,
                             selectInput("dashtable_future_select", label = "Select Data to View:",
                                       choices = list("Forecast" = 1, "Promise" = 2, "Delta" = 3),
                                       selected = 1)
                           ),
                           column(width=3,
                             selectInput("dashtable_future_select_zeros", label = "Show Items With No Impact ?:",
                                         choices = list("No" = 1, "Yes" = 2),
                                         selected = 1)
                           )
                           ),
                           fluidRow(
                             column(width=12,
                                    div("Prediction of future performance in selected time period, by category:",
                                        style="background-color: #ece7f2;font-weight: bold;")
                             )
                           ),
                           # strong("Prediction of future performance in selected time period, by category:"),
                           DT::dataTableOutput("dashtable_future_table"),
                           br(), br(),
                           fluidRow(
                             column(width=12,
                                    div("Detailed view of predictions. Select rows in the table above to filter categories:",
                                        style="background-color: #ece7f2;font-weight: bold;")
                             )
                           ),
                           # strong("Detailed view of predictions. Select rows in the table above to filter categories:"),
                           br(),
                           DT::dataTableOutput("dashtable_future_table_details")
                           ),
                  tabPanel("Selected Month",
                           DT::dataTableOutput("plot_clickedpoints")
                           )
                    )
              )
              # fluidRow(
              #   box(title = "Month details", collapsible = TRUE,
              #       "No Month Selected"
              #       )
              # )
              # fluidRow(
              #   box(tableOutput("plot_clickedpoints")
              #       )
              # )
            ),

      # Second tab content
      tabItem(tabName = "detailview",
              fluidRow(
                    box(title="GM Impact of Activities",width = 9,
                        plotOutput("detailview_deltagmplot",height = 450)    
                        )
              ),
              # fluidRow(
              #       tabBox(
              #                           title = "Details",
              #                           id = "dashtable_tabs", width = 9,
              #                           tabPanel("Volume Bal",
              #                                    fluidRow(
              #                                               DT::dataTableOutput("detailview_volscentable")
              #                                    )
              #                           )
              #       )
              # ),
              fluidRow(
                                  box(title="Scenarios",collapsible = TRUE, width=9,
                                      DT::dataTableOutput("detailview_scentable")
                                      )
              ),
              fluidRow(
                    tabBox(title="Activities",width=12, id="detailview_activ_tabs",
                           tabPanel("Individual",
                                    fluidRow(
                                        DT::dataTableOutput("detailview_acttable")
                                        )
                                    ),
                           tabPanel("By Scenario",
                                    fluidRow(
                                        # DT::dataTableOutput("detailview_acttable")
                                    )
                           ),
                           tabPanel("Group 1",
                                    fluidRow(
                                        # DT::dataTableOutput("detailview_acttable")
                                    )
                           ),
                           tabPanel("Group 2",
                                    fluidRow(
                                                        # DT::dataTableOutput("detailview_acttable")
                                    )
                           ),
                           tabPanel("Group 3",
                                    fluidRow(
                                                        # DT::dataTableOutput("detailview_acttable")
                                    )
                           ),
                           tabPanel("Group 4",
                                    fluidRow(
                                                        # DT::dataTableOutput("detailview_acttable")
                                    )
                           ),
                           tabPanel("Group 5",
                                    fluidRow(
                                                        # DT::dataTableOutput("detailview_acttable")
                                    )
                           ),
                           tabPanel("Group 6",
                                    fluidRow(
                                                        # DT::dataTableOutput("detailview_acttable")
                                    )
                           )
                           )
              )
            ),
      tabItem(tabName = "volbal",
              fluidRow(
                                  tabBox(
                                                      title = "Details",
                                                      id = "dashtable_tabs", width = 9,
                                                      tabPanel("Volume Bal",
                                                               fluidRow(
                                                                                   DT::dataTableOutput("detailview_volscentable")
                                                               )
                                                      )
                                  )
              )
              )
    )
  )
)

server <- function(input, output){
  output$GMpermonth <- renderPlot({
                    data.plot <- filter(dash$gm.mth, id=="Actual" | id == "Forecast" | id=="Promise", measure=="gm")
                    ggplot(data.plot,aes(month,value))+
                    geom_line(aes(color=id))+geom_point(aes(color=id))+
                    scale_y_continuous(breaks=seq(round(min(data.plot$value),0)-1,round(max(data.plot$value),0)+1,by=2))+
                    ylab("USD$M per Month")+xlab("Time")+
                    theme(axis.title.x = element_text(size=17),
                                        axis.text.x  = element_text(size=14),
                                        axis.title.y = element_text(size=17),
                                        axis.text.y = element_text(size=14)
                          )
                    })

  output$GMpermonthcum <- renderPlot({
                    data.plot <- filter(dash$gm.mth, id=="Actual" | id == "Forecast" | id=="Promise", measure=="gm.delta.cum")
                    ggplot(data.plot,aes(month,value))+
                    geom_line(aes(color=id))+geom_point(aes(color=id))+
                                        scale_y_continuous(breaks=seq(round(min(data.plot$value),-1),round(max(data.plot$value),-1),by=5))+
                                        ylab("USD$M Cumulative")+xlab("Time")+
                                        theme(axis.title.x = element_text(size=17),
                                              axis.text.x  = element_text(size=14),
                                              axis.title.y = element_text(size=17),
                                              axis.text.y = element_text(size=14))
                    })

  output$dashtable_summary_table <- renderTable(dash$table.summary)

  output$dashtable_past_table <- DT::renderDataTable({
                    dash$table.past.cat
      # rbind(dashtable[["pastbycat"]],c("TOTAL",colSums(select(dashtable[["pastbycat"]],-(Category)))))
      # dashtable[["pastbycat"]]
    },
    options = list(dom = "t",pageLength = 100),
    # options = list(autoWidth = TRUE),
    rownames = FALSE
    )

  output$dashtable_past_table_details <- DT::renderDataTable({
    if(is.null(input$dashtable_past_table_rows_selected)){
                    table.data <- select(dash$table.past.act,-Activity)
    }else{
      cat <- dash$table.past.cat[input$dashtable_past_table_rows_selected,"Category"]
      table.data <- select(filter(dash$table.past.act,Category %in% as.matrix(cat)),-Activity)
      # browser()
    }
    # browser()
    rbind(table.data,c("TOTAL","TOTAL",colSums(select(table.data,-Title,-Category))))
  },
  options = list(pageLength = 100),
  # options = list(autoWidth = TRUE),
  rownames = FALSE
  )

  output$dashtable_future_table <- DT::renderDataTable({
    table.data.cat <- select(filter(dash$table.future.cat,scen==dash$future.types[
                        as.numeric(input$dashtable_future_select)]),
                        -(scen))
    # browser()
    rbind(table.data.cat,c("TOTAL",colSums(select(table.data.cat,-(Category)))))
    # return(table.data.cat)
    # browser()
  },
  options = list(dom = "t",pageLength = 100),
  rownames = FALSE
  )

  futurecat <- reactive({
    if(is.null(input$dashtable_future_table_rows_selected)){
      unique(dash$table.future.cat$Category)
    }else{
                    # browser()
      filter(dash$table.future.cat,scen==dash$future.types[
        as.numeric(input$dashtable_future_select)])[input$dashtable_future_table_rows_selected,"Category"]
    }
    })

  # observeEvent(futurecat(),print(futurecat()))

  output$dashtable_future_table_details <- DT::renderDataTable({
    if(input$dashtable_future_select_zeros==1){
                    table.data <- filter(dash$table.future.act,
                                         !(Activity %in% dash$table.future.emptact)) 
      # rowidx <- !apply(select(filter(dash$table.future.act, type != "Fcast.to.Prom"),
      #                         -Category,-Title,-Activity,-scen),1,
      #                  FUN=function(x){all(x==0)})
      # table.data <- select(dashtable[["future"]],-(type))
    }else if(input$dashtable_future_select_zeros==2){
                        table.data <- dash$table.future.act
      # rowidx <- rep(TRUE,nrow(filter(dashtable[["future"]], type != "Fcast.to.Prom")))
    }
                      # if(is.null(input$dashtable_future_table_rows_selected)){
                      #                         futurecat <- unique(dash$table.future.cat$Category)
                      #                       }else{
                      #                                       # browser()
                      #                         futurecat <- filter(dash$table.future.cat,scen==dash$future.types[
                      #                           as.numeric(input$dashtable_future_select)])[input$dashtable_future_table_rows_selected,"Category"]
                      #                       }
                      # # browser()
    # rowidxdeltas <- apply(matrix(rowidx, ncol=2),1,any)
    # table.data <- dashtable[["future"]]
    # browser()
    # table.data <- rbind(filter(dashtable[["future"]], type != "fcast.prom")[rowidx,],
    #                     filter(dashtable[["future"]], type == "fcast.prom")[rowidxdeltas,])
    table.data <- arrange(table.data,desc(Total),Category,Title)
    # browser()

    table.data <- select(filter(table.data,scen==dash$future.types[
                  as.numeric(input$dashtable_future_select)] & Category %in% as.vector(t(futurecat()))),-scen,-Activity)
    rbind(table.data,c("TOTAL","TOTAL",colSums(select(table.data,-(Category),-(Title)))))
    # browser()
    },
  options = list(pageLength = 100),
  # options = list(autoWidth = TRUE),
  rownames = FALSE
  )

  # detailview_scentable
  output$detailview_deltagmplot <- renderPlot({
                      if(is.null(input$detailview_scentable_rows_selected)){
                                        chart.scens <- c(fcasttag,promisetag)
                      }else{
                                        chart.scens <- detailview$scen.table[
                                                            input$detailview_scentable_rows_selected,"id"]
                      }
                      data.plot <- select(filter(money$gm.profile.comb,id %in% chart.scens)
                                          ,id,month,gm.delta)
                      ggplot(data.plot,aes(month,gm.delta))+
                                          geom_line(aes(color=id))+geom_point(aes(color=id))+
                                          scale_y_continuous(breaks=seq(round(min(data.plot$gm.delta),0)-1,round(max(data.plot$gm.delta),0)+1,by=2))+
                                          ylab("USD$M per Month")+xlab("Time")+
                                          theme(axis.title.x = element_text(size=17),
                                                axis.text.x  = element_text(size=14),
                                                axis.title.y = element_text(size=17),
                                                axis.text.y = element_text(size=14)
                                          )
  })
  
  output$detailview_scentable <- DT::renderDataTable({
                      detailview$scen.table
  })
  
  output$detailview_acttable <- DT::renderDataTable({
                      detailview$act.table
  },
  options = list(pageLength = 100))
  
  output$detailview_volscentable <- DT::renderDataTable({
                      detailview$scen.vol.table
  },
  options = list(pageLength = 100)
  )
  # output$dashtable_past_table_details <- renderPrint(input$dashtable_past_table_rows_selected)
  # 
  output$plot_clickedpoints <- DT::renderDataTable({
    # For base graphics, we need to specify columns, though for ggplot2,
    # it's usually not necessary.
    res <- nearPoints(dash$gm.mth, input$plot_click_GMpermonth)
    # res$month <- as.POSIXct(as.numeric(res$month), origin="1970-01-01", tz="GMT")
    # browser()
    if (nrow(res) == 0)
      return()
    # table.data.mth <- filter(dash$gm.mth, month == res$month)
    if(res$month %in% mths.with.actuals){
                    table.data.mth <- select(filter(dash$table.past, month == res$month),-gm,-gm.delta.cum,-month) %>%
                                        spread(key=scen, value=gm.delta)
    }else{
                        table.data.mth <- select(filter(dash$table.future, month == res$month),-gm,-gm.delta.cum,-month,-id) %>%
                                            spread(key=scen, value=gm.delta)
    }
    # browser()
    temp.lookup <- match(c("Activity","Category","Title"),colnames(table.data.mth))
    table.data.mth[,-temp.lookup] <- apply(table.data.mth[,-temp.lookup],2,round,1)
    table.data.mth
    # res
  },
  options = list(pageLength = 100),
  rownames = FALSE)
  # 
  # observeEvent(input$plot_click_GMpermonth,{test <<- nearPoints(dashgmpermonth, input$plot_click_GMpermonth,
  #                                                             threshold = 5,
  #                                                             maxpoints = 1)})
}

shinyApp(ui = ui, server = server)