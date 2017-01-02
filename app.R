library(shiny)
library(shinydashboard)
library(DT)

source("datawrangling.R")

ui <- dashboardPage(
  dashboardHeader(title = "Margin Improvement"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
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
                           tableOutput("plot_clickedpoints")
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
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
            )
    )
  )
)

server <- function(input, output){
  output$GMpermonth <- renderPlot(ggplot(filter(dashgmpermonth, variable=="actualgm" | variable == "fcastgm" | variable=="promgm"),aes(month,value))
        +geom_line(aes(color=variable))+geom_point(aes(color=variable)))

  output$GMpermonthcum <- renderPlot(ggplot(filter(dashgmcum, variable=="actualgm" | variable == "fcastgm" | variable=="promgm"),aes(month,value))
                                  +geom_line(aes(color=variable))+geom_point(aes(color=variable)))

  output$dashtable_summary_table <- renderTable(dashtable[["summary"]])
  
  output$dashtable_past_table <- DT::renderDataTable({
      rbind(dashtable[["pastbycat"]],c("TOTAL",colSums(select(dashtable[["pastbycat"]],-(Category)))))
      # dashtable[["pastbycat"]]
    },
    options = list(dom = "t",pageLength = 100),
    # options = list(autoWidth = TRUE),
    rownames = FALSE
    )

  output$dashtable_past_table_details <- DT::renderDataTable({
    if(is.null(input$dashtable_past_table_rows_selected)){
      table.data <- select(dashtable[["past"]],-(activity))
    }else{
      cat <- dashtable[["pastbycat"]][input$dashtable_past_table_rows_selected,"Category"]
      table.data <- filter(select(dashtable[["past"]],-(activity)),Category %in% cat)
    }
    rbind(table.data,c("TOTAL","TOTAL",colSums(select(table.data,-(Category),-(Title)))))
  },
  options = list(pageLength = 100),
  # options = list(autoWidth = TRUE),
  rownames = FALSE
  )
  
  output$dashtable_future_table <- DT::renderDataTable({
    table.data.cat <- select(filter(dashtable[["futurebycat"]],type==dashtable[["future_types"]][
      as.numeric(input$dashtable_future_select)]),-(type))
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
      unique(dashtable[["future"]]$Category)
    }else{
      filter(dashtable[["futurebycat"]],type==dashtable[["future_types"]][
        as.numeric(input$dashtable_future_select)])[input$dashtable_future_table_rows_selected,"Category"]
    }
    })
  
  # observeEvent(futurecat(),print(futurecat()))
  
  output$dashtable_future_table_details <- DT::renderDataTable({
    if(input$dashtable_future_select_zeros==1){
      rowidx <- !apply(select(filter(dashtable[["future"]], type != "fcast.prom"),-(type),-(Category),
                              -(Title),-(Total)),1,FUN=function(x){all(x==0)})
      # table.data <- select(dashtable[["future"]],-(type))
    }else if(input$dashtable_future_select_zeros==2){
      rowidx <- rep(TRUE,nrow(filter(dashtable[["future"]], type != "fcast.prom")))
    }
    rowidxdeltas <- apply(matrix(rowidx, ncol=2),1,any)
    # table.data <- dashtable[["future"]]
    # browser()
    table.data <- rbind(filter(dashtable[["future"]], type != "fcast.prom")[rowidx,],
                        filter(dashtable[["future"]], type == "fcast.prom")[rowidxdeltas,])
    table.data <- arrange(table.data,desc(Total),Category,Title)
    # browser()    
    
    table.data <- select(filter(table.data,type==dashtable[["future_types"]][
                  as.numeric(input$dashtable_future_select)] & Category %in% futurecat()),-(type))
    # browser()
    rbind(table.data,c("TOTAL","TOTAL",colSums(select(table.data,-(Category),-(Title)))))
    },
  options = list(pageLength = 100),
  # options = list(autoWidth = TRUE),
  rownames = FALSE
  )
  
  # output$dashtable_past_table_details <- renderPrint(input$dashtable_past_table_rows_selected)
  
  output$plot_clickedpoints <- renderTable({
    # For base graphics, we need to specify columns, though for ggplot2,
    # it's usually not necessary. 
    res <- nearPoints(dashgmpermonth, input$plot_click_GMpermonth)
    if (nrow(res) == 0)
      return()
    res
  })

  observeEvent(input$plot_click_GMpermonth,{test <<- nearPoints(dashgmpermonth, input$plot_click_GMpermonth,
                                                              threshold = 5,
                                                              maxpoints = 1)})
}

shinyApp(ui = ui, server = server)