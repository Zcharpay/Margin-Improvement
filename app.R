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
    sliderInput("slider2", "Number of observations:", min=1, max=100, value=c(25,75))
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
                # box(title = "Summary Of Selected Time Period", collapsible = TRUE,
                #   tableOutput("GMdeltatable")
                #     )
                tabBox(
                  title = "Summary of Selected Time Period",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "dashtable_tabs", width = 9,
                  # height = "250px",
                  tabPanel("Summary",
                           tableOutput("dashtable_summary_table")
                           ),
                  tabPanel("Past Performance",
                           DT::dataTableOutput("dashtable_past_table"),
                           br(),
                           DT::dataTableOutput("dashtable_past_table_details")
                           ),
                  tabPanel("Future",
                           selectInput("dashtable_future_select", label = "Select:",
                                       choices = list("Forecast" = 1, "Promise" = 2, "Delta" = 3),
                                       selected = 1),
                           DT::dataTableOutput("dashtable_future_table"),
                           br(),
                           DT::dataTableOutput("dashtable_future_table_details")
                           ),
                  tabPanel("Selected Month",
                           tableOutput("plot_clickedpoints")
                           )
                    )
              ),
              fluidRow(
                box(title = "Month details", collapsible = TRUE,
                    "No Month Selected"
                    )
              )
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

  output$GMpermonthcum <- renderPlot(ggplot(filter(dashgmpermonth, variable=="actualgm" | variable == "fcastgm" | variable=="promgm"),aes(month,value))
                                  +geom_line(aes(color=variable))+geom_point(aes(color=variable)))

  output$dashtable_summary_table <- renderTable(dashtable[["summary"]])
  
  output$dashtable_past_table <- DT::renderDataTable({
    # if(input$dashtable_past_select==1){
      dashtable[["pastbycat"]]
    # }else{
    # select(dashtable[["past"]],-(activity))
    },
    options = list(dom = "t"),
    # options = list(autoWidth = TRUE),
    rownames = FALSE
    )

  output$dashtable_past_table_details <- DT::renderDataTable({
    if(is.null(input$dashtable_past_table_rows_selected)){
      select(dashtable[["past"]],-(activity))
    }else{
      cat <- dashtable[["pastbycat"]][input$dashtable_past_table_rows_selected,"Category"]
      filter(select(dashtable[["past"]],-(activity)),Category %in% cat)
    }
  },
  options = list(pageLength = 25),
  # options = list(autoWidth = TRUE),
  rownames = FALSE
  )
  
  output$dashtable_future_table <- DT::renderDataTable({
    # browser()
    select(filter(dashtable[["futurebycat"]],type==dashtable[["future_types"]][as.numeric(
      input$dashtable_future_select)]),-(type))
    # if(input$dashtable_future_select==1){
    #   type <- "fcastgm"
    #   select(filter(dashtable[["futurebycat"]],type=="fcastgm"),-(type))
    # }else if(input$dashtable_future_select==2){
    #   select(filter(dashtable[["futurebycat"]],type=="promgm"),-(type))
    # }else if(input$dashtable_future_select==3){
    #   select(filter(dashtable[["futurebycat"]],type=="fcast.prom"),-(type))
    # }
  },
  options = list(dom = "t"),
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
  
  # observeEvent(futurecat,print(futurecat()))
  
  output$dashtable_future_table_details <- DT::renderDataTable({
    # if(is.null(input$dashtable_past_table_rows_selected)){
    #   cat <- unique(dashtable[["future"]]$Category)
    # }else{
    #   cat <- filter(dashtable[["futurebycat"]],type==dashtable[["future_types"]][
    #     as.numeric(input$dashtable_future_select)])[input$dashtable_future_table_rows_selected,"Category"]
    # }
    # print(cat)
    select(filter(dashtable[["future"]],type==dashtable[["future_types"]][
        as.numeric(input$dashtable_future_select)] & Category %in% futurecat()),-(type))
    # if(input$dashtable_future_select==1){
    #   select(filter(dashtable[["future"]],type=="fcastgm" & Category %in% cat),-(type))
    # }else if(input$dashtable_future_select==2){
    #   select(filter(dashtable[["future"]],type=="promgm" & Category %in% cat),-(type))
    # }else if(input$dashtable_future_select==3){
    #   select(filter(dashtable[["future"]],type=="fcast.prom" & Category %in% cat),-(type))
    # }
  },
  options = list(pageLength = 25),
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