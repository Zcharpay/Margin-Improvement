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
                           # selectInput("dashtable_past_select", label = NULL, 
                           #             choices = list("By Category" = 1, "Details" = 2), 
                           #             selected = 1),
                           DT::dataTableOutput("dashtable_past_table"),
                           br(),
                           DT::dataTableOutput("dashtable_past_table_details")
                           ),
                  tabPanel("Future", "Tab content 3"),
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