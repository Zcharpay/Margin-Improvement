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
                value=c(as.Date("2017-01-01"),as.Date("2019-12-01")), step = NULL, timeFormat = "%b-%y"),
    dateRangeInput("daterange","Date Filter:",start=phasedates[1],
                   end=phasedates.max, min=phasedates[1], max=phasedates.max,
                   startview = "year", format = "M-yy")
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
                    box(title="GM Impact of Activities (per month)",
                        plotOutput("detailview_deltagmplot",height = 450)    
                        ),
                    box(title="GM Impact of Activities (cumulative)",
                        plotOutput("detailview_cumulgmplot",height = 450)    
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
              # fluidRow(
              #                     box(title="Scenarios",collapsible = TRUE, width=9,
              #                         DT::dataTableOutput("detailview_scentable")
              #                         )
              # ),
              fluidRow(
                    tabBox(title="Activities",width=11, id="detailview_activ_tabs",
                           tabPanel("Individual",
                                    fluidRow(
                                        DT::dataTableOutput("detailview_scentable")                
                                    ),
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

str.from <- c(promisetag, fcasttag,"actual.base"); str.to <- c("Promise","Forecast","Actual @ base prices")

server <- function(input, output){

  dash$series <- c("Actual @ base prices","Forecast","Promise")
  
  data.filtered <- reactive({
                      date.start <- input$daterange[1]; day(date.start) <- 1
                      date.end <- input$daterange[2]; day(date.end) <- 1
                      filter(dash$data,month >= date.start, month <= date.end)
                      # browser()
  })
  
  dash$gm.mth <- reactive({
                      data <- fun_dash_gmmth(data.filtered())
                      # for(x in seq_along(str.from)){data$id <- gsub(str.from[x],str.to[x],data$id)}
                      data
  })
  
  series.inscope <- reactive({which.series(dash$gm.mth()$id,dash$series)>0})
  
  dash$is.there.actuals <- reactive({
                      sum(data.filtered()$month <= actuals.latest.mth)>0
                      })
  # observeEvent(data.filtered(),{
  #                     dash$is.there.actuals <<- sum(data.filtered()$month <= actuals.latest.mth)>0
  #                     })
  
  output$GMpermonth <- renderPlot({
                    # browser()
                    # browser()
                    data.plot <- filter(dash$gm.mth(), id %in% dash$series[series.inscope()], measure=="gm")
                    pal <- arrange(filter(colour_pal,colour.series %in% dash$series[series.inscope()]),colour.series)$colour.code
                    # browser()
                    ggplot(data.plot,aes(month,value))+
                    geom_line(aes(color=id))+geom_point(aes(color=id))+
                    scale_y_continuous(breaks=seq(round(min(data.plot$value),0)-1,round(max(data.plot$value),0)+1,by=2))+
                    ylab("USD$M per Month")+xlab("Time")+
                    theme(axis.title.x = element_text(size=17),
                                        axis.text.x  = element_text(size=14),
                                        axis.title.y = element_text(size=17),
                                        axis.text.y = element_text(size=14)
                          )+
                    scale_colour_manual(values=pal)
                    })

  output$GMpermonthcum <- renderPlot({
                    data.plot <- filter(dash$gm.mth(), id %in% dash$series[series.inscope()], measure=="gm.delta.cum")
                    pal <- arrange(filter(colour_pal,colour.series %in% dash$series[series.inscope()]),colour.series)$colour.code
                    ggplot(data.plot,aes(month,value))+
                    geom_line(aes(color=id))+geom_point(aes(color=id))+
                                        scale_y_continuous(breaks=seq(round(min(data.plot$value),-1),round(max(data.plot$value),-1),by=5))+
                                        ylab("USD$M Cumulative")+xlab("Time")+
                                        theme(axis.title.x = element_text(size=17),
                                              axis.text.x  = element_text(size=14),
                                              axis.title.y = element_text(size=17),
                                              axis.text.y = element_text(size=14))+
                                        scale_colour_manual(values=pal)
                    })
# output$dashtable_summary_table <- renderTable(dash$table.summary)
# eventReactive(data.filtered(),{
#              dash$is.there.actuals <- sum(data.filtered()$month <= actuals.latest.mth)>0
#              browser()
#   if(dash$is.there.actuals){
                      dash$table.past <- reactive({
                                          fun_dash_tablepast(data.filtered(),dash$is.there.actuals())
                      })
                      
                      dash$table.past.cat <- reactive({
                                          fun_dash_tablepastcat(dash$table.past(),dash$is.there.actuals())
                      })
                      # browser()
                      dash$table.past.act <- reactive({
                                          fun_dash_tablepastact(dash$table.past(),dash$is.there.actuals())
                      })
                      output$dashtable_past_table <- DT::renderDataTable({
                                        dash$table.past.cat()
                        },
                        options = list(dom = "t",pageLength = 100),
                        # options = list(autoWidth = TRUE),
                        rownames = FALSE
                        )
  # } else {
                      # output$dashtable_past_table <<- renderText("No Actuals Data")
                      # }
  
  # if(dash$is.there.actuals()){
                      output$dashtable_past_table_details <- DT::renderDataTable({
                                          if(is.null(input$dashtable_past_table_rows_selected)){
                                                              table.data <- select(dash$table.past.act(),-Activity)
                                          }else{
                                                              cat <- dash$table.past.cat()[input$dashtable_past_table_rows_selected,"Category"]
                                                              table.data <- select(filter(dash$table.past.act(),Category %in% as.matrix(cat)),-Activity)
                                                              # browser()
                                          }
                                          # browser()
                                          rbind(table.data,c("TOTAL","TOTAL",colSums(select(table.data,-Title,-Category))))
                      },
                      options = list(pageLength = 100),
                      # options = list(autoWidth = TRUE),
                      rownames = FALSE
                      )
  # } else {
                      # output$dashtable_past_table_details <<- renderText("No Actuals Data")
  # }
  dash$table.future <- reactive({
                      fun_dash_tablefuture(data.filtered())
  })
  
  dash$table.future.cat <- reactive({
                      fun_dash_tablefuturecat(dash$table.future())
  })
  
  dash$table.future.act <- reactive({
                      fun_dash_tablefutureact(dash$table.future())
  })
  
  dash$table.future.emptact <- reactive({
                      fun_dash_tablefutureemptact(dash$table.future.act())
  })
                      
  output$dashtable_future_table <- DT::renderDataTable({
    table.data.cat <- select(filter(dash$table.future.cat(),type==dash$future.types[
                        as.numeric(input$dashtable_future_select)]),
                        -(type))
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
      unique(dash$table.future.cat()$Category)
    }else{
                    # browser()
      filter(dash$table.future.cat(),type==dash$future.types[
        as.numeric(input$dashtable_future_select)])[input$dashtable_future_table_rows_selected,"Category"]
    }
    })

  # observeEvent(futurecat(),print(futurecat()))

  output$dashtable_future_table_details <- DT::renderDataTable({
    if(input$dashtable_future_select_zeros==1){
                        table.data <- dash$table.future.act()
                    # table.data <- filter(dash$table.future.act(),
                    #                      !(Activity %in% dash$table.future.emptact())) 
      # rowidx <- !apply(select(filter(dash$table.future.act, type != "Fcast.to.Prom"),
      #                         -Category,-Title,-Activity,-scen),1,
      #                  FUN=function(x){all(x==0)})
      # table.data <- select(dashtable[["future"]],-(type))
    }else if(input$dashtable_future_select_zeros==2){
                        table.data <- dash$table.future.act()
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

    table.data <- select(filter(table.data,type==dash$future.types[
                  as.numeric(input$dashtable_future_select)] & Category %in% as.vector(t(futurecat()))),-type,-Activity)
    rbind(table.data,c("TOTAL","TOTAL",colSums(select(table.data,-(Category),-(Title)))))
    # browser()
    },
  options = list(pageLength = 100),
  # options = list(autoWidth = TRUE),
  rownames = FALSE
  )

  detailview_chart.scens <- reactive({
                      if(is.null(input$detailview_scentable_rows_selected)){
                                          chart.scens <- c(fcasttag,promisetag)
                      }else{
                                          chart.scens <- detailview$scen.table[
                                                              input$detailview_scentable_rows_selected,"id"]
                      }
  })
  
  detailview_chart.data <- reactive({
                      filter(detailview$gmdelta.plot,id %in% detailview_chart.scens())
  })
  
  output$detailview_deltagmplot <- renderPlot({
                      # if(is.null(input$detailview_scentable_rows_selected)){
                      #                   chart.scens <- c(fcasttag,promisetag)
                      # }else{
                      #                   chart.scens <- detailview$scen.table[
                      #                                       input$detailview_scentable_rows_selected,"id"]
                      # }
                      data.plot <- select(detailview_chart.data(),id,title,month,gm.delta)
                      # browser()
                      # data.plot$title <- with(data.plot,factor(title,levels=title[order(title)]))
                      # pal <- filter(colour_pal,id %in% chart.scens)$hex
                      # pal <- arrange(filter(colour_pal,id %in% chart.scens),colour.series)$colour.code
                      pal <- colour_pal[unique(arrange(data.plot,title)$id),"colour.code"]
                      # browser()
                      ggplot(data.plot,aes(month,gm.delta))+
                                          geom_line(aes(color=title))+geom_point(aes(color=title))+
                                          scale_y_continuous(breaks=seq(round(min(data.plot$gm.delta),0)-1,round(max(data.plot$gm.delta),0)+1,by=0.5))+
                                          ylab("Delta GM, USD$M per Month")+xlab("Time")+
                                          theme(axis.title.x = element_text(size=17),
                                                axis.text.x  = element_text(size=14),
                                                axis.title.y = element_text(size=17),
                                                axis.text.y = element_text(size=14)
                                          )+
                                          scale_colour_manual(values=pal)
  })
  # detailview_cumulgmplot
  output$detailview_cumulgmplot <- renderPlot({
                      data.plot <- select(detailview_chart.data(),id,title,month,gm.delta.cum)
                      pal <- colour_pal[unique(arrange(data.plot,title)$id),"colour.code"]
                      ggplot(data.plot,aes(month,gm.delta.cum))+
                                          geom_line(aes(color=title))+geom_point(aes(color=title))+
                                          scale_y_continuous(breaks=seq(round(min(data.plot$gm.delta.cum),-1),round(max(data.plot$gm.delta.cum),-1),by=5))+
                                          ylab("Cumulative Delta GM, USD$M")+xlab("Time")+
                                          theme(axis.title.x = element_text(size=17),
                                                axis.text.x  = element_text(size=14),
                                                axis.title.y = element_text(size=17),
                                                axis.text.y = element_text(size=14)
                                          )+
                                          scale_colour_manual(values=pal)
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
    res <- nearPoints(dash$gm.mth(), input$plot_click_GMpermonth)
    # res$month <- as.POSIXct(as.numeric(res$month), origin="1970-01-01", tz="GMT")
    # browser()
    if (nrow(res) == 0)
      return()
    # table.data.mth <- filter(dash$gm.mth, month == res$month)
    if(res$month %in% mths.with.actuals){
                    table.data.mth <- select(filter(dash$table.past, month == res$month),-gm,-gm.delta.cum,-month) %>%
                                        spread(key=type, value=gm.delta)
    }else{
                        table.data.mth <- select(filter(dash$table.future, month == res$month),-gm,-gm.delta.cum,-month,-id) %>%
                                            spread(key=type, value=gm.delta)
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