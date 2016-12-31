library(dplyr)
library(xlsx)
library(lubridate)
library(reshape2)

filename <- "Test tracker structure6.xlsx"

## Read price data from the excel workbook, tidy up, convert to numeric matrix
prices <- read.xlsx(file=filename,sheetName = "Prices",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
prices <- prices[rowSums(is.na(prices))<ncol(prices),colSums(is.na(prices))<nrow(prices)]
rnames <- as.character(prices[,1]); cnames <- colnames(prices)[-1]
prices <- as.matrix(prices[,-1],nrow=nrow(prices))
colnames(prices) <- cnames; rownames(prices) <- rnames
prices <- t(prices)

## Read base case volume balance data from the excel workbook, tidy up, convert to numeric matrix
volbasecases <- read.xlsx(file=filename,sheetName = "Vol Base Cases",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
volbasecases <- volbasecases[rowSums(is.na(volbasecases))<ncol(volbasecases),
                             colSums(is.na(volbasecases))<nrow(volbasecases)]
rnames <- as.character(volbasecases[,1]); cnames <- colnames(volbasecases)[-1]
volbasecases <- as.matrix(volbasecases[,-1],nrow=nrow(volbasecases))
colnames(volbasecases) <- cnames; rownames(volbasecases) <- rnames
volbasecases <- t(volbasecases)

## Read activity volume balance data from the excel workbook, tidy up, convert to numeric matrix
volactcases <- read.xlsx(file=filename,sheetName = "Vol Activity Cases",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
volactcases <- volactcases[rowSums(is.na(volactcases))<ncol(volactcases),colSums(is.na(volactcases))<nrow(volactcases)]
rnames <- as.character(volactcases[,1]); cnames <- colnames(volactcases)[-1]
volactcases <- as.matrix(volactcases[,-1],nrow=nrow(volactcases))
colnames(volactcases) <- cnames; rownames(volactcases) <- rnames
volactcases <- t(volactcases)

## Read actual results from the excel workbook, tidy up
actuals <- read.xlsx(file=filename,sheetName = "Actuals",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
actuals <- actuals[rowSums(is.na(actuals))<ncol(actuals),colSums(is.na(actuals))<nrow(actuals)]
actuals <- actuals[grepl("^VA|^MA",actuals$activity.id),]
actuals[is.na(actuals)] <- 0
volactuals <- as.matrix(select(actuals,-(month),-(activity.id)))
rownames(volactuals) <- paste(format(actuals$month,"%b-%y"),actuals$activity.id,sep="|")

## Read non-volume-balance GM items from the excel workbook, tidy up, convert to numeric matrix
gmadj <- read.xlsx(file=filename,sheetName = "GM Manual Adjustments",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
gmadj <- gmadj[rowSums(is.na(gmadj))<ncol(gmadj),colSums(is.na(gmadj))<nrow(gmadj)]
rnames <- as.character(gmadj[,1]); cnames <- colnames(gmadj)[-1]
gmadj <- as.matrix(gmadj[,-1],nrow=nrow(gmadj))
colnames(gmadj) <- cnames; rownames(gmadj) <- rnames

## Read activity phasing data from the excel workbook, tidy up, convert to numeric matrix
activityphasing <- read.xlsx(file=filename,sheetName = "Activity Phasing",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
activityphasing <- activityphasing[rowSums(is.na(activityphasing))<ncol(activityphasing),
                                   colSums(is.na(activityphasing))<nrow(activityphasing)]
rnames <- as.character(activityphasing[,1]); cnames <- colnames(activityphasing)[-1]
activityphasing <- as.matrix(activityphasing[,-1],nrow=nrow(activityphasing))
colnames(activityphasing) <- cnames; rownames(activityphasing) <- rnames
colnames(activityphasing) <- gsub("^X","",colnames(activityphasing))
phasedates <- as.POSIXct(as.numeric(colnames(activityphasing))*(60*60*24), origin="1899-12-30", tz="GMT")
colnames(activityphasing) <- format.POSIXct(phasedates,format = "%b-%y")

## Read metadata table from the excel workbook
metadata <- read.xlsx(file=filename,sheetName = "metadata",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
metadata <- metadata[rowSums(is.na(metadata))<ncol(metadata),colSums(is.na(metadata))<nrow(metadata)]
rnames <- as.character(metadata[,1]); cnames <- tolower(colnames(metadata)[-1])
metadata <- metadata[,-1]
colnames(metadata) <- cnames; rownames(metadata) <- rnames
# Apart from title and description fields, all fields in this table need to be factors
index <- !grepl("title|description",colnames(metadata))
metadata[,index] <- lapply(metadata[,index],as.factor)

## Read scenario configuration table from the excel workbook
scenarioconfig <- read.xlsx(file=filename,sheetName = "Scenario Config",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
scenarioconfig <- scenarioconfig[rowSums(is.na(scenarioconfig))<ncol(scenarioconfig),colSums(is.na(scenarioconfig))<nrow(scenarioconfig)]
rnames <- as.character(scenarioconfig[,1]); cnames <- tolower(colnames(scenarioconfig)[-1])
scenarioconfig <- scenarioconfig[,-1]
colnames(scenarioconfig) <- cnames; rownames(scenarioconfig) <- rnames
idxscenarioconfig.scen <- !grepl("Actual",rownames(scenarioconfig))
scenarios <- scenarioconfig[idxscenarioconfig.scen,]

## Read scenario detail table from the excel workbook
scenariodetail <- read.xlsx(file=filename,sheetName = "Scenario",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
scenariodetail <- scenariodetail[rowSums(is.na(scenariodetail))<ncol(scenariodetail),colSums(is.na(scenariodetail))<nrow(scenariodetail)]
rownames(scenariodetail) <- as.character(scenariodetail[,1])


################## Intermediate data ##############################
## Data wrangling in this section to setup everything required to calculate final results
# Identify parent scenario for the individual activities in scenario detail
scenariodetail$scenario <- sapply(scenariodetail$scenariodetail,
                                              FUN=function(x){strsplit(x,split="-")[[1]][1]})
scenariodetail$basevolid <- scenarios[scenariodetail$scenario,"base.vol.id"]
# Process information for the activities with volume balance entries
subsetscenario <- scenariodetail[grepl("^VA",scenariodetail$activity.id),]
volscenariodetail <- with(subsetscenario,volbasecases[basevolid,]+volactcases[activity.id,])
rownames(volscenariodetail) <- subsetscenario$activity.id
volscenario_base <- volbasecases[scenarios$base.vol.id,]
volscenario_activity <- as.matrix(aggregate(volactcases[subsetscenario$activity.id,],list(paste(subsetscenario$scenario,sep="")),sum)[,-1])
volscenario <- volscenario_base + volscenario_activity
rownames(volscenario) <- rownames(scenarios); rownames(volscenario_base) <- rownames(scenarios)
rownames(volscenario_activity) <- rownames(scenarios)

#Process phasing information
# subsetphasing <- activityphasing[grepl("^VA",rownames(activityphasing)),]
scenariophasing <- activityphasing[scenariodetail$activity.id,]
rownames(scenariophasing) <- rownames(scenariodetail)

################## Output data ##############################
## Calculate the various outputs required
# calculate GM for base cases
gmbasecases <- (volbasecases[scenarioconfig$base.vol.id,]*prices[scenarioconfig$base.price.id,])*1000/1e6
gmbasecases <- rowSums(gmbasecases[,-1])-gmbasecases[,1]
names(gmbasecases) <- rownames(scenarioconfig)

# calculate GM (per month) for the scenario details (activities)
gmmonthlyactivities <- matrix(0,nrow=nrow(scenariodetail),ncol=1,dimnames=list(NULL,"delta.gm"),byrow=TRUE)
gmmonthlyactivities_items <- (volactcases[subsetscenario$activity.id,]*
                                prices[scenarios[subsetscenario$scenario,"base.price.id"],])*1000/1e6
rownames(gmmonthlyactivities) <- scenariodetail$scenariodetail
rownames(gmmonthlyactivities_items) <- subsetscenario$scenariodetail
scenariotags_vol <- subsetscenario$scenariodetail
subsetscenarioman <- scenariodetail[grepl("^MA",scenariodetail$activity.id),]
scenariotags_man <- subsetscenarioman$scenariodetail
gmmonthlyactivities[scenariotags_vol,"delta.gm"] <- rowSums(gmmonthlyactivities_items[,-1])-
                                                    gmmonthlyactivities_items[,1]
gmmonthlyactivities[scenariotags_man,"delta.gm"] <- gmadj[subsetscenarioman$activity.id,]
gmmonthlyactivities <- cbind(gmmonthlyactivities, base.gm = gmbasecases[scenariodetail$scenario])
gmmonthlyactivities <- as.matrix(transform(gmmonthlyactivities, gm = delta.gm + base.gm))

# calculate GM profiles over time
gmdeltaprofile_act <- gmmonthlyactivities[,"delta.gm"] * scenariophasing
gmdeltaprofile <- aggregate(gmdeltaprofile_act,list(paste(scenariodetail$scenario,sep="")),sum)
rownames(gmdeltaprofile) <- gmdeltaprofile[,1]; gmdeltaprofile <- gmdeltaprofile[,-1]
gmdeltaprofile <- as.matrix(gmdeltaprofile)
gmprofilescenario <- gmdeltaprofile + gmbasecases[rownames(gmdeltaprofile)]
gmdeltaprofile_cum_act <- t(apply(gmdeltaprofile_act,1,cumsum))
gmdeltaprofile_cum <- t(apply(gmdeltaprofile,1,cumsum))
gmprofilescenario_cum <- t(apply(gmprofilescenario,1,cumsum))

# Calculate GM for actuals
gmdelta_actuals_items <- (volactuals*prices[scenarioconfig[rep("Actual",times=nrow(volactuals)),"base.price.id"],])*1000/1e6
gmdelta_actuals <- rowSums(gmdelta_actuals_items[,-1])- gmdelta_actuals_items[,1]
gmdelta_actuals <- cbind(gmdelta_actuals, base.gm = gmbasecases[rep("Actual",times=length(gmdelta_actuals))])
colnames(gmdelta_actuals)[1] <- "delta.gm"
gmdelta_actuals <- as.matrix(transform(gmdelta_actuals, gm = delta.gm + base.gm))
actuals <- cbind(actuals,gmdelta_actuals)
gmactuals <- aggregate(x=actuals[,"delta.gm"],by=list(paste(actuals$month,sep="")),FUN=sum)
colnames(gmactuals) <- c("month","delta.gm"); gmactuals$month <- ymd(gmactuals$month)
gmactuals$base.gm <- gmbasecases[rep("Actual",times=nrow(gmactuals))]
gmactuals <- transform(gmactuals, gm = base.gm + delta.gm)
gmactuals_cum <- gmactuals
gmactuals_cum[,-1] <- apply(gmactuals[,-1],2,cumsum)
# print(ggplot(actuals,aes(month,gm))+geom_point()+scale_x_date(date_labels = "%b-%Y",date_breaks = "1 month"))
# print(ggplot(gmactuals,aes(month,gm))+geom_point()+scale_x_date(date_labels = "%b-%Y",date_breaks = "1 month"))
################## Data for App Output Elements ##############################
## Marshal the data required for the app's output elements e.g. charts etc

# Dashboard: GM per month chart and table
promisetag <- rownames(scenarios)[scenarios$type=="latest.promise"]
fcasttag <- rownames(scenarios)[scenarios$type=="latest.forecast"]
dashgmpermonth <- data.frame(month=phasedates,actualgm=numeric(length(phasedates)),
                             fcastgm=numeric(length(phasedates)),promgm=numeric(length(phasedates)))
dashgmpermonth[1:nrow(gmactuals),"actualgm"] <- gmactuals$gm
dashgmpermonth$fcastgm <- gmprofilescenario[fcasttag,]
dashgmpermonth$promgm <- gmprofilescenario[promisetag,]
dashgmpermonth[dashgmpermonth$actualgm==0, "actualgm"] <- NA
dashgmpermonth <- transform(dashgmpermonth, act.fcast = actualgm - fcastgm,
                            fcast.prom = fcastgm - promgm, act.prom = actualgm - promgm)
dashgmpermonth <- melt(dashgmpermonth, id="month", na.rm = TRUE)

# Dashboard: GM cumulative chart
dashgmcum <- data.frame(month=phasedates,actualgm=numeric(length(phasedates)),
                             fcastgm=numeric(length(phasedates)),promgm=numeric(length(phasedates)))
dashgmcum[1:nrow(gmactuals_cum),"actualgm"] <- gmactuals_cum$gm
dashgmcum$fcastgm <- gmprofilescenario_cum[fcasttag,]
dashgmcum$promgm <- gmprofilescenario_cum[promisetag,]
dashgmcum[dashgmcum$actualgm==0, "actualgm"] <- NA
dashgmcum <- transform(dashgmcum, act.fcast = actualgm - fcastgm,
                            fcast.prom = fcastgm - promgm, act.prom = actualgm - promgm)
dashgmcum <- melt(dashgmcum, id="month", na.rm = TRUE)

# Dashboard: GM per month by activity
activitytags <- unique(c(actuals$activity.id, scenariodetail$activity.id))
len <- length(phasedates)*length(activitytags)
# dashgmpermonth_acty <- data.frame(month=rep(phasedates,each=length(activitytags)),activity=activitytags,
#                                   actualgm=numeric(len),fcastgm=numeric(len),promgm=numeric(len))
dashgmpermonth_acty <- data.frame(month=rep(phasedates,each=length(activitytags)),activity=activitytags)
dashgmpermonth_acty$mergeidx <- with(dashgmpermonth_acty, paste(format(month,"%b-%y"),activity,sep="|"))
actuals$mergeidx <- with(actuals, paste(format(month,"%b-%y"),activity.id,sep="|"))
dashgmpermonth_acty <- merge(x = dashgmpermonth_acty, y = subset(actuals,select=c(mergeidx,delta.gm)),
               by = "mergeidx" ,all = TRUE, sort = FALSE)
colnames(dashgmpermonth_acty) <- sub("delta.gm","actualgm",colnames(dashgmpermonth_acty))
gmdeltaprofile_act_melt <- melt(gmdeltaprofile_act)
colnames(gmdeltaprofile_act_melt) <- c("scenariodetail","month","delta.gm")
gmdeltaprofile_act_melt$activity <- scenariodetail[gmdeltaprofile_act_melt$scenariodetail,"activity.id"]
gmdeltaprofile_act_melt$mergeidx <- with(gmdeltaprofile_act_melt, paste(month,activity,sep="|"))
fcastidx <- gmdeltaprofile_act_melt$scenariodetail %in% subset(scenariodetail, scenario==fcasttag)$scenariodetail
promidx <- gmdeltaprofile_act_melt$scenariodetail %in% subset(scenariodetail, scenario==promisetag)$scenariodetail
dashgmpermonth_acty <- merge(x = dashgmpermonth_acty, y = gmdeltaprofile_act_melt[fcastidx,c("mergeidx","delta.gm")],
                             by = "mergeidx" ,all = TRUE, sort = FALSE)
colnames(dashgmpermonth_acty) <- sub("delta.gm","fcastgm",colnames(dashgmpermonth_acty))
dashgmpermonth_acty <- merge(x = dashgmpermonth_acty, y = gmdeltaprofile_act_melt[promidx,c("mergeidx","delta.gm")],
                             by = "mergeidx" ,all = TRUE, sort = FALSE)
colnames(dashgmpermonth_acty) <- sub("delta.gm","promgm",colnames(dashgmpermonth_acty))
dashgmpermonth_acty <- arrange(dashgmpermonth_acty,month,activity)
dashgmpermonth_acty[is.na(dashgmpermonth_acty)] <- 0
dashgmpermonth_acty <- transform(dashgmpermonth_acty, act.fcast = actualgm - fcastgm,
                                 fcast.prom = fcastgm - promgm, act.prom = actualgm - promgm)

# print(ggplot(filter(dashgmpermonth, variable=="actualgm" | variable == "fcastgm" | variable=="promgm"),aes(month,value))
#       +geom_line(aes(color=variable))+geom_point(aes(color=variable)))
#       # +scale_x_date(date_labels = "%b-%Y",date_breaks = "1 month"))

library(shiny)
library(shinydashboard)
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
                box(tableOutput("plot_clickedpoints"))
                
                # box(
                #   title = "Controls",
                #   sliderInput("slider", "Number of observations:", 1, 100, 50)
                # )
              )
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