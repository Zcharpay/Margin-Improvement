library(dplyr)
library(xlsx)
library(lubridate)
library(reshape2)
library(ggplot2)

filename <- "Test tracker structure7.xlsx"

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
# # Apart from title and description fields, all fields in this table need to be factors
# index <- !grepl("title|description",colnames(metadata))
# metadata[,index] <- lapply(metadata[,index],as.factor)

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

# Dashboard: GM per month chart
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

# Dashboard: GM delta per month
dashgmdeltamonth <- data.frame(month=phasedates,actualgm=numeric(length(phasedates)),
                               fcastgm=numeric(length(phasedates)),promgm=numeric(length(phasedates)))
dashgmdeltamonth[1:nrow(gmactuals),"actualgm"] <- gmactuals$delta.gm
dashgmdeltamonth$fcastgm <- gmdeltaprofile[fcasttag,]
dashgmdeltamonth$promgm <- gmdeltaprofile[promisetag,]
dashgmdeltamonth[dashgmdeltamonth$actualgm==0, "actualgm"] <- NA
dashgmdeltamonth <- transform(dashgmdeltamonth, act.fcast = actualgm - fcastgm,
                              fcast.prom = fcastgm - promgm, act.prom = actualgm - promgm)
dashgmdeltamonth <- melt(dashgmdeltamonth, id="month", na.rm = TRUE)

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

mthwithactuals <- gmactuals$month == dashgmdeltamonth$month
collookback <- filter(dcast(dashgmdeltamonth[mthwithactuals,],variable ~ month),
                      variable == "actualgm" | variable == "fcastgm" | variable == "promgm")
collookback$Realised <- rowSums(as.matrix(select(collookback,-(variable))))
collookfwd <- filter(dashgmdeltamonth[!mthwithactuals,],
                     variable == "actualgm" | variable == "fcastgm" | variable == "promgm")
collookfwd$year <- year(collookfwd$month)
collookfwd <- group_by(collookfwd, variable, year) %>% summarise(sum = sum(value)) %>% ungroup
collookfwd <- dcast(collookfwd, variable ~ year, value.var = "sum")
dashtable <- list(summary = merge(select(collookback,variable,Realised), collookfwd, 
                                  by = "variable", all = TRUE, sort = FALSE))
dashtable[["summary"]]$variable <- sapply(dashtable[["summary"]]$variable,as.character)
dashtable[["summary"]][match(dashtable[["summary"]]$variable,
                             c("promgm","fcastgm","actualgm")),"variable"] <- c("Promise","Forecast","Actual")
dashtable[["summary"]] <- dashtable[["summary"]][match(c("Promise","Forecast","Actual")
                                                       ,dashtable[["summary"]]$variable),]

dashtable[["past_whichmths"]] <- as.logical(rowSums(mapply(FUN = function(x,y){x == y},gmactuals$month,
                                                MoreArgs = list(dashgmpermonth_acty$month))))
dashtable[["past"]] <- select(dashgmpermonth_acty[dashtable[["past_whichmths"]],],-(mergeidx),-(month))
dashtable[["past"]] <- dashtable[["past"]] %>% melt(id.vars = "activity") %>% 
              group_by(activity, variable) %>% summarise(sum = sum(value)) %>% ungroup %>%
              dcast(activity ~ variable, value.var = "sum")
dashtable[["past_dropempties"]] <- with(dashtable[["past"]], actualgm==0 & fcastgm==0 & promgm ==0)
# dashtable[["past_alldata"]] <- dashtable[["past"]]
dashtable[["past"]] <- dashtable[["past"]][!dashtable[["past_dropempties"]],]
dashtable[["past"]]$activity <- sapply(dashtable[["past"]]$activity,as.character)
dashtable[["past"]]$Title <- metadata[dashtable[["past"]]$activity,"title"]
dashtable[["past"]]$Category <- metadata[dashtable[["past"]]$activity,"summary.category"]
colorder <- c(1,match(c("Category","Title"),names(dashtable[["past"]])),2:(ncol(dashtable[["past"]])-2))
dashtable[["past"]] <- dashtable[["past"]][,colorder]
colnames(dashtable[["past"]])[-1:-3] <- c("Actual","Forecast","Promise","Act.to.Fcast","Fcast.to.Prom","Act.to.Prom")
dashtable[["past"]][-(1:3)] <- round(dashtable[["past"]][-(1:3)],1)
dashtable[["past"]] <- arrange(dashtable[["past"]],desc(Act.to.Prom),Title)

dashtable[["pastbycat"]] <- melt(select(dashtable[["past"]],-(activity),-(Title)), id.vars = "Category")
dashtable[["pastbycat"]] <- dashtable[["pastbycat"]] %>% group_by(Category, variable) %>% summarise(sum = sum(value)) %>%
                            ungroup %>% dcast(Category ~ variable, value.var = "sum") %>%
                            arrange(desc(Act.to.Prom),Category)

dashtable[["future_whichmths"]] <- !dashtable[["past_whichmths"]]
dashtable[["future_data"]] <- select(dashgmpermonth_acty[dashtable[["future_whichmths"]],],-(mergeidx),
                                -(actualgm),-(act.fcast),-(act.prom))
dashtable[["future_data"]]$year <- year(dashtable[["future_data"]]$month)
# dashtable[["future_dropempty"]] <- with(dashtable[["future"]],fcastgm==0 & promgm==0)
# dashtable[["future"]] <- dashtable[["future"]][!dashtable[["future_dropempty"]],]
dashtable[["future_data"]] <- select(dashtable[["future_data"]],-(month)) %>% melt(id.vars = c("activity","year")) %>%
                      group_by(activity, year, variable) %>% summarise(sum = sum(value)) %>% ungroup
                      # dcast(activity ~ variable, value.var = "sum")
dashtable[["future_data"]]$activity <- sapply(dashtable[["future_data"]]$activity,as.character)
dashtable[["future_data"]]$Title <- metadata[dashtable[["future_data"]]$activity,"title"]
dashtable[["future_data"]]$Category <- metadata[dashtable[["future_data"]]$activity,"summary.category"]
dashtable[["future_data"]]$sum <- round(dashtable[["future_data"]]$sum,1)
dashtable[["future"]] <- dashtable[["future_data"]] %>% select(-(activity)) %>% 
                                dcast(variable + Category + Title ~ year, value.var="sum")
colnames(dashtable[["future"]]) <- sub("variable","type",colnames(dashtable[["future"]]))
dashtable[["futurebycat"]] <- dashtable[["future"]] %>% melt(id.vars = c("type","Category","Title")) %>%
                                  group_by(type,Category,variable) %>% summarise(sum = sum(value)) %>% ungroup %>%
                                  dcast(type + Category ~ variable, value.var = "sum")
dashtable[["future_types"]] <- c("fcastgm","promgm","fcast.prom")
# dashtable[["future_fcast"]] <- dashtable[["future"]] %>% filter(variable == "fcastgm") %>% 
#                                 select(-(activity),-(variable)) %>% dcast(Category + Title ~ year, value.var="sum")
# dashtable[["future_prom"]] <- dashtable[["future"]] %>% filter(variable == "promgm") %>% 
#                                 select(-(activity),-(variable)) %>% dcast(Category + Title ~ year, value.var="sum")
# dashtable[["future_delta"]] <- dashtable[["future"]] %>% filter(variable == "fcast.prom") %>% 
#                                 select(-(activity),-(variable)) %>% dcast(Category + Title ~ year, value.var="sum")
# dashtable[["future_fcastbycat"]] <- dashtable[["future_fcast"]] %>% melt(id.vars = c("Category","Title")) %>%
#                                 group_by(Category,variable) %>% summarise(sum = sum(value)) %>% ungroup %>%
#                                 dcast(Category ~ variable, value.var = "sum")
# dashtable[["future_prombycat"]] <- dashtable[["future_prom"]] %>% melt(id.vars = c("Category","Title")) %>%
#                                 group_by(Category,variable) %>% summarise(sum = sum(value)) %>% ungroup %>%
#                                 dcast(Category ~ variable, value.var = "sum")
# dashtable[["future_deltabycat"]] <- dashtable[["future_delta"]] %>% melt(id.vars = c("Category","Title")) %>%
#                                 group_by(Category,variable) %>% summarise(sum = sum(value)) %>% ungroup %>%
#                                 dcast(Category ~ variable, value.var = "sum")
# colorder <- c(1,match(c("Category","Title"),names(dashtable[["future"]])),2:(ncol(dashtable[["future"]])-2))
# dashtable[["future"]] <- dashtable[["future"]][,colorder]
# colnames(dashtable[["future"]])[-1:-3] <- c("Forecast","Promise","Fcast.to.Prom")
# dashtable[["future"]] <- arrange(dashtable[["future"]],desc(Fcast.to.Prom),Title)

