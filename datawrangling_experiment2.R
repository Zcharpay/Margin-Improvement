library(dplyr)
library(xlsx)
library(lubridate)
library(reshape2)
library(ggplot2)
library(tidyr)

options(stringsAsFactors = FALSE)
filename <- "Test tracker structure10.xlsx"

## Read price data from the excel workbook, tidy up, convert to numeric matrix
prices <- read.xlsx(file=filename,sheetName = "Prices",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
prices <- prices[rowSums(is.na(prices))<ncol(prices),colSums(is.na(prices))<nrow(prices)]
rnames <- as.character(prices[,1]); cnames <- colnames(prices)[-1]
prices <- as.matrix(prices[,-1],nrow=nrow(prices))
colnames(prices) <- cnames; rownames(prices) <- rnames
prices <- t(prices)

volstruc <- read.xlsx(file=filename,sheetName = "Vol Structure",stringsAsFactors=FALSE,header=FALSE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
volstruc <- volstruc[rowSums(is.na(volstruc))<ncol(volstruc),colSums(is.na(volstruc))<nrow(volstruc)]
rnames <- as.character(volstruc[,1]); 
volstruc <- as.matrix(volstruc[,-1],nrow=nrow(volstruc)); rownames(volstruc) <- rnames

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
gmadj <- rename(gmadj, id = GMmanualadj)
rownames <- gmadj$id
# rnames <- as.character(gmadj[,1]); cnames <- colnames(gmadj)[-1]
# gmadj <- as.matrix(gmadj[,-1],nrow=nrow(gmadj))
# colnames(gmadj) <- cnames; rownames(gmadj) <- rnames

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
rnames <- as.character(scenarioconfig[,1]); cnames <- tolower(colnames(scenarioconfig))
# scenarioconfig <- scenarioconfig[,-1]
colnames(scenarioconfig) <- cnames; rownames(scenarioconfig) <- rnames
colnames(scenarioconfig)[1] <- "id"
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

default.volbase <- filter(scenarioconfig,type=="latest.forecast")$base.vol.id
default.prices <- filter(scenarioconfig,type=="latest.forecast")$base.price.id

volumes <- list()
volumes$total <- data.frame(volbasecases) %>% mutate (id = rownames(volbasecases))
volumes$delta <- data.frame(volactcases) %>% mutate (id = rownames(volactcases))
rownames(volumes$total) <- volumes$total$id; rownames(volumes$delta) <- volumes$delta$id
volumes$delta.scen <- volumes$delta[match(subsetscenario$activity.id,volumes$delta$id),] %>%
                    mutate(id = subsetscenario$'scenariodetail', scen = subsetscenario$scenario)
volumes$delta.scen <- aggregate(select(volumes$delta.scen,-id,-scen),by=list(volumes$delta.scen$scen),FUN=sum) %>%
                    rename(id = Group.1) %>% rbind(select(volumes$delta.scen,-scen),.)
rownames(volumes$delta.scen) <- volumes$delta.scen$id
# volumes$total.scen <- volumes$delta.scen[grepl("^SC(.*)-",volumes$delta.scen$id),]

volumes$total.scen.comps <- volumes$delta.scen$id[grepl("^SC(.*)-",volumes$delta.scen$id)]
volumes$total.scen <- as.matrix(select(volumes$total[subsetscenario[volumes$total.scen.comps,"basevolid"],],-id))
# mutate(id = volumes$total.scen.comps)
volumes$total.scen <- (volumes$total.scen + as.matrix(select(volumes$delta.scen[volumes$total.scen.comps,],-id))) %>%
                    data.frame %>% mutate(id = volumes$total.scen.comps) 
# scen = scenariodetail[volumes$total.scen.comps,"scenario"])
temp.join <- inner_join(volumes$delta.scen,scenarioconfig,by="id")
volumes$total.scen <- (select(volumes$delta.scen,-id)[temp.join$id,] + 
                    select(volumes$total[temp.join$base.vol.id,],-id)) %>%
                    cbind(.,id=temp.join$id) %>% 
                    rbind(volumes$total.scen,.)    
rownames(volumes$total.scen) <- volumes$total.scen$id

len <- nrow(volumes$delta)
volumes$total <- (as.matrix(select(volumes$delta,-id)) + 
                    as.matrix(select(volumes$total[rep(default.volbase,times=len),],-id))) %>% data.frame %>%
                    mutate(id = volumes$delta$'id') %>%
                    rbind(volumes$total,.)
rownames(volumes$total) <- volumes$total$id

# volscenariodetail <- with(subsetscenario,volbasecases[basevolid,]+volactcases[activity.id,])
# rownames(volscenariodetail) <- subsetscenario$activity.id
# volscenario_base <- volbasecases[scenarios$base.vol.id,]
# volscenario_activity <- as.matrix(aggregate(volactcases[subsetscenario$activity.id,],list(paste(subsetscenario$scenario,sep="")),sum)[,-1])
# volscenario <- volscenario_base + volscenario_activity
# rownames(volscenario) <- rownames(scenarios); rownames(volscenario_base) <- rownames(scenarios)
# rownames(volscenario_activity) <- rownames(scenarios)

#Process phasing information
# subsetphasing <- activityphasing[grepl("^VA",rownames(activityphasing)),]
scenariophasing <- activityphasing[scenariodetail$activity.id,]
rownames(scenariophasing) <- rownames(scenariodetail)
scenariophasing <- data.frame(scenariophasing,id=scenariodetail$scenariodetail)
activityphasing <- data.frame(activityphasing,id=rownames(activityphasing))

################## Output data ##############################
## Calculate the various outputs required
# calculate GM for base cases
# gmbasecases <- (volbasecases[scenarioconfig$base.vol.id,]*prices[scenarioconfig$base.price.id,])*1000/1e6
# gmbasecases <- rowSums(gmbasecases[,-1])-gmbasecases[,1]
# names(gmbasecases) <- rownames(scenarioconfig)
# scen.tags <- match(volumes$delta$id,subsetscenario$activity.id); scen.tags <- scen.tags[!is.na(scen.tags)]
# temp.sub <- select(volumes$delta,-id)
money <- list()
temp.join <- inner_join(volumes$delta.scen, scenariodetail, by = c("id" = "scenariodetail"))
money$inout.delta.scen <- as.matrix(select(volumes$delta.scen[temp.join$id,],-id))*
                    prices[scenarioconfig[temp.join$scenario,"base.price.id"],]*1000/1e6
temp.join <- inner_join(volumes$delta.scen, scenarioconfig, by = "id")
money$inout.delta.scen <- rbind(money$inout.delta.scen,
                                as.matrix(select(volumes$delta.scen[temp.join$id,],-id))*
                                                    prices[scenarioconfig[temp.join$id,"base.price.id"],]*1000/1e6)

temp.join <- inner_join(volumes$total.scen, scenariodetail, by = c("id" = "scenariodetail"))
money$inout.total.scen <- as.matrix(select(volumes$total.scen[temp.join$id,],-id))*
                    prices[scenarioconfig[temp.join$scenario,"base.price.id"],]*1000/1e6
temp.join <- inner_join(volumes$delta.scen, scenarioconfig, by = "id")
money$inout.total.scen <- rbind(money$inout.total.scen,
                                as.matrix(select(volumes$total.scen[temp.join$id,],-id))*
                                                    prices[scenarioconfig[temp.join$id,"base.price.id"],]*1000/1e6)


money$inout.delta <- (select(volumes$delta,-id)*prices[rep(default.prices,nrow(volumes$delta)),]*1000/1e6) %>%
                    mutate(id = volumes$delta$'id')
temp.lookup <- grepl("^VA",volumes$total$id)
money$inout.total <- select(volumes$total[temp.lookup,],-id)*prices[rep(default.prices,times=sum(temp.lookup)),]*1000/1e6
money$inout.total$id <- volumes$'total'$'id'[temp.lookup]
money$default.base.gm <- rowSums(select(volumes$total[default.volbase,],-id)*prices[default.prices,]*(1000/1e6)
                                 *t(volstruc))

money$inout.base.scen <- (select(volumes$total[scenarios$base.vol.id,],-id) * prices[scenarios$base.price.id,]*1000/1e6) %>%
                    data.frame %>% mutate(id = scenarios$'id')
volstruc.mat <- matrix(rep(t(volstruc),times=nrow(money$inout.base.scen)),nrow=nrow(money$inout.base.scen),byrow=TRUE)
money$gm.base.scen <- rowSums(select(money$inout.base.scen,-id)*volstruc.mat) %>% 
                    data.frame(gm=., id=scenarios$'id', row.names=scenarios$'id')

volstruc.mat <- matrix(rep(t(volstruc),times=nrow(money$inout.delta.scen)),nrow=nrow(money$inout.delta.scen),byrow=TRUE)
money$gm.month <- rowSums(money$inout.delta.scen*volstruc.mat) %>% data.frame
colnames(money$gm.month) <- "gm.delta"; money$gm.month$id <- rownames(money$gm.month)
money$gm.month$gm <- rowSums(money$inout.total.scen*volstruc.mat)

volstruc.mat <- matrix(rep(t(volstruc),times=nrow(money$inout.delta)),nrow=nrow(money$inout.delta),byrow=TRUE)
money$gm.month <- rbind(money$gm.month,
                        data.frame(gm.delta=rowSums(select(money$inout.delta,-id)*volstruc.mat),
                                   id=money$inout.delta$id, 
                                   gm=rowSums(select(money$inout.total,-id)*volstruc.mat)))
money$gm.month <- rbind(money$gm.month,
                        data.frame(gm.delta=gmadj$GM.impact.monthly,
                                   id=gmadj$id, 
                                   gm=gmadj$GM.impact.monthly + money$default.base.gm, row.names = gmadj$id))
temp.join <- inner_join(scenariodetail,gmadj,by=c("activity.id" = "id"))
money$gm.month <- rbind(money$gm.month,
                        data.frame(gm.delta=temp.join$GM.impact.monthly,
                                   id=temp.join$scenariodetail,
                                   gm=temp.join$GM.impact.monthly + money$gm.base.scen[temp.join$scenario,"gm"],
                                   row.names = temp.join$'scenariodetail'))


# Phasing of delta GM
temp.join <- inner_join(select(money$gm.month,-gm),scenariophasing,by="id")
money$gm.profile.delta <- apply(select(temp.join,-id,-gm.delta),2,FUN = function(x){x*temp.join$gm.delta}) %>%
                    data.frame %>% mutate(id = temp.join$'id')
temp.join <- inner_join(select(money$gm.month,-gm),activityphasing,by="id")
money$gm.profile.delta <- apply(select(temp.join,-id,-gm.delta),2,FUN = function(x){x*temp.join$gm.delta}) %>%
                    data.frame %>% mutate(id = temp.join$'id') %>%
                    rbind(money$gm.profile.delta,.)
money$gm.profile.delta.cum <- t(apply(select(money$gm.profile.delta,-id),1,cumsum)) %>%
                    data.frame %>% mutate(id = money$gm.profile.delta$'id')

# Phasing of GM
temp.join <- inner_join(select(money$gm.month,-gm.delta),scenariophasing,by="id")
money$gm.profile <- apply(select(temp.join,-id,-gm),2,FUN = function(x){x*temp.join$gm}) %>%
                    data.frame %>% mutate(id = temp.join$'id')
temp.join <- inner_join(select(money$gm.month,-gm.delta),activityphasing,by="id")
money$gm.profile <- apply(select(temp.join,-id,-gm),2,FUN = function(x){x*temp.join$gm}) %>%
                    data.frame %>% mutate(id = temp.join$'id') %>%
                    rbind(money$gm.profile,.)
money$gm.profile.cum <- t(apply(select(money$gm.profile,-id),1,cumsum)) %>%
                    data.frame %>% mutate(id = money$gm.profile$'id')