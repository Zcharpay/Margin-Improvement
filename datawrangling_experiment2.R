library(dplyr)
library(xlsx)
library(lubridate)
library(reshape2)
library(ggplot2)
library(tidyr)
library(randomcoloR)

options(stringsAsFactors = FALSE)
filename <- "Test tracker structure17.xlsx"

################## Read Inputs from File ##############################
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
actuals$id <- paste(format(actuals$month,"%b-%y"),actuals$activity.id,sep="|")
volactuals <- select(actuals,-(month),-(activity.id),-id,-man.adj)
# rownames(volactuals) <- paste(format(actuals$month,"%b-%y"),actuals$activity.id,sep="|")

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
phasedates.max <- phasedates[length(phasedates)] %m+% months(1); phasedates.max <- phasedates.max - (24*60*60)
colnames(activityphasing) <- format.POSIXct(phasedates,format = "%b-%y")

## Read metadata table from the excel workbook
metadata <- read.xlsx(file=filename,sheetName = "metadata",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
metadata <- metadata[rowSums(is.na(metadata))<ncol(metadata),colSums(is.na(metadata))<nrow(metadata)]
rnames <- as.character(metadata[,1])
# cnames <- tolower(colnames(metadata)[-1])
# metadata <- metadata[,-1]
metadata <- rename(metadata, id=metadata)
# colnames(metadata) <- cnames
rownames(metadata) <- rnames; colnames(metadata) <- tolower(colnames(metadata))
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
scenarioconfig$scen.num <- gsub("SC","",scenarioconfig$id)
scenarioconfig$colour.series = scenarioconfig$title
scenariogroups <- data.frame(type=character(),level=character())
scenariogroup_titles <- c("bip"="BIP","v.o"="V&O","project"="Project","summary.category"="Category")
for(type in names(scenariogroup_titles)){
                    scenariogroups <- rbind(scenariogroups,data.frame(
                                        type=type, level=unique(metadata[,type])
                    ))
}
scenario_maxid <- max(as.numeric(filter(scenarioconfig,scen.num!="Actual @ base prices")$scen.num))
scenariogroups <- filter(scenariogroups,level!="No",level!="System") %>%
                    mutate(title = paste(scenariogroup_titles[type]," (",level,")",sep=""),
                           num = seq_along(type)+scenario_maxid) %>%
                    mutate(id = paste("SC",num,sep=""))

fcasttag <- filter(scenarioconfig,type=="latest.forecast")$id
promisetag <- filter(scenarioconfig,type=="latest.promise")$id

scenarioconfig <- rbind(scenarioconfig,
                        data.frame(id=scenariogroups$id,
                                   base.vol.id=scenarioconfig[fcasttag,"base.vol.id"],
                                   base.price.id=scenarioconfig[fcasttag,"base.price.id"],
                                   title=scenariogroups$title,
                                   description=scenariogroups$title,
                                   type="Auto",
                                   scen.num=scenariogroups$num,
                                   colour.series=scenariogroups$level)
                        )
rownames(scenarioconfig) <- scenarioconfig$id

metadata_long <- gather(metadata,key=type,value=level,-id)

## Read scenario detail table from the excel workbook
scenariodetail <- read.xlsx(file=filename,sheetName = "Scenario",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
scenariodetail <- scenariodetail[rowSums(is.na(scenariodetail))<ncol(scenariodetail),colSums(is.na(scenariodetail))<nrow(scenariodetail)]
# rownames(scenariodetail) <- as.character(scenariodetail[,1])
for(rowidx in 1:nrow(scenariogroups)){
                    x <- scenariogroups[rowidx,]
                    temp.lookup <- filter(metadata_long,type==x$type,level==x$level) %>%
                                        mutate(seq=seq_along(id))
                    scenariodetail <- rbind(scenariodetail,
                          data.frame(scenariodetail=paste("SC",as.numeric(x$num),"-",temp.lookup$seq,sep=""),
                                     activity.id=temp.lookup$id))
}
# rownames(scenariodetail) <- scenariodetail$scenariodetail

# Generate other auto scenarios e.g. all Os.
scenariogroups_combos <- list("O(all)"=data.frame(type="v.o",level=c("O(high)","O(medium)","O(low)"),title="O(all)"),
                              "O(hi+med)"=data.frame(type="v.o",level=c("O(high)","O(medium)"),title="O(hi+med)"))
scenariogroups_combos <- lapply(scenariogroups_combos,FUN=function(x){mutate(x,
                                        id.title=paste(scenariogroup_titles[x$type[1]]," (",x$title[1],")",sep=""))})

temp.count <- 0
for(combo in scenariogroups_combos){
                    temp.count <- temp.count + 1
                    x <- scenariogroups_combos[[temp.count]]
                    temp.rownum <- as.numeric(last(scenarioconfig$scen.num))+1
                    temp.str <- paste(scenariogroup_titles[x$type[1]]," (",x$title[1],")",sep="")
                    scenarioconfig <- rbind(scenarioconfig,data.frame(
                                        id=paste("SC",temp.rownum,sep=""),
                                        base.vol.id=scenarioconfig[fcasttag,"base.vol.id"],
                                        base.price.id=scenarioconfig[fcasttag,"base.price.id"],
                                        title=temp.str,
                                        description=temp.str,
                                        type="Auto",
                                        scen.num=temp.rownum,
                                        colour.series=x$title[1]))
                    for(idx in 1:nrow(x)){
                                        temp.lookup <- filter(metadata_long,type==x[idx,"type"],level==x[idx,"level"]) %>%
                                                            mutate(seq=seq_along(id))
                                        if(nrow(temp.lookup)>0){
                                        scenariodetail <- rbind(scenariodetail,
                                                                data.frame(scenariodetail=paste("SC",temp.rownum,"-",temp.lookup$seq,sep=""),
                                                                           activity.id=temp.lookup$id))
                                        }
                    }
}
# Identify parent scenario for the individual activities in scenario detail
scenariodetail$scenario <- sapply(scenariodetail$scenariodetail,
                                  FUN=function(x){strsplit(x,split="-")[[1]][1]})
idxscenarioconfig.scen <- !grepl("Actual",scenarioconfig$id)
scenarios <- scenarioconfig[idxscenarioconfig.scen,]
scenariodetail$basevolid <- scenarios[scenariodetail$scenario,"base.vol.id"]
scenariodetail <- group_by(scenariodetail,scenario) %>% 
                    mutate(scenariodetail = paste(scenario,"-",seq_along(scenario),sep="")) %>% 
                    ungroup
scenariodetail <- as.data.frame(scenariodetail)
rownames(scenarioconfig) <- scenarioconfig$id
rownames(scenariodetail) <- scenariodetail$scenariodetail

# scenarioconfig <- rbind(scenarioconfig,do.call(rbind,lapply(scenariogroups_combos,FUN=function(x){
#                     temp.rownum <- as.numeric(last(scenarioconfig$scen.num))+1
#                     temp.str <- paste(scenariogroup_titles[x$type[1]]," (",x$title[1],")",sep="")
#                     data.frame(
#                     id=paste("SC",temp.rownum,sep=""),
#                     base.vol.id=scenarioconfig[fcasttag,"base.vol.id"],
#                     base.price.id=scenarioconfig[fcasttag,"base.price.id"],
#                     title=temp.str,
#                     description=temp.str,
#                     type="Auto",
#                     scen.num=temp.rownum,
#                     colour.series=x$title[1]
#                     )
# })))

# scenariodetail <- apply(scenariogroups,1,FUN = function(x){
#                     temp.lookup <- filter(metadata_long,type==x["type"],level==x["level"]) %>%
#                                         mutate(seq=seq_along(id))
#                     rbind(scenariodetail,
#                           data.frame(scenariodetail=paste("SC",as.numeric(x["num"]),"-",temp.lookup$seq,sep=""),
#                                      activity.id=temp.lookup$id))
#                     browser()
# })

## Read scenario detail table from the excel workbook
plotcolours <- read.xlsx(file=filename,sheetName = "Plotcolours",stringsAsFactors=FALSE,header=TRUE)
# This will remove any column/row that is entirely NA values (bad excel behaviour)
plotcolours <- plotcolours[rowSums(is.na(plotcolours))<ncol(plotcolours),colSums(is.na(plotcolours))<nrow(plotcolours)]
colnames(plotcolours)[1] <- "series"
################## Intermediate data ##############################
## Data wrangling in this section to setup everything required to calculate final results

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

#Process phasing information
# subsetphasing <- activityphasing[grepl("^VA",rownames(activityphasing)),]
scenariophasing <- activityphasing[scenariodetail$activity.id,]
rownames(scenariophasing) <- rownames(scenariodetail)
scenariophasing <- data.frame(scenariophasing,id=scenariodetail$scenariodetail)
activityphasing <- data.frame(activityphasing,id=rownames(activityphasing))

#Scenarios automatically created
new.id <- paste("SC",max(as.numeric(subset(scenarioconfig,scen.num!="Actual")$scen.num))+1,sep="")

################## Output data ##############################
## Calculate the various outputs required
# Monetise the volume balance (inputs and outputs)
#                   delta.scen = volume deltas, by scenario
#                   total.scen = volume balance total (i.e. with base case), by scenario
#                   delta = volumes deltas, by activity
#                   total = volume balance total, by activity
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

# Also monetise the base case volumes balances
money$default.base.gm <- rowSums(select(volumes$total[default.volbase,],-id)*prices[default.prices,]*(1000/1e6)
                                 *t(volstruc))
money$inout.base.scenario <- (select(volumes$total[scenarioconfig$base.vol.id,],-id) 
                              * prices[scenarioconfig$base.price.id,]*1000/1e6) %>%
                    data.frame %>% mutate(id = scenarioconfig$'id')
volstruc.mat <- matrix(rep(t(volstruc),times=nrow(money$inout.base.scenario)),nrow=nrow(money$inout.base.scenario),byrow=TRUE)
money$gm.base.scenario <- rowSums(select(money$inout.base.scenario,-id)*volstruc.mat) %>%
                    data.frame(gm=., id=scenarioconfig$'id', row.names=scenarioconfig$'id')

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
                                   gm=temp.join$GM.impact.monthly + money$gm.base.scenario[temp.join$scenario,"gm"],
                                   row.names = temp.join$'scenariodetail'))


# Phasing of delta GM
temp.join <- inner_join(select(money$gm.month,-gm),scenariophasing,by="id")
money$gm.profile.delta <- apply(select(temp.join,-id,-gm.delta),2,FUN = function(x){x*temp.join$gm.delta}) %>%
                    data.frame %>% mutate(id = temp.join$'id')
temp.join <- inner_join(select(money$gm.month,-gm),activityphasing,by="id")
money$gm.profile.delta <- apply(select(temp.join,-id,-gm.delta),2,FUN = function(x){x*temp.join$gm.delta}) %>%
                    data.frame %>% mutate(id = temp.join$'id') %>%
                    rbind(money$gm.profile.delta,.)
temp.join <- inner_join(money$gm.profile.delta,scenariodetail,by =c("id" = "scenariodetail"))
temp.lookup <- which(money$gm.profile.delta$id %in% temp.join$id)
money$gm.profile.delta <- aggregate(select(money$gm.profile.delta[temp.lookup,],-id),
                                    list(temp.join$scenario),FUN = sum) %>%
                    rename(id = Group.1) %>%
                    rbind(money$gm.profile.delta,.)
money$gm.profile.delta.cum <- t(apply(select(money$gm.profile.delta,-id),1,cumsum)) %>%
                    data.frame %>% mutate(id = money$gm.profile.delta$'id')

# Phasing of GM
temp.join <- inner_join(select(money$gm.month,-gm),scenariophasing,by="id")
money$gm.profile <- (apply(select(temp.join,-id,-gm.delta),2,FUN = function(x){x*temp.join$gm}) + 
                    money$gm.base.scenario[scenariodetail[temp.join$id,"scenario"],"gm"]) %>%
                    data.frame %>% mutate(id = temp.join$'id')
temp.join <- inner_join(select(money$gm.month,-gm),activityphasing,by="id")
money$gm.profile <- (apply(select(temp.join,-id,-gm.delta),2,FUN = function(x){x*temp.join$gm}) + 
                    money$default.base.gm) %>%
                    data.frame %>% mutate(id = temp.join$'id') %>%
                    rbind(money$gm.profile,.)
temp.join <- inner_join(money$gm.profile.delta,scenarioconfig,by="id")
temp.lookup <- which(money$gm.profile.delta$id %in% temp.join$id)
money$gm.profile <- (select(money$gm.profile.delta[temp.lookup,],-id) + 
                    money$gm.base.scenario[temp.join$id,"gm"]) %>%
                    mutate(id = temp.join$'id') %>%
                    rbind(money$gm.profile,.)
money$gm.profile.cum <- t(apply(select(money$gm.profile,-id),1,cumsum)) %>%
                    data.frame %>% mutate(id = money$gm.profile$'id')

# Calculate GM for actuals
money$inout.actual.base <- (volactuals*prices[scenarioconfig[rep("Actual",times=nrow(volactuals)),"base.price.id"],])*1000/1e6
volstruc.mat <- matrix(rep(t(volstruc),times=nrow(money$inout.actual.base)),nrow=nrow(money$inout.actual.base),byrow=TRUE)
money$gm.actual <- rowSums(money$inout.actual.base*volstruc.mat) %>% 
                    data.frame(gm.delta = .,
                               gm = . + money$gm.base.scenario["Actual","gm"],
                               merge.id = actuals$'id',
                               month = actuals$'month',
                               activity.id = actuals$'activity.id',
                               price.type = "base", type = "actual.base.elem")
# rownames(money$gm.actual) <- money$gm.actual$id
temp.join <- inner_join(money$gm.actual[grepl("^MA",money$gm.actual$activity.id),], actuals, by=c("merge.id" = "id"))
money$gm.actual[temp.join$id,"gm.delta"] <- temp.join$man.adj
money$gm.actual <- mutate(money$gm.actual, gm = gm.delta + money$gm.base.scenario["Actual","gm"])
money$gm.actual <- group_by(money$gm.actual,price.type,activity.id) %>% 
                    mutate(gm.delta.cum = cumsum(gm.delta)) %>% ungroup
money$gm.actual <- mutate(money$gm.actual,id=paste("actual",price.type,activity.id,sep="."))

money$gm.actual.monthly <- aggregate(select(filter(money$gm.actual,price.type=="base"),gm.delta), 
                                     list(money$gm.actual$month),FUN = sum) %>%
                    rename(month = Group.1) %>%
                    mutate(gm = gm.delta + money$gm.base.scenario["Actual","gm"],
                           price.type="base",id="actual.base",activity.id="actual.base")
money$gm.actual.monthly <- mutate(money$gm.actual.monthly,
                                  gm.delta.cum = cumsum(gm.delta),
                                  gm.cum = cumsum(gm), type = "actual.base")



################## Data for App Output Elements ##############################
## Marshal the data required for the app's output elements e.g. charts etc

# promisetag <- rownames(scenarios)[scenarios$type=="latest.promise"]
# fcasttag <- rownames(scenarios)[scenarios$type=="latest.forecast"]
combine.with <- list("V&O"=fcasttag)
# Get the GM profiles for non-actuals data. Will need to overwrite month
# field to get it back to an actual date field, too many errors with TZ trying
# to use as.Date
money$gm.profile.delta <- gather(money$gm.profile.delta,key=month,value=gm.delta,-id)
money$gm.profile.delta <- mutate(money$gm.profile.delta, merge.id=paste(month,id,sep="|"))
money$gm.profile <- gather(money$gm.profile,key=month,value=gm,-id)
money$gm.profile <- mutate(money$gm.profile, merge.id=paste(month,id,sep="|"))
money$gm.profile.comb <- inner_join(money$gm.profile.delta, select(money$gm.profile,merge.id,gm), by="merge.id")
money$gm.profile.delta.cum <- gather(money$gm.profile.delta.cum,key=month,value=gm.delta.cum,-id)
money$gm.profile.delta.cum <- mutate(money$gm.profile.delta.cum, merge.id=paste(month,id,sep="|"))
money$gm.profile.comb$month <- rep(phasedates, each=nrow(money$gm.profile.comb)/length(phasedates))
money$gm.profile.comb <- inner_join(money$gm.profile.comb, 
                                    select(money$gm.profile.delta.cum,merge.id,gm.delta.cum),
                                    by="merge.id")
# temp.join <- inner_join(money$gm.profile.comb,select(scenariodetail,scenariodetail,activity.id),
#                         by=c("id"="scenariodetail"))
temp.lookup <- money$gm.profile.comb$id %in% scenariodetail$scenariodetail
money$gm.profile.comb[temp.lookup, "activity.id"] <- scenariodetail[money$gm.profile.comb[temp.lookup,"id"],"activity.id"]
money$gm.profile.comb[temp.lookup, "type"] <- "scenario.elem"
temp.lookup <- money$gm.profile.comb$id %in% metadata$id
# money$gm.profile.comb[temp.lookup,"title"] <- metadata[money$gm.profile.comb[temp.lookup,"id"],"title"]
money$gm.profile.comb[temp.lookup,"activity.id"] <- money$gm.profile.comb[temp.lookup,"id"]
money$gm.profile.comb[temp.lookup, "type"] <- "activity"
temp.lookup <- money$gm.profile.comb$id %in% scenarioconfig$id
# money$gm.profile.comb[temp.lookup,"title"] <- scenarioconfig[money$gm.profile.comb[temp.lookup,"id"],"title"]
money$gm.profile.comb[temp.lookup,"activity.id"] <- money$gm.profile.comb[temp.lookup,"id"]
money$gm.profile.comb[temp.lookup, "type"] <- "scenario"
# Append the actuals results to the dataset
money$gm.profile.comb <- rbind(select(money$gm.profile.comb,-merge.id),select(money$gm.actual,-price.type,-merge.id))
money$gm.profile.comb <- rbind(money$gm.profile.comb,select(money$gm.actual.monthly,-price.type,-gm.cum))
# temp.lookup <- grepl("^actual.base$",money$gm.profile.comb$id)
# money$gm.profile.comb[temp.lookup, "type"] <- "actual.base"
# temp.lookup <- grepl("^actual.base.",money$gm.profile.comb$id)
# money$gm.profile.comb[temp.lookup, "type"] <- "actual.base.elem"
temp.lookup <- money$gm.profile.comb$id == fcasttag
money$gm.profile.comb[temp.lookup,"type"] <- "fcast"
temp.lookup <- money$gm.profile.comb$id %in% scenariodetail[
                    scenariodetail$scenario %in% fcasttag,"scenariodetail"]
money$gm.profile.comb[temp.lookup,"type"] <- "fcast.elem"
temp.lookup <- money$gm.profile.comb$id == promisetag
money$gm.profile.comb[temp.lookup,"type"] <- "promise"
temp.lookup <- money$gm.profile.comb$id %in% scenariodetail[
                    scenariodetail$scenario %in% promisetag,"scenariodetail"]
money$gm.profile.comb[temp.lookup,"type"] <- "promise.elem"

# Combine with metadata to create the (mostly) unified dataset required to run the dashboard
dash <- list()
dash$data <- left_join(money$gm.profile.comb,metadata,by = c("activity.id"="id"))
temp.lookup <- dash$data$id %in% scenarioconfig$id
dash$data[temp.lookup,"title"] <- scenarioconfig[dash$data[temp.lookup,"id"],"title"]
dash$data[temp.lookup,"description"] <- scenarioconfig[dash$data[temp.lookup,"id"],"description"]

# # Filter the data so only using dates within user-selected range
user <- list()
user$date.start <- ymd("2017-01-01")
user$date.end <- ymd("2018-02-01")
dash$data.filtered <- filter(dash$data,month >= user$date.start, month <= user$date.end)

# Compile the subset required for the gm per month charts
fun_dash_gmmth <- function(data.filtered){
                    dash$gm.mth <- select(filter(data.filtered, id %in% c(promisetag,fcasttag,"actual.base")),
                                          id,month,gm.delta,gm,gm.delta.cum)
                    str.from <- c(promisetag, fcasttag,"actual.base"); str.to <- c("Promise","Forecast","Actual @ base prices")
                    # if any of the series are not in the data, insert them with NA values to avoid breaking the
                    # downstream data massaging which expects to find those series
                    for(type in str.from){
                                        if(!(type %in% dash$gm.mth$id)){
                                                            dash$gm.mth <- rbind(dash$gm.mth,data.frame(
                                                                                id = type,
                                                                                # month = dash$gm.mth$month[1],
                                                                                month = NA,
                                                                                gm = NA,
                                                                                gm.delta = NA,
                                                                                gm.delta.cum = NA
                                                            ))
                                        }
                    }
                    dash$gm.mth <- spread(select(dash$gm.mth,-gm.delta,-gm.delta.cum), key=id,value=gm) %>%
                                        mutate_(act.prom = paste("actual.base",promisetag,sep="-"), 
                                                fcast.prom = paste(fcasttag,promisetag,sep="-"),
                                                act.fcast = paste("actual.base",fcasttag,sep="-")) %>%
                                        select(-actual.base, -get(fcasttag), -get(promisetag)) %>%
                                        left_join(dash$gm.mth,.,by="month") %>%
                                        gather(key=measure, value=value,-id,-month,na.rm=TRUE)
                    for(x in seq_along(str.from)){dash$gm.mth$id <- gsub(str.from[x],str.to[x],dash$gm.mth$id)}
                    dash$gm.mth
}

actuals.latest.mth <- max(money$gm.actual.monthly$month)
mths.with.actuals <- phasedates[which(phasedates <= actuals.latest.mth)]
dash$is.there.actuals <- sum(dash$data.filtered$month <= actuals.latest.mth)>0

fun_dash_tablepast <- function(data.filtered,have.actuals){
                    if(have.actuals){
                                        dash$table.past <- filter(data.filtered, type %in% 
                                                                                      c("fcast.elem","promise.elem","actual.base.elem")) %>%
                                                            filter(month <= actuals.latest.mth) %>%
                                                            select(id,month,gm.delta,gm,gm.delta.cum,activity.id,
                                                                   summary.category,title,type) %>%
                                                            rename(Activity = activity.id, Category=summary.category,
                                                                   Title = title)
                    }else{
                                        dash$table.past <- select(data.filtered,id,month,gm.delta,gm,gm.delta.cum,activity.id,
                                                                  summary.category,title,type) %>%
                                                            rename(Activity = activity.id, Category=summary.category,
                                                                   Title = title)
                                        dash$table.past <- dash$table.past[FALSE,]
                    }
                    dash$table.past
}

fun_dash_tablepastcat <- function(dash.table.past,have.actuals){
                    if(have.actuals){
                                        dash$table.past.cat <- group_by(dash.table.past,Category,type) %>%
                                                            summarise(value=sum(gm.delta)) %>% ungroup
                                        dash$table.past.cat <- spread(dash$table.past.cat, key=type,value=value, fill=0) %>%
                                                            mutate(Act.to.Prom = actual.base.elem - promise.elem, 
                                                                   Fcast.to.Prom = fcast.elem - promise.elem,
                                                                   Act.to.Fcast = actual.base.elem - fcast.elem) %>%
                                                            arrange(desc(Act.to.Prom))
                                        temp.lookup <- match("Category",colnames(dash$table.past.cat))
                                        dash$table.past.cat[,-temp.lookup] <- apply(dash$table.past.cat[,-temp.lookup],2,round,1)
                                        str.from <- c("promise.elem","fcast.elem","actual.base.elem"); str.to <- c("Promise","Forecast","Actual @ base prices")
                                        for(x in seq_along(str.from)){colnames(dash$table.past.cat) <- gsub(str.from[x],str.to[x],colnames(dash$table.past.cat))}
                    }else{
                                        dash$table.past.cat <- dash.table.past
                    }
                    dash$table.past.cat
}

fun_dash_tablepastact <- function(dash.table.past,have.actuals){
                    if(have.actuals){
                                        dash$table.past.act <- group_by(dash.table.past,Activity,type) %>%
                                                            summarise(value=sum(gm.delta)) %>% ungroup
                                        dash$table.past.act <- spread(dash$table.past.act, key=type,value=value, fill=0) %>%
                                                            mutate(Act.to.Prom = actual.base.elem - promise.elem, 
                                                                   Fcast.to.Prom = fcast.elem - promise.elem,
                                                                   Act.to.Fcast = actual.base.elem - fcast.elem) %>%
                                                            arrange(desc(Act.to.Prom))
                                        temp.lookup <- match("Activity",colnames(dash$table.past.act))
                                        dash$table.past.act[,-temp.lookup] <- apply(dash$table.past.act[,-temp.lookup],2,round,1)
                                        dash$table.past.act$Title <- metadata[dash$table.past.act$Activity,"title"]
                                        dash$table.past.act$Category <- metadata[dash$table.past.act$Activity,"summary.category"]
                                        cnames <- colnames(dash$table.past.act)
                                        temp.lookup <- match(c("Category","Title"),cnames)
                                        dash$table.past.act <- dash$table.past.act[,c("Category","Title",cnames[-temp.lookup])]
                                        for(x in seq_along(str.from)){colnames(dash$table.past.act) <- gsub(str.from[x],str.to[x],colnames(dash$table.past.act))}
                    }else{
                                        dash$table.past.act <- dash.table.past
                    }
                    dash$table.past.act
}

fun_dash_tablefuture <- function(data.filtered){
                    dash$table.future <- filter(data.filtered, type %in% c("fcast.elem","promise.elem","actual.base.elem")) %>%
                                        filter(month > actuals.latest.mth) %>%
                                        select(id,month,gm.delta,gm,gm.delta.cum,type,activity.id,summary.category,title) %>%
                                        rename(Activity = activity.id, Category = summary.category, Title = title)
                    dash$table.future
}

fun_dash_tablefuturecat <- function(dash.table.future){
                    dash$table.future.cat <- mutate(dash.table.future, year=year(month)) %>% 
                                        group_by(type, Category, year) %>%
                                        summarise(value=sum(gm.delta)) %>% ungroup
                    dash$table.future.cat <- spread(dash$table.future.cat, key=type,value=value, fill=0) %>%
                                        mutate(Fcast.to.Prom = fcast.elem - promise.elem) %>%
                                        gather(key = type, value=gm.delta,-Category,-year) %>%
                                        spread(key = year, value=gm.delta)
                    temp.lookup <- match(c("Category","type"),colnames(dash$table.future.cat))
                    dash$table.future.cat[,-temp.lookup] <- apply(dash$table.future.cat[,-temp.lookup],2,round,1)
                    dash$table.future.cat$Total <- rowSums(dash$table.future.cat[,-temp.lookup])
                    str.from <- c("promise.elem","fcast.elem"); str.to <- c("Promise","Forecast")
                    for(x in seq_along(str.from)){dash$table.future.cat$type <- gsub(str.from[x],str.to[x],dash$table.future.cat$type)}
                    
                    dash$table.future.cat
}

fun_dash_tablefutureact <- function(dash.table.future){
                    dash$table.future.act <- mutate(dash.table.future, year=year(month)) %>% 
                                        group_by(type,Activity,year) %>%
                                        summarise(value=sum(gm.delta)) %>% ungroup
                    dash$table.future.act <- spread(dash$table.future.act, key=type,value=value, fill=0) %>%
                                        mutate(Fcast.to.Prom = fcast.elem - promise.elem) %>%
                                        gather(key = type, value=gm.delta,-Activity,-year) %>%
                                        spread(key = year, value=gm.delta) %>% rename(id=Activity)
                    dash$table.future.act <- left_join(dash$table.future.act,
                                                       select(metadata,id,summary.category,title),by = "id") %>%
                                        rename(Activity=id,Category=summary.category, Title=title)
                    temp.lookup <- match(c("Activity","Category","Title","type"),colnames(dash$table.future.act))
                    dash$table.future.act[,-temp.lookup] <- apply(dash$table.future.act[,-temp.lookup],2,round,1)
                    dash$table.future.act$Total <- rowSums(dash$table.future.act[,-temp.lookup])
                    cnames <- colnames(dash$table.future.act)
                    temp.lookup <- match(c("Category","Title"),cnames)
                    dash$table.future.act <- dash$table.future.act[,c("Category","Title",cnames[-temp.lookup])]
                    for(x in seq_along(str.from)){dash$table.future.act$type <- gsub(str.from[x],str.to[x],dash$table.future.act$type)}
                    
                    dash$table.future.act
}

fun_dash_tablefutureemptact <- function(dash.table.future.act){
                    dash$table.future.empties <- select(dash.table.future.act,-Category,-Title) %>%
                                        gather(key=period, value=gm.delta, -Activity,-type) %>%
                                        spread(key=type, value=gm.delta) %>%
                                        group_by(Activity) %>% 
                                        summarise(sum.fast=sum(abs(fcast.elem)),sum.prom=sum(abs(promise.elem))) %>%
                                        mutate(total = sum.fast + sum.prom)
                    dash$table.future.emptact <- dash$table.future.empties[which(dash$table.future.empties$total==0),"Activity"]
                    
                    dash$table.future.emptact
}

dash$future.types <- c("Forecast","Promise","Fcast.to.Prom")

detailview <- list()
detailview$scen.table <- select(filter(scenarioconfig, id!="Actual"),id,title,description)
detailview$act.table.data <- filter(dash$data.filtered, type=="activity") %>%
                    mutate(year = year(month))
detailview$act.table <- group_by(detailview$act.table.data,id,year) %>%
                    summarise(gm.delta=sum(gm.delta)) %>% ungroup %>%
                    spread(key=year,value=gm.delta)
temp.lookup <- match("id",colnames(detailview$act.table))
detailview$act.table[,-temp.lookup] <- apply(detailview$act.table[,-temp.lookup],2,round,1)
detailview$act.table$Total <- rowSums(select(detailview$act.table,-id))
temp.summary <- group_by(detailview$act.table.data,id) %>% summarise(From = min(month))

detailview$act.table$Title <- metadata[detailview$act.table$id,"title"]
detailview$act.table$Category <- metadata[detailview$act.table$id,"summary.category"]
cnames <- colnames(detailview$act.table)
temp.lookup <- match(c("Title","Category"),cnames)
detailview$act.table <- detailview$act.table[,c("Category","Title",cnames[-temp.lookup])]
temp.lookup <- which(detailview$act.table$id %in% filter(scenariodetail,scenario==fcasttag)$activity.id)
detailview$act.table[temp.lookup,"dash.group"] <- "Forecast"
temp.lookup <- which(detailview$act.table$id %in% filter(scenariodetail,scenario==promisetag)$activity.id)
detailview$act.table[temp.lookup,"dash.group"] <- "Promise"
detailview$act.table <- left_join(detailview$act.table, select(metadata,id,project,v.o,resource,score,owner,status),by="id")
                    # mutate(dash$table.future, year=year(month)) %>% 
                    # group_by(scen,Activity,year) %>%
                    # summarise(value=sum(gm.delta)) %>% ungroup

temp.key <- "V&O"
temp.lookup <- filter(scenarioconfig,grepl(paste("^",temp.key,sep=""),scenarioconfig$title))$id
temp.rownum <- which(dash$data.filtered$id %in% temp.lookup)
detailview$gmdelta.plot <- select(dash$data.filtered,id,month,gm,gm.delta,gm.delta.cum,activity.id,title)
detailview$gmdelta.plot$gm.fcast <- left_join(data.frame(month=detailview$gmdelta.plot$month)
                                                         ,select(filter(detailview$gmdelta.plot,id==combine.with[[temp.key]]),
                                                                 month,gm.delta),by="month")$gm.delta
for(colm in c("gm.delta","gm")){
                    detailview$gmdelta.plot[temp.rownum,colm] <- as.matrix(detailview$gmdelta.plot[temp.rownum,colm])+
                                        as.matrix(detailview$gmdelta.plot[temp.rownum,"gm.fcast"])
}
temp.colorder <- grepl("gm.delta.cum",colnames(detailview$gmdelta.plot))
detailview$gmdelta.plot <- detailview$gmdelta.plot[,!temp.colorder] %>%
                    group_by(id) %>% mutate(gm.delta.cum = cumsum(gm.delta)) %>%
                    ungroup

temp.lookup <- which(volumes$total.scen$id %in% scenarioconfig$id)
detailview$scen.vol.table <- apply(t(select(volumes$total.scen[temp.lookup,],-id)),2,round,1)
colnames(detailview$scen.vol.table) <- scenarioconfig[colnames(detailview$scen.vol.table),"title"]

# colour_pal <- data.frame(id=unique(money$gm.profile.comb$id),hex=rainbow_hcl(length(unique(money$gm.profile.comb$id))))
colour_pal <- left_join(select(scenarioconfig,id,colour.series),plotcolours,by=c("colour.series"="series"))
temp.lookup <- which(is.na(colour_pal$colour.code))
colour_pal[temp.lookup,"colour.code"] <- distinctColorPalette(length(temp.lookup))
rownames(colour_pal) <- colour_pal$id
# temp.rownum <- length(unique(money$gm.profile.comb$id))

# colour_pal <- data.frame(id=unique(money$gm.profile.comb$id),hex=sample(colorRampPalette(c("Red","Green","Blue"))(temp.rownum),temp.rownum))
# colour_pal <- data.frame(id=unique(money$gm.profile.comb$id),hex=distinctColorPalette(temp.rownum))

which.series <- function(vector,lookups){
                    results <- numeric()
                    for(idx in 1:length(lookups)){
                                        results[idx] <- sum(vector == lookups[idx])
                    }
                    results
}