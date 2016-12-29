# Margin Improvement Tracker

## Overview
Codebook for the margin improvement tracker tool (app) used to record, visualise, and analyse margin improvement activities including understanding of how we have performed, and how we expect to perform in the next few years

## Data Objects
### Inputs
### Processing

### Outputs
* gmbasecases: numeric vector, holds the gross margin for the base case of each scenario
* gmmonthlyactivities: numeric matrix, delta and total gross margin for each activity that is part of a scenario, in the order listed inside the scenario. There can be repeats if one activity is listed in multiple scenarios
* gmdeltaprofile_act: numeric matrix, gross margin delta impact of each activity over the time period specified in the activity phasing input table.
* gmdeltaprofile: numeric matrix. Same information as gmdeltaprofile_act, summed together by scenario i.e. gross margin delta impact of each scenario over time.
* gmdeltaprofile_cum_act: numeric matrix, cumulative expression of gmdeltaprofile_act
* gmdeltaprofile_cum: numeric matrix, cumulative expression of gmdeltaprofile
* gmprofilescenario: numeric matrix, total gross margin of each scenario over time.
* gmprofilescenario_cum: numeric matrix, cumulative expression of gmprofilescenario
* gmdelta_actuals: numeric matrix, gross margin delta of each activity included in the Actuals input, for each month of Actuals provided
* gmactuals: dataframe, month (date) and gross margin (numeric) of each month provided in Actuals (total for month)

### Marshalling for app UI
* dashgmpermonth: dataframe, intended to provide data on gross margin per month for actual, forecast, promise and other user-selected scenarios. This data is used for charts and tables.
* dashgmcum: dataframe, cumulative expression of dashgmpermonth


