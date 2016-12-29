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
* 


