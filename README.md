# Fork of VMS-NWFSC-2
Apply update to the NOAA/NWFSC VMS and fish ticket data processing pipeline

# Purpose
Process VMS data from crab fishing seasons 2009-10 (2010 crab year) through 2017-18 (2018 crab year) to evaluate changing fishing grounds. 

*Original by Owen Liu*
This repository collates the code used to process VMS data and link it to PacFIN fish ticket data for the west coast. These outputs are useful for a wide array of applications in west coast fishery management. For example, fish-ticket-linked VMS data are useful for describing fishing behavior and dynamics, risk to protected resources, and drivers of change in west coast fisheries.

This is a direct update to work done primarily by M. Fisher and J. Samhouri on a previous version of this workflow.

# Structure
The repository is structured into multiple required steps in the processing of VMS data from raw inputs into useful outputs. This process has a number of steps, including multiple QA/QC steps. These steps are described in detail in the associated `.Rmd` files. The scripts are organized hierarchically. Individual data processing steps have their own associated scripts, which are then knit into the overall process file. This is to facilitate editing of individual steps, while maintaining the overall framework in one document.

Most raw data in this project are large in size and confidential. This repository therefore does not include any raw data, but refers to these data using the relational command from the `here` package `here::here()`. Therefore, authorized users of the data that wish to run or utilize specific pieces of the workflow should obtain the relevant data from one of the moderators of this repository and place it in the `data` folder. Then, all of the code herein should run without needing to change any file path references.

Some data from specific years necessitate pre-processing to prepare them to enter the main data pipeline smoothly. These steps are documented in the `process steps/pre-process` folder.