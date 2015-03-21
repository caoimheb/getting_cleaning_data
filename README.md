# Data Science Specialization - Getting and Cleaning Data Project

This repository contains my submission for the Getting and Cleaning Data Course Project as part of the Coursera Data Science Speciliaztion. This includes one script file, `run_analysis.R`, and a code book describing the variables involved. 

## Pre-requisites

Before running the code, you must download the required data. The .zip of the data directory is available from [here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip). Unzip the directory into your R working directory, ensuring that `run_analysis.R` is also in the working directory. 

## Execution

To execute the required script to join, filter and tidy the data, following the steps below in RStudio or another R text editor. 

1. ```source('./run_analysis.R') ```
2. ``` tidy_data_set <- run_analysis() ```
3. ``` View(tidy_data)```

The `run_analysis()` function will combine all test and train data sets, which includes adding activity labels and a subject field, as well as meaningful names for each value. It also filters all values to include in the final data set only those values which are a mean or standard deviation, as required by the project description. 