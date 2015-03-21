# Code Book - Getting and Cleaning Data Project

## Original Data

The original data used in this project is available [here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip). 

A full description of all files and variables is included with the data set. The data files that particularly concern this project are as follows. 

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

- 'train/subject_train.txt': Each row identifies the subject who performed the activity.

- 'test/subject_test.txt': Each row identifies the subject who performed the activity.


## Data Tidying

Using `run_analysis()`, the above sets were combined to form one data set, which relates activities, values and subjects. This data set is then filtered to include on those values which are a mean or standard deviation. Averages are then taken after grouping by subject and activity. Finally, the columns are renamed to a meaningful, human-readable list of headings. This results in a data set with 180 rows (30 subjects and 6 acitivities), and 82 columns. The columns are as follows. 

- Column 1 : Activity name
- Column 2 : Subject
- Column 3:82 : Average values of measurements for each activity and each subject.

The following rules were applied to tidy the column names.

- `t` at start of line, replaced with `Time`
- `f` at start of line, replaced with `Frequency`
- `()`, `-`, replaced with empty string
- `mean`, replaced with `Mean`
- `std`, replaced with `Std`

For example, the variable `tBodyAcc-mean()-X` from the original data set is replaced with `TimeBodyAccMeanX` in the final tudy data set. 