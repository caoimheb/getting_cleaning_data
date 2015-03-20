run_analysis <- function() {
  
      # Read in test data - x, y and subject
      x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
      activities_test <- read.table("UCI HAR Dataset/test/y_test.txt")
      subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
      
      # Read in train data - x, y and subject
      x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
      activities_train <- read.table("UCI HAR Dataset/train/y_train.txt")
      subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
      
      # Read in column names data
      column_file <- read.table("UCI HAR Dataset/features.txt")
      column_names <- column_file[, 2]
      
      # Read in activity labels
      activities_names <- read.table("UCI HAR Dataset/activity_labels.txt")
      colnames(activities_names) <- c("activity_id", "activity_name")
      
      
      combined_test <- combine_data_sets(column_names, x_test, activities_test, activities_names, subject_test)
      combined_train  <- combine_data_sets(column_names, x_train, activities_train, activities_names, subject_train)
            
      # Rename columns on test and train data sets to column names
      #colnames(x_test) <- column_names
      #colnames(x_train) <- column_names
      
      # Add activities as first column on test and train data sets
      #complete_test <- cbind(activities_test, x_test)
      #complete_train <- cbind(activities_train, x_train)
      
      # Rename this new activity column to a more meaningful name
      #colnames(complete_test)[1] <- "activity"
      #colnames(complete_train)[1] <- "activity"
      
      # Add activity name to each data set
      #test_with_activity_name <- right_join(activities_names, complete_test, by = c("activity_id" = "activity"))
      #train_with_activity_name <- right_join(activities_names, complete_train, by = c("activity_id" = "activity"))
      
      # Add subject to each data set
      #final_combined_test <- cbind(subject_test, test_with_activity_name)
      #final_combined_train <- cbind(subject_train, train_with_activity_name)
            
      # Rename subject column
      #colnames(final_combined_test)[1] <- "subject"
      #colnames(final_combined_train)[1] <- "subject"
      
      
      
      # Combine test and train data sets
      tidy_data_set <- rbind(combined_test, combined_train)
      
      # Filter column names to get those including mean or std
      final_col_names <- colnames(tidy_data_set)
      mean_std_cols <- grep("(mean)|(std)", final_col_names)
      filtered_tidy_data_set <- tidy_data_set[ , mean_std_cols]
      final_data <- cbind(tidy_data_set[ , 1:3], filtered_tidy_data_set)
      
      # Take average of each variable by activity and subject
      averages <- summarise_each(group_by(final_data, activity_name,Subject), funs(mean))

      answer <- tidy_names(averages)
      
      # Return the final tidy data set, minus the activity_id column
      return (answer[ , -3])
}

combine_data_sets <- function(column_names, x_set, activities, activities_names, subject) {
      # Rename columns on data set to column names
      colnames(x_set) <- column_names
      
      # Add activities as first column
      with_activity <- cbind(activities, x_set)
      
      # Rename this new activity column to a more meaningful name
      colnames(with_activity)[1] <- "Activity"
      
      # Add activity name to each data set
      with_activity_name <- right_join(activities_names, with_activity, by = c("activity_id" = "Activity"))
      
      # Add subject to each data set
      combined_data_set <- cbind(subject, with_activity_name)
      
      # Rename subject column
      colnames(combined_data_set)[1] <- "Subject"
      
      return (combined_data_set)
}

tidy_names <- function(data_frame) {
      names(data_frame) <- gsub('^t', 'Time', names(data_frame))
      names(data_frame) <- gsub('^f', 'Frequency', names(data_frame))
      names(data_frame) <- gsub('\\(', '', names(data_frame))
      names(data_frame) <- gsub('\\)', '', names(data_frame))
      names(data_frame) <- gsub('-', '', names(data_frame))
      names(data_frame) <- gsub('mean', 'Mean', names(data_frame))
      names(data_frame) <- gsub('std', 'Std', names(data_frame))
      colnames(data_frame)[1] <- "Activity"
      return(data_frame)
}