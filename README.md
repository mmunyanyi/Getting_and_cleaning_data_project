# Getting_and_cleaning_data_project
Getting and Cleaning Data practical assignment

One of the most exciting areas in all of data science right now is wearable computing - see for example this article. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users.

In this project, data collected from the accelerometer and gyroscope of the Samsung Galaxy S Smartphone was retrieved and cleaned to prepare a tidy data set for use in analysis.

Contained in this repository are the following files:

    README.md provides an overview of repository.
    Tidy_Data_Set.txt contains the data set.
    CodeBook.md Describes the contents of the data set.
    run_analysis.R is the R script that for creating the data set.

Study design

The source data set for this project was obtained from the Human Activity Recognition Using Smartphones Data Set, which describes how the data was initially collected as follows:

    The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

    The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Training and test data were first merged together to create one data set, then the measurements on the mean and standard deviation were extracted for each measurement (79 variables extracted from the original 561), and then the measurements were averaged for each subject and activity, resulting in the final data set.
Creating the data set

The R script run_analysis.R is the code to create the data set. It reads the original data set and converts it to a final data set through a series of steps as highlighted in the steps below (the file CodeBook.md contains more details):

    Downloading and unzipping source data file if not yet downloaded.
    Read data.
    Merging the training and the test sets to create one data set.
    Extracting only the mean and standard deviation for each measurement.
    Using descriptive activity names to name the activities in the data set.
    Appropriately labelling the data set with descriptive variable names.
    Creating a single tidy data set with the mean of each variable for each activity and each subject.
    Saving the data set to the Tidy_Data_Set.txt file.

The Tidy_Data_Set.txt in this repository was created by running the run_analysis.R script using R version 3.5.1 on Windows 7 64-bit edition.

This script requires the dplyr package which was built under R version 3.5.2.
