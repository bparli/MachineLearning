CS 7641 Machine Learning Spring 2016

What is it?
------------
Project 1 submission includes all the R scripts used on the analysis of a series of Supervised 
Learning algorithms.  The data comes from the UCI repository at the below urls:

Cars Evaluation dataset: https://archive.ics.uci.edu/ml/machine-learning-databases/car/
Wine Quality dataset (winequality-red.csv): https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/

Installation and Use
---------------------
To use, download the datafiles into any folder and update the given script to read from that
directory (i.e. "C:/Users/bparli/Downloads/winequality-red.csv").  The scripts were created 
using R Studio (version 0.99) and R (veriosn 3.2.3) on a Windows 10 laptop.

Relevant Files and Descriptions
---------
bparli3-analysis.Rmd: The Project 1 report written in R Markup using R Studio.  Although
some tables were input by hand the majority of neccessary script is contained in this markdown file.

wine_knn.R: R script to download, prepare, and classify the data using the KNN algorithms
wine_nn.R: R script to download, prepare, and classify the data using the Neural Network algorithm
wine_boost.R: script to download, prepare, and classify the data using the Boosting algorithm
wine_svm.R: script to download, prepare, and classify the data using the SVM algorithm
wine_tree.R script to download, prepare, and classify the data using the C50 Decision Trees algorithm
NN algorithms
cars_nn.R: R script to download, prepare, and classify the data using the Neural Network algorithm
cars_boost.R: script to download, prepare, and classify the data using the Boosting algorithm
cars_svm.R: script to download, prepare, and classify the data using the SVM algorithm
cars_tree.R script to download, prepare, and classify the data using the C50 Decision Trees algorithm
cars_knn.R: R script to download, prepare, and classify the data using the KNN algorithms
graphs.R: script to download fresh datasets and run through a series of iterations in order
to analyse the training and test errors at differnt sample sizes

Additional Notes
------------------
These files are also being publically hosted at:  https://github.com/bparli/CS-7641-Assign1

Contact
------------
Ben Parli
bparli3@gatech.edu