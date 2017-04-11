# ALDA-Project

## IMDB Movie Rating Prediction and Gross Income Prediction


#####A short description of the files:

corr_analysis.r - Code to perform correlation analysis of the numeric attributes extracted from the original dataset.

movie_metadata.csv - dataset conatining 5043 rows and 28 columns of IMDB data

svr_ratings.r - It contains the SVR prediction model for the dataset - To predict Movie ratings.

ann_ratings.r - It has the ANN Model implemented for the dataset - To predict movie ratings.

linear_regression_ratings.r - Contains the prediction model using linear regression method - To predict Movie rating.

preprocessing-IMDB.r - Contains code for preprocessing required for gross income prediction.

clustering-IMDB.r - Contains code which creates the decision tree models and k means clustreing algorithm on the preprocessed dataset - To categorize the gross income.

######Packages to include
- e1071
- neuralnet
- nnet
- fpc
- mclust
- ggplot
- flexclut
- lattice
- mvtnorm
- party
- rpart
