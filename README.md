# kobe-shot-prediction
Final project for UPenn STAT 471 - Modern Data Mining.
Analysis of basketball shots Kobe took over the course of his NBA career, in order to build a predictive model for whether he would successfully make a given shot or not.


Although many statistics are collected for basketball players, application of modern statistical techniques on that data has been rare. In this study, classification methods are applied to a dataset of attempted shots in the career of Kobe Bryant to predict their success. They are evaluated on training misclassification error, as well as log loss on a test dataset of 5,000 shots. It is found that the logistic classifier with variables chosen by hand outperforms random forest as well as logistic regression using LASSO-selected variables. Finally, a discussion and attempt to deal with data leakage is presented.
