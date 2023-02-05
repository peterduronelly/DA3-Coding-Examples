# DA3-Coding-Examples

Coding classes for Data Analysis 3 on MSc in Business Analytics on the Central European University

## How to run the codes?

Open an R project as given on the book's home page: [How to set up your computer for R](https://gabors-data-analysis.com/howto-r/).

Download the appropriate files, or fork this repo, clone it, and open the code from your R project's environment. Once you're done, you are good to go.

## Contents

1. **class 13, used cars**<br>
Basic data manipulation and exploratory data analysis.<br>
Basic visualizations; plotting logged values in ggplot.<br>
Multiple linear regression.<br>
Model selection by goodness-of-fit metrics.<br>
Cross-validation and model comparison.

2. **class 14, airbnb**<br>
Handling missing data; integrating missing data information in the analytics.<br>
Model setup.<br>
Interactions and dummies.<br>
Train, test and holdout sets.<br>
Cross-validation, train and test metrics.<br>
Lasso:
    - running a lasso optimization
    - interpreting the results
    - RMSE<br>

    Diagnostics on the holdout set.<br>
    Plotting prediction results.

3. **class 15, used cars**<br>
Data manipulation as in class 13<br>
Basic regression trees<br>
Plotting trees and regression results as step functions<br>
Building more complex regression trees with control parameters<br>
Pruning<br>
Comparing tree-based and OLS models<br>
Variable importance: with final only and with competing variables<br>

4. **class 16, airbnb, hitters**<br>
Setting up grid for grid search in `caret::train`<br>
Running random forest model using the `ranger` package in `caret`<br>
Getting and plotting individual and grouped variable importances<br>
Partial dependence plots for rf models
Predictions and RMSE for subsets of data<br>
Comparing OLS, LASSO, CART, and random forest<br>
Gradient Boosting Machines: tuning and model run<br>
Hitters: parameter grid search on a smaller and easier-to-handle dataset<br>
The airbnb analysis is implemented both in R and in Python using a Jupyter notebook<br>
