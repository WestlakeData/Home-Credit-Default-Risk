# Home Credit Default Risk
Group Project for IS 6812\
Summer 2023
- - - - 

## Introduction
Unbanked individuals represent both an underserved demographic as well as a typically
untapped market by reputable creditors. Home Credit seeks to fill this gap in service. There are
unique challenges that accompany establishing creditworthiness among a population that by
definition has little to no financial history, verifiable assets, or traditional means to qualify for a
loan.
### Scope of Project
This project will utilize machine learning algorithms to develop a classification model which will
use available data about Home Credit customers to improve prediction of those that are likely to
repay loans granted by Home Credit. The team will test a number of possible classification
models in order to develop the most accurate model on data outside the training data. An
added benefit of this project is the potential to identify possible additional data that might further
inform the classification model. A successful model will provide greater predictive power than a
simple prediction based upon majority class statistics and will allow Home Credit to loan to
customers with confidence that repayment will in return grow available assets to the company in
order to further its mission of providing credit to the underserved.

### Team
Roman, Che, Kalyani, and Chris

## Exploratory Data Analyis
The dataset consists of 307,511 rows and 122 columns, including the target variable. The target variable represents payment difficulties, with 24,825 clients experiencing difficulties and 282,686 clients having no payment issues. The data exhibits an imbalance, with 91.92% of clients falling into the majority class. There are no missing values in the target variable. Correlation analysis reveals several numerical predictors positively and negatively correlated with the target variable, while certain categorical predictors also show positive correlations. The dataset contains missing values in various columns, which after careful consideration either imputed or removed. Additionally, analysis of credit bureau data shows a right-skewed distribution and missing values that can be imputed using the median value. Some columns contain erroneous data points and outliers, which need to be addressed to ensure data integrity. Overall, these findings provide insights into the dataset, including the target variable, correlations, missing data, and data quality.

In summary, the dataset analysis reveals an imbalance in the target variable, with a majority of clients having no payment difficulties. Correlation analysis identifies numerical and categorical predictors associated with the target variable. Missing values are present in various columns, requiring appropriate imputation or removal. Credit bureau data displays right-skewed distribution and missing values that can be imputed with the median value. Erroneous data points and outliers are identified and need to be addressed. These findings provide valuable insights for data pre-processing and modeling to predict loan repayment probabilities for Home Credit customers. The dataset provides a significant number of records and even when columns and rows are removed or imputed as discussed above, there is sufficient information to justify moving forward with this dataset into the modelling phase.  
**Indivdual EDA** [Credit Default EDA](https://github.com/WestlakeData/Home-Credit-Default-Risk/blob/main/Combined%20Notebook.Rmd)  
**Deliverable:** [Combined EDA Notebook](https://github.com/WestlakeData/Home-Credit-Default-Risk/blob/main/Combined%20Notebook.Rmd)

## Modelling
Multiple machine learning models were developed in an attempt to accurately predict default more accurately than use of the majority class for prediction.

### XGBoost
One of the models developed was an Extreme Gradient Boosted (XGBoost) Classifier.  This model had several hyperparameters which required tuning.  Tuning was done on the training set (70% split) and validated using the validation set (30%).  The final trained model was then used to make predicitons against the Test data set with those predictions submitted to Kaggle for evaluation.

Other models were completed by other group members.  The combined work of the team is included in the the [Combined Modelling Notebook](https://github.com/WestlakeData/Home-Credit-Default-Risk/blob/main/Combined%20Notebook%20Modelling.Rmd)
