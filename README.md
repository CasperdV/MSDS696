# Predicting medical scheme demographics and claims experience

![Banner](images/banner.png?)

Ageing membership is arguably one of the most significant challenges faced by South African medical schemes. The [2017-2018 Annual Report of the Council for Medical Schemes](http://medicalschemes.com/files/Annual%20Reports/CMS_AnnualReport2017-2018.pdf) shows that annual increases in medical scheme member contributions consistently outstrip South African consumer price inflation (CPI) by approximately 3% each year. The ageing membership profile of medical schemes, and the corresponding increase in chronic disease prevalence, is a key driver of the higher than CPI increase in private health expenditure. The Council for Medical Schemes explains:

> "The increase of members in the age bands over 50 years has greater cost implications, as beneficiaries in the older age bands have higher average healthcare costs. This trend is more prominent in the open schemes, and a negative change in the age distribution can have a significant impact on the cost of healthcare." [(Council for Medical Schemes, 2018)](http://medicalschemes.com/files/Annual%20Reports/CMS_AnnualReport2017-2018.pdf)

In their paper titled [Supervised Learning Methods for Predicting Healthcare Costs: Systematic Literature Review and Empirical Evaluation](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5977561/#!po=3.12500), Morid *et al.* (2016) evaluate the predictive performance of various supervised learning methods. They found that gradient boosting and artificial neural networks (ANNs) outperformed other supervised learning methods when predicting healthcare costs. They further found that "cost on cost prediction", i.e. predicting future costs based only on historical costs, performed just as well or better than prediction methods using clinical information.

## Objective

In this project I use a multi-class classifier to predict medical scheme beneficiaries' monthly exposure to all possible membership states. These membership states are based on the members' membership status (principal member, adult dependant or child dependant), benefit option, income status and chronic status. Secondly, I use the cost on cost prediction methodology described by [Morid *et al.* (2016)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5977561/#!po=3.12500) to train an ANN to predict individual claims costs for a cohort of approximately 150,000 South African medical scheme members. I extend the method proposed by [Morid *et al.* (2016)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5977561/#!po=3.12500) by including demographic information in the feature set. Finally, I combined the results from the multi-class classifier and the regression model to predict the scheme's overall risk profile.

## Method

My objective is to predict the membership states, demographic details and claims costs for a cohort of medical scheme members for 12 future months. At a high level, the method I use in this analysis is comprised of the following steps:

1. Model monthly exposure to future membership states as a multi-class, multi-label classification problem using an ANN. The feature set consists of demographic factors only.

2. Model monthly claims expenditure for every beneficiary as a multivariate regression problem using an ANN. The feature set consists of demographic factors and various combinations of historical claims costs as discussed by [Morid *et al.* (2016)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5977561/#!po=3.12500).

3. Combine the predicted monthly exposure from step 1 and the predicted monthly claims expenditure from step 2 to predict the overall future experience for the cohort.

## Environment

All analyses are performed in R. The ANNs are trained using the Keras library for R with the Tensorflow backend. I have also used the following libraries for data manipulation:

* **readr** to load and write the data to file
* **dplyr** to manipulate data frames
* **BalancedSampling** to create stratified training and validation splits
* **ggplot2** for plotting
* **parallel** to use a parallelized version of `lapply`
* **reshape2** for reshaping data frames

The original dataset was extracted and anonymized using SQL, but all further data manipulation was done in R.

## Data structure

I have used medical scheme claims of approximately 150,000 beneficiaries over a period of four years. The data is not publicly available.

The beneficiary dataset is described below:

| Column | Description | Values |
| :--- | :--- | :--- |
| `Member` | A unique integer identifier for every family | 1:85053 |
| `Dependant` | A unique integer identifier for every dependent within a family| 1:16 |
| `Beneficiary` | A unique integer identifier for every individual beneficiary | 1:222856 |
| `Period` | The month number | 1:48 |
| `Year` | The year number | 1:4 |
| `Month` | The calendar month | 1:12 |
| `Gender` | An integer representing the beneficiary's gender | 1 (F), 2 (M)|
| `Age` | The beneficiary's age at the end of the relevant `Period` | float |
| `Duration` | The number of years since the beneficiary joined the scheme | float |
| `Type` | An integer representing membership type (principal member, adult dependant, child dependant) | 1 (P), 2 (A), 3 (C)|
| `Plan` | An integer representing the benefit option in which the beneficiary participates | 1:3 |
| `Income` | An integer representing the beneficiary's income band (where relevant) | 1:3 |
| `Chronic` | An integer representing the beneficiary's chronic status | 1 (non-chronic), 2 (chronic) |
| `Adults` | The number of adult dependants in the beneficiary's family (excluding the principal member) | 0:5 |
| `Children` | The number of child dependants in the beneficiary's family | 0:14 |

A sample of the beneficiary dataset is provided below.

```r
> head(beneficiaries)
  Member Period Beneficiary Dependant Year Month Gender      Age Duration Type Plan Income Chronic Adults Children
1      1      1        6684         1    1     1      2 61.60438 10.08077    1    3      1       2      1        0
2      1      1        6685         2    1     1      1 59.02806 10.08077    2    3      1       2      1        0
3      1      2        6685         2    1     2      1 59.10472 10.15743    2    3      1       2      1        0
4      1      2        6684         1    1     2      2 61.68104 10.15743    1    3      1       2      1        0
5      1      3        6685         2    1     3      1 59.18960 10.24230    2    3      1       2      1        0
6      1      3        6684         1    1     3      2 61.76591 10.24230    1    3      1       2      1        0
```

The claims dataset is described below:

| Column | Description | Values |
| :--- | :--- | :--- |
| `Period` | The month number | 1:48 |
| `Beneficiary` | A unique integer identifier for every individual beneficiary | 1:222856 |
| `ClaimTotal` | Total claims for the `Period` | float |
| `ClaimHospital` | Hospital claims for the `Period` | float |
| `ClaimChronic` | Chronic claims for the `Period` | float |
| `ClaimOther` | Other claims for the `Period` | float |

A sample of the claims dataset is provided below.

```r
> head(claims)
  Period Beneficiary ClaimTotal ClaimHospital ClaimChronic ClaimOther
1     18           1   53.64546         0.000            0   53.64546
2     19           1   28.63818         0.000            0   28.63818
3     20           1   14.07000         0.000            0   14.07000
4     21           1 5418.46455      5309.627            0  108.83727
5     13           2   13.75455         0.000            0   13.75455
6     15           2   13.15455         0.000            0   13.15455
```

I use data for `Periods` 1 to 36 (three calendar years) to train the ANN for the beneficiary model and data for period 13 to 36 to train the ANN for the claims model. The predictions are then made for `Periods` 37 to 48 to test the accuracy of the predictions. The categorical feature variables were one-hot encoded and continuous variables were normalized before training.

## Exploratory data analysis

It is convenient to transform the categorical beneficiary variables into one state variable. Since I am using a multi-class classifier the ANN output will be a probability that the beneficiary belongs to each state. I also use a Softmax activation which ensures that the probabilities for every state (including the resigned state) add up to one. The probabilities can then be used as the expected exposure in each state. The state variable is constructed as follows:

```r
> state.mapping
   State Plan Income Type Chronic
1      0    0      0    0       0
2      1    1      1    1       1
3      2    1      1    2       1
4      3    1      1    3       1
5      4    1      1    1       2
6      5    1      1    2       2
7      6    1      1    3       2
8      7    2      1    1       1
9      8    2      1    2       1
10     9    2      1    3       1
11    10    2      1    1       2
12    11    2      1    2       2
13    12    2      1    3       2
14    13    3      1    1       1
15    14    3      2    1       1
16    15    3      3    1       1
17    16    3      1    2       1
18    17    3      2    2       1
19    18    3      3    2       1
20    19    3      1    3       1
21    20    3      2    3       1
22    21    3      3    3       1
23    22    3      1    1       2
24    23    3      2    1       2
25    24    3      3    1       2
26    25    3      1    2       2
27    26    3      2    2       2
28    27    3      3    2       2
29    28    3      1    3       2
30    29    3      2    3       2
31    30    3      3    3       2
```

There are a total of 30 member states for active beneficiaries (states 1 to 30) and one state for resigned beneficiaries (state 0). The majority of beneficiaries participate in Plan 1 (states 2 to 7). The plots below show the number of beneficiaries by plan, income band, type and chronic status for periods 1 to 48.

![Beneficiaries by plan](images/eda1.png?) ![Beneficiaries by income](images/eda2.png?)
![Beneficiaries by type](images/eda3.png?) ![Beneficiaries by chronic status](images/eda4.png?)

The plots below show the average age of beneficiaries by plan, income band, type and chronic status for periods 1 to 48.

![Age by plan](images/eda5.png?) ![Age by income](images/eda6.png?)
![Age by type](images/eda7.png?) ![Age by chronic status](images/eda8.png?)

The plots below show the number of new entrants and resignations as well as the average age of new entrants and resignations during the periods 1 to 48.

![New entrants](images/eda9.png?) ![Age of new entrants](images/eda10.png?)
![Resignations](images/eda11.png?) ![Age of resignations](images/eda12.png?)

Plan changes can occur at the start of each year. The plots below show the flow of beneficiaries between plans, for those who change plans, in periods 25 and 37.

![Plan change in period 25](images/eda13.png?) ![Plan changes in period 37](images/eda14.png?)

The age distribution of beneficiaries is an important indicator of the scheme's risk profile. A higher proportion of beneficiaries in older age bands will likely result in higher average claims costs for the scheme. The plots below show the age distribution at the end of each year as well as the distribution of beneficiaries by duration of membership at the end of each year.

![Age distribution](images/eda15.png?) ![Duration distribution](images/eda16.png?)

The plots show that period, age, duration, plan, income, type and chronic status are important predictor variables for future exposure. The scheme's membership is increasing, generally with younger members, and mostly on Plan 1. Most of the beneficiary trends appear to be easily predictable. Income trends are slightly more erratic, but still follows a general trend. However, there are no predictable trends in the average age of new entrants or resignations. New entrants will require a completely separate exposure model and are ignored in the remainder of this project.

## The beneficiary exposure model

I have modeled beneficiaries' exposure to membership states using a basic fully connected feedforward artificial neural network (ANN). The ANN has two hidden layers with rectified linear unit activation functions. The number of nodes in the input layer is equal to the number of predictor variables while the hidden layers have twice the number of nodes of the input layer. The output layer has 31 nodes, one for each membership state, and a Softmax activation function to ensure that the probabilities for different membership states sum to one (including the exit state). Since this is a multi-class classifier problem, the model uses a categorical cross entropy loss function. The network structure is provided below.

```r
> summary(model)
Model: "sequential_1"
_________________________________________________________________________________________
Layer (type)                                                        Output Shape
Param #
=========================================================================================
dense (Dense)                                                       (None, 68)
2380
_________________________________________________________________________________________
dense_1 (Dense)                                                     (None, 68)
4692
_________________________________________________________________________________________
dense_2 (Dense)                                                     (None, 31)
2139
=========================================================================================
Total params: 9,211
Trainable params: 9,211
Non-trainable params: 0
_________________________________________________________________________________________
```

The continuous predictor variables in the training data were normalized and the categorical predictor variables were one-hot encoded. A sample of the training data is provided below.

```r
> head(training.data$predictors.train[,1:30])
  Gender.1 State.1 State.2 State.3 State.4 State.5 State.6 State.7 State.8 State.9 ... State.29
1        1       0       1       0       0       0       0       0       0       0 ...        0
2        1       0       0       0       0       0       0       1       0       0 ...        0
3        1       0       0       0       0       0       0       1       0       0 ...        0
4        0       1       0       0       0       0       0       0       0       0 ...        0
5        0       1       0       0       0       0       0       0       0       0 ...        0
6        0       0       1       0       0       0       0       0       0       0 ...        0

> head(training.data$predictors.train[,continuous.predictors])
        Age   Duration     Adults   Children
1 0.5528394 -0.6649328  0.6447128  0.2104632
2 0.7581789  1.0334974 -1.0651453  0.2104632
3 0.8117840  1.3739275 -1.0651453  0.2104632
4 2.1116329  0.1861426  0.6447128 -1.2793953
5 2.1652379  0.5265727  0.6447128 -1.2793953
6 0.7389924 -0.7486452  0.6447128  0.2104632
```

The network is trained with data for periods 1 to 36. Data for periods 37 to 48 are kept for "blind" prediction. The training data is split between a training set and a validation set using balanced stratified sampling with a 30% probability of being included in the validation set. The network trains over a maximum of 150 epochs and stops training when the validation loss has not improved for 10 consecutive epochs. The validation and training loss and accuracy are plotted below.

![MCC training loss](images/mcc1.png?)
![MCC training accuracy](images/mcc2.png?)

The validation loss decreases gradually from approximately 0.39 to 0.35 over 147 epochs. The validation accuracy improves gradually from 0.90 to 0.91 over the 147 epochs.

## The claim prediction model

Claims are also modeled using a basic fully connected feedforward ANN. The ANN also has two hidden layers with rectified linear unit activation functions. The number of nodes in the input layer is equal to the number of predictor variables while the hidden layers have 64 and 128 nodes respectively. The output layer has 12 nodes, one for each prediction period, and a linear activation function. Since this the main objective is to predict the average claim for the scheme as a whole, the model uses a mean squared error loss function. A mean absolute error loss function was also tested and provided good predictions for individual members, but worse predictions for the scheme as a whole. The network structure is provided below.

```r
> summary(model)
Model: "sequential"
______________________________________________________________________________________________________________________
Layer (type)                                         Output Shape                                   Param #           
======================================================================================================================
dense (Dense)                                        (None, 64)                                     3840              
______________________________________________________________________________________________________________________
dense_1 (Dense)                                      (None, 128)                                    8320              
______________________________________________________________________________________________________________________
dense_2 (Dense)                                      (None, 12)                                     1548              
======================================================================================================================
Total params: 13,708
Trainable params: 13,708
Non-trainable params: 0
______________________________________________________________________________________________________________________
```

[Morid *et al.* (2016)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5977561/#!po=3.12500) proposed the following predictor variables for a cost on cost prediction algorithm:

* Total claims paid during the last 12 months
* Total medicine claims paid during the last 12 months
* Total non-medicine claims paid during the last 12 months
* Total claims paid during the last 6 months
* Total claims paid during the last 3 months
* Total claims paid in each of the last 12 months (12 separate variables)
* An indicator of the trend in claims over the last 12 months (increasing or decreasing). I have used the slope of a regression line fitted to claims paid in the last 12 months
* The maximum claims paid in any of the last 12 months
* The average claims paid per month during the last 12 months
* A variable measuring the difference between the maximum claim in any of the last 12 months and the average claim over the last 12 months
* The number of months in the last twelve months during which claims exceeded the average monthly claim

In addition to the predictor variables proposed by [Morid *et al.* (2016)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5977561/#!po=3.12500), I have also included:

* Hospital claims paid during the last 12 months
* Chronic claims paid during the last 12 months
* The beneficiary's latest age, duration, number of adult dependants, number of child dependants
* The latest member state occupied by the beneficiary

The continuous predictor variables in the training data were normalized and the categorical predictor variables were one-hot encoded. A list of the variables included in the training data is provided below.

```r
> colnames(train_data)
 [1] "Age"           "Duration"      "Adults"        "Children"      "ClaimTotal"    "ClaimHospital" "ClaimChronic"
 [8] "ClaimOther"    "ClaimSix"      "ClaimThree"    "ClaimTotal.13" "ClaimTotal.14" "ClaimTotal.15" "ClaimTotal.16"
[15] "ClaimTotal.17" "ClaimTotal.18" "ClaimTotal.19" "ClaimTotal.20" "ClaimTotal.21" "ClaimTotal.22" "ClaimTotal.23"
[22] "ClaimTotal.24" "ClaimSlope"    "ClaimMax"      "ClaimAvg"      "ClaimAcute"    "ClaimHigher"   "Gender.1"     
[29] "Gender.2"      "State.1"       "State.2"       "State.3"       "State.4"       "State.5"       "State.6"      
[36] "State.7"       "State.8"       "State.9"       "State.10"      "State.11"      "State.12"      "State.13"     
[43] "State.14"      "State.15"      "State.16"      "State.17"      "State.18"      "State.19"      "State.20"     
[50] "State.21"      "State.22"      "State.23"      "State.24"      "State.25"      "State.26"      "State.27"     
[57] "State.28"      "State.29"      "State.30”
```

The network is trained with data for periods 13 to 36 with claims paid during the preceding 12 months for each month of data. Data for periods 37 to 48 are kept for "blind" prediction. The training data is split between a training set and a validation set with a 30% probability of being included in the validation set. The network trains over a maximum of 200 epochs and stops training when the validation loss has not improved for 20 consecutive epochs. The validation and training loss, measured using the mean squared error and the mean absolute error, are plotted below.

![Regression training mse](images/reg1.png?)
![Regression training mae](images/reg2.png?)

The mean squared error decreases gradually from approximately 473,000 to 469,000 over 45 epochs. The mean absolute error increases gradually from 71 to 106 over the 45 epochs. The simultaneous decrease in the mean squared error and increase in the mean absolute error is mainly a result of the impact of very high cost members. The mean squared error is very sensitive to very high claiming members. The model optimized using mean squared error will therefore prioritize high cost members. However, this results in overestimated costs for very low claiming members. The mean absolute error is not as sensitive to errors when predicting claims for high cost members, but is more sensitive to errors for low claiming members.

## The predictions

The network accuracy is tested by predicting outcomes for periods 37 to 48. Period 36 is the base period from which the predictions are made. In other words, it is assumed that all data up to period 36 is known.

Since I am interested in the prediction error for the scheme as a whole, I have used the mean average percentage error to measure accuracy. This is a natural metric to use in this instance since it provides logical and easy to interpret explanations such as "Claims were overestimated by 2.4%". The mean average percentage error (MAPE) is provided below the title on each plot (where relevant).

The plots below show the actual and predicted number of beneficiaries by plan, income band, type and chronic status.

![Predicted beneficiaries by plan](images/mcc3.png?) ![Predicted beneficiaries by income](images/mcc4.png?)
![Predicted beneficiaries by type](images/mcc5.png?) ![Predicted beneficiaries by chronic status](images/mcc6.png?)

The plots below show the actual and predicted average age of beneficiaries by plan, income band, type and chronic status.

![Predicted age by plan](images/mcc7.png?) ![Predicted age by income](images/mcc8.png?)
![Predicted age by type](images/mcc9.png?) ![Predicted age by chronic status](images/mcc10.png?)

The plots below show the predicted and actual flow of beneficiaries between plans, for those who change plans, in period 37.

![Predicted plan change in period 37](images/mcc13.png?) ![Plan changes in period 37](images/eda14.png?)

The plots below show the predicted and actual age distribution as well as the distribution of beneficiaries by duration of membership in period 48.

![Predicted age distribution](images/mcc11.png?) ![Predicted duration distribution](images/mcc12.png?)

The plots below show the average claim per beneficiary per month for each month in the prediction period and the average claim per beneficiary per month by age.

![Predicted claim per month](images/reg3.png?) ![Predicted claim by age](images/reg6.png?)

The plots below show the average claim per beneficiary per month by plan and beneficiary type.

![Predicted claim by plan](images/reg4.png?) ![Predicted claim by type](images/reg5.png?)

## Summary

The classifier model used to predict beneficiary exposure was extremely accurate for the existing cohort of beneficiaries. It was almost to predict most trends with 90% plus accuracy. The accuracy of the predicted age distribution (12 months in advance) was especially impressive. However, due to the change in income trends during the prediction period, the model was not able to predict the exposure to different income bands with the same level of accuracy. It should be noted that this is a big dataset for a stable medical scheme with easily predictable beneficiary trends. I would have been surprised if the model was not able to predict these trends with a very high level of accuracy.

Although not as accurate as the predicted beneficiary exposure, the regression model predictions were still surprisingly good. The overall MAPE was less than 2% which is well within acceptable norms for South African medical schemes. The model was not adjusted to account for inflationary medical tariff increases, and therefore, it was expected that it would predict slightly lower costs than the actual claims costs. The prediction accuracy was reasonable when predicting claims by plan and beneficiary type, but was very good when predicting claims by age.

The high accuracy when predicting the beneficiary age distribution and the claims by age is very encouraging. These two curves are vital when estimating changes in medical scheme benefit utilization.

## Possible Further Investigations

There are two main areas for further investigation:

* The development of a model to predict exposure and demographic details of new members
* Refine the claims model to account for seasonality trends due to school holidays, public holidays, number of workdays per month etc.

## References
Morid et al. (2018). Supervised Learning Methods for Predicting Healthcare Costs: Systematic Literature Review and Empirical Evaluation. AMIA Annual Symposium proceedings. AMIA Symposium. 2017. 1312-1321. Retrieved from: https://www.researchgate.net/
