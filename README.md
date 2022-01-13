Introductory Statistics for Data Analytics
================

## Your one-stop-shop for stats essentials.

### Overview

Statistics can be an overwhelming subject when first approaching data
analytics. This repository is for the code related to the “Introductory
Statistics for Data Analytics” Medium series detailing useful statistics
for those looking for a gentle introduction to the topic or for
refresher material on commonly used approaches.

### The Articles

The “Introductory Statistics for Data Analytics Part I: Descriptive
Stats” can be found hosted on Medium [HERE](xxx) and the follow-up
article “Introductory Statistics for Data Analytics Part II: Inferential
Stats” can again be found hosted on Medium [HERE](xxx). Both designed
and authored by Emily Burns.

### The Data

The “SF Salaries” dataset is currently available on Kaggle
[HERE](https://www.kaggle.com/kaggle/sf-salaries). The dataset consists
of over 100,000 salaries and their respective San Francisco located
employees across the years 2011 to 2014, For the purpose of this
project, the dataset is used to demonstrate how to apply basic
statistical concepts to mock data analytic deliverables.

#### /raw\_data

-   **SF\_Salary.csv**: raw data pulled from Kaggle

#### /processed\_data

-   **SF\_Salary\_Clean.csv**: cleansed data (product of
    *01\_data\_cleaning.R* script).

### The Code

#### /code/script

-   **01\_data\_cleaning.R**: Steps taken to clean original dataset.

-   **02\_do\_descriptive\_stats.R**: Steps taken to demonstrate
    descriptive statistics concepts.

-   **03\_do\_inferential\_stats.R**: Steps taken to demonstrate
    inferential statistics concepts.

-   **/functions**: Functions used in the previously identified scripts.
    Includes funs\_do\_descriptive\_stats.R and
    funs\_do\_inferential\_stats.R
