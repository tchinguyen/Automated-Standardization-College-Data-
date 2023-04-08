# Automated Standardization College Data
Project: US News and World Report Best Colleges ranking
Client: Chancellor’s Office of Data Analytics (CODA)

## Project Name

Automated String Matching and Standardization for Longitudinal US News Rankings

## Introduction

This project aims to develop a method that calculates the difference (or similarity) between two strings (such as school or metric names) and automatically provides recommendations for top matches and standardized names. The primary objective is to append the latest 2023 data into the longitudinal US News ranking data that has been compiled by CODA since 2014. The project uses fuzzy string method, string regular expression, and string comparing techniques to identify and standardize the names of schools and metrics.

## Dataset

The project uses two datasets, provided by CODA. The first dataset is the longitudinal US News ranking data (Longitudinal USNWR ranking data.xlsx), which includes all ranking component metrics, overall rank and overall score for all national universities from ranking issue year 2014 to 2022. The data has been cleaned and standardized by CODA. The second dataset (compressed csv file in ‘Raw data extracted from Academic Insights’ folder) is the raw data recently extracted from the Academic Insights portal, which includes both historical issue years (1990 – 2022) and the latest issue year (2023).

## Methodology

The project utilizes fuzzy string matching, string regular expressions, and string comparison techniques to identify and standardize the names of schools and metrics. The method first compares each raw school and metric name from the latest dataset with the standardized names in the longitudinal dataset. Fuzzy string matching algorithms are used to calculate the similarity or difference between two strings, taking into account spelling mistakes, typos, punctuation, and whitespace. The algorithm produces a similarity score, which is used to identify top matches between the raw and standardized names.

Once the top matches are identified, string regular expressions are applied to detect and correct minor differences such as case differences, punctuation, and whitespace. The corrected names are then compared using string comparison techniques to ensure that the corrected names match the standardized names in the longitudinal dataset. If the names do not match, the algorithm will repeat the process of fuzzy matching and regular expression correction until a standardized name is found.

## Conclusion

The developed method provides an automated approach to matching and standardizing the names of schools and metrics in the longitudinal US News ranking data. The method can be applied to future datasets to ensure that the standardized names remain consistent over time. The automated approach saves time and reduces the potential for errors in the manual matching and standardization process.
