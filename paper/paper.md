---
title: 'ANCRTAdjust: An R package to adjust ANC-RT data to reduce bias in estimating HIV prevalence trends'
tags:
  - R
  - antenatal care routine HIV testing data
  - HIV prevalence trends
  - data cleaning
  - bias adjustment
authors:
  - name: Brittany Blouin
    orcid: 0000-0001-7447-7467
    affiliation: 1
  - name: Mathieu Maheu-Giroux
    orcid: 0000-0002-8363-4388
    affiliation: 1
affiliations:
  - name: McGill University, Department of Epidemiology, Biostatistics, and Occupational Health
    index: 1
date: July 12, 2019
bibliography: paper.bib
---

# Summary

HIV surveillance in high-burden countries has traditionnally been conducted using sentinel surveillance of pregnant women attending antenatal care (ANC-SS). Nowadays, program data from routine HIV testing at antenatal care (ANC-RT) are increasingly being used for monitoring HIV epidemic trends [@WHO:2015]. Importantly, using ANC-RT data could increase representativeness, geographical coverage, and granularity of HIV surveillance systems. If ANC-RT data on HIV serostatus are to be used for HIV surveillance, several challenges need to be addressed, however [@Diaz:2005]. First, the use of routinely collected program data raises concerns regarding data completeness, with some health facilities not reporting HIV testing data [@WHO:2013]. Second, ANC-RT data could be inconsistent with missing and/or invalid data. Missing data problems include: missing entire reporting periods or missing one or more variables within a reporting period. Invalid data problems include: negative count values, the *‘number of HIV-positive women’* exceeding the *‘number of women tested for HIV’*, and, the *‘number of women tested for HIV’* exceeding the *‘number of women who attended the health facility’*. Finally, imperfect testing coverage during antenatal care could result in selection bias, leading to overestimation of HIV prevalence [@Maheu-Giroux:2019].

``ANCRTAdjust`` is an R package that faciliates assessment of ANC-RT data quality, helps clean ANC-RT data, and, if warranted, can perform specific adjustments for common data inconsistencies as to minimize both information and selection in HIV prevalence estimates. Functionalities to longitudinally visualize and tabulate data quality indicators, as well as to identify outliers, are provided. A standard data cleaning function can be used to impute missing data (when possible), and to eliminate impossible negative count values and the possibility that the *‘number of HIV-positive women’* exceeds the *‘number of women tested’*. Specifically, different functions can be used to:

1. Evaluate data quality and completeness;
2. Perform data cleaning;
3. Identify outlying observations; and
4. Perform adjustments for common data inconsistencies due to incomplete reporting periods (using inverse probability of censoring weighting), multiple testing occuring when the same women are tested multiple times during the reporting period (leading to the *‘number of women tested for HIV’* exceeding the *‘number of women who attended the health facility’*), and/or imperfect testing coverage using empirically derived adjustment factors.

As several countries transition from ANC-SS to ANC-RT data, it is imperative that clear and simple guidelines be established for the use of such data so that valid conclusions can be drawn for HIV surveillance purposes. The ``ANCRTAdjust`` R package was designed to be used by program managers, and public health practionners and researchers alike, to help clean and adjust (if warranted) ANC-RT data for monitoring HIV trends in endemic countries.  A standard protocol for data validation, implemented across all countries providing ANC-RT data for monitoring HIV trends, will ensure that HIV prevalence estimates are consistent, reproducible, and valid.

# Acknowledgements

This project was funded by the *Joint United Nations Programme on HIV/AIDS* (UNAIDS). MMG’s research program is funded by a career award from the *Fonds de recherche du Québec – Santé*.

# References

