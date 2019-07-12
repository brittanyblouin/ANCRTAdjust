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
  - name: McGill University, Department of Epidemiology, Biostatistics and Occupational Health
    index: 1
date: July 12, 2019
bibliography: paper.bib
---

# Summary

Program data from routine HIV testing at antenatal care (ANC-RT) are increasingly being used for monitoring HIV epidemic trends.  However, if ANC-RT data on HIV serostatus are to be used to monitor epidemiological trends in HIV prevalence, several challenges need to be addressed [@Diaz:2005]. Despite important advantages of using ANC-RT over conducting sentinel surveillance (ANC-SS), the use of routinely collected program data raises concerns regarding data completeness and consistency [@WHO:2013].  The most common challenges related to using ANC-RT data include missing data and invalid data.  Missing data problems include: missing entire reporting periods, missing one or more variables within a reporting period, and, imperfect HIV testing coverage.  Invalid data problems include: negative count values, the ‘number of HIV-positive women’ exceeding the ‘number of women tested for HIV’, and, the ‘number of women tested for HIV’ exceeding the ‘number of women who attended the health facility’. 

``ANCRTAdjust`` is an R package that automatically evaluates ANC-RT data quality, cleans ANC-RT data, and, performs specific adjustments for common data inconsistencies to minimize selection bias and information bias in HIV prevalence estimates.  Data quality evaluation can be visualized in tables and graphs of data quality over time.  A standard data cleaning function can be used to impute missing data (when possible), and to eliminate impossible negative count values and the possibility that the ‘number of HIV-positive women’ exceeds the ‘number of women tested’.  Finally different adjustment functions can be used to: 1- adjust HIV prevalence estimates for missing reporting periods using inverse probability of censoring weighting; 2- adjust the ‘number of women tested for HIV’ for the possibility that the same women were tested multiple times throughout the reporting period (leading to the ‘number of women tested for HIV’ exceeding the ‘number of women who attended the health facility’); and, 3- adjust for imperfect HIV testing coverage using testing coverage-specific adjustment factors.

With many countries transitioning from ANC-SS data to the use of ANC-RT data, it is imperative that clear and simple guidelines be established for the use of such data so that valid conclusions can be drawn.  The ``ANCRTAdjust`` R package was designed to be used program managers using ANC-RT data to monitor HIV trends in endemic countries.  A standard protocol for data validation, implemented across all countries providing ANC-RT data for monitoring HIV trends, will ensure that HIV prevalence estimates are consistent, reproducible and valid.

# Acknowledgements

Funding for this project was obtained from UNAIDS.

# References

