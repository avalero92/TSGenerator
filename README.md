# TSGenerator
![TSGenerator](Logo1.png)

**TSGenerator** is an R package designed to facilitate the generation and analysis of time series. This package includes functions for data imputation, analysis and visualization.

Within the pan-European component the Copernicus Land Monitoring Service produces and disseminates a set of products with high spatial resolution on phenology and vegetation productivity (10 m2) and high repetition frequency (WEkEO, 2021; Eklundh et al., 2023; Goihl, 2023). These products are derived from the Sentinel-2 satellite constellation (2A and 2B) and cover 32 EU member countries, UK and 6 cooperating countries of the Western Balkans since January 2017 with daily, decadal and annual frequency. Thirty-one types of HR-VPP products are contained in three groups, a total of 1522 files and more than 900 000 mosaics per year, totaling a volume of 80 Tera bytes per year. These data are free of charge and freely accessible through the WEkEO Data and Information Access Service.

## Author and collaborators
**Author:** MSc.Alexey Valero Jorge; 

**Email:** avalero@cita-aragon.es

**Contributor:** Dr. José Tomás Alcalá

**Contributor:** Dra. Ma. Auxiliadora Casterad

## Table of Contents

- [Installation](#installation)
- [Use](#use)
- [Main Functions](#functions-main)
- [Examples](#examples)
- [Contributions](#contributions)
- [License](#license)

## Installation

You can install the package directly from GitHub using the following code:

```r
# Install devtools if you do not have it
install.packages("devtools")

# Install TSGenerator from GitHub
devtools::install_github("avalero92/TSGenerator")
```

## Use

This tool uses certain Python libraries to function properly. Before you can use the TSGenerator package, you must make sure you have the following Python libraries installed:

-hda (Harmonized Data Acces): For more information on HDA-API developed by WEkEO see: https://help.wekeo.eu/en/articles/9515753-what-is-the-harmonized-data-access-hda-api

In order to interoperate with the Python environment in R, be sure to install the "reticulate" package.

## Main Functions
