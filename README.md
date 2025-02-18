# TSGenerator
![TSGenerator](Logo1.png)

**TSGenerator** is an R package designed to facilitate the generation and time series analysis of Copernicus HR-VPP products. This package includes functions for data imputation, analysis and visualization.

Within the pan-European component the Copernicus Land Monitoring Service produces and disseminates a set of products with high spatial resolution on phenology and vegetation productivity (10 m2) and high repetition frequency (WEkEO, 2021; Eklundh et al., 2023; Goihl, 2023). These products are derived from the Sentinel-2 satellite constellation (2A and 2B) and cover 32 EU member countries, UK and 6 cooperating countries of the Western Balkans since January 2017 with daily, decadal and annual frequency. Thirty-one types of HR-VPP products are contained in three groups, a total of 1522 files and more than 900 000 mosaics per year, totaling a volume of 80 Tera bytes per year. These data are free of charge and freely accessible through the WEkEO Data and Information Access Service.

## Author and collaborators
**Author:** *MSc.Alexey Valero Jorge* 

**Email:** avalero@cita-aragon.es

**Contributor:** *Dr. José Tomás Alcalá*

**Contributor:** *Dra. Ma. Auxiliadora Casterad*

## Table of Contents

- [Installation](#installation)
- [Use](#use)
- [Main Functions](#functions-main)
- [Examples](#examples)
- [Contributions](#contributions)
- [License](#license)
- [References](#references)

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

- hda (Harmonized Data Acces): For more information on HDA-API developed by WEkEO see: https://help.wekeo.eu/en/articles/9515753-what-is-the-harmonized-data-access-hda-api

In order to interoperate with the Python environment in R, be sure to install the **reticulate** package.

## Main Functions

The main functions of TSGenerator are grouped into several modules which are presented below:

### Download module
 1- **Download.HRVPP** function: it is used for the discharge of the different products of phenology and productivity of the vegetation.
 
 2- **Download.VI** function: it is used for downloading the different raw vegetation indices (NDVI, LAI, fAPAR and PPI) and the quality product QFLAG2.
 
 3- **Download.STPPI** function : is used to download the seasonal trajectories of the PPI.
### Preprocessing module

 1- **getStack** function: this function searches for .TIFF files of vegetation indexes (VI) in a specified directory, and for each IV file, it tries to find a QFLAG file corresponding to its date. As a result, it generates a new raster-stack file. 
 
 2- **getCleanIV** function: from the created raster-stack files, noisy pixels are removed from the IV images (clouds, cloud shadows, water, etc.), using the QFLAG2 product as a criterion (values != 1 are considered noisy).

### Time series extraction


1- **get.Series.mean** function: used to extract the average time series of the IVs from a shapefile of the area of interest.

2- **get.Series.median** function: used to extract the median time series of the IVs from a shapefile of the area of interest.

3- **get.Series.VPP** function: is used to extract median phenology and productivity data from Copernicus (SOS, EOS, MAX, etc).

### Missing data analysis and visualization

1- **general.Quality** function: is used to obtain the average percentage of the quality of all time series (in case of multiple time series) as a function of the number of observations in each series.


## Examples

```r
# It is necessary to configure the PATH where python.exe and the hda module are located by creating the object "ruta_python".
ruta_python <- "PATH/python.exe" # replace "PATH" with the path to the directorywhere the python executable is located

# Use of the "Download.VI" function
Download.VI(
user = "user_name", # replace "user_name" with the user registered in the WEkEO platform
password = "Password", # replace "password" with the password connected to the same user
dataset_id = "EO:EEA:DAT:CLMS_HRVPP_VI", # product identifier
productType = "NDVI", 
platformSerialIdentifier = "S2A",
tileId = "30TXL",
start = "2020-01-01T00:00:00.000Z",
end = "2020-01-10T00:00:00.000Z",
bbox = c(-0.89285, 41.48762, -0.86284, 41.50456),
download_path = "local_directory" # replace with the directory where the data is to be stored
)
```

```r



```
## License

This project is licensed under the MIT License. See the LICENSE file for more details.

## References
