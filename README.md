# Introduction

Here we provide the code for: 
_Repository to Post-Hurricane Vital Statistics Expose Fragility of Puerto Rico’s Health System_ by Rolando J Acosta and Rafael A. Irizarry.

We provide all the data in the _data_ directory as well, except for those we have no permission to share. However, these data are available and below explain how to obtain them from the providers.

# Data not shared

The data that were are not permitted to share are described below. 

## Teralytics Data

For our code to run you will need to obtain the following five files from [Teralytics Inc](https://www.teralytics.net/):

* `OD_PuertoRicoAftermath_SepFeb_CountyLvlTop10States.csv`
* `OD_PuertoRicoAftermath_SepFeb_OverallDisplacements.csv`
* `OD_PuertoRicoAftermath_SepFeb_StateLvlTop10.csv`
* `teralytics_DailyPopulationTrend_PuertoRico.csv`
* `teralytics_WeeklyPopulationTrend_PuertoRico.csv`

and put then in the `data/teralytics`  directory.

However, we include a csv file with the necessary population estimates needed to run our models. You will have to edit the code to accommodate this. The data are in

* `data/florida/florida-population-estimate.csv`
* `data/puerto-rico/pr-population-estimate.csv`

## Louisiana Daily Mortality Data

We used Louisiana daily mortality data to obtain counts for the day of the hurricane and the two days after. You will need to obtain the following file from the [Louisiana Vital Statistics System](http://ldh.la.gov)

* `D0306 date.xlsx` 
 
You need to put this file in the `data/louisiana` directory.


## Florida Daily Moratlity Data

We used Florida mortality data for Figure 1, Supplemental Figure 6H, and Supplemental Figure 7. You will need to obtain these data from the [Florida Vital Statistics System](http://www.floridahealth.gov)

* `Florida_mortality_data.xlsx`

You will have to put this file in directory `data/florida`


# Reproducing the figures and tables

We include a Makefile that shows how the order the R scripts should be run to reproduce the figures. In general the steps are

* Read-in and wrangle the death count data and population data
* Fit the models
* Generate the figures and tables.

# Correspondence
For any issues with the functionality of the script please [create an issue](https://github.com/rafalab/maria/issues).

## License
The data collected and presented is licensed under the [Creative Commons Attribution 3.0 license](http://creativecommons.org/licenses/by/3.0/us/deed.en_US), and the underlying code used to format, analyze and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
 
 
 
 
 