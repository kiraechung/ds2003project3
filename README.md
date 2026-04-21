# ds2003project3
Codebase for DS 2003 Project 3, Spring 2026

## Description of files in this repository:
- **data-story.qmd:** This quarto file runs the main data story, which includes the interactive dashboard at the end. Closeread, tidyverse, and plotly libraries should be installed to run the file. Instructions for Closeread are below.
- **interactive-exploration.R:** This r file runs the shiny interactive dashboard. Shiny, tidyverse, plotly, stringr, and bslib libraries should be installed to run the file.
- **styles.css:** This css file styles the Quarto data story.

Within the **dataset** folder:
-   **dsproject3data.csv:** This csv file is our cleaned dataset.
-   **ds2003datacleaning.R** This r file runs the data tidying code.
-   **countrymetadata.csv:** This dataset contains region and income group by country. We used this dataset for it's region classification only, as it's income group classifcation only classified income group's at one point in time.
-   **income.csv:** This dataset contains GNI per cap income group classifications per year.
-   **shares-of-gdp-by-economic-sector.csv:** This dataset contains shares of GDP by economic sector for additional countries, filling in missing values.

## Instructions for running our data story locally:

To run this app locally, use the csv file in the dataset folder labeled "dsproject3data.csv", which is the cleaned version of data from the World Bank that was originally accessed at https://github.com/light-and-salt/World-Bank-Data-by-Indicators/blob/master/economy-and-growth/economy-and-growth-raw-2021.csv. We used the "economy-and-growth-raw-2021.csv" dataset.

To address missing values in the data, we manually coalesced data from Our World In Data ("shares-of-gdp-by-economic-sector.csv") and the World Bank website. Given the manual effort this required, we recommend you use the cleaned dataset, but the data cleaning script and individual data source files are also provided in the same folder.

To scroll through the article version of our data story, run the quarto file "data-story.qmd", using "styles.css" to adjust the html styling. In the same directory as the qmd file, run the following command to make the Closeread extension available to this document: quarto add qmd-lab/closeread. The end of the data story links to the published version of our shiny app dashboard that can also be run locally using "interactive-exploration.R". 
