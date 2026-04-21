# ds2003project3
Codebase for DS 2003 Project 3, Spring 2026

To run this app locally, use the csv file in the dataset folder labeled "dsproject3data.csv", which  is the cleaned version of data from the World Bank that was originally accessed at https://github.com/light-and-salt/World-Bank-Data-by-Indicators/blob/master/economy-and-growth/economy-and-growth-raw-2021.csv.

To address missing values in the data, we manually coalesced data from Our World In Data (shares-of-gdp-by-economic-sector.csv) and the World Bank website. Given the manual effort this required, we recommend you use the cleaned dataset, but the data cleaning script and individual data source files are also provided in the same folder.

To scroll through the article version of our data story, run the quarto file "data-story.qmd", using "styles.css" to adjust the html styling. The end of the data story links to the published version of our shiny app dashboard that can also be run locally using "interactive-exploration.R". 
