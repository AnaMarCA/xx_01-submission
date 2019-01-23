# Assignment for candidates for the position - Statistician - Infection Epidemiology and Modelling (1884-SM) at the Norwegian Institute of Public Health

## Email to applicants

We will be sending you a basic skills test that will test your skills in R and the surveillance of infectious diseases. As a part of this test, you will need to sign up for a free GitHub account (https://github.com/join).

## Background

This is an assignment related to infectious disease surveillance that will test your skills in R. The deadline for submission of the test is found in your email. Please read through all of this document before beginning the test.

The scenario is broadly as follows:

You are responsible for `Disease X`. There is a data file `data_raw/individual_level_data.RDS` which contains individual level daily data for `Disease X` for 422 municipalities between `2000-01-01` and `2010-12-31`. Each row of this dataset corresponds to 1 sick person on that date. To be more explicit:

- If there are 100 rows for `Municip0301` on `2000-01-01` it means that there were 100 sick people in `Municip0301` on `2000-01-01`.
- If there are 0 rows for `Municip0301` on `2000-01-01` it means that there were 0 sick people in `Municip0301` on `2000-01-01`.

For each municipality you will use the data between `2000-01-01` and `2009-12-31` to build a regression model that predicts the expected number of sick people. You will then use this model to predict the expected number of sick people between `2010-01-01` and `2010-12-31`. You will then produce Excel sheets and graphs that contain information regarding the suspected outbreaks in 2010.

The data file `data_raw/norwayLocations.RDS` contains a data.frame that links `location` (e.g. `municip0301`) to `municipName` (e.g. `Oslo`) and `county` (e.g. `county03`) and `countyName` (e.g. `Oslo`). These are real municipality numbers (`kommunenummer`) based on the 2019 municipality lists (https://en.wikipedia.org/wiki/List_of_municipality_numbers_of_Norway).

## English <-> Norwegian

`municipality` = `kommune`

`county` = `fylke`

## Assumptions

- You can assume that the population for each municipality is 10000 people and this number remains constant throughout the entire period.
- Even though the data represents real municipalities from Norway, **DO NOT** download real population data for Norway and use it.

## Test: Setup 

1. Create a new repository called `xx_01_submission` in GitHub (https://help.github.com/articles/creating-a-new-repository/)
2. Clone your new repository to your local computer (https://help.github.com/articles/cloning-a-repository/)
3. Download the following zip file: https://github.com/folkehelseinstituttet/xx_01/archive/master.zip
4. Copy the files into your local `xx_01_submission` folder

## Folder/File Setup

You are required to put your code in the following locations:

- All functions are to be placed in .R files located in the `code` folder (e.g. see `code/CreateFakeData.R`)
- You will write your master file/script/code (that runs all of the requested analyses by calling the functions in `code/*`) in `Run.R` (this file already has a few lines of code in it)
- Your results will be saved in the `results` folder

## Test: Coding

5. Load in the data file `data_raw/individual_level_data.RDS`
6. Create a dataset that contains the aggregated number of sick people per day per municipality.
7. Ensure that your aggregated dataset includes rows/days with zero sick people (e.g. if there were no rows for `2000-01-01`/`Municip0301` in `data_raw/individual_level_data.RDS` then your aggregated dataset will still need to have one row for `2000-01-01`/`Municip0301` with the value 0).
8. Collapse your data down to iso-year/iso-weeks for each municipality. If you are not familiar with iso-week/years, there is information available at https://en.wikipedia.org/wiki/ISO_week_date and https://rdrr.io/cran/surveillance/man/isoWeekYear.html

Do the following for each of the 422 municipalities:

9. Split the data into training data (`2000-01` to `2009-53`) and production data (`2010-01` to `2010-52`)
10. Use the training data to create a regression model that predicts the expected weekly number of sick people. You will need to determine an appropriate regression model (e.g. does the data have seasonality? a trend?). You can do this by either a) examining the code in the function `CreateFakeDataForOneMunicipality` inside `code/CreateFakeData.R` or b) by performing your own statistical analyses. Please write a comment above your model indicating if you examined the code or performed your own statistical analysis. We consider both techniques to be valid and neither choice will be penalized or rewarded over the other. Remember that you do not need to worry about the population changing over time.
11. For the training data, create a 2 standard deviation prediction interval
12. Identify the potential outbreaks in the training data (i.e. number of sick people > prediction interval)
13. Exclude the potential outbreaks from the training data
14. Refit the model using the new training data (without any outbreaks in it)
15. Create a 2 standard deviation prediction interval for the production data
16. Identify the potential outbreaks in the production data
17. Create and save an excel sheet with the potential outbreaks in `results/municipality_results/outbreaks_MunicipXXXX.xlsx` (i.e. one Excel file for each municipality)
18. Create and save a figure that provides a good overview of the situation in the municipality in `results/municipality_results/overview_MunicipXXXX.png` (i.e. one figure for each municipality). An example of one such graph is available at https://github.com/folkehelseinstituttet/xx_01/blob/master/example_report/5218---overvaking-av-totaldodelighet-5218-uke-52.pdf

**Note:** the graphs must include titles with the real municipality name (e.g. `Oslo` instead of `municip0301`). This information is available in the data file `data_raw/norwayLocations.RDS`.

## Test: Creative Assignment

Every week there is a meeting at Folkehelseinstituttet to discuss the current outbreak situation in Norway. The current date is `2010-12-31`. You need to produce graph(s) and/or table(s) for this meeting that will give the meeting participants a good summary of the current situation for `Disease X` in Norway and the situation in the last few weeks. In this meeting you would have a maximum of 2 minutes to present, so your graph(s) and/or table(s) must be easily understood.

You may use external structural data files for this creative assigment (e.g. shapefiles for Norway) if you think it is appropriate.

Please save your graph(s) and/or table(s) into `results/creative_assignment`.

Please provide comments in your code that indicate which parts of your code produce the graph(s) and/or table(s) for this creative assignment.

## Test: Submission

19. Commit your results and push them back to GitHub
20. Verify that your code is viewable in your GitHub repository on the internet (both in `Run.R` and in `code/*.R`)
21. Verify that in your GitHub repository on the internet you have 422 Excel files and 422 graphs in `results/municipality_results/`
22. Verify that in your GitHub repository on the internet you have graph(s) and/or table(s) in `results/creative_assignment/`
23. Send GryMarysol.Groneng@fhi.no and RichardAubrey.White@fhi.no an email with the link to your repository
