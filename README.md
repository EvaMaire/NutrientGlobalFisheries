# NutrientGlobalFisheries

Data, statistical analyses, and R scripts accompanying Maire et al. Micronutrient supply from global marine fisheries under climate change and overfishing, 2021, Current Biology.

Using the [SAU catch reconstruction database](http://www.seaaroundus.org/tools-guide/), we extracted catches from the EEZ of each country in tonnes and by species for the period 2010–2014. 
We considered reported and unreported catches from the EEZ of each country and we excluded discards from these data. We limited the scope of our study to marine fisheries. 

[data](/data) contains .csv and .RData files of data underlying Figs 1-4 and S1-S5.

[r](/r) contains .R scripts with code to create Figs 1-4 and S1-S5. 

[figures](/figures) contains outputs (Figs 1-4 and S1-S5).

This repo contains the summary data but provides one example (see [clean_catch_data](/r/clean_catch_data.R)) which shows how to convert the raw SAU catch data into the summary data.
Contact the SAU team to request additional data. 

Data viz and analyses were run in R 4.0.3.

If you would like to replicate the analysis, clone this repository and run the R code.

Please contact Eva Maire (emg.maire@gmail.com) for questions regarding the code or modelling.