# Excess Pneumonia Mortality During a Legionnaires' Disease Outbreak in Flint, Michigan

Data and code for the preprint paper with this title. This analysis was conducted for the *PBS Frontline* documentary "[Flint's Deadly Water] (https://www.pbs.org/wgbh/frontline/film/flints-deadly-water/)," which aired September 10, 2019. We are currently waiting on medRxiv to post the paper in the next several days, but in the meantime we have placed a draft of the manuscript in the "manuscript" folder.

The folder "Frontline_Legionnaires_FinalAnalysis" contains reproducible analysis files with all our code. You may run the .Rmd version in RStudio on your local machine. The .md version will display (imperfectly) in Github. You can download the .html version and view it in any web browser - this will look cleaner than using .md in Github.

The folder "data" contains, surprisingly, our data as well as the shapefiles we used for the maps.

The folder "model_objects" contains .rds files of 3 models (our primary + 2 sensitivity analyses). The analysis file is set up to simply read these in to save some time (these models take ~3-5 minutes to run on a Dell XPS13 laptop), but you can simply un-comment and run the appropriate code if you want to check or modify our models, as well!