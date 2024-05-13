# Predicting patent lawsuits with machine learning

This repository contains the necessary code to reproduce most the results in "Predicting patent lawsuits with machine learning", currently under revision in the International Review of Law & Economics.

This analysis is conducted in R, using the tidyverse approach, with particular use of the [tidymodels](www.tidymodels.org) metapackage for the modeling. 

The repository is structured as follows:

- The main data and metadata files are contained in the `data/` folder. The cleaned up data that we use for the predicive modeling in the paper is contained in the file `block1-2-3-4-5-data-Rdata`. We do not include the full raw data set in this repository because it is too big (~3.1GB). The analyses in the robustness section makes use of the full raw data, so this code will not run without the full data file, which we will supply upon request. 
- The code files for running the models and for extracting and formatting the results are contained in the `src/` folder. Please start by consulting the file `00-master.R`. All the code files are documented in the comments.
- The estimation takes a long time to run on a personal computer, in partiular due to the variable importance calculations. We have therefore supplied the estimation results in the folder `est-results/`.
- Finally, we store the tables and figures, computed using the estimation results, in the folder `for-paper/`.

Note that we use the R package `here` for easy and robust handling of file paths in the project. 

## References

Kuhn, Max, and Hadley Wickham. "Tidymodels: a collection of packages for modeling and machine learning using tidyverse principles." (2020).

Müller Kirill. "here: A Simpler Way to Find Your Files". R package version 1.0.1, (2020) [https://CAN.R-project.org/package=here].

Wickham, Hadley, et al. "Welcome to the Tidyverse." Journal of open source software 4.43 (2019): 1686.

