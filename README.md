# CancerCovid_Characterisations

Code for characterising cancer patients on a number of variables before and after COVID19 lockdown.

The frequency of screening appointments (specific to breast, colorectal, lung and prostate cancer), diagnostic tests (specific to each cancer) and interactions in the healthcare system, baseline characteristics, measurements and procedures, will be calculated at multiple time points (before, during and after the COVID-19 lockdown) in the general population (denominator), and in separate populations of breast, colorectal, lung and prostate cancer patients. Significant differences in the frequencies of these variables across time points will be estimated by observing the standardised mean difference.

Characterisation includes standard charactersitics using OHDSI's 'Feature Extraction' package, as well as bespoke characterisations for cancer specific variables.

# Running the analysis
Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop).
Open the project CancerCovid_Characterisations.Rproj in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)

Open and work though the CodeToRun.R file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study (source(here("RunStudy.R")).

After running you should then have a results folder with numerous files specific to each of the outcomes above ready to share.