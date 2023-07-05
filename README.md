# CancerCovid_Characterisations

Code for characterising cancer patients on a number of variables before and after COVID19 lockdown.

The frequency of screening appointments (specific to breast, colorectal, lung and prostate cancer), diagnostic tests (specific to each cancer) and interactions in the healthcare system, baseline characteristics, measurements and procedures, will be calculated at multiple time points (before, during and after the COVID-19 lockdown) in the general population (denominator), and in separate populations of breast, colorectal, lung and prostate cancer patients. Significant differences in the frequencies of these variables across time points will be estimated by observing the standardised mean difference.

Characterisation includes standard charactersitics using OHDSI's 'Feature Extraction' package, as well as bespoke characterisations for cancer specific variables.

These analyses are featured in the following paper:

Barclay, N.L., Pineda Moncusi, M., Jödicke, A. M., Prieto-Alhambra, D., Raventós, B., Newby, D., Delmestri, A., Man, W-Y., Chen, X., & Català, M. (in preparation). Changes in Incidence of Breast, Colorectal, Lung and Prostate Cancer, and Screening and Diagnostic Tests, Before, During and After the UK National COVID-19 Lockdown: A Retrospective Cohort Study Using UK Primary Care Health Records

# Running the analyses
Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop).
Open the project CancerCovid_Characterisations.Rproj in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)

Open and work though the CodeToRun.R file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study (source(here("RunStudy.R")).

After running you should then have a zip folder with results to share in your home directory.

# Associated repositories and documents
The paper (Barclay et al., in prep) contains analyses from other github repositories which can be found here:

https://github.com/oxford-pharmacoepi/CancerCovid_CohortDiagnostics

https://github.com/oxford-pharmacoepi/CancerCovid_Characterisations

https://github.com/oxford-pharmacoepi/CancerCovid_IncidencePrevalence

In addition, a shiny app through which to view additional cohort diagnostics can be found here: https://dpa-pde-oxford.shinyapps.io/CancerCovid_CohortDiagnosticsShiny_paper1/
