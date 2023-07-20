# GCDofDisease
Code and data for Mahon et al. _Global change drivers and the risk of infectious disease_

Note: All analyses were conducted on a multithread version of R, using a computer with 32 GB of ram and 16 logical processors. Because of the large number of observations and studies, on a non-multithreaded version of R on the same computer, the `rma.mv()` models took greater than 30 minutes to run, while only taking 30 seconds to run on the multithreaded version of R. So, be aware, that running these models will take a large amount of time and computation resources. This is why the knitted Rmarkdown has been included in the "Code" folder of this Rproject. Additionally, all meta-data for the dataset is present in the first sheet of the Excel file "GCDofDisease_Metaanalysis_Database.xlsx" wihtin the Data folder of this Rproject.

Please report issues/questions on the [Issues tab](https://github.com/mahonmb/GCDofDisease/Issues).
