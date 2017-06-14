

This repository provides all codes that accompany the paper: **_Regional intense precipitation events: robust inference from
credibly simulated GCM state variables_**

This code has been tested in **R** version version 3.3.3 **RStudio** version 1.0.136. The scripts' compatability with other versions is unknown.

Requirements: the following **R** packages (and any dependencies) are necessary and should be installed before attempting to
compile these codes: "akima", "dataRetrieval", "data.table", "dplyr", "ggmap", "ggplot2", "gridExtra", "locfit", "lubridate", "maps", "ncdf4", "readr", "reshape2", "tidyr".

There are two main scripts:

1. **ORB_1_LoadProcessSaveData.R**: This script processes the data in the project folder **Raw_Data** and saves the relevant outputs to the project folder **Processed_Data**.

2. **ORB_2_PaperFigs&Stats.R**: this script creates all of the paper’s figures and computes other statistics reported in the paper. Often these statistical outputs are embedded in the script as comments.

The scripts in the project folder **R** folder are functions that are sourced in **ORB_1_LoadProcessSaveData.R** and **ORB_2_PaperFigs&Stats.R**

If you find an error, please message me at _djf2137@columbia.edu_
