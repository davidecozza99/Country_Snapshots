This repository provides access to the R scripts used to generate all figures available in the [FABLE country snapshots](https://fableconsortium.org/publications/fable-pathways-for-sustainable-food-and-land-use-systems/).

The scripts were developed by Davide Cozza and Clara Douzal from the FABLE Secretariat. Some of the figures are inspired by and have been enhanced from previous FABLE reports and briefs.
Getting Started

To run the scripts on your own device, follow these steps:

    Clone the Repository:

    bash

git clone [repository URL]

Set Up the R Environment:

    Navigate to the cloned repository directory.
    Install the required R packages using the renv environment. Ensure you have renv installed:

    R

install.packages("renv")

Restore the project’s R environment:

R

    library(renv)
    renv::restore()

Run the Scripts:

    Open R or RStudio.
    Set your working directory to the cloned repository:

    R

        setwd("path/to/cloned/repository")

        Execute the desired R script.

Dependencies

This project uses the renv package to manage R dependencies. The necessary packages and their versions are specified in the renv.lock file.
Contact

For any questions or issues related to the scripts, please contact:

    Davide Cozza (davide.cozza@unsdsn.org)
    Clara Douzal (clara.douzal@unsdsn.org)
