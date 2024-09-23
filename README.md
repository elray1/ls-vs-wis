# ls-vs-wis

This repo has some stuff related to investigations/comparisons of log score and weighted interval score (WIS).  As a fair warning, all of this code is very rough!

The main product here is a comparison of these scores for some forecasts in the flusight challenge.  To reproduce the analysis, follow these steps:

- clone this repository as well as [FluSightNetwork/cdc-flusight-ensemble](https://github.com/FluSightNetwork/cdc-flusight-ensemble) to the same folder on your computer.
- if desired, run `flusight-network-hub/make-flusight-network-hub-model-outputs.R` and `flusight-network-hub/make-flusight-network-hub-target-data.R` to reproduce the contents of that folder.  Essentially, these copy over the data from the `cdc-flusight-ensemble` repository, update it to be in a format consistent with the hubverse, and convert bin probability predictions to quantile predictions. You will likely have to install some package dependencies to run these scripts, including the R package [distfromq](https://github.com/reichlab/distfromq), which has functionality related to converting between different representations of forecast distributions.
- run `compute-scores.R` to compute log scores and wis.  Again, check that you have the needed package dependencies installed.
- step through `analyze scores.R` to create some plots.

There is also a stub of a simulation study in the simulation folder.
