# article-belief-hierarchical-spatial
This repository contains data and code supporting the following paper:

Katayama et al., "Belief inference for hierarchical hidden states in spatial navigation"

FigureX.R files will generate the specified plots from the paper.

Unthresholded group-level statistical maps supporting our results are available on NeuroVault: [https://neurovault.org/collections/NZJMDMFQ/](https://neurovault.org/collections/NZJMDMFQ/).

**Folders**

_data_

contains anonymised behavioural data for the behavioral experiment (outside of the scanner; in _behavioural_ subfolder) and the scanning experiment (in _scanning_ subfolder).

sX_Y_Z.mat files contain the data from subject #X in session #Z of the behavioural (Y=1) or scanning (Y=0) experiment.

data.RData file contains the data from all participants and sessions of both experiments in data.frame format (trials in which no action was selected have been already excluded).



_model_

contains the scripts for fitting the inference models to the data (in _models_ subfolder), estimated parameters for each models (in _parameter_ subfolder) and the posterior distributions of the inference of the tiger door position and the grid location generated using the model.

In _behavioural_ and _scanning_ subfolders, sX_Y_Z_res.mat files correspond the results of subject #X based on inference model Z.
