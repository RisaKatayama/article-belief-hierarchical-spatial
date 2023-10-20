# article-belief-hierarchical-spatial
This repository contains data and code supporting the following paper:

Katayama et al., "Belief inference for hierarchical hidden states in spatial navigation"

FigureX.R files will generate the specified plots from the paper.

Unthresholded group-level statistical maps supporting our results are available on NeuroVault: [https://neurovault.org/collections/NZJMDMFQ/](https://neurovault.org/collections/NZJMDMFQ/).

## Folders
### data
contains anonymised behavioural data for the behavioral experiment (outside of the scanner; in _behavioral_ folder) and the scanning experiment (in _scanning_ folder). sX_Y_Z.mat files contain the data from subject #X in session #Z of the behavioural (Y=1) or scanning (Y=0) experiment. data.RData file contains the data from all participants and sessions of both experiments in data.frame format (trials in which no action was selected have been already excluded).
