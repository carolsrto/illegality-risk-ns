# Repository Overview
This repository is under development. It will contain the reproduceability package for the study *Putting numbers on timber illegality risk: the case of ipê in Pará*. Data utilized in the reproduction of this study will be made available through Zenodo upon publication and is now accessible [in this link here](https://chalmers-my.sharepoint.com/:f:/g/personal/franca_chalmers_se/EkG_O4WZAvNMlKcpPH0ZPRYBBibInEe1BEhhKP5sfVpjSg?e=QK8xhj)

## Citation
Franca, S.S.C., Persson, M., Carvalho, T., Lentini, M. Putting numbers on timber illegality risk: the case of ipê in Pará, 21 December 2022, PREPRINT (Version 1) available at Research Square [https://doi.org/10.21203/rs.3.rs-2297880/v1]

## Repository Content 

Code
- data collection when automated
- data import and pre-processing
- implementation of input-output model and environmental extensions
- manuscript graphics 
- LaTeX files

Data 
- documentation of sources and aquisition
- data is not part of the repository 
- a *processed* (intermediary) dataset (will be) available via Zenodo 
- other data are available from authors upon request 

## Instructions for results reproduction

For a *last mile* reproduction of study numbers and figures, load the "illegality-risk-vyc44.Rdata". The file can be directly used with *mapping-illegality-risks.R*.

For the reproduction of the "input-output-model.R" analysis, the needed data objects are available from "illegality-risk-v1.1.Rdata".     


## Repository Structure

```
|-- illegality-risk/
|-- README.txt
|   |-- data/
|   |   |-- raw/
|   |   |   |-- README.txt <-- documentation on data sources
|   |   |   |-- ...
|   |   |-- temp/
|   |   |-- processed/
|   |   |   |-- illegality-risk-v1.1.RData <-- will be available through Zenodo
|   |   |   |-- illegality-risk-vyc44.RData <-- will be available through Zenodo
|   |-- results/
|   |-- src/
|   |   |-- data-collection-and-pre-processing/
|   |   |   |-- download-autef-pa/ <-- tools to auto-download autef pdf files
|   |   |   |-- autef-pa-scrapping/ <-- tools to scrape autef pdf data
|   |   |   |-- 01_import-sisflora-pa.R 
|   |   |   |-- 02_clean-sisflora-pa1.R
|   |   |   |-- 03_clean-sisflora-pa2.R
|   |   |   |-- 04_download-sinaflor.R 
|   |   |   |-- 05_import-sinaflor.R
|   |   |   |-- 06_clean-sinaflor-transporte.R 
|   |   |   |-- 07_consolidate-sisflora-sinaflor.R
|   |   |   |-- 08_actors.R 
|   |   |   |-- 09_products.R
|   |   |   |-- 10_logging-permits.R
|   |   |-- data-analysis/
|   |   |   |-- 11_input-output-model.R
|   |   |   |-- 12_mapping-illegality-risks.R
|   |   |-- data-supplementary/
|   |   |-- latex/
```

## References 

Research reproduceability based on good practices from Kitzes, J., Turek, D., & Deniz, F. (Eds.). (2018). The Practice of Reproducible Research: Case Studies and Lessons from the Data-Intensive Sciences. Oakland, CA: University of California Press. 