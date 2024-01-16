# Repository Overview
This repository contains the reproduceability package for the study *Quantifying timber illegality risk in the Brazilian forest frontier*. Data utilized in the reproduction of this study is available on Zenodo at [https://doi.org/10.5281/zenodo.8068431](https://doi.org/10.5281/zenodo.8068431)

## Citation
Franca, C.S.S., Persson, U.M., Carvalho, T., Lentini, M. Quantifying timber illegality risk in the Brazilian forest frontier. Nat Sustain 6, 1485–1495 (2023). [https://doi.org/10.1038/s41893-023-01189-3]

## Repository Content 

Code
- data collection when automated
- data import and pre-processing
- implementation of input-output model and environmental extensions
- manuscript graphics 
- LaTeX files
- code where individual actors are not anonymized have been ommitted from the repository but can be made available upon request

Data 
- documentation of sources and aquisition
- data is not part of the repository 
- a *processed* (intermediary) dataset is available via Zenodo 
- other data are available from authors upon request 

## Instructions for results reproduction

For a *last mile* reproduction of study numbers and figures, load the "illegality-risk-vyc44-v1.1.Rdata". The file can be directly used with *mapping-illegality-risks.R*.

For the reproduction of the "input-output-model.R" analysis, the needed data objects are available from "illegality-risk-v1.1.Rdata".

For a full reproduction (from data collection through pre-processing and analysis), you can follow the repository structure below. Not all scripts have been added at this point given anonymization steps needed, but these can be made available upon request.

## Repository Structure

```
|-- illegality-risk-ns-main/
|-- README.txt
|   |-- data/
|   |   |-- raw/
|   |   |   |-- README.txt <-- documentation on data sources
|   |   |   |-- ...
|   |   |-- temp/
|   |   |-- processed/
|   |   |   |-- illegality-risk-v1.1.RData <-- available on Zenodo
|   |   |   |-- illegality-risk-vyc44-v1.1.RData <-- available on Zenodo
|   |-- results/
|   |-- src/
|   |   |-- data-collection-and-pre-processing/
|   |   |   |-- download-autef-pa/ <-- tools used to auto-download autef pdf files
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


## License
<a rel="license" href="http://creativecommons.org/licenses/by/3.0/">
  <img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/3.0/88x31.png" /> 
</a>
<br />
This work is licensed under a 
<a rel="license" href="http://creativecommons.org/licenses/by/3.0/">
  Creative Commons Attribution 3.0 Unported License
</a>.
<br />
Check the 
<a rel="license" href="https://github.com/carolsrto/illegality-risk-ns/blob/main/LICENSE">
LICENSE
</a> 
for more details
