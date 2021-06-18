# COVID19_place_based_intervention
This repository contains code and data necessary to replicate the findings of "Estimating the effect of place-based interventions on COVID-19 in 512 urban areas",
## Abstract
Nonpharmaceutical interventions in the COVID-19 pandemic have burdened the society, especially the closure of various daily-life-related places. Despite of several research into the effectiveness of these interventions, the effects of restricting activities at each single type of place remain unclear. We studied 16 place-based interventions commonly implemented by governments, ranging from closing restaurants to banning indoor/outdoor gatherings. Using a self-gathered intervention dataset on 512 cities/counties of six countries in the first pandemic wave, we estimated the causal effects of the interventions through the difference-in-difference approach. The effects are heterogenous across countries. Generally, closing cultural venues, banning gatherings both indoor and outdoor and ordering stay-at-home are most likely to reduce transmission. Closing bars, entertainment venues, outdoor sports grounds, restaurants, non-essential retails and schools are medium effective.
## Notes on the code
did_multiplegt_v2.R is our modified version of R package 'DIDmultiplegt' (v0.1.0).
### Rt_model
This folder stores the inputs and codes for estimating instantaneous Rt per city/county per day, which is combined into the input data in the data folder.
### Data
This folder stores the cleaned input data containing the instantaneous Rt, status of place-based interventions and temperature by city/county by day, as well as the codes for calculating the "simultaneity index" among interventions.  
### Model
This folder stores the codes for running the main analysis and sensitivity analysis.  
xxx_main.R is the code for the difference-in-difference analysis producing our main results. The bootstrap takes time and you may need high performance computing to perform the analysis.  
xxx_2stages.R is the code for estimating intervention effects in the locking down stage and reopening stage separately (Fig. S3).  
xxx_sensitivity_x.R is the code for performing the four groups of sensitivity analysis.  
### Result_process
This folder stores the codes for processing the outputs from the "Model" folder to interpretable results and the inputs to plots. We also provide our outputs in the .RData format which can be loaded here.
### Plot
This folder stores the codes for producing the graphs in our paper. The codes receive the outputs from the folder "result_process".




