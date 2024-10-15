# Evaluating spatially explicit management alternatives for an invasive species in a riverine network

### Thompson BK, JD Olden, and SJ Converse

### Journal

### DOI: 

### Please contact the first author for questions about the code or data: Brielle K Thompson (bkwarta@uw.edu)

__________________________________________________________________________________________________________________________________________

## Citation: 

## Abstract: 

## Code: 

The repository is organized as follows. 

* **scripts**: simulation scripts for each management strategy
  + <em>Abundance</em>: simulation scripts for the Target Abundance strategy (remove at segments with highest total crayfish abundance)
    - 8 scripts. Each script, is defined as: abundance_#rem_s#. Where #rem is the number of segments receiving removal effort (1,4,8,16), and #sim is the simulation script (s1 = the first 25 simulations for each parameter set, s2 = the second 25 simulations for each parameter set). Due to memory issues we divided these simulations to two scripts. 
  + <em>Downstream</em> : simulation scripts for the Target Downstream strategy (remove at the most downstream segments on the mainstem with crayfish abundance)
    - 8 scripts. The scripts are organized similarly to the abundance scripts above
  + <em>Edge</em> : simulation scripts for the Target Edges strategy (remove at edges of invasion with the highest abundance)
    - 8 scripts. The scripts are organized similarly to the abundance scripts above
  + <em>Growth</em> : simulation scripts for the Target Growth strategy (remove at segments with highest crayfish population growth) 
    - 8 scripts
  + <em>Random</em> : simulation scripts for the Target Random strategy (remove at randomly selected segments) 
    - 8 scripts
    
* **resultplots**: Figures in the maintext and manuscript
  + data processing: folder with R scripts for post simulation data processing 
  + map_plots.R: spatial final abundance plots for each management strategy
  + plots.R : Figures and tables in the main text 
  + plots_appendix.R: Figures in the appendix
  
* **data**: Data to run simulation scripts
  + initial_population: folder with data of the initial 2016 spatial population (includes csv and shapefiles)
  + movement: folder with spatial movement probability data within the John Day River
  + parameters: Scripts to generate parameter sets (movement parameters: distance_matrix.R and movement_rates.R, full parameters: parameter_sets)
  + spatial: includes crayfish file geodatabase
  + temperature: includes spatial monthly average temperature 
  

* **parameters.RDA**: R data of all the parameter sets  

## Supplemental data:
 * Link to arcgis [online map](https://uw.maps.arcgis.com/home/item.html?id=8ceee549c4b54b9f939060f099fb6508)
 * Link to full result files [online link](https://zenodo.org/records/12761044?token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6IjNjMjllZTc5LTcyZmMtNDg1Ni04ZDBmLThmYjE0MTg4MTYwMiIsImRhdGEiOnt9LCJyYW5kb20iOiIwNWI0YTVmY2JiMzFiZmE0NjlkOTZiZGVmYWQ1MDA5NyJ9.xOVEaERtKzGM2Pe_evGMF6x2eShYL2jy9YojM3Kfel2OUyP7Aev3AFuh3sRtKtijH3RzwXTvfCn26xtf9Pyp_w)
 
