# Evaluating spatially explicit management alternatives for an invasive species in a riverine network

### Thompson BK, JD Olden, and SJ Converse

### Journal

Neobiota

### DOI: 

### Please contact the first author for questions about the code or data: Brielle K Thompson (bkwarta@uw.edu)

__________________________________________________________________________________________________________________________________________

## Citation: 

## Abstract: 

Invasive species have substantial ecological and economic costs and removing them can require large investments by management agencies. Optimal spatial allocation of removal effort is critical for efficient and effective management of invasive species. Using a series of ecologically informed model simulations, we evaluated and compared different spatially explicit removal strategies for invasive rusty crayfish (<em>Faxonius rusticus</em>) in the John Day River, USA. We assessed strategies in terms of their performance on three likely management objectives: suppression (minimize overall population abundance), containment (minimize the spatial extent of invasion), and prevention (minimize spread into a specific area). We developed four spatial removal strategies to achieve those objectives, denoted as: Target Abundance (removal at locations with the highest population abundance), Target Growth (removal at locations with the highest population growth), Target Edges (removal at the most distal locations in the river), and Target Random (removal at randomly selected locations). Each strategy was assessed at various effort levels, referring to the number of spatial segments in the river in which removals were conducted, after seven years of management. We identified the alternative that best achieved each objective based on decision criteria for risk-neutral and risk-averse decision makers and further evaluated strategies based on Pareto efficiency, which identifies the set of alternatives for which an improvement on one objective cannot be had without a decline in performance on another. We found that Target Abundance and Target Growth strategies best achieved the suppression objective, for risk neutral and risk averse decision makers respectively, and Target Downstream was always best in achieving the prevention objective across both types of decision makers. No single strategy consistently performed best in terms of the containment objective. In terms of all three objectives, Target Downstream was consistently Pareto efficient across all levels of management effort and both decision criteria. The modeling framework we provided is adaptable to a variety of riverine invasive species to help assess and compare spatial management strategies.

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
 
