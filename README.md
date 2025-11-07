# heterogeneous_learning_in_networks

This repository contains the simulation code used in the study on how network structure and social learning strategies influence collective problem-solving in dynamic environments.

---

## Overview

Agents organized in groups explore NK fitness landscapes across multiple generations.  
Each agent can use one of several learning strategies (exploration, success bias, or frequency bias) while interacting in either **regular** or **irregular** network structures.  

After a fixed number of search time steps (*t*), the best-performing groups are replicated with mutation to simulate cumulative cultural evolution over *generations*.  

---

## Repository Structure

- **NK_landscapes.R** — R script to generate NK fitness landscapes following  
  Barkoczi, D., & Galesic, M. (2016).  
  *Social learning strategies modify the effect of network structure on group performance.*  
  *Nature Communications*, 7(1), 13109.  
  [https://doi.org/10.1038/ncomms13109](https://doi.org/10.1038/ncomms13109)

- **main.jl** — main Julia script that runs the full simulation across multiple conditions.  
  It calls supporting Julia files and saves aggregated results to `results.csv`.

- **model.jl**, **strategy.jl**, **mutation.jl**, **landscapes.jl** — supporting Julia files that define the model logic, agent strategies, mutation process, and landscape-handling functions.

- **NK_LandscapesN10K0/** and **NK_LandscapesN10K8/** — folders containing pre-generated NK landscapes (for K = 0 and K = 8), created through NK_landscapes.R

---

## Simulation Summary

Each simulation run:
- Creates **regular** and **irregular** networks with varying degrees of connectivity.  
- Runs `i_simul` independent simulations per condition.  
- Simulates **groups of S agents** exploring over **t time steps** for **g generations**.  
- Replicates the top-performing groups with mutation (`μ = 0.01`).  
- Saves group-level aggregated strategy proportions to `results.csv`.

---

## Requirements

### Julia environment
- **Julia** ≥ 1.9  
- **Required packages:**
  ```julia
  using Random, CSV, DataFrames, Dates, StatsBase, BenchmarkTools
