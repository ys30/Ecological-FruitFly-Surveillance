# Spatial Ecological Surveillance Simulation

A spatially explicit **ecological risk and surveillance-design** project that uses stochastic simulation to compare fruit-fly monitoring strategies under heterogeneous spatial conditions.

## Decision problem

Environmental surveillance systems must decide **where and how densely to monitor** when risk is spatially uneven. This project builds a simulation workflow for testing alternative trap-placement designs and examining how spatial structure affects detection performance.

**Focus:** ecological surveillance · spatial risk · simulation · monitoring design  
**Tools:** R · spatial analysis · stochastic simulation · statistical comparison

## Analytical workflow

1. **Simulate population patterns** — generate fruit-fly population conditions used in surveillance experiments.
2. **Calculate spatial distances** — quantify relationships between population locations and surveillance locations.
3. **Construct monitoring designs** — evaluate grid, reduced-grid, random, expert, clustered, and sparse-border designs represented in the repository data.
4. **Run stochastic simulations** — repeatedly evaluate surveillance designs under simulated conditions.
5. **Summarize performance** — aggregate simulation outputs and compare design behavior.
6. **Quantify uncertainty** — produce percentile summaries, confidence-interval/CDF analyses, and pairwise statistical comparisons.

## Repository map

```
data/       # Alternative surveillance designs
scripts/    # Population, distance, design, simulation, and analysis workflow
results/    # Simulation outputs, statistical summaries, and figures
outputs/    # Exported analytical outputs
docs/       # Supporting documentation
```

Key scripts include:

```
01_fruitfly_population.R
02_calc_distances.R
03_make_grid_design.R
04_run_sim.R
05_run_many_sims.R
06_show_design.R
07_plot_hist_density.R
08_result summary.R
09_CDF with confidence intervals.R
```

## Outputs currently included

The repository contains simulation result tables, percentile summaries, pairwise t-test results, and figures generated from the surveillance-design experiments.

## What this repository demonstrates

- Translating an ecological monitoring question into a quantitative simulation
- Spatially explicit surveillance design
- Repeated stochastic simulation
- Comparison of alternative monitoring strategies
- Statistical summaries and uncertainty analysis
- Reproducible R workflow from design inputs to analytical outputs

## Scientific context

The modeling approach is informed by spatially heterogeneous surveillance theory and trap-placement design, including work referenced in the project documentation such as Triska (2018, *Pest Management Science*).

## Portfolio context

This project complements my broader work in **environmental and climate data science**, GIS/remote sensing, ecological modeling, and decision-support analytics.

[View my environmental & climate data portfolio](https://ys30.github.io/)

## License

MIT License
