# AMMultivariateEquivalency

**AMMultivariateEquivalency** is an R package that provides tools for performing **multivariate equivalency testing** in the context of additive manufacturing (AM).  
It is designed to support cost-effective qualification of AM processes by comparing candidate datasets against a well-characterized reference process across multiple correlated variables.

---

## Features
- **Reference process stability check**  
  Identify whether the reference dataset is statistically in control before qualification.
- **Bin-based discretization**  
  Partition multivariate space into bins for robust equivalency testing.
- **Candidate process comparison**  
  Evaluate whether candidate datasets are statistically equivalent to the reference process.
- **Step-by-step tutorial**  
  Includes a guided tutorial using simulated data to demonstrate how equivalency is determined.

---

## Usage

1. Prepare your data
- Place all functions from this package into the same directory.
- Import your reference and candidate datasets into the same folder. 
- Each CSV must contain only numeric columns (variables) and rows (artifacts).
- Candidate datasets must use the same variables and ordering as the reference dataset.

2. Run scripts
- Import reference and candidate datasets
- binLimits.R will characterize each reference variable
- multivaraiteProcessStability.R will produce a control chart to see if the reference and/or candidate datasets are stable
- multivariateEquivalency.R will compare the reference
- tutorial.R will show how the syntax for each of these scripts works. 

