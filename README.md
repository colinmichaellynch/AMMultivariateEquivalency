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
- **Driver script included**  
  A minimal script (`run_equivalency.R`) demonstrates end-to-end usage.

---

## Installation

Clone this repository:

```bash
git clone https://github.com/yourusername/AMMultivariateEquivalency.git
cd AMMultivariateEquivalency
