
<img src="https://github.com/user-attachments/assets/8638189c-e418-46b9-aa4a-a50806d21ead" alt="icon-removebg-preview" width="100"/>

# _ShiNyP_: An Interactive Shiny-based Platform for Genome-Wide SNP Analysis and Visualization

![CI](https://img.shields.io/badge/build-passing-brightgreen)
[![R-CMD-check](https://github.com/irudnyts/openai/workflows/R-CMD-check/badge.svg)](https://github.com/irudnyts/openai/actions)
![Version](https://img.shields.io/badge/version-0.1.0-blue)
<!-- badges: end -->

- [Quickstart](#Quickstart)
- [Overview](#Overview)
- [Instructions](#Instructions)
- [Support](#Support)
- [Citation](#Citation)
- [URLs](#URLs)

## Quickstart

### Prerequisites
- R Version: R ≥ 4.4 (compatible with Bioconductor version 3.19)
- R Tools:
  - Windows: Rtools
  - macOS: Xcode Command Line Tools
  - Linux: Development tools (e.g., r-base-dev)


### Step 1: Pre-install Required Package
   ```R
   install.packages("BiocManager")
   BiocManager::install(version = "3.19")
   BiocManager::install(c("qvalue", "SNPRelate", "ggtree"), force = TRUE)
   ```
### Step 2: Install the _ShiNyP_ Package from GitHub
   ```R
   install.packages("remotes")
   remotes::install_github("TeddYenn/ShiNyP", force = TRUE)
   ```
### Step 3: Start the _ShiNyP_ Platform
   ```R
   library(ShiNyP)
   ShiNyP::run_ShiNyP()
   ```
### Step 4: Run _ShiNyP_ Analysis
Input your SNP data in VCF format, or feel free to use the built-in demo data.



## Overview

**Input data:** Genome-wide biallelic SNP in Variant Call Format (VCF).

**Analysis:** Data QC, Population genetics analysis, Core collection…

**Output:** Publication-ready figures, tables, analyzed data objects, and AI-driven reports.
- Statistical and computational exploration
- Customizable visualization options
- Publication-ready figures and tables
- Analyzed R data objects
- Auto-generate customized preliminary results
- AI-driven report - powered by OpenAI

  
## Instructions
For detailed instructions on each feature, please visit the [User Guide](https://teddyenn.github.io/ShiNyP-guide/).


## Support
If you encounter any issues or have suggestions for new features, please submit a report through our [Feedback Form](https://forms.gle/GPCggSo5czyNLfoB7).


## Citation

```
Huang et al. (upcoming 2025) ShiNyP: An Interactive Shiny-based Platform for Genome-Wide SNP Analysis and Visualization
Under Review…
```

## URLs

- Journal Article: 
- User Manual: [https://teddyenn.github.io/ShiNyP-guide](https://teddyenn.github.io/ShiNyP-guide)
- Demo Datasets: [https://github.com/TeddYenn/ShiNyP/tree/main/tools/Demo_Datasets](https://github.com/TeddYenn/ShiNyP/tree/main/tools/Demo_Datasets)
- ShiNyP Outputs: [https://zenodo.org/records/14813628](https://zenodo.org/records/14813628)
- Online Platform (Demo): [https://teddyhuang.shinyapps.io/ShiNyP_Demo/](https://teddyhuang.shinyapps.io/ShiNyP_Demo/)
- GitHub Repository: [https://github.com/TeddYenn/ShiNyP](https://github.com/TeddYenn/ShiNyP)

