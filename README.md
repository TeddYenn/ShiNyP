
<img src="https://github.com/user-attachments/assets/d503aec3-c21a-4e48-99e9-4e5285845b45" alt="ShiNyP-icon" width="100"/>

# _ShiNyP_: An Interactive Shiny-based Platform for Genome-Wide SNP Analysis and Visualization

![CI](https://img.shields.io/github/actions/workflow/status/TeddYenn/ShiNyP/R-CMD-check-Windows-latest.yaml)
![R-CMD-check](https://img.shields.io/github/actions/workflow/status/TeddYenn/ShiNyP/R-CMD-check-macOS_Ubuntu-release.yaml?label=R-CMD-check)
![Version](https://img.shields.io/github/r-package/v/TeddYenn/ShiNyP?label=ShiNyP&color=blue)

<!-- badges: end -->


> [!NOTE] 
> **🆕 Now available: _ShiNyP_ v0.1.2**
> - Enhanced AI report functionality with new configuration options and new AI models (*Free*: Gemini 2.0; GPT-4.1).
> - Improved the layout of preliminary results for better readability.
> - Added more methods for constructing core SNP set.


## 🔸Overview

**_ShiNyP_ is a platform designed for real-time processing, analysis, and visualization of SNP datasets.**

**📄Input data:** Genome-wide biallelic SNP in Variant Call Format (VCF).

**📊Analysis:** Data QC, population genetics analysis, core collection, and more.

**📋Output:** Publication-ready figures, tables, analyzed data objects, and free AI-driven reports.

For detailed instructions on each feature, please visit ➡️ [**User Guide**](https://teddyenn.github.io/ShiNyP-guide/) ⬅️

## 🔸Quickstart

### ✅ Prerequisites
- R Version: R ≥ 4.4 (Compatible with Bioconductor version 3.20)
- R Tools (Recommended, but not required).


### 1️⃣ Install Required Packages
   ```R
   install.packages("BiocManager")
   BiocManager::install(version = "3.20")
   BiocManager::install(c("qvalue", "SNPRelate", "ggtree", "snpStats"), force = TRUE)
   ```
### 2️⃣ Install the _ShiNyP_ Package
   ```R
   install.packages("remotes")
   remotes::install_github("TeddYenn/ShiNyP", force = TRUE)
   ```
### 3️⃣ Start the _ShiNyP_ Platform
   ```R
   library(ShiNyP)
   ShiNyP::run_ShiNyP()
   ```
### 4️⃣ Run Analysis on _ShiNyP_
Input your SNP data in VCF, or feel free to use the built-in demo data.


## 🔸URLs

▪️ Journal Article

▪️ User Manual: [https://teddyenn.github.io/ShiNyP-guide](https://teddyenn.github.io/ShiNyP-guide)

▪️ Demo Datasets: [https://github.com/TeddYenn/ShiNyP_Test/tree/main/inst/demo_data](https://github.com/TeddYenn/ShiNyP_Test/tree/main/inst/demo_data)

▪️ ShiNyP Outputs (Samples): [https://zenodo.org/records/14813628](https://zenodo.org/records/14813628)

▪️ Online Platform (Demo): [https://teddyhuang.shinyapps.io/ShiNyP_Demo/](https://teddyhuang.shinyapps.io/ShiNyP_Demo/)

▪️ GitHub Repository: [https://github.com/TeddYenn/ShiNyP](https://github.com/TeddYenn/ShiNyP)

## 🔸Updates

- Aug 2024: Initial release alpha version.
- Oct 2024: Release v0.1.0.
- Feb 2025: Release v0.1.1.
- Apr 2025: Release v0.1.2.


## 🔸Support

If you encounter any issues or have suggestions for new features, please submit a report through our [Feedback Form](https://forms.gle/GPCggSo5czyNLfoB7) or email us at: teddyhuangyh@gmail.com


## 🔸Citation

```
Huang et al. (upcoming 2025) ShiNyP: An Interactive Shiny-based Platform for Genome-Wide SNP Analysis and Visualization.
```
