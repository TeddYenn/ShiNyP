
<img src="https://github.com/user-attachments/assets/d503aec3-c21a-4e48-99e9-4e5285845b45" alt="ShiNyP-icon" width="100"/>

# _ShiNyP_: An Interactive Shiny-based Platform for Genome-Wide SNP Analysis and Visualization

![CI](https://img.shields.io/github/actions/workflow/status/TeddYenn/ShiNyP/R-CMD-check-Windows-latest.yaml)
![R-CMD-check](https://img.shields.io/github/actions/workflow/status/TeddYenn/ShiNyP/R-CMD-check-macOS_Ubuntu-release.yaml?label=R-CMD-check)
![Version](https://img.shields.io/github/r-package/v/TeddYenn/ShiNyP?label=ShiNyP&color=blue)

<!-- badges: end -->


> [!NOTE] 
> **🆕 _ShiNyP_ v0.1.2  is now available!**
> - Enhanced AI report functionality with new configuration options and new AI models (*Free*: Gemini 2.0; GPT-4.1; o4-mini).
> - Improved the layout of preliminary results for better readability.
> - Added more methods for constructing core SNP set.
> - Added the Docker-based installation.

<br>

##  🧬 Quickstart

[Run *ShiNyP* via R](#run-shinyp-via-r)

[Run *ShiNyP* via Docker](#run-shinyp-via-docker)

<br>

## 🔸Overview

**_ShiNyP_ is a platform designed for real-time processing, analysis, and visualization of SNP datasets.**

**📄Input data:** Genome-wide biallelic SNP in Variant Call Format (VCF).

**📊Analysis:** Data QC, population genetics analysis, core collection, and more.

**📋Output:** Publication-ready figures, tables, analyzed data objects, and free AI-driven reports.

For detailed instructions on each feature, please visit ➡️ [**User Guide**](https://teddyenn.github.io/ShiNyP-guide/) ⬅️

<br>

## 🔸Run _ShiNyP_ via R

### ✅ Prerequisites
- [**R**](https://www.r-project.org/): Version ≥ 4.4 

   ℹ️ Check your current version in R:

   ```R
   getRversion()
   ```

- [**Bioconductor**](https://www.bioconductor.org/install/): Version ≥ 3.20 
  
  ⚠️ Match your Bioconductor version with your R version (e.g., use Bioconductor 3.21 if R = 4.5).

### 1️⃣ Install Required Packages
   ```R
   install.packages("BiocManager")
   BiocManager::install(version = "3.21") # Adjust this version if needed version
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
Input your SNP dataset in VCF, or try the built-in demo data.

<br>

## 🔸Run _ShiNyP_ via Docker

If you have 🐳 [Docker](https://www.docker.com/) installed, you can launch _ShiNyP_ without installing R or any packages.

### ✅ Prerequisite

- [**Docker**](https://www.docker.com/)

   ℹ️ Verify your Docker installation:

   ```bash
   docker --version
   ```

### 1️⃣ Pull the Docker Image
   ```bash
   docker run -d -p 3838:3838 teddyenn/shinyp-platform
   ```   
### 2️⃣ Start the _ShiNyP_ Platform
   Open your browser and visit 👉 [http://localhost:3838](http://localhost:3838/).

<br>

## 🔸URLs

🔗 Journal Article

🔗 User Manual: [https://teddyenn.github.io/ShiNyP-guide](https://teddyenn.github.io/ShiNyP-guide)

🔗 Demo Datasets: [https://github.com/TeddYenn/ShiNyP_Test/tree/main/inst/demo_data](https://github.com/TeddYenn/ShiNyP_Test/tree/main/inst/demo_data)

🔗 ShiNyP Outputs (Demo): [https://zenodo.org/records/14813628](https://zenodo.org/records/14813628)

🔗 Online Platform (Trial): [https://teddyhuang.shinyapps.io/ShiNyP_Demo/](https://teddyhuang.shinyapps.io/ShiNyP_Demo/)

🔗 Docker Image: [https://hub.docker.com/r/teddyenn/shinyp-platform/tags](https://hub.docker.com/r/teddyenn/shinyp-platform/tags)

🔗 GitHub Repository: [https://github.com/TeddYenn/ShiNyP](https://github.com/TeddYenn/ShiNyP)

<br>

## 🔸Updates and Support

If you encounter any issues or have suggestions for new features, please submit a report through our [Feedback Form](https://forms.gle/GPCggSo5czyNLfoB7) or email us at: teddyhuangyh@gmail.com

- Aug 2024: Initial release alpha version.
- Oct 2024: Release v0.1.0.
- Feb 2025: Release v0.1.1.
- Apr 2025: Release v0.1.2.

<br>

## 🔸Citation

```
Huang et al. (upcoming 2025) ShiNyP: An Interactive Shiny-based Platform for Genome-Wide SNP Analysis and Visualization.
```
