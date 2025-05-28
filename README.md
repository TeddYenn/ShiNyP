
<img src="https://github.com/user-attachments/assets/b1e125b2-c97d-4a97-9169-5669cadc5bd4" alt="ShiNyP-icon_20250510" width="100"/>

# _ShiNyP_: SNP Analysis and Visualization Platform.


![CI](https://img.shields.io/github/actions/workflow/status/TeddYenn/ShiNyP/R-CMD-check-Windows-latest.yaml)
![R-CMD-check](https://img.shields.io/github/actions/workflow/status/TeddYenn/ShiNyP/R-CMD-check-macOS_Ubuntu-release.yaml?label=R-CMD-check)
![Version](https://img.shields.io/github/r-package/v/TeddYenn/ShiNyP?label=ShiNyP&color=blue)

<!-- badges: end -->


> [!NOTE] 
> **🆕 _ShiNyP_ v1.0.0  is now available!**
> - Introduced the **new _ShiNyP_ AI chatbot**.
> - Enhanced AI report features and deprecated older AI models.
> - Added publication details [https://doi.org/10.1093/molbev/msaf117](https://doi.org/10.1093/molbev/msaf117).
> - Made minor UI improvements.
> - Fixed several bugs.
> <img src="https://github.com/user-attachments/assets/96a22260-d75a-4750-b438-1664af8e06b9" alt="ShiNyP_AI_Demo" width="300">
<br>

##  🧬 Quickstart

🚀 [Run *ShiNyP* via R](#run-shinyp-via-r)

🚀 [Run *ShiNyP* via Docker](#run-shinyp-via-docker)

📢 [*ShiNyP* Online Version – Trial Platform](https://teddyhuang.shinyapps.io/ShiNyP_Demo/)

<br>

## 🔸Overview

**_ShiNyP_ is a platform designed for real-time processing, analysis, and visualization of SNP datasets.**

**📄Input data:** Genome-wide biallelic SNP in Variant Call Format (VCF).

**📊Analysis:** Data QC, population genetics analysis, core collection, and more.

**📋Output:** Publication-ready figures, tables, analyzed data objects, and free AI-driven reports.

For detailed instructions on each feature, please visit ➡️ [**User Guide**](https://teddyenn.github.io/ShiNyP-guide/) ⬅️

<br>

<img src="https://github.com/user-attachments/assets/28e37373-3d4a-47c6-97ff-4613ef6f7b24" alt="ShiNyP_Demo" width="750">

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
   BiocManager::install(version = "3.21") # Use the version that matches your R
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

<img src="https://github.com/user-attachments/assets/f4d45945-0034-4c62-beb7-808da194c1bd" alt="Run_ShiNyP_via_R_Demo" width="750">


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
   
<img src="https://github.com/user-attachments/assets/368ac520-3615-4f4e-b474-ee0942623da4" alt="Run_ShiNyP_via_Docker_Demo" width="750">


<br>

## 🔸URLs

🔗 Journal Article: [https://doi.org/10.1093/molbev/msaf117](https://doi.org/10.1093/molbev/msaf117)

🔗 User Manual: [https://teddyenn.github.io/ShiNyP-guide](https://teddyenn.github.io/ShiNyP-guide)

🔗 Demo Datasets: [https://github.com/TeddYenn/ShiNyP_Test/tree/main/inst/demo_data](https://github.com/TeddYenn/ShiNyP_Test/tree/main/inst/demo_data)

🔗 ShiNyP Outputs (Demo): [https://zenodo.org/records/14813628](https://zenodo.org/records/14813628)

🔗 Online Platform (Trial): [https://teddyhuang.shinyapps.io/ShiNyP_Demo/](https://teddyhuang.shinyapps.io/ShiNyP_Demo/)

🔗 Docker Image: [https://hub.docker.com/r/teddyenn/shinyp-platform/tags](https://hub.docker.com/r/teddyenn/shinyp-platform/tags)

🔗 GitHub Repository: [https://github.com/TeddYenn/ShiNyP](https://github.com/TeddYenn/ShiNyP)

<br>

## 🔸Citation

If you use _ShiNyP_ in your research, please cite: 

> Huang, Y.-H., Chen, L.-Y., Septiningsih E. M., Kao, P.-H., Kao, C.-F.
> (2025) _ShiNyP_: Unlocking SNP-Based Population Genetics—An AI-Assisted Platform for Rapid and Interactive Visual Exploration.
> _Molecular Biology and Evolution_, _43_(6), msaf117. [https://doi.org/10.1093/molbev/msaf117](https://doi.org/10.1093/molbev/msaf117)

In addition, please acknowledge the R packages utilized in your analysis. The relevant citations and descriptions for each module are detailed in the _ShiNyP_ [User Guide](https://teddyenn.github.io/ShiNyP-guide/).

<br>

## 🔸Updates and Support

If you encounter any issues or have suggestions for new features, please submit a request on the [GitHub Issues page](https://github.com/TeddYenn/ShiNyP/issues) or email us at: teddyhuangyh@gmail.com

- Aug 2024: Initial release alpha version.
- Oct 2024: Release v0.1.0.
- Feb 2025: Release v0.1.1.
- Apr 2025: Release v0.2.0.
- May 2025: Release v1.0.0.

<br>
<br>

![Graphical Abstract](https://github.com/user-attachments/assets/7b4ac329-9697-49dc-988b-3f79b2bbaf79)

