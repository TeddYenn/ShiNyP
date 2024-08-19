
<img src="https://github.com/user-attachments/assets/2140e02f-f35b-4f04-bc41-e53d302d3485" alt="icon-removebg-preview" width="100"/>

# ShiNyP: A Shiny-Based Interactive Platform for Genome-Wide SNP Analysis and Visualization

![CI](https://img.shields.io/badge/build-passing-brightgreen)
[![R-CMD-check](https://github.com/irudnyts/openai/workflows/R-CMD-check/badge.svg)](https://github.com/irudnyts/openai/actions)
![Version](https://img.shields.io/badge/version-1.0-blue)
<!-- badges: end -->

- [Quickstart](#Quickstart)
- [Overview](#Overview)
- [User Guide](#User-Guide)
- [Case Studies](#Case-Studies)
- [Support](#Support)
- [Citation](#Citation)

## Quickstart

### Step 1: Pre-install Required Packages
   ```R
   install.packages("BiocManager")
   BiocManager::install(version = "3.19")
   BiocManager::install("qvalue")
   ```
### Step 2: Install the ShiNyP Package from GitHub
   ```R
   install.packages("remotes")
   remotes::install_github("TeddYenn/ShiNyP", force = TRUE)
   library(ShiNyP)
   ```
### Step 3: Start the ShiNyP Platform
   ```R
   ShiNyP::run_ShiNyP()
   ```

## Overview

**Input data:** Genome-wide biallelic SNP in Variant Call Format (VCF) file format.

**Analysis:** Data QC, Population genetics analysis, Core collection…

**Output:** Publication-ready figures, tables, analyzed data objects, and AI driven report.
- Statistical and computational exploration
- Customizable visualization options
- Download publication-ready figures and tables
- Download analyzed data objects
- Auto-generate customized preliminary results
- AI-driven report - powered by OpenAI

  
## User Guide
Visit the User Guide for detailed instructions on using each feature.

[User Guide](https://teddy-tw.notion.site/ShiNyP-User-Guide-68b21f9bd4d94152b85feda55452d2a3?pvs=4)

## Case Studies
Explore how ShiNyP has been applied in WGS SNP datasets: wild rice & chicken. Check it out!

[Case Studies](https://teddy-tw.notion.site/ShiNyP-Case-Studies-a8117cb6e8c44280a25686f9cdddca1c?pvs=4)

## Support
If you encounter any issues or have suggestions for new features, please submit a report through our feedback form

[Feedback Form](https://docs.google.com/forms/d/e/1FAIpQLScbMJILRWTJC_5AsbslVH59uPqHZJunur7wag8A2h7YYN4wVw/viewform?usp=send_form)

## Citation

```

```
