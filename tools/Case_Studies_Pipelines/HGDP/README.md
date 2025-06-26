# HGDP Dataset

This README file provides a step-by-step guide for reproducing the core collection and selection scan analyses described in our study. 

The input data and output files can be downloaded from [Zenodo repository](https://zenodo.org/records/14806044) under **Case Studies Datasets (HGDP).zip** folder.

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Analysis Workflow](#analysis-workflow)
- [Resource Guidelines](#resource-guidelines)
  
---

## Overview
This project uses the _**ShiNyP**_ tool to analyze population diversity, detect selection signatures, and visualize results via phylogenetic trees and genomic plots. Key objectives:

Using the full dataset (929 samples, 87K SNPs):

- **Develop Core Collection**: Identify a subset of 354 individuals that preserves genetic diversity for downstream analysis.
- **Generate Phylogenetic Tree**: Construct a tree to visualize genetic relationships and population structure.

Using the core sample set (354 samples, 87K SNPs):

- **Apply QC Measures**: Filter SNPs in the core dataset based on predefined quality thresholds.
- **Calculate Diversity Metrics**: Compute genetic diversity statistics and visualize results using a genome-wide Circos plot.
- **Detect Selection Signals**: Utilize the pcadapt method to identify significant selection signatures.

---

## Prerequisites

1. **_ShiNyP_ Installation**: Refer to the [ShiNyP User Guide](https://teddyenn.github.io/ShiNyP-guide/sec-quickstart.html) for installation details.

2. **Input Data**: SNP dataset (VCF format) and Metadata (CSV format) can be accessed at: [Zenodo repository](https://zenodo.org/records/14806044).

3. **System Requirements**: Intel i5 with 8GB RAM or equivalent and above is recommended. Refer to [Resource Guidelines](#resource-guidelines) for detailed hardware recommendations.

---

## **Analysis Workflow**  

### **1. Data Upload**  
1. Launch the _**ShiNyP**_ platform and navigate to **"Data Input" → "VCF"** page.  
2. Click **"Browse"** and upload the raw SNP dataset (e.g., `Human_929_85K.vcf`).  
3. Once the upload is complete, click the **"Input VCF File"** button.  
4. Click **"Transform to data.frame"** to convert the VCF file into a DataFrame format.  
5. Download the processed data for future reuse if needed.  

---

### **2. Core Collection**  
1. Navigate to **"Core Collection" → "Core Sample Set"** page.  
2. If the dataset was previously transformed, _ShiNyP_ will automatically import the **data.frame** file; otherwise, manually upload the processed dataset via **"Data Input" → "data.frame/genlight"** page.  
3. Set the **minimum genetic coverage (%)** to **99%** and the **coverage difference between iterations** to **0.001**.  
4. Click **"Run Core Sample"** to initiate the core collection process.  
5. Download all results for further analysis.  

---

### **3. Data Transformation**  
1. Navigate to **"Data Transform"** page.  
2. If the dataset was previously transformed, _ShiNyP_ will automatically import the **data.frame** file; otherwise, manually upload the processed dataset via **"Data Input" → "data.frame/genlight"** page.  
3. Click **"Transform to genlight"** to generate the respective files.  
4. Download the processed data for future reuse.  

---

### **4. Phylogenetic Tree Construction**  

#### **Step 1: Generate Neighbor-Joining (NJ) Tree**  
1. Navigate to **"Population Structure" → "NJ Tree"** page.  
2. If the dataset was previously transformed, _ShiNyP_ will automatically import the **genlight** file; otherwise, upload the processed dataset manually.  
3. Click **"Run NJ"** to generate the tree plot.  
4. Download all results for further analysis.  

#### **Step 2: Visualize NJ Tree with Tree Plot Plus**  
1. Navigate to **"Population Structure" → "Tree Plot Plus"** page.  
2. Upload the **NJ Object (NJ_phylo_Object.rds)** and **Group Info.csv**.  
3. Click **"Run Tree Plot"** to generate the tree plot.  
4. Customize the visualization as needed, then click **"Run Tree Plot"** again to apply changes.  
5. Download the final visualization.  

---

### **5. Data Quality Control**  

#### **Step 1: Upload the Core Sample Set**  
1. Navigate to **"Data Input" → "data.frame/genlight"** page.  
2. Click **"Browse"** and upload the SNP dataset (`data.frame_354_85325SNPs_Core_Sample_Set.rds`).  
3. Click the **"Input"** button to upload the dataset.  

#### **Step 2: Perform Data QC**  
1. Navigate to **"Data QC" → "SNP QC"** page.  
2. Click all **"Summary"** buttons to obtain SNP summary statistics:  
   - **Missing rate**  
   - **Minor Allele Frequency (MAF)**  
   - **Heterozygosity rate**  
   - **Hardy-Weinberg Equilibrium (HWE)**  
3. Adjust the QC thresholds:  
   - **Missing rate**: 0.1  
   - **MAF**: 0.05  
   - **HWE**: 1×10⁻⁶  
4. Click **"SNP QC by Thresholds"** to generate the **Post-QC data.frame** file.  
5. Download the processed dataset for future reuse.  

---

### **6. Diversity Metrics Calculation**  

#### **Step 1: Calculate Genetic Diversity**  
1. Navigate to **"Genetic Diversity" → "Diversity Parameter"** page.  
2. Upload **Site Info.** and **Group Info.** files.  
3. Click **"Run Diversity Analysis"** to compute genetic diversity parameters.  

#### **Step 2: Visualize Diversity Metrics with Circos Plot**  
1. Navigate to **"Genetic Diversity" → "Circos Plot"** page.  
2. Select the following parameters:  
   - **Observed heterozygosity (Ho)**  
   - **Nucleotide diversity (π)**  
   - **FST values**  
3. Set **window size** to **200 kb** and **step size** to **50 kb**.  
4. Click **"Run Sliding Window"** to generate data for the Circos plot.  
5. Upload the **Chromosome Info. (CSV)** file ([Zenodo repository](https://zenodo.org/records/14806044)).  
6. Click **"Run Circos Plot"** to generate the visualization.  
7. Download the final Circos plot.  

---

### **7. Selection Sweep Analysis**  

#### **Step 1: Identify Genetic Variance**  
1. Navigate to **"Population Structure" → "PCA"** page.  
2. Click **"Run PCA"** to generate PCA plots.  
3. Examine the **PCA eigenvalues plot (scree plot)** to determine the significant principal components (PCs).  

#### **Step 2: Genomic Scan for Selection**  
1. Navigate to **"Selection Sweep" → "pcadapt"** page.  
2. Upload **Site Info.** file.  
3. Retain the **first six principal components (PCs)** and apply a **false discovery rate (FDR) correction** with **α = 0.10**.  
4. Click **"Run pcadapt"** to perform a **genome-wide selection scan**.  

#### **Step 3: Visualize Selection Signatures**  
1. Navigate to **"Selection Sweep" → "Manhattan Plot Plus"** page.  
2. Upload **pcadapt p-value per site (RDS)** and **Chromosome Info. (CSV)**.  
3. Click **"Run Manhattan Plot"** to generate the **Manhattan plot**.  
4. Customize as needed, then re-run and download the final visualization.  

---

## Resource Guidelines

The following runtime estimates are based on a laptop computer with an Intel i7-1185G7 (3.0 GHz) CPU and 16GB RAM. Performance may vary depending on hardware specifications and dataset size.  

**Estimated Runtime for Each Process**  

| **Process**                      | **Estimated Runtime** |
|-----------------------------------|----------------------|
| **Data Upload**                   | < 20 seconds        |
| **Core Collection**               | < 3 minutes         |
| **Data Transformation**           | < 10 minutes        |
| **Phylogenetic Tree Construction** | < 20 minutes        |
| **Data Quality Control**          | < 10 seconds        |
| **Diversity Metrics Calculation** | < 3 minutes         |
| **Selection Sweep Analysis**      | < 10 seconds        |

**System Recommendations**  

For optimal performance, we recommend using a system with:  
- **CPU**: Multi-core processor (Intel i5 or equivalent and above)  
- **Memory**: At least **8GB RAM**
- **Storage**: Sufficient disk space for intermediate and output files  

For large dataset, the high-performance computing (HPC) environment or Unix-based systems (Linux/macOS) with multi-threading support is advised.  

