# Wild Rice Dataset

This README file provides a step-by-step guide for reproducing the population genetic analyses described in our study. 

The input data and output files can be downloaded from [Zenodo repository](https://zenodo.org/records/14806044) under **Case Studies Datasets (Wild Rice).zip** folder.

1. [Overview](#overview)  
2. [Prerequisites](#prerequisites)
3. [Analysis Workflow](#analysis-workflow)
4. [Resource Guidelines](#resource-guidelines)
---

## **Overview**  
This project utilizes the _**ShiNyP**_ platform to analyze **wild rice population diversity**, perform **clustering analysis**, and visualize **genetic relationships** through various genomic plots. 

### **Using the Rice Dataset (446 Samples, 221K SNPs)**
- **Analyze SNP Distribution**: Assess genome-wide SNP spacing and variability across chromosomes.  
- **Perform Clustering Analysis**: Use **DAPC** to partition samples into seven genetically distinct groups.  
- **Genetic Variation Assessment**: Evaluate genetic differentiation to quantify variation between and within groups.  
- **Develop a Core SNP Panel**: Construct a subset of **13,077 SNPs** preserving genome-wide genetic variation.  
- **Principal Component Analysis (PCA) Validation**: Confirm core SNP set retains genetic structure through PCA scatter plots.  

---

## **Prerequisites**  
- **_ShiNyP_ Installation**: Refer to the [ShiNyP User Guide](https://teddyenn.github.io/ShiNyP-guide) for installation details.  
- **Input Data**: SNP dataset (VCF format) and Metadata (CSV format) are available at: [Zenodo repository](https://zenodo.org/records/14806044).
- **System Requirements**: Recommended: **Intel i5 with 8GB RAM or equivalent and above**. Refer to [Resource Guidelines](#resource-guidelines) for detailed hardware recommendations.  

---

## **Analysis Workflow**  

### **1. Data Upload**  
1. Launch the _**ShiNyP**_ platform and navigate to **"Data Input" → "VCF"** page.  
2. Click **"Browse"** and upload the raw SNP dataset (`WildRice_446_221K.vcf`).  
3. Once the upload is complete, click the **"Input VCF File"** button.  
4. Click **"Transform to data.frame"** to convert the VCF file into a data.frame format.  
5. Download the processed data for future reuse if needed.  

---

### **2. Data Transformation**  
1. Navigate to **"Data Transform"** page.  
2. If the dataset was previously transformed, _ShiNyP_ will automatically import the **data.frame** file; otherwise, manually upload the processed dataset via **"Data Input" → "data.frame/genlight"** page.  
3. Click **"Transform to genlight"** to generate the genlight file.  
4. Download the processed data for future reuse.  

---

### **3. SNP Density Calculation**  
1. Navigate to **"Data QC" → "SNP Density"** page.  
2. If the dataset was previously transformed, _ShiNyP_ will automatically import the **data.frame** file.  
3. Set the **window size** to **250 kb**.  
4. Click the **"Summary"** button to calculate SNP density across the genome.  
5. Download all results for further analysis.  

---

### **4. Clustering Analysis**  

#### **Step 1: Perform DAPC**  
1. Navigate to **"Population Structure" → "DAPC"** page.  
2. If the dataset was previously transformed, _ShiNyP_ will automatically import the **genlight** file; otherwise, upload the processed dataset manually.  
3. Set the **number of PCs** to **356** and the **maximum number of clusters** to **15**.  
4. Click **"Run DAPC I"** to determine the optimal number of clusters (lowest BIC value).  
5. Set the **number of clusters (K)** to **7**.  
6. Click **"Run DAPC II"** to generate **DAPC plots** and related files.  
7. Download all results for further analysis.  

#### **Step 2: Visualize Scatter Plot**  
1. Navigate to **"Population Structure" → "Scatter Plot Plus"** page.  
2. Upload the **PCA Object (PCA_prcomp_Object.rds)** and **Group Info.csv**.  
3. Click **"Run Scatter Plot"** to generate **2D and 3D interactive scatter plots**.  
4. Customize the visualization using the following settings:  
   - **X-axis**: LD1  
   - **Y-axis**: LD2  
   - **Z-axis**: LD3  
   - **Color Variable**: Group  
   - **Point Size**: 8  
   - **Opacity**: 0.8  
5. Click **"Run Scatter Plot"** again and download the final visualization.  

---

### **5. Genetic Diversity Analysis**  

#### **Step 1: Perform AMOVA**  
1. Navigate to **"Genetic Diversity" → "AMOVA"** page.  
2. Click **"Run AMOVA"** to partition genetic variation among and within populations.  
3. Set the **number of randomizations** to **999** for the permutation test.  
4. Click **"Run Permutation Test"** to perform the statistical test.  
5. Download all results for further analysis.  

#### **Step 2: Calculate Genetic Variation**  
1. Navigate to **"Genetic Diversity" → "Diversity Parameter"** page.  
2. Upload **Site Info.** and **Group Info.** files.  
3. Click **"Run Diversity Analysis"** to compute genetic diversity parameters.  
4. Download the processed data for future reference.  

---

### **6. Core SNP Set Development**  

#### **Step 1: Develop Core SNP Set**  
1. Navigate to **"Core Collection" → "Core SNP Set"** page.  
2. If the dataset was previously uploaded, _ShiNyP_ will automatically import the **data.frame** file.  
3. Upload **Site Info.** and **DAPC Object (DAPC_dapc_Object.rds)** files.  
4. Upload **Chromosome Info. (CSV)** file ([Zenodo repository](https://zenodo.org/records/14806044)).  
5. Set **maximum core SNPs ratio (%)** to **5**.  
6. Click **"Run Core SNP"** to generate the **Core SNP Set**.  
7. Download the processed data for future use.  

#### **Step 2: Upload the Core SNP Set**  
1. Navigate to **"Data Input" → "data.frame/genlight"** page.  
2. Click **"Browse"** and upload the SNP dataset (`data.frame_446_13077SNPs_Core_SNP_Set.rds`).  
3. Click the **"Input"** button to upload the dataset.  

#### **Step 3: Verify Core Set Representation**  
1. Navigate to **"Population Structure" → "PCA"** page.  
2. If the dataset was previously uploaded, _ShiNyP_ will automatically import the **data.frame** file.  
3. Click **"Run PCA"** to generate **PCA plots** for validation.  

---

## Resource Guidelines

The following runtime estimates are based on a laptop computer with an Intel i7-1185G7 (3.0 GHz) CPU and 16GB RAM. Performance may vary depending on hardware specifications and dataset size.  

**Estimated Runtime for Each Process**  

| **Process**                      | **Estimated Runtime** |
|-----------------------------------|----------------------|
| **Data Upload**                   | < 30s        |
| **Data Transformation**           | < 30m         |
| **SNP Density Calculation**           | < 5s        |
| **Clustering Analysis** | < 10m       |
| **Genetic Diversity Analysis**          | < 2m        |
| **Core SNP Set Development** | < 30s         |

**System Recommendations**  

For optimal performance, we recommend using a system with:  
- **CPU**: Multi-core processor (Intel i5 or equivalent and above)  
- **Memory**: At least **8GB RAM**
- **Storage**: Sufficient disk space for intermediate and output files  

For large dataset, the high-performance computing (HPC) environment or Unix-based systems (Linux/macOS) with multi-threading support is advised.  

