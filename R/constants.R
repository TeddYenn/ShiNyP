options(warn = -1)

Tree_layout_choice = c("Rectangular" = "rectangular",
                       "Round rectangular" = "roundrect",
                       "Dendrogram" = "dendrogram",
                       "Ellipse" = "ellipse",
                       "Circular" = "circular",
                       "Inward circular" = "inward_circular",
                       "Radial" = "radial",
                       "Equal angle" = "equal_angle")

Legend_choice = c("Top" = "top",
                  "Bottom" = "bottom",
                  "Right" = "right",
                  "Left" = "left",
                  "None" = "none")

GT_method_choice = c("Cavalli-Sforza's chord distance" = "Dch",
                     "Nei's genetic distance" = "Da",
                     "Nei's standard genetic distance" = "Ds",
                     "Reynolds's genetic distance" = "Fst",
                     "Nei's minimum distance" = "Dm",
                     "Rogers's distance" = "Dr",
                     "Prevosti's distance" = "Cp",
                     "Sanghvi's distance" = "X2")

pval_adj_method_choice = c("Bonferroni" = "bonferroni",
                           "Benjamini & Hochberg (FDR)" = "BH")

AI_model_choice = c("GPT-4o" = "gpt-4o",
                    "GPT-4o mini" = "gpt-4o-mini",
                    "GPT-4 Turbo" = "gpt-4-turbo-2024-04-09",
                    "GPT-4" = "gpt-4",
                    "GPT-3.5 Turbo" = "gpt-3.5-turbo")

custom_palette = c("#4482a8", "#91bb67", "#e3b800", "#E85C0D", "#e9788e", "#c493ff")

Summary_Request_Prompt = "Summarize key findings from a report analyzing genome-wide SNP data using various statistical and computational methods, excluding any GWAS results. 
Focus on specific areas if their results are provided below, such as SNP data description, PCA, DAPC clustering, genetic diversity, genetic distance, selection sweep analysis, and core collection construction.
# Steps
1. **SNP Data Description and Quality Control (QC):**
   - Detail the description of the SNP dataset, including the sample size, number of variants, SNP density.
   - Summarize the data cleaning and filtering procedures, mentioning metrics like missing rate, MAF, and HWE pruning.
2. **Principal Component Analysis (PCA):**
   - Describe the allocation of genetic variance across principal components and interpret the significance of major components.
3. **Discriminant Analysis of Principal Components (DAPC) Clustering:**
   - Summarize how well individuals are grouped into genetic clusters via DAPC clustering and any insights into population structure.
4. **Genetic Diversity Statistics:**
   - Summarize within-group and across-dataset genetic diversity metrics, and note any trends like population bottlenecks or expansions
5. **Genetic Distance and Differentiation Statistics:**
   - Present findings on genetic distance and differentiation between groups, citing specific metrics used.
6. **Selection Sweep Analysis:**
   - Summarize findings of genomic regions under potential selection, highlighting key regions and potential implications.
7. **Core Collection Construction:**
   - Provide an overview of core collections constructed from the SNP data, explaining their significance and potential applications.
# Output Format
Provide a detailed summary in paragraph form for each section if their results are provided. Each section should be clearly labeled and should not exceed a few sentences unless a more detailed explanation is necessary.
# Notes
- If information for a section (e.g., Principal Component Analysis) is not provided below, omit that section from the summary.
- Exclude any GWAS results from the summary.
- Pay special attention to methodological details and their impact on the findings.
- Clearly indicate analytical techniques and any assumptions or limitations associated with the findings."

Data_Interpretation_Prompt = "Interpret a report of genome-wide SNP data generated from various statistical and computational methods, excluding GWAS results.
Focus on specific areas if their results are provided, such as SNP data description, PCA, DAPC clustering, genetic diversity, genetic distance, selection sweep analysis, and core collection construction.
# Steps
1. **Data Description:**
   - Provide a detailed description of the SNP dataset, including sample size, the number of variants, and SNP density.
   - Generate reports summarizing SNP data quality metrics and applied QC procedures to ensure data integrity and reliability.
2. **Principal Component Analysis (PCA):** 
   - Examine how genetic variance is distributed across principal components.
   - Provide insights into potential population structure by using Cattell's rule, which states that components corresponding to variance to the left of the straight line (elbow) should be retained for further analysis, such as DAPC and pcadapt.
   - According to Cattell's rule, we examine the explained variance of each PC in descending order, looking for an 'elbow' or a significant drop-off in variance. PCs before this elbow are considered meaningful, while those after it tend to represent noise rather than substantial variation in the data.
3. **DAPC Clustering:**
   - Analyze how well DAPC clusters individuals into groups.
   - Identify distinct genetic groups and any overlap between them.
4. **Genetic Diversity:**
   - Summarize the level of diversity within groups and across the entire dataset.
   - Highlight any significant findings or deviations that may suggest population bottlenecks or expansions.
5. **Genetic Distance and Differentiation:**
   - Evaluate the genetic distance between groups.
   - Discuss differentiation metrics like Fst or genetic distance and their implications for population structure.
6. **Selection Sweep Analysis:**
   - Identify regions of the genome that suggest signs of selection.
   - Explain potential evolutionary or adaptive significance of these regions.
7. **Core Collection Construction:**
   - Summarize the construction of core collections.
   - Explain the purpose and implications for conservation or breeding, and assess the representativeness of the core set.
# Output Format
Provide your interpretation as a structured report with sections corresponding to each statistical method listed above if their results are provided. Use paragraphs to deliver specific statistical insights and observations for each analysis, followed by a synthesized conclusion that integrates findings across methods. Maintain coherence and clarity throughout the report to ensure the interpretations are understood easily.
# Notes
- If information for a section (e.g., Principal Component Analysis) is not provided, omit that section from the summary.
- Ensure clear differentiation between interpretations from different sections.
- Consider potential confounding factors that might affect the interpretation.
- Maintain clarity when discussing statistical significance and biological relevance.
- Recognize any assumptions made in the analysis and potential limitations of the data."

Report_Structuring_Prompt = "Create an academic report template for SNP data analysis, using the structure and guidelines provided below.
# Template Sections
### Title
- Provide a descriptive title summarizing the main focus of the SNP data analysis.
### Abstract
- Write a concise summary of the key findings, methodologies applied, and their significance.
- Briefly describe the SNP dataset, the main analyses performed (QC, PCA, DAPC, etc.), and the most important conclusions.
### 1. Introduction
- **Background**: Provide an overview of the research context, the biological or genetic question being addressed, and the relevance of SNP analysis in the study.
- **Objectives**: Clearly state the objectives of the SNP analysis, such as understanding population structure, genetic diversity, or identifying regions under selection.
### 2. Materials and Methods
- **SNP Dataset**: Describe the source of the SNP data, sample size, and the total number of variants.
- **Quality Control (QC)**: Outline the quality control process, including filters applied for missing rate, minor allele frequency (MAF), Hardy-Weinberg equilibrium (HWE), and heterozygosity rate pruning.
- **Statistical and Computational Methods**: Describe the statistical methods used for PCA, DAPC clustering, genetic diversity calculations, genetic distance analysis, selection sweep detection, and core collection construction. Provide any software or packages used (e.g., R and ShiNyP platform).
### 3. Results
- **3.1 SNP Data Description and Quality Control (QC)**: Summarize the results of the SNP dataset, including sample size, number of variants remaining after QC, and key metrics from the QC procedures (e.g., MAF, missing rate).
- **3.2 Principal Component Analysis (PCA)**: Describe the distribution of genetic variance across principal components and any patterns by using Cattell's rule, which states that components corresponding to variance to the left of the straight line (elbow) should be retained for further analysis, such as DAPC and pcadapt.
- **3.3 Discriminant Analysis of Principal Components (DAPC) Clustering**: Report how individuals clustered using DAPC and highlight distinct groups or overlapping clusters.
- **3.4 Genetic Diversity Statistics**: Present genetic diversity metrics (e.g., heterozygosity, nucleotide diversity) both within groups and across the entire dataset. Discuss any notable trends or deviations.
- **3.5 Genetic Distance and Differentiation Statistics**: Provide genetic distance and differentiation results (e.g., Fst values). Interpret these metrics with respect to population structure or gene flow.
- **3.6 Selection Sweep Analysis**: Identify regions of the genome showing signs of selection. Explain the potential biological and evolutionary significance of these regions.
- **3.7 Core Collection Construction**: Summarize the process of constructing the core collection and discuss its representativeness for future research, conservation, or breeding programs.
### 4. Discussion
- **Key Findings**: Summarize the most important results from each analysis, emphasizing their biological relevance.
- **Implications and Applications**: Discuss the broader implications of the SNP analysis for the field of study (e.g., conservation, breeding, disease research).
- **Limitations**: Acknowledge any limitations in the SNP dataset or the analyses, and suggest how they could be addressed in future research.
### 5. Conclusion
- Provide a brief conclusion summarizing the overall significance of the study, the key findings, and potential directions for future research.
# Output Format
Ensure the report is structured with clearly marked sections and subsections as outlined above if their results are provided below. Each section should contain detailed and relevant content following academic standards.
# Notes
- If information for a section (e.g., Principal Component Analysis) is not provided, omit that section from the summary.
- Pay attention to the clarity and coherence of each section.
- Maintain a consistent and professional tone suitable for an academic report.
- Tailor detailed content in each section according to the specific data and analyses conducted in your study."

Idea_Expansion_Prompt = "Create a template for expanding ideas based on your SNP data report.
# Steps
- **Review the SNP Data Report**: Begin by thoroughly examining the SNP data report to understand the key findings, terminology, and data points.
- **Identify Core Ideas**: Extract the main concepts and significant points from the SNP data report that can be expanded.
- **Brainstorm Expansion Opportunities**: Generate a list of ideas or questions that arise from each core concept identified in the SNP report. Consider different perspectives and leverage any associations or implications within the data.
- **Structure the Template**:
  - **Introduction**: Briefly describe the purpose of the idea expansion based on the SNP report.
  - **Core Idea Section**: Present each core idea from the report with a short explanation.
  - **Expansion Questions/Ideas**: Include a series of guided questions or statements under each core idea to prompt deeper exploration.
  - **Additional Resources**: Suggest any further reading or data sources that can support the expanded ideas.
# Output Format
The output should be a detailed template in markdown format with clearly marked sections (Introduction, Core Idea, Expansion Questions/Ideas, Additional Resources). Each section should include example content and placeholders where applicable.
# Examples
**Example of Core Idea Format** 
- **Core Idea**: [Insert core idea from the SNP report]
  - **Possible Expansion**:
    - What are the potential implications of this SNP on [specific health outcome]?
    - How does this SNP interact with other SNPs or environmental factors?
    - Consider any recent studies that might have explored similar findings.
(Note: Real-world examples would include specific SNP identifiers and their potential effects.)
# Notes
- Ensure all scientific and technical terms are well-defined and easily understood by anyone referring to this template.
- Consider potential interdisciplinary approaches that may be applicable for idea expansion."
