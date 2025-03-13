#' @title Tree Layout Choices
#' @description A named character vector for layout keywords
#' @export
Tree_layout_choice = c("Rectangular" = "rectangular",
                       "Round rectangular" = "roundrect",
                       "Dendrogram" = "dendrogram",
                       "Ellipse" = "ellipse",
                       "Circular" = "circular",
                       "Inward circular" = "inward_circular",
                       "Radial" = "radial",
                       "Equal angle" = "equal_angle")
#' @title Tree Legend Choices
#' @description A named character vector for legend keywords
#' @export
Legend_choice = c("Top" = "top",
                  "Bottom" = "bottom",
                  "Right" = "right",
                  "Left" = "left",
                  "None" = "none")
#' @title Genetic Distance Choices
#' @description A named character vector for genetic distance keywords
#' @export
GT_method_choice = c("Cavalli-Sforza's chord distance" = "Dch",
                     "Nei's genetic distance" = "Da",
                     "Nei's standard genetic distance" = "Ds",
                     "Reynolds's genetic distance" = "Fst",
                     "Nei's minimum distance" = "Dm",
                     "Rogers's distance" = "Dr",
                     "Prevosti's distance" = "Cp",
                     "Sanghvi's distance" = "X2")
#' @title P value Adjustment Method Choices
#' @description A named character vector for p value adjustment keywords
#' @export
pval_adj_method_choice = c("Bonferroni" = "bonferroni",
                           "Benjamini & Hochberg (FDR)" = "BH")
#' @title AI Model Choices
#' @description A named character vector for AI Model keywords
#' @export
AI_model_choice = c("DeepSeek-V3" = "deepseek-chat",
                    # "DeepSeek-R1" = "deepseek-reasoner", -> Waiting time too long!
                    "o3-mini" = "o3-mini",
                    # "o1" = "o1", -> Waiting time too long!
                    "o1-mini" = "o1-mini",
                    # "GPT-4.5 Preview" = "gpt-4.5-preview",
                    "GPT-4o" = "gpt-4o",
                    "GPT-4o mini" = "gpt-4o-mini",
                    "GPT-4 Turbo" = "gpt-4-turbo",
                    "GPT-4" = "gpt-4",
                    "GPT-3.5 Turbo" = "gpt-3.5-turbo")
#' @title Color Choices
#' @description A named character vector for color
#' @export
custom_palette = c("#4482a8", "#91bb67", "#e3b800", "#E85C0D", "#e9788e", "#c493ff")

#' @title Summary Request Prompt
#' @description Prompt for Summary Request
#' @export
Summary_Request_Prompt = 
"Summarize key findings from a genome-wide SNP data analysis report (excluding GWAS) by addressing the following:

- **SNP Data & QC:** Describe the dataset (sample size, variants, SNP density) and QC procedures (missing rate, MAF, HWE).
- **PCA:** Explain variance distribution across principal components and the significance of major components.
- **DAPC Clustering:** Summarize clustering insights and population structure.
- **Genetic Diversity:** Highlight within-group and overall diversity trends (e.g., bottlenecks, expansions).
- **Genetic Distance:** Present differentiation metrics (e.g., Fst) and their implications.
- **Selection Sweep:** Identify regions under selection and their biological significance.
- **Core Collection:** Explain construction, significance, and applications.

Output each section as a concise paragraph only if data is available, clearly label each section, and address methods, assumptions, and limitations."

#' @title Data Interpretation Prompt
#' @description Prompt for Data Interpretation
#' @export
Data_Interpretation_Prompt = 
"Interpret a genome-wide SNP data report (excluding GWAS results) by focusing on the following sections:

1. **Data Description:**
    - Summarize SNP dataset characteristics (sample size, variant count, density) and QC procedures (integrity, reliability).
2. **PCA:**
    - Analyze variance distribution and reveal population structure insights.
3. **DAPC Clustering:**
    - Evaluate genetic clustering, identifying distinct groups and overlaps.
4. **Genetic Diversity:**
    - Summarize intra- and inter-group diversity, noting any bottlenecks or expansions.
5. **Genetic Distance:**
    - Assess differentiation (e.g., Fst) and discuss its impact on population structure.
6. **Selection Sweep Analysis:**
    - Identify genomic regions under selection and discuss their evolutionary significance.
7. **Core Collection Construction:**
    - Explain the rationale, representativeness, and applications (e.g., conservation, breeding) of the core collection.

Structure your report in clear, concise sections with integrated conclusions, highlighting statistical significance, assumptions, limitations, and omitting any sections without data."

#' @title Report Structuring Prompt
#' @description Prompt for Report Structuring
#' @export
Report_Structuring_Prompt = 
"Generate an academic report template for SNP data analysis in markdown with these sections:

1. **Title:** A descriptive title summarizing the SNP analysis focus.
2. **Abstract:** A concise summary of key findings, methods (e.g., QC, PCA, DAPC), dataset details, and conclusions.
3. **Introduction:**
    - **Background:** Context, research questions, and relevance.
    - **Objectives:** Clear study goals (e.g., population structure, genetic diversity).
4. **Materials and Methods:**
    - **SNP Dataset:** Source, sample size, and variant count.
    - **QC:** Filters (missing rate, MAF, HWE, heterozygosity).
    - **Methods:** Techniques (PCA, DAPC, genetic diversity, selection sweep, core collection) with software/tools.
5. **Results:**
    - **QC Metrics:** Sample size, variant count, MAF, missing rate.
    - **PCA & DAPC:** Patterns in population structure and clustering.
    - **Genetic Diversity & Distance:** Metrics and implications (e.g., Fst).
    - **Selection Sweep & Core Collection:** Significant regions and representativeness.
6. **Discussion:** Summarize key findings, implications, limitations, and future research directions.
7. **Conclusion:** Briefly restate study significance, results, and next steps.

Ensure each section is clearly marked, follows academic standards, and omits sections without data."

#' @title Idea Expansion Prompt
#' @description Prompt for Idea Expansion
#' @export
Idea_Expansion_Prompt = 
"Draft a concise template to expand ideas from an SNP data report. The template should include:

- **Introduction:** Briefly state the objective of expanding ideas based on the SNP report.
- **Core Ideas:** List key findings from the report with succinct explanations.
- **Expansion Prompts:** Under each core idea, provide targeted questions or prompts to explore implications, interactions, and interdisciplinary perspectives.
- **Additional Resources:** Recommend further readings or data sources.

Ensuring that all scientific and technical terms are clearly defined."
