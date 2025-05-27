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
GT_method_choice = c("Nei's genetic distance  (1983)" = "Da", # Eq. 7
                     "Nei's standard genetic distance (1972)" = "Ds", # Eq. 1
                     "Nei's minimum distance  (1973)" = "Dm", # Eq. 2
                     # "Reynolds's genetic distance  (1983)" = "Fst", # Eq. 3
                     "Rogers's distance  (1972)" = "Dr", # Eq. 4
                     "Prevosti's distance  (1975)" = "Cp", # Eq. 5
                     "Cavalli-Sforza's chord distance  (1967)" = "Dch", # Eq. 6
                     "Sanghvi's distance  (1953)" = "X2") # Eq. 8

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
                    "Gemini 2.0 Flash" = "gemini-2.0-flash",
                    "Gemini 2.0 Flash (API Free)" = "gemini-2.0-flash",
                    "Gemini 2.0 Flash-Lite" = "gemini-2.0-flash-lite",
                    "o4-mini" = "o4-mini",
                    "o3-mini" = "o3-mini",
                    # "o1" = "o1", -> Waiting time too long!
                    # "o1-mini" = "o1-mini", -> Discard 
                    # "GPT-4.5 Preview" = "gpt-4.5-preview",
                    "GPT-4.1" = "gpt-4.1",
                    "GPT-4.1 mini" = "gpt-4.1-mini",
                    "GPT-4.1 nano" = "gpt-4.1-nano",
                    "GPT-4o" = "gpt-4o",
                    "GPT-4o mini" = "gpt-4o-mini"
                    # "GPT-4 Turbo" = "gpt-4-turbo", -> Discard 
                    # "GPT-4" = "gpt-4", -> Discard 
                    # "GPT-3.5 Turbo" = "gpt-3.5-turbo" -> Discard 
                    )

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
- **Genetic Distance:** Present differentiation metrics and their implications.
- **Selection Sweep:** Detect regions under selective pressure and assess their biological relevance.
- **Core Collection:** Explain construction, significance, and applications.

Output each section as a concise paragraph only if data is available, clearly label each section."

#' @title Data Interpretation Prompt
#' @description Prompt for Data Interpretation
#' @export
Data_Interpretation_Prompt = 
  "Interpret a genome-wide SNP data analysis report (excluding GWAS results) by focusing on the following sections:

1. **Data Description:**
    - Summarize SNP dataset characteristics (sample size, variant count, density) and QC procedures (integrity, reliability).
2. **PCA:**
    - Analyze variance distribution and reveal population structure insights.
3. **DAPC Clustering:**
    - Evaluate genetic clustering, identifying distinct groups and overlaps.
4. **Genetic Diversity:**
    - Summarize intra- and inter-group diversity, noting any bottlenecks or expansions.
5. **Genetic Distance:**
    - Assess differentiation and discuss its impact on population structure.
6. **Selection Sweep Analysis:**
    - Identify genomic regions under selection and discuss their evolutionary significance. Compare the two methods to evaluate the consistency or divergence of the identified signals.
7. **Core Collection Construction:**
    - Explain the rationale, representativeness, and applications (e.g., conservation, breeding) of the core collection.

Structure your report in clear, concise sections with integrated conclusions, highlighting statistical significance, assumptions, limitations, and omitting any sections without data."

#' @title KEY
#' @description KEY
#' @export
KEY = "AIzaSyBCblK9s5AZo5WmT5UeQutpmg13mG0jblw"

#' @title Report Structuring Prompt
#' @description Prompt for Report Structuring
#' @export
Report_Structuring_Prompt = 
  "Generate an academic report template for SNP data analysis with these sections:

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
    - **Genetic Diversity & Distance:** Metrics and implications.
    - **Selection Sweep & Core Collection:** Overlapping significant regions and representativeness.
6. **Discussion:** Summarize key findings, implications, limitations, and future research directions.
7. **Conclusion:** Briefly restate study significance, results, and next steps."

#' @title Idea Expansion Prompt
#' @description Prompt for Idea Expansion
#' @export
Idea_Expansion_Prompt = 
  "Draft a concise template to expand ideas from an SNP data report. The template should include:

- **Introduction:** Briefly state the objective of expanding ideas based on the SNP report.
- **Core Ideas:** List key findings from the report with succinct explanations.
- **Expansion Prompts:** Under each core idea, provide targeted questions or prompts to explore implications, interactions, and interdisciplinary perspectives.

Ensuring that all scientific and technical terms are clearly defined."