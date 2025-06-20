df = reactiveVal(NULL)
vcfData = reactiveVal(NULL)
VCFdf = reactiveVal(NULL)
Site_Info = reactiveVal(NULL)
fileName = reactiveVal(NULL)
fileInfo = reactiveVal(NULL)
fileInfo2 = reactiveVal(NULL)
guide_input = reactiveVal("Waiting for input VCF file. You can upload: \n✅ A VCF file from PLINK (recommended), or \n✅ A VCF or gzipped VCF file (vcf.gz) from VCFtools, or \n✅ VCF file in RDS from ShiNyP. \n Once you see 'Upload complete' on the progress bar, click the 'Input VCF file' button.")
input1 = reactiveVal("")
input2 = reactiveVal("")
input3 = reactiveVal("")
pre_results = reactiveVal(list())

guide_input2 = reactiveVal("You can input the SNP data in RDS: ✅ data.frame, ✅ genlight, or ✅ genind (for faster DAPC) that have already been transformed.")
dfstatus = reactiveVal("")
glstatus = reactiveVal("")
gistatus = reactiveVal("")
dfinfo = reactiveVal(NULL)
glinfo = reactiveVal(NULL)
giinfo = reactiveVal(NULL)

guide_sampleQC = reactiveVal("1️⃣ Need to obtain the summary statistics first! Then, scroll down to review the results. \n2️⃣ Adjust the thresholds and click the 'Sample QC by Thresholds' button.")
sampleQCstatus = reactiveVal("")
QCData = reactiveVal(NULL)
SampleQC_sample = reactiveVal(0)
SampleQC_SNP = reactiveVal(0)
samplemissingrate = reactiveVal(NULL)
samplemissing1 = reactiveVal(NULL)
samplemissing2 = reactiveVal(NULL)
samplemissing3 = reactiveVal(NULL)
sampleh = reactiveVal(NULL)
sampleh1 = reactiveVal(NULL)
sampleh2 = reactiveVal(NULL)
sampleh3 = reactiveVal(NULL)

guide_QC = reactiveVal("1️⃣ Need to obtain the summary statistics first! Then, scroll down to review the results. \n2️⃣ Adjust the thresholds and click the 'SNP QC by Thresholds' button.")
SNPQCstatus = reactiveVal("")
SNPQC_sample = reactiveVal(0)
SNPQC_SNP = reactiveVal(0)
missingrate = reactiveVal(NULL)
missing1 = reactiveVal(NULL)
missing2 = reactiveVal(NULL)
missing3 = reactiveVal(NULL)
maf = reactiveVal(NULL)
maf1 = reactiveVal(NULL)
maf2 = reactiveVal(NULL)
maf3 = reactiveVal(NULL)
h = reactiveVal(NULL)
h1 = reactiveVal(NULL)
h2 = reactiveVal(NULL)
h3 = reactiveVal(NULL)
HWE = reactiveVal(NULL)
HWE1 = reactiveVal(NULL)
HWE2 = reactiveVal(NULL)
HWE3 = reactiveVal(NULL)
progressVal = reactiveVal(0)

guide_SNPdensity = reactiveVal("Need to upload the ▶️ Site Info file (in RDS format) and ▶️ Chromosome Info file (in CSV format). \nPlease select the optimal window size and step, then click the 'Summary' button.")
SNPdensity1 = reactiveVal("")
SNPdensity2 = reactiveVal("")
SNPdensityresult1 = reactiveVal(NULL)
SNPdensityresults2 = reactiveVal(NULL)
densityplot = reactiveVal(NULL)

gl = reactiveVal(NULL)
groupInfo1 = reactiveVal(NULL)
guide_C = reactiveVal("Waiting to transform the data.frame to genlight.")
Cstatus2 = reactiveVal("")
Cstatus3 = reactiveVal("")
CTable2 = reactiveVal(NULL)
CTable3 = reactiveVal(NULL)
T2_Group1Info = reactiveVal(NULL)
T2_Group2Info = reactiveVal(NULL)
T2_Path1 = reactiveVal(NULL)
T2_Path2 = reactiveVal(NULL)
T2_Path3 = reactiveVal(NULL)
T2_Path4 = reactiveVal(NULL)
T2_Path5 = reactiveVal(NULL)
T2_Path6 = reactiveVal(NULL)
T2_Path7 = reactiveVal(NULL)
T2_Path8 = reactiveVal(NULL)
T2_Path9 = reactiveVal(NULL)

guide_PCA = reactiveVal("To run PCA, the input data must be in ✅ data.frame format. \nPlease click the 'Run PCA' button")
PCAfileInfo = reactiveVal("")
PCAtitle1 = reactiveVal("")
PCAtitle2 = reactiveVal("")
groupfile4 = reactiveVal(NULL)
PCA2Dplot = reactiveVal(NULL)
PCAexpplot = reactiveVal(NULL)
pca_result = reactiveVal(NULL)
PCA_SD = reactiveVal(data.frame())
PCA_Trans = reactiveVal(data.frame())
PC_number = reactiveVal(NULL)

guide_DAPC = reactiveVal("To run DAPC, the input data must be in ✅ genlight or ✅ genind format. \nPlease click the 'Run DAPC I' button first.")
DAPCfileInfo = reactiveVal("")
gi = reactiveVal(NULL)
DAPC1 = reactiveVal(NULL)
DAPC2 = reactiveVal(NULL)
DAPCtitle1 = reactiveVal("")
DAPCtitle2 = reactiveVal("")
DAPCtitle3 = reactiveVal("")
DAPCtitle4 = reactiveVal("")
DAPCtitle5 = reactiveVal("")
BICplot = reactiveVal(NULL)
DAPC_pop = reactiveVal(NULL)
DAPC_Trans = reactiveVal(NULL)

guide_UPGMA = reactiveVal("To run the UPGMA phylogenetic tree, the input data must be in ✅ genlight format. \nPlease click the 'Run UPGMA' button.")
UPGMAfileInfo = reactiveVal("")
UPGMAtitle1 = reactiveVal("")
tree = reactiveVal(NULL)
UPGMA = reactiveVal(NULL)

guide_NJ = reactiveVal("To run the NJ phylogenetic tree, the input data must be in ✅ genlight format.\nPlease click the 'Run NJ' button.")
NJfileInfo = reactiveVal("")
NJtree = reactiveVal(NULL)
NJ = reactiveVal(NULL)
NJtitle1 = reactiveVal("")

guide_Kinship = reactiveVal("To run the kinship matrix, the input data must be in ✅ data.frame format. \nThe 'Group Info' CSV file from DAPC analysis is optional. \nPlease click the 'Run Kinship' button.")
KinshipfileInfo = reactiveVal("")
groupInfo2 = reactiveVal(NULL)
KinshipMatrix = reactiveVal(NULL)
Kinshiptitle1 = reactiveVal("")

scatter_object = reactiveVal(NULL)
scatter_file = reactiveVal("")
scatter_data = reactiveVal(NULL)
scatterInfo = reactiveVal(NULL)
guide_scatter = reactiveVal("This page allows you to customize a scatter plot. You can upload: \n✅ PCA Object (in RDS), or\n✅ DAPC Object (in RDS), and\n✅ Group and Other Info. (in CSV).\nOnce your files are uploaded, click the 'Run Scatter Plot' button.")
scatter2D = reactiveVal("")
scatter3D = reactiveVal("")
Plot2D = reactiveVal(NULL)
Plot3D = reactiveVal(NULL)

treeInfo = reactiveVal(NULL)
guide_Tree = reactiveVal("This page allows you to customize a phylogenetic tree plot. You can upload:\n✅ UPGMA Object (in RDS), or\n✅ NJ Object (in RDS), and\n✅ Group and Other Info. (in CSV).\nOnce your files are uploaded, click the 'Run Tree Plot' button.")
TreePlot1 = reactiveVal("")
tree_object = reactiveVal(NULL)
TreePlot = reactiveVal(NULL)

guide_GD = reactiveVal("To analyze genetic diversity, the input data must be in ✅ data.frame format.\nYou also need to upload a ▶️ Site Info file (in RDS).\nThe 'Group Info' CSV file from DAPC analysis is optional. \nPlease click the 'Analysis' button.")
GDtitle1 = reactiveVal("")
GDtitle2 = reactiveVal("")
GDtitle3 = reactiveVal("")
GDfileInfo = reactiveVal("")
groupInfo3 = reactiveVal(NULL)
popgen = reactiveVal(NULL)
group_stat = reactiveVal("")
fst_matrix = reactiveVal("")
site_stat = reactiveVal(NULL)
GDplot = reactiveVal(NULL)

guide_Circos = reactiveVal("To run the sliding window analysis, you need to run 'Diversity Parameter' first! \nPlease select the optimal window size, then click the 'Run Sliding Window' button.")
Circostitle1 = reactiveVal("")
Circostitle2 = reactiveVal("")
GDInfo = reactiveVal("")
Track3 = reactiveVal(NULL)
Track4 = reactiveVal(NULL)
Track5 = reactiveVal(NULL)
Track6 = reactiveVal(NULL)
Chr_Info = reactiveVal(NULL)
SW_data = reactiveVal(NULL)
n_SelePara = reactiveVal(0)

guide_GT = reactiveVal("To run the genetic distance analysis, the input data must be in ✅ data.frame format.\nYou also need to upload a ▶️ Group Info. file.")
GTfileInfo = reactiveVal("")
groupInfo4 = reactiveVal(NULL)
GTmatrix = reactiveVal(NULL)
GTdf = reactiveVal(NULL)
GTtitle1 = reactiveVal("")
GTtitle2 = reactiveVal("")
GTplot = reactiveVal(NULL)

guide_AMOVA = reactiveVal("To run AMOVA, the input data must be in ✅ genlight file with 'Group Info.' \nYou can obtain this genlight file from the 'Data Transform' page ")
AMOVAfileInfo = reactiveVal("")
amova.result = reactiveVal(NULL)
amova.test = reactiveVal(NULL)
AMOVA_res = reactiveVal(NULL)
AMOVAvarplot = reactiveVal(NULL)
AMOVAtitle1 = reactiveVal("")
AMOVAtitle2 = reactiveVal("")
AMOVAtitle3 = reactiveVal("")

guide_pcadapt = reactiveVal("To run pcadapt, the input data must be in ✅ data.frame format. \nYou also need to upload a ▶️ Site Info file (in RDS). \nPlease click the 'Run pcadapt' button.")
pcadaptfileInfo = reactiveVal("")
pcadapttitle1 = reactiveVal("")
pcadapttitle2 = reactiveVal("")
pcadapttitle3 = reactiveVal("")
pcadapttitle4 = reactiveVal("")
pcadapttitle5 = reactiveVal("")
SNP_Info = reactiveVal(NULL)
pcadapt_data = reactiveVal(NULL)
pcadapt_data2 = reactiveVal(NULL)
pcadapt_data3 = reactiveVal(NULL)
pcadaptplot1 = reactiveVal(NULL)
pcadaptplot2 = reactiveVal(NULL)
pcadaptplot3 = reactiveVal(NULL)
pcadaptplot4 = reactiveVal(NULL)

guide_OutFLANK = reactiveVal("To run OutFLANK, the input data must be in ✅ genlight file with 'Group Info.' \nYou also need to upload a ▶️ Site Info file (in RDS).\nPlease click the 'Run OutFLANK' button.")
OutFLANKfileInfo = reactiveVal("")
OutFLANKtitle1 = reactiveVal("")
OutFLANKtitle2 = reactiveVal("")
OutFLANKtitle3 = reactiveVal("")
OutFLANKtitle4 = reactiveVal("")
OutFLANKtitle5 = reactiveVal("")
outflank = reactiveVal(NULL)
outflank_data2 = reactiveVal(NULL)
outflank_data3 = reactiveVal(NULL)
outflank_data4 = reactiveVal(NULL)
Chr_axis = reactiveVal(NULL)
OutFLANKplot1 = reactiveVal(NULL)
OutFLANKplot2 = reactiveVal(NULL)
OutFLANKplot3 = reactiveVal(NULL)
OutFLANKplot4 = reactiveVal(NULL)
OutFLANKplot5 = reactiveVal(NULL)

guide_IBS = reactiveVal("To run IBS, the input data must be in ✅ data.frame format. \nYou also need to upload the ▶️ Site Info file (in RDS) and ▶️ Chromosome Info file (in CSV). \nPlease click the 'Run IBS' button.")
IBSfileInfo = reactiveVal("")
IBStitle1 = reactiveVal("")
IBStitle2 = reactiveVal("")
IBS_result = reactiveVal()
IBSplot = reactiveVal()

Manhattan_data = reactiveVal(NULL)
guide_Manhattan = reactiveVal("This page allows you to customize a Manhattan plot. You can upload:\n✅ Genetic diversity per site (in RDS), or\n✅ pcadapt p-value per site (in RDS), or \n✅ OutFLANK p-value per site (in RDS), and\n✅ Chromosome Info.\nOnce your files are uploaded, click the 'Run Manhattan Plot' button.")
ManhattanPlot1 = reactiveVal("")
ManhattanPlot = reactiveVal(NULL)

guide_CoreSample = reactiveVal("To run core sample set, the input data must be in ✅ data.frame format. \nPlease click the 'Run Core Sample' button.")
CoreSamplefileInfo = reactiveVal("")
CoreSampletitle1 = reactiveVal("")
CoreSampletitle2 = reactiveVal("")
core_sample_coverage = reactiveVal(NULL)
core_sample_dataset = reactiveVal(NULL)
core_sample_info = reactiveVal(NULL)
CoreSampleplot = reactiveVal(NULL)

guide_CoreSNP = reactiveVal("To run core SNP set, the input data must be in ✅ data.frame format. \nYou also need to upload the ▶️ Site Info. and ▶️ Chromosome Info file (in CSV). \nPlease click the 'Run Core SNP' button.")
CoreSNPfileInfo = reactiveVal("")
CoreSNPtitle1 = reactiveVal("")
CoreSNPtitle2 = reactiveVal("")
core_SNP_dataset = reactiveVal(NULL)
selected_Site_Info = reactiveVal(NULL)
core_SNP_info = reactiveVal(NULL)
CoreSNPplot = reactiveVal(NULL)

guide_AI = reactiveVal("This page allows you to 1️⃣ generate your preliminary results from prior analysis, 2️⃣ select an AI model, 3️⃣ input your API key, and 4️⃣ get an AI-driven report.")
AItitle1 = reactiveVal("")
AItitle2 = reactiveVal("")
preliminary_results = reactiveVal(NULL)
AI_report = reactiveVal(NULL)
AI_template_text = reactiveVal(NULL)