# Base image with R 4.5.0 and Shiny Server
FROM rocker/shiny:4.5.0

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libglpk-dev \
    libgsl-dev \
    libhdf5-dev \
    zlib1g-dev \
    libv8-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libfftw3-dev \
    libtiff-dev \
    libjpeg-dev \
    libgdal-dev \
    libproj-dev \
    git \
    curl \
    wget \
    locales \
    && apt-get clean

# Install required CRAN packages including BiocManager
RUN Rscript -e "install.packages(c(\
  'shiny','shinyjs','ggplot2','RColorBrewer','ggthemes','dplyr','data.table','DT','dartRverse',\
  'rms','haplo.stats','SNPassoc','dartR.base','dartR.popgen','vctrs','sass','stringr','poppr',\
  'ape','statgenGWAS','popkin','pcadapt','tidyr','circlize','hierfstat','ggrepel','remotes',\
  'fastmap','plotly','ggnewscale','ellmer','rmarkdown','BiocManager'\
), repos='https://cloud.r-project.org')"

# Set Bioconductor version (compatible with R 4.5)
RUN Rscript -e "BiocManager::install(version = '3.21')"

# Install required Bioconductor packages
RUN Rscript -e "BiocManager::install(c(\
  'qvalue', 'SNPRelate', 'snpStats', 'ggtree', 'treeio'\
), force = TRUE)"

# Copy local tar.gz package and install it
COPY ShiNyP_0.1.2.tar.gz /tmp/
RUN Rscript -e "remotes::install_local('/tmp/ShiNyP_0.1.2.tar.gz', force = TRUE)" \
  > /ShiNyP_install.log 2>&1 || (cat /ShiNyP_install.log && false)

# Start the ShiNyP app
CMD ["Rscript", "-e", "ShiNyP::run_ShiNyP(host = '0.0.0.0', port = 3838)"]
