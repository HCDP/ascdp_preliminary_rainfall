FROM ghcr.io/hcdp/task-base:latest
LABEL org.opencontainers.image.source="https://github.com/hcdp/preliminary_rainfall"
LABEL org.opencontainers.image.description="Base Image for preliminary rainfall workflows."

## PACKAGE/LIBRARY INSTALLATIONS ##
# Fetch the latest apt repo information.
RUN apt update
# Let APT know that we don't have a terminal.
ENV DEBIAN_FRONTEND=noninteractive


# # Install OS dependencies for the rainfall workflows.
RUN apt install -y libxml2-dev
RUN apt install -y libssl-dev
RUN apt install -y curl libcurl4-openssl-dev
RUN apt install -y libgdal-dev gdal-bin
RUN apt install -y libxtst-dev libxt6 libxrender1
RUN apt install -y libudunits2-dev
RUN apt install -y r-base
# RUN apt install -y ghostscript
# RUN apt install -y cmake

WORKDIR /build


RUN R -e 'install.packages("sf", version="1.0-14", repos="https://ftp.ussg.iu.edu/CRAN/")'
RUN R -e 'install.packages("dplyr", version="1.1.3", repos="https://ftp.ussg.iu.edu/CRAN/")'
RUN R -e 'install.packages("gstat", version="2.1-1", repos="https://ftp.ussg.iu.edu/CRAN/")'
RUN R -e 'install.packages("Metrics", version="0.1.4", repos="https://ftp.ussg.iu.edu/CRAN/")'
RUN R -e 'install.packages("readr", version="2.1.4", repos="https://ftp.ussg.iu.edu/CRAN/")'
RUN R -e 'install.packages("stringr", version="1.5.2", repos="https://ftp.ussg.iu.edu/CRAN/")'
RUN R -e 'install.packages("httr", version="1.4.7", repos="https://ftp.ussg.iu.edu/CRAN/")'
RUN R -e 'install.packages("lubridate", version="1.9.3", repos="https://ftp.ussg.iu.edu/CRAN/")'
RUN R -e 'install.packages("jsonlite", version="1.8.7", repos="https://ftp.ussg.iu.edu/CRAN/")'
RUN R -e 'install.packages("jsonlite", version="1.8.7", repos="https://ftp.ussg.iu.edu/CRAN/")'
RUN R -e 'install.packages("matrixStats", version="1.5.0", repos="https://cloud.r-project.org/")'
RUN R -e 'install.packages("geosphere", version="1.5-20", repos="https://cloud.r-project.org/")'
RUN R -e 'install.packages("tidyr", version="1.3.2", repos="https://cloud.r-project.org/")'


RUN mkdir -p /workspace/envs
RUN mkdir -p /sync

RUN wget https://raw.githubusercontent.com/ikewai/data-uploader/main/upload.py -O /sync/upload.py
ADD /tools /sync