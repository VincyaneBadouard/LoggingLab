FROM rocker/tidyverse:latest
RUN apt-get update && apt-get install -y  cmake gdal-bin git libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libgdal-dev libgeos-dev libgit2-dev libgl1-mesa-dev libglpk-dev libglu1-mesa-dev libharfbuzz-dev libicu-dev libjpeg-dev libpng-dev libproj-dev libsqlite3-dev libssl-dev libtiff-dev libudunits2-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN R -e 'remotes::install_cran("attempt")'
RUN R -e 'remotes::install_cran("remotes")'
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("units",upgrade="never", version = "0.8-5")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("terra",upgrade="never", version = "1.7-78")'
RUN Rscript -e 'remotes::install_version("sp",upgrade="never", version = "2.1-4")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.46")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.2.1")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.5.1")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.26")'
RUN Rscript -e 'remotes::install_version("foreach",upgrade="never", version = "1.5.2")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "1.0-17")'
RUN Rscript -e 'remotes::install_version("raster",upgrade="never", version = "3.6-26")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1.1")'
RUN Rscript -e 'remotes::install_version("pkgdown",upgrade="never", version = "2.0.9")'
RUN Rscript -e 'remotes::install_version("tryCatchLog",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("doSNOW",upgrade="never", version = "1.0.20")'
RUN Rscript -e 'remotes::install_version("lwgeom",upgrade="never", version = "0.2-14")'
RUN Rscript -e 'remotes::install_version("smoothr",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("gdistance",upgrade="never", version = "1.6.4")'
RUN Rscript -e 'remotes::install_version("matlib",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("fasterize",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("nngeo",upgrade="never", version = "0.4.8")'
RUN Rscript -e 'remotes::install_version("BIOMASS",upgrade="never", version = "2.1.11")'
RUN Rscript -e 'remotes::install_version("devtools",upgrade="never", version = "2.4.5")'
RUN Rscript -e 'remotes::install_version("kableExtra",upgrade="never", version = NA)'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
EXPOSE 8787
ENV "PASSWORD"="password"
