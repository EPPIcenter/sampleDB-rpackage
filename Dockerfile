FROM rocker/shiny:4.3.0
RUN apt-get update && apt-get install -y  libicu-dev make pandoc zlib1g-dev sqlite3 libssl-dev libcurl4-openssl-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/ /etc/xdg/ 
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.0.6")'
RUN Rscript -e 'remotes::install_version("rappdirs",upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.4.2")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("yaml",upgrade="never", version = "2.3.7")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.4")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinybusy",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.3.0")'
RUN Rscript -e 'remotes::install_version("rjson",upgrade="never", version = "0.2.21")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.4")'
RUN Rscript -e 'remotes::install_version("reactable",upgrade="never", version = "0.4.3")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.9.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.27")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.0")'
Run Rscript -e 'remotes::install_version("httr2",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinyTime",upgrade="never", version = "1.0.3")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone

RUN R -e 'library(sampleDB); SampleDB_Setup(env=TRUE, db=FALSE, server=TRUE)'

# Enable Logging from stdout
ENV SHINY_LOG_STDERR=1
# USER shiny
EXPOSE 3838
CMD R -e 'library(sampleDB); SampleDB_Setup(env=FALSE, db=TRUE, server=FALSE); Run_SampleDB();'
