FROM rocker/shiny-verse:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
	libicu-dev \
	make \
	pandoc \
	zlib1g-dev \
	sqlite3 && rm -rf /var/lib/apt/lists/*

# Create necessary directories
RUN mkdir -p /usr/local/lib/R/etc/ \
	/usr/lib/R/etc/ \
	/usr/local/share/sampleDB/upload_files \
	/usr/local/share/sampleDB/backups \
	/usr/local/share/sampleDB/move_files \
	/etc/xdg/sampleDB \
	/tmp/sass-cache 

# Update R configurations
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4, sass.cache = '/tmp/sass-cache', shiny.port = 3838, shiny.host = '0.0.0.0')" \
    | tee /usr/local/lib/R/etc/Rprofile.site \
    | tee /usr/lib/R/etc/Rprofile.site

# Update the .Renviron file with the SDB_CONFIG and SDB_PATH variables
RUN echo 'SDB_CONFIG="/etc/xdg/sampleDB/config.yml"' >> /usr/local/lib/R/etc/Renviron.site \
	&& echo 'SDB_PATH="/usr/local/share/sampleDB/sampledb_database.sqlite"' >> /usr/local/lib/R/etc/Renviron.site

# Install R packages
RUN R -e 'install.packages(c("remotes", "markdown"))'
RUN Rscript -e 'remotes::install_version("vctrs",upgrade="never", version = "0.6.3")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("rappdirs",upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("yaml",upgrade="never", version = "2.3.7")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.1.1")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinybusy",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.3.0")'
RUN Rscript -e 'remotes::install_version("rjson",upgrade="never", version = "0.2.21")'
RUN Rscript -e 'remotes::install_version("reactable",upgrade="never", version = "0.4.3")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.9.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.27")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.6.0")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "3.1.0")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.7.0")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("shinyTime",upgrade="never", version = "1.0.3")'
RUN Rscript -e 'remotes::install_version("curl",upgrade="never", version = "5.2.0")'
RUN Rscript -e 'remotes::install_version("openssl",upgrade="never", version = "2.1.1")'
RUN Rscript -e 'remotes::install_version("base64enc",upgrade="never", version = "0.1-3")'
RUN Rscript -e 'remotes::install_version("cronR", upgrade="never", version = "0.6.5")'

# Install local R package and set up
RUN mkdir /build_zone
COPY . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN R -e 'library(sampleDB); SampleDB_Setup(env=TRUE, db=FALSE, server=FALSE)'
WORKDIR /srv/shiny-server
COPY ./inst/sampleDB sampleDB

# Remove the work directory now
RUN rm -rf /build_zone

# Enable Logging from stdout
ENV SHINY_LOG_STDERR=1

# Run the Shiny app
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
