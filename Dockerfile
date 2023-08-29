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
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4, sass.cache = '/tmp/sass-cache')" \
    | tee /usr/local/lib/R/etc/Rprofile.site \
    | tee /usr/lib/R/etc/Rprofile.site

# Install R packages
RUN R -e 'install.packages(c("remotes", "markdown"))'
RUN Rscript -e 'remotes::install_version("vctrs",upgrade="never", version = "0.6.3")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("rappdirs",upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("yaml",upgrade="never", version = "2.3.7")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.4")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinybusy",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.3.0")'
RUN Rscript -e 'remotes::install_version("rjson",upgrade="never", version = "0.2.21")'
RUN Rscript -e 'remotes::install_version("reactable",upgrade="never", version = "0.4.3")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.9.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.27")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.6.0")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.5.1")'

# Update the .Renviron file with the SDB_CONFIG and SDB_PATH variables
RUN echo 'SDB_CONFIG="/etc/xdg/sampleDB/config.yml"' >> /usr/local/lib/R/etc/Renviron.site \
	&& echo 'SDB_PATH="/usr/local/share/sampleDB/sampledb_database.sqlite"' >> /usr/local/lib/R/etc/Renviron.site

# Copy and install the R package
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN R -e 'library(sampleDB); SampleDB_Setup(env=TRUE, db=FALSE, server=FALSE)'
RUN rm -rf /build_zone

# Copy Shiny application files
RUN cp -R /usr/local/lib/R/site-library/sampleDB/sampleDB /srv/shiny-server/

# Change ownership to shiny user for necessary directories
RUN chown -R shiny:shiny /srv/shiny-server \
	&& chown -R shiny:shiny /usr/local/share/sampleDB \
	&& chmod -R 777 /usr/local/share/sampleDB \
	&& chown -R shiny:shiny /etc/xdg/sampleDB \
	&& chown -R shiny:shiny /var/log/shiny-server \
	&& chown -R shiny:shiny /tmp/sass-cache \
	&& chmod -R 777 /var/log/shiny-server

# Enable Logging from stdout
ENV SHINY_LOG_STDERR=1

# # Use shiny user
# USER shiny

# Expose port 3838 to access Shiny
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]
