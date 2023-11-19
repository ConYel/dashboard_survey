FROM docker.io/rocker/r-ver:4.3.1

# Update env
RUN apt-get update && apt-get install -y  \
cmake git-core libcurl4-openssl-dev libpng-dev libproj-dev libssl-dev \
libxml2-dev make pandoc pandoc-citeproc python3 zlib1g-dev \
libxt-dev && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 2)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.2")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.4.3")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.28")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.5")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("vroom", upgrade="never", version = "1.6.3")'
RUN Rscript -e 'remotes::install_version("viridis", upgrade="never", version = "0.6.4")'


COPY Rprofile.site /usr/lib/R/etc/

RUN mkdir /home/dashboard
COPY dashboard_survey.R .
EXPOSE 3131

ENTRYPOINT [ "Rscript", "dashboard_survey.R"]
