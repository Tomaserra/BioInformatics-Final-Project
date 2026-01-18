FROM rocker/shiny:4.3.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    pkg-config \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('httr','jsonlite'), repos='https://cloud.r-project.org'); \
          stopifnot(requireNamespace('httr', quietly=TRUE))"

WORKDIR /srv/shiny-server/app
COPY shiny/app.R ./app.R

EXPOSE 3838



