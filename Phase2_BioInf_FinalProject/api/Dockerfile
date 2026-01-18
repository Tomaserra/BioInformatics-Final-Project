FROM rocker/r-ver:4.3.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    pkg-config \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libsodium-dev \
    zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('plumber','jsonlite'), repos='https://cloud.r-project.org'); \
          stopifnot(requireNamespace('plumber', quietly=TRUE))"

WORKDIR /app
COPY plumber.R /app/plumber.R
COPY model/ /app/model/

EXPOSE 8000
CMD ["R","-e","pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]




