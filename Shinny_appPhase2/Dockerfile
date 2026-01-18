# ==============================================================================
# DOCKERFILE: INTERACTIVE USER INTERFACE (FRONTEND)
# ==============================================================================
# PROJECT:      Biomedical Informatics Final Project
# AUTHORS:      Carmen Caltagirone, Marc Verdaguer, Maria Artigas
# DESCRIPTION: Containerizes the Shiny Application.
# ==============================================================================

# 1. Base Image
FROM rocker/shiny:latest

# 2. Install System Dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# 3. Install R Packages
RUN R -e "install.packages(c('shiny', 'httr', 'jsonlite', 'ggplot2', 'shinythemes', 'DT'), repos='http://cran.rstudio.com/')"

# 4. Deploy Application
RUN rm -rf /srv/shiny-server/*
COPY app.R /srv/shiny-server/

# 5. Network Configuration
EXPOSE 3838

CMD ["sh", "-c", "echo API_URL=$API_URL >> /home/shiny/.Renviron && /usr/bin/shiny-server"]
