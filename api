# ==============================================================================
# DOCKERFILE: ALZHEIMER'S PREDICTION API (BACKEND)
# ==============================================================================
# PROJECT:      Biomedical Informatics Final Project
# AUTHORS:      Carmen Caltagirone, Marc Verdaguer, Maria Artigas
# DESCRIPTION: Containerizes the R Plumber API. Installs necessary R packages,
#              copies the trained model files, and exposes port 8000.
# ==============================================================================

# 1. Base Image: Official R image with Plumber pre-installed
FROM rstudio/plumber

# 2. Install System Dependencies (Required for R packages)
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev

# 3. Install R Dependencies
# We install the specific packages required by APIFinalProject.r
RUN R -e "install.packages(c('jsonlite', 'dplyr'), repos='http://cran.rstudio.com/')"

# 4. Setup Working Directory
WORKDIR /app

# 5. Copy Source Code & Model Assets
# CHANGE: We now copy the file keeping its original name to avoid confusion
COPY APIFinalProject.r /app/APIFinalProject.r
COPY alzheimers_model.rds /app/alzheimers_model.rds
COPY selected_cpgs.rds /app/selected_cpgs.rds
COPY alzheimers_model_api.rds /app/alzheimers_model_api.rds

# 6. Expose Port
# Allow traffic on port 8000
EXPOSE 8000

# 7. Entry Point
# Automatically start the API on container launch
#  Updated the path to point to 'APIFinalProject.r' instead of 'api.R'
ENTRYPOINT ["R", "-e", "pr <- plumber::pr('/app/APIFinalProject.r'); pr %>% plumber::pr_run(host='0.0.0.0', port=8000)"]
