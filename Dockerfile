# Use rocker/shiny image, which already has Shiny Server installed
FROM rocker/shiny:latest

# Install system dependencies (common for spatial and scientific packages)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git libcurl4-openssl-dev libfontconfig1-dev libxml2-dev \
    libudunits2-dev libssl-dev libproj-dev libgdal-dev \
    libharfbuzz-dev libfribidi-dev libgsl-dev libglu1-mesa \
    libtiff-dev libjpeg-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Set working directory for renv.lock
WORKDIR /srv/shiny-server

# Copy renv lock file and restore R packages
COPY renv.lock .
RUN R -e "install.packages('renv', repos = 'https://cran.rstudio.com')" && \
    R -e "renv::restore()" && \
    rm renv.lock

# Expose Shiny Server port
EXPOSE 3838

# Start Shiny Server (the app will be mounted from the host)
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/ShinyForestry', host = '0.0.0.0', port = 3838)"]
