FROM rocker/shiny:latest

# Install system dependencies (common for spatial and scientific packages)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git libcurl4-openssl-dev libfontconfig1-dev libxml2-dev \
    libudunits2-dev libssl-dev libproj-dev libgdal-dev \
    libharfbuzz-dev libfribidi-dev libgsl-dev libglu1-mesa \
    libtiff-dev libjpeg-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Copy the entire Planting-Tools directory to /srv/shiny-server
# Ideally for development we would mount not copy?
COPY . /srv/shiny-server/Planting-Tools/

# https://rstudio.github.io/renv/articles/docker.html
# Change default location of cache
ENV RENV_PATHS_CACHE renv/.cache 

# Set working directory to Planting-Tools for renv
WORKDIR /srv/shiny-server/Planting-Tools/

# Install renv and restore dependencies
RUN R -e "install.packages('renv', repos = 'https://cran.rstudio.com')" && \
    R -e "renv::restore()"
RUN R -e "renv::status()"

# Expose Shiny Server port
EXPOSE 3838

# # Start Shiny Server
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/Planting-Tools/ShinyForestry', host = '0.0.0.0', port = 3838)"]
