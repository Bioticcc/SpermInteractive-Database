FROM rocker/shiny:4.3.3

# System libraries needed by common R packages used in the app (Seurat, hdf5r, etc.)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    patch \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libcairo2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    fonts-open-sans \
    fonts-quicksand \
    libglpk-dev \
    libhdf5-dev \
    libgdal-dev \
    && rm -rf /var/lib/apt/lists/*

# Install renv to restore packages from the lockfile
RUN R -q -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages('renv')"

WORKDIR /srv/shinyapp

# Copy the Shiny app and lockfile
COPY ShinyApp/shinyApp/ ./
COPY renv.lock renv.lock

# Restore R packages declared in the lockfile
RUN R -q -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); renv::restore(lockfile = 'renv.lock', clean = TRUE, prompt = FALSE)"

EXPOSE 3838

# Launch the Shiny app
CMD ["R", "-q", "-e", "options(shiny.host='0.0.0.0', shiny.port=3838); shiny::runApp('/srv/shinyapp')"]
