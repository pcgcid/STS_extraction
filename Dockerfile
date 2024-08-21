FROM rocker/r-ver:4.4.1

RUN R --quiet -e "install.packages('remotes', repos = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R --quiet -e "remotes::install_github('rstudio/renv')"

RUN R -e "utils::install.packages('renv')"

# Install additional system packages
RUN apt-get update && apt-get install -y \
    wget \
    libxss1 \
    libnss3 \
    libxtst6 \
    libasound2 \
    && apt-get clean
    
    
RUN mkdir /app
WORKDIR /app

COPY renv.lock /app/renv.lock
COPY entrypoint.R /app/entrypoint.R

RUN R -e "renv::restore()"
RUN R -e "renv::hydrate()"

WORKDIR /tmp

RUN chmod +x /app/entrypoint.R

ENTRYPOINT ["/app/entrypoint.R"]
