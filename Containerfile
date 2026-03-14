FROM rocker/r-ver:4.5.1

ARG DEBIAN_FRONTEND=noninteractive

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    openjdk-17-jdk-headless \
  && rm -rf /var/lib/apt/lists/*

RUN R -q -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages(c('remotes','cli','rcrossref','stringdist','xml2','dplyr','readr','purrr','janitor','readxl','testthat'))"

WORKDIR /tmp/src
COPY . /tmp/src
RUN R CMD INSTALL /tmp/src

COPY container/task.R /usr/local/bin/backsearchr-task.R
RUN chmod +x /usr/local/bin/backsearchr-task.R \
  && printf '%s\n' '#!/usr/bin/env bash' 'set -euo pipefail' 'exec Rscript /usr/local/bin/backsearchr-task.R "$@"' > /usr/local/bin/backsearchr-task \
  && chmod +x /usr/local/bin/backsearchr-task

RUN useradd --create-home --shell /bin/bash appuser \
  && mkdir -p /work/data/in /work/data/out \
  && chown -R appuser:appuser /work

WORKDIR /work
USER appuser

ENTRYPOINT ["backsearchr-task"]
CMD ["--help"]
