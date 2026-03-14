FROM rocker/r-ver:4.5.1

ARG DEBIAN_FRONTEND=noninteractive
ARG GROBID_VERSION=0.8.2
ARG GROBID_DIST_URL=https://github.com/kermitt2/grobid/archive/${GROBID_VERSION}.zip
ARG BACKSEARCHR_GITHUB_REF=main

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
    unzip \
  && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/grobid \
  && curl -fsSL "${GROBID_DIST_URL}" -o /tmp/grobid.zip \
  && unzip -q /tmp/grobid.zip -d /opt/grobid \
  && test -d "/opt/grobid/grobid-${GROBID_VERSION}" \
  && chmod +x "/opt/grobid/grobid-${GROBID_VERSION}/gradlew" \
  && cd "/opt/grobid/grobid-${GROBID_VERSION}" \
  && ./gradlew --no-daemon clean install \
  && mkdir -p "/opt/grobid/grobid-${GROBID_VERSION}/grobid-home/tmp" \
  && rm -f /tmp/grobid.zip

ENV BACKSEARCHR_GROBID_JAR=/opt/grobid/grobid-${GROBID_VERSION}/grobid-core/build/libs/grobid-core-${GROBID_VERSION}-onejar.jar \
    BACKSEARCHR_GROBID_HOME=/opt/grobid/grobid-${GROBID_VERSION}/grobid-home

RUN test -f "${BACKSEARCHR_GROBID_JAR}" \
  && test -d "${BACKSEARCHR_GROBID_HOME}"

RUN R -q -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages(c('remotes','cli','rcrossref','stringdist','xml2','dplyr','readr','purrr','janitor','readxl','testthat'))"
RUN R -q -e "remotes::install_github('mrc-ide/backsearchr@${BACKSEARCHR_GITHUB_REF}', upgrade = 'never', dependencies = TRUE)"

COPY container/task.R /usr/local/bin/backsearchr-task.R
RUN chmod +x /usr/local/bin/backsearchr-task.R \
  && printf '%s\n' '#!/usr/bin/env bash' 'set -euo pipefail' 'exec Rscript /usr/local/bin/backsearchr-task.R "$@"' > /usr/local/bin/backsearchr-task \
  && chmod +x /usr/local/bin/backsearchr-task

RUN useradd --create-home --shell /bin/bash appuser \
  && mkdir -p /work/data/in /work/data/out \
  && chown -R appuser:appuser /work /opt/grobid

WORKDIR /work
USER appuser

ENTRYPOINT ["backsearchr-task"]
CMD ["--help"]
