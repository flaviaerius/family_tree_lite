FROM rocker/r-ver:4.5

# curl: to install rv | libuv1t64: runtime dependency of the R package fs (used by bslib/sass)
RUN apt-get -y update && apt-get -y install curl libuv1t64 && rm -rf /var/lib/apt/lists/*

# install rv binary
RUN curl -sSL https://raw.githubusercontent.com/A2-ai/rv/refs/heads/main/scripts/install.sh | bash
ENV PATH="/root/.local/bin:$PATH"

WORKDIR /app

# copy rv lock and toml with packages and versions needed
COPY rv.lock rproject.toml ./
RUN rv sync

# copy shiny app code to the docker image workdir (app)
COPY . .

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
