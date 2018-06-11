FROM rocker/rstudio-stable:3.4.2

RUN apt-get update -qq && \
    apt-get install -y \
    pkg-config \
    libnlopt-dev \
    libmariadb-client-lgpl-dev \
    zlib1g-dev \
    libssh2-1-dev \
    libxml2-dev \
    curl \
    libpng-dev \
    sudo \
    wget \
    xclip \
    git

RUN export ADD=shiny && bash /etc/cont-init.d/add

RUN R -e "install.packages('readr', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"
RUN R -e "install.packages('shiny', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"
RUN R -e "install.packages('shiny.semantic', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"
RUN R -e "install.packages('shinyjs', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"
RUN R -e "install.packages('DT', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"
RUN R -e "install.packages('dplyr', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"
RUN R -e "install.packages('crosstalk', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"
RUN R -e "install.packages('plotly', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"
RUN R -e "install.packages('markdown', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"
RUN R -e "install.packages('clipr', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"
RUN R -e "install.packages('rclipboard', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')"


RUN git clone https://github.com/sorgerlab/drug_browser.git

RUN mv drug_browser/hms_small_mol /srv/shiny-server
RUN mv drug_browser/SelectivitySelectR /srv/shiny-server
RUN mv drug_browser/SimilaritySelectR /srv/shiny-server
RUN mv drug_browser/LibraryR /srv/shiny-server
