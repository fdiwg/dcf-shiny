FROM rocker/r-ver:4.0.5

MAINTAINER Emmanuel Blondel "eblondel.pro@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git 

# install R package dependencies
RUN R -e "install.packages(c('remotes','jsonlite','yaml), repos='https://cran.r-project.org/')"
RUN R -e "source('install.R')"

RUN git -C /root/ clone https://github.com/eblondel/dcf-shiny.git && echo "OK!"
RUN ln -s /root/dcf-shiny /srv/dcf-shiny

#etc dirs (for config)
RUN mkdir -p /etc/dcf-shiny/

EXPOSE 3838

CMD ["R", "-e shiny::runApp('/srv/dcf-shiny',port=3838,host='0.0.0.0')"]
