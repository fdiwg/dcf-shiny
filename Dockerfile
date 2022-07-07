FROM rocker/r-ver:4.0.5

MAINTAINER Emmanuel Blondel "eblondel.pro@gmail.com"

# system libraries for LaTeX reporting & keyring
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    texlive-xetex \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-formats-extra \
	libsodium-dev \
    libsecret-1-dev \
    git
    
# general system libraries
# Note: this includes rdf/redland system libraries
RUN /rocker_scripts/install_verse.sh

# geospatial system libraries
RUN /rocker_scripts/install_geospatial.sh

# install R core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 httpuv
RUN R -e "install.packages(c('remotes','jsonlite','yaml'), repos='https://cran.r-project.org/')"
# clone app
RUN git -C /root/ clone https://github.com/eblondel/dcf-shiny.git && echo "OK!"
RUN ln -s /root/dcf-shiny /srv/dcf-shiny
# install R app package dependencies
RUN R -e "source('./srv/dcf-shiny/install.R')"

#etc dirs (for config)
RUN mkdir -p /etc/dcf-shiny/

EXPOSE 3838

CMD ["R", "-e shiny::runApp('/srv/dcf-shiny',port=3838,host='0.0.0.0')"]
