FROM rocker/shiny:latest

RUN R -e "install.packages(c('dplyr','ggplot2','plotly','scales'), repos='https://cran.rstudio.com/')"

COPY . /srv/shiny-server/

RUN cd /srv/shiny-server && R -e "source('app.R')" 2>&1 || true

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
