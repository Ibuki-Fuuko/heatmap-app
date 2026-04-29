FROM rocker/shiny:latest

RUN R -e "install.packages(c('dplyr','ggplot2','plotly','scales'), repos='https://cran.rstudio.com/')"

COPY . /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]