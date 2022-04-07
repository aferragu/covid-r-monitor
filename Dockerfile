FROM rocker/shiny:4.1.3

WORKDIR /home/shiny/

COPY init.R .
RUN /usr/local/bin/R --no-init-file --no-save --quiet --slave -f init.R

ENV PORT=3838

COPY server.R run.R ui.R ./
COPY www/* ./www/
CMD ["/usr/local/bin/R", "--no-save", "--gui-none", "-f run.R"]
