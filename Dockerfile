FROM rocker/shiny:4.0.4

WORKDIR /home/shiny/

COPY init.R .
RUN /usr/local/bin/R --no-init-file --no-save --quiet --slave -f init.R

ENV PORT=8080

COPY server.R run.R ui.R ./
COPY www/* ./www/
CMD ["/usr/local/bin/R", "--no-save", "--gui-none", "-f run.R"]
