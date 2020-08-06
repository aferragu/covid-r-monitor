FROM virtualstaticvoid/heroku-docker-r

COPY Aptfile /app
RUN apt-get update -q && cat Aptfile | xargs apt-get -qy install && rm -rf /var/lib/apt/lists/*

COPY init.R /app
RUN /usr/bin/R --no-init-file --no-save --quiet --slave -f /app/init.R

ENV PORT=8080

COPY server.R run.R ui.R /app/
CMD ["/usr/bin/R", "--no-save", "--gui-none", "-f /app/run.R"]
