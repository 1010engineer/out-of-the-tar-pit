FROM debian

RUN apt-get update && \
    apt-get install -y racket hugs

ADD . /ootp

CMD ["/usr/bin/mzscheme", "/ootp/rf-server.ss"]
