FROM debian

RUN apt-get update && \
    apt-get install -y wget


ENV RACKET_INSTALLER_URL https://mirror.racket-lang.org/installers/7.3/racket-7.3-x86_64-linux.sh

RUN wget --output-document=racket-install.sh $RACKET_INSTALLER_URL && \
    echo "yes\n1\n" | /bin/bash racket-install.sh && \
    rm racket-install.sh

CMD ['racket']
