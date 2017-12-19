FROM fedora

ARG inifile=repository_docker.ini

RUN dnf -y update
RUN dnf -y install openssl-devel
RUN dnf clean all

RUN useradd --create-home --shell /bin/bash repository
USER repository
WORKDIR /home/repository

RUN mkdir gitrepos

COPY repository/repository /home/repository
COPY repository/config/$inifile /home/repository/repository.ini

EXPOSE 8089

CMD [ "/home/repository/repository" ]