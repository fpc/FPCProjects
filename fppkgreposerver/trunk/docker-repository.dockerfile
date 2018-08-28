FROM fedorabaseimage

RUN dnf -y install git
RUN dnf clean all

USER locuser
WORKDIR /home/locuser

ARG inifile=repository_docker.ini

RUN mkdir gitrepos

COPY repository/repository /home/locuser
COPY repository/config/$inifile /home/locuser/repository.ini

EXPOSE 8089

CMD [ "/home/locuser/repository" ]