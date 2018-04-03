FROM fedora

ARG inifile=buildmanager_docker.ini

RUN dnf -y update
RUN dnf -y install openssl-devel
RUN dnf clean all

RUN useradd --create-home --shell /bin/bash buildmanager
USER buildmanager
WORKDIR /home/buildmanager

COPY buildmanager/buildmanager /home/buildmanager
COPY buildmanager/config/$inifile /home/buildmanager/buildmanager.ini

EXPOSE 8181

CMD [ "/home/buildmanager/buildmanager" ]