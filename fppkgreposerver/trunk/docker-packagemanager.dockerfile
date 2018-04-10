FROM fedora

ARG inifile=packagemanager_docker.ini

RUN dnf -y update
RUN dnf -y install openssl-devel
RUN dnf clean all

RUN useradd --create-home --shell /bin/bash packagemanager
USER packagemanager
WORKDIR /home/packagemanager

RUN mkdir data

COPY packagemanager/packagemanager /home/packagemanager
COPY packagemanager/config/$inifile /home/packagemanager/packagemanager.ini

EXPOSE 8088

CMD [ "/home/packagemanager/packagemanager" ]