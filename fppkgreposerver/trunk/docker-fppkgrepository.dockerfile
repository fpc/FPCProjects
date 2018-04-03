FROM fedora

ARG inifile=fppkgrepository_docker.ini

RUN dnf -y update
RUN dnf -y install openssl-devel git
RUN dnf clean all

RUN useradd --create-home --shell /bin/bash fppkgrepository
USER fppkgrepository
WORKDIR /home/fppkgrepository

RUN mkdir repo
RUN mkdir mirrorfiles
COPY fppkgrepository/config/mirrors.xml.testing mirrorfiles
COPY fppkgrepository/config/mirrors.xml.prod mirrorfiles

COPY fppkgrepository/fppkgRepository /home/fppkgrepository
COPY fppkgrepository/config/$inifile /home/fppkgrepository/fppkgRepository.ini

EXPOSE 8282

CMD [ "/home/fppkgrepository/fppkgRepository" ]