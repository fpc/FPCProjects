FROM fedorabaseimage

USER locuser
WORKDIR /home/locuser

ARG inifile=fppkgrepository_docker.ini

RUN mkdir repo
RUN mkdir data

COPY fppkgrepository/fppkgRepository /home/locuser
COPY fppkgrepository/config/$inifile /home/locuser/fppkgRepository.ini

EXPOSE 8282

CMD [ "/home/locuser/fppkgRepository", "-e" ]
