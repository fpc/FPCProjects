FROM fedorabaseimage

ARG inifile=packagemanager_docker.ini

USER locuser
WORKDIR /home/locuser

RUN mkdir data

COPY packagemanager/packagemanager /home/locuser
COPY packagemanager/config/$inifile /home/locuser/packagemanager.ini

EXPOSE 8088

CMD [ "/home/locuser/packagemanager" ]