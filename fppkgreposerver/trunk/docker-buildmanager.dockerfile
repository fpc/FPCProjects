FROM fedorabaseimage

USER locuser
WORKDIR /home/locuser

ARG inifile=buildmanager_docker.ini

COPY buildmanager/buildmanager /home/locuser
COPY buildmanager/config/$inifile /home/locuser/buildmanager.ini

EXPOSE 8181

CMD [ "/home/locuser/buildmanager" ]