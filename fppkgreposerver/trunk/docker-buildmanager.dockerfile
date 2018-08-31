FROM fedorabaseimage

USER locuser
WORKDIR /home/locuser

COPY buildmanager/buildmanager /home/locuser

EXPOSE 8181

CMD [ "/home/locuser/buildmanager" ]