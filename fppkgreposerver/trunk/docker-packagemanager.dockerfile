FROM fedorabaseimage

USER locuser
WORKDIR /home/locuser

RUN mkdir data

COPY packagemanager/packagemanager.ini /home/locuser
COPY packagemanager/packagemanager /home/locuser

EXPOSE 8088

CMD [ "/home/locuser/packagemanager", "-e" ]