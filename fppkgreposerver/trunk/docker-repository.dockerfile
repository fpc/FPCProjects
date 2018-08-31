FROM fedorabaseimage

RUN dnf -y install git
RUN dnf clean all

USER locuser
WORKDIR /home/locuser

RUN mkdir gitrepos

COPY repository/repository /home/locuser

EXPOSE 8089

CMD [ "/home/locuser/repository" ]