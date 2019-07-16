FROM fedorabaseimage

RUN dnf -y install git
RUN dnf clean all

USER locuser
WORKDIR /home/locuser

COPY fppkgstack/fppkgstack /home/locuser
COPY fppkgstack/fppkgstack.ini /home/locuser

EXPOSE 5425

CMD [ "/home/locuser/fppkgstack", "-e" ]