FROM fedorabaseimage

# For mimes.types
RUN dnf -y install mailcap
RUN dnf clean all

USER locuser
WORKDIR /home/locuser
RUN mkdir /home/locuser/wwwroot

COPY identityserver/identityserver /home/locuser
COPY identityserver/wwwroot/bootstrap.css /home/locuser/wwwroot
COPY identityserver/wwwroot/bootstrap.css.map /home/locuser/wwwroot
COPY identityserver/wwwroot/site.css /home/locuser/wwwroot
COPY identityserver/logintemplate.html /home/locuser
COPY identityserver/logouttemplate.html /home/locuser

EXPOSE 5000

CMD [ "/home/locuser/identityserver", "-e" ]