FROM fedorabaseimage

RUN dnf -y install make binutils subversion glibc-devel rsync diffutils
RUN dnf clean all

USER locuser
WORKDIR /home/locuser

RUN mkdir svn
RUN mkdir bin
RUN mkdir buildfiles
RUN mkdir templates
ENV PATH="/home/locuser/bin:${PATH}"
WORKDIR /home/locuser/svn

COPY buildagent/buildagent /home/locuser
COPY buildagent/config/ppcx64_3.0.2 /home/locuser/bin
COPY buildagent/config/fppkg.cfg.template /home/locuser/templates
COPY buildagent/config/fppkg.cfg.304.template /home/locuser/templates

EXPOSE 8080

CMD [ "/home/locuser/buildagent", "-e" ]