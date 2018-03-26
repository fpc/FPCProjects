FROM fedora

ARG inifile=buildagent_docker_x86_64_linux_trunk.ini

RUN dnf -y update
RUN dnf -y install make binutils subversion openssl-devel glibc-devel rsync
RUN dnf clean all

RUN useradd --create-home --shell /bin/bash buildagent
USER buildagent
WORKDIR /home/buildagent

RUN mkdir svn
RUN mkdir bin
RUN mkdir buildfiles
ENV PATH="/home/buildagent/bin:${PATH}"
WORKDIR /home/buildagent/svn
RUN svn checkout https://svn.freepascal.org/svn/fpc/trunk fpc-trunk

COPY buildagent/buildagent /home/buildagent
COPY buildagent/config/$inifile /home/buildagent/buildagent.ini
COPY buildagent/config/ppcx64_3.0.2 /home/buildagent/bin

EXPOSE 8080

CMD [ "/home/buildagent/buildagent" ]