FROM fedora

RUN dnf -y update
RUN dnf -y install openssl-devel
RUN dnf clean all

RUN useradd --create-home --shell /bin/bash locuser