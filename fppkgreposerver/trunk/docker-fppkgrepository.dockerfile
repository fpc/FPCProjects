FROM fedorabaseimage

USER locuser
WORKDIR /home/locuser

RUN mkdir repo
RUN mkdir data

COPY fppkgrepository/fppkgRepository.ini /home/locuser
COPY fppkgrepository/fppkgRepository /home/locuser

EXPOSE 8282

CMD [ "/home/locuser/fppkgRepository", "-e" ]
