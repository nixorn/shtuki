FROM ubuntu:17.04
MAINTAINER Artem Kanev "murchendaizer@gmail.com"
RUN apt-get update
RUN apt-get install -y libgmp-dev
WORKDIR /srv/shtuki
CMD ./bin/shtuki-exe
