FROM dplusic/stack-ghcjs:lts-7.19

ADD project /project

RUN cd /project && stack setup
RUN cd /project && stack build --only-dependencies

WORKDIR /project

CMD ["bash"]
