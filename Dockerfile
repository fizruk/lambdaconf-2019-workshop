FROM dplusic/stack-ghcjs:lts-7.19

ADD project /project

WORKDIR /project

RUN stack setup --stack-yaml=stack-ghcjs.yaml
RUN stack build --stack-yaml=stack-ghcjs.yaml --only-dependencies

RUN stack build --stack-yaml=stack.yaml --only-dependencies

RUN stack install ghcid-0.7.4 extra-1.6.6

CMD ["bash"]
