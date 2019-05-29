FROM dplusic/stack-ghcjs:lts-7.19

ADD project /project

WORKDIR /project

RUN stack setup --stack-yaml=stack-ghcjs.yaml
RUN stack build --stack-yaml=stack-ghcjs.yaml --only-dependencies

RUN stack build --stack-yaml=stack.yaml --only-dependencies

CMD ["bash"]
