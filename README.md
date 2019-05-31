# lambdaconf-2019-workshop

LambdaConf 2019 workshop «Augmenting Reality in Haskell».

## How to prepare

Clone this repository:

```sh
git clone https://github.com/fizruk/lambdaconf-2019-workshop.git
```

Pull docker image:

```sh
docker pull fizruk/stack-ghcjs:lts-7.19
```

Just to make sure everything is fine, build the project (this may take a couple minutes first time):

```sh
cd lambdaconf-2019-workshop/
./with-docker.sh build
```

Now run server:

```sh
./with-docker.sh run
```

You should see an output similar to this:

```
Starting corridor-server...
============================================================
Static files served from /project/.stack-work/install/x86_64-linux/lts-7.19/ghcjs-0.2.1.9007019_ghc-8.0.1/bin/corridor-client.jsexe/
Starting corridor-server at https://localhost:8019
------------------------------------------------------------
Open AR application (on a smartphone) at https://10.0.1.13:8019/play/
Open AR marker (on a desktop) at
https://localhost:8019/play/assets/markers/lc-2019-marker.png
------------------------------------------------------------
```

You can now open [AR marker][ar_marker] on your desktop, open application
on your smartphone (using whatever local IP address you computer has),
allow the app to use smartphone's camera and point it to the marker on
your desktop!

[ar_marker]: project/client/static/assets/markers/lc-2019-marker.png

![AR marker.][ar_marker]

The result should look like this:

![AR demo.](images/ar_demo.jpg)

## Development


