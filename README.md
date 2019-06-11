# Augmenting Reality in Haskell

![AR cube game.](images/ar-stage-5.gif)

LambdaConf 2019 workshop «Augmenting Reality in Haskell».

## How to prepare

See preparation instructions in [`PREPARE.md`](PREPARE.md)
to get ready for the workshop ahead of time!
It is still possible to set up everything at the workshop,
but it can steal some time and leave you with less time to play around :)

## How to install SSL certificates

For the last stage, when you start using client-server communication
via websockets you need to install SSL certificates on the client smartphones.

Follow instructions in [`PREPARE_SSL.md`](PREPARE_SSL.md) to create and
install certificates so that a true multiplayer is possible for a local
server.

## Exercises

This project is designed in a way that allows you to focus
on pure game logic and rendering. This is done in `ARCube.Game.Stage_X` files.
By default Stage 0 (`ARCube.Game.Stage_0`) is used, but you should change
that to whatever stage you are currently working on. To do that go to
[project/ar-cube/src/ARCube/Game.hs](project/ar-cube/src/ARCube/Game.hs)
and change the import line like this:

```diff
 module ARCube.Game (
   module Game,
 ) where
 
-import           ARCube.Game.Stage_0 as Game
+import           ARCube.Game.Stage_1 as Game
```

In Stages 1 to 5 exercises are given in a form of `_exercise` expressions.
Simply remove (or comment) the definition of `_exercise` in the stage module
and fix the compile errors that you get:

```haskell
-- NOTE: remove _exercise and fix all type holes
-- to complete this stage
_exercise :: a
_exercise = error "Exercise in Stage 1 is not implemented!"
```

Note that solutions for all stages are provided
in `ARCube.Game.Solution.Stage_X` modules.
However, be sure to try the exercises yourself
before peaking at the suggested answers!

### Stage 0 — setup verification

If your setup is working you should be able to connect
to your locally span server and see a Haskell logo
over the AR marker:

![Stage 0 in AR.](images/ar-stage-0.png)

### Stage 1 — a 3×3×3 cube

**Exercise goal:**
render a 3×3×3 cube made up of cells.

The result should look like this:

![Stage 1 in AR.](images/ar-stage-1.png)

### Stage 2 — marking cells

**Exercise goal:**
cells should be marked (change color) on click.

The result should look like this:

![Stage 2 in AR.](images/ar-stage-2.gif)

### Stage 3 — focusing on cells

**Exercise goal:**
cells should also be rendered as focused when pointed to (gazed at).

The result should look like this:

![Stage 3 in AR.](images/ar-stage-3.gif)

### Stage 4 — rotating slices

**Exercise goal:**
clicking on a marked cell should rotate the corresponding slice of the cube.

The result should look like this:

![Stage 4 in AR.](images/ar-stage-4.gif)

### Stage 5 — two player game

**Exercise goal:**
game should support two players (e.g. red and blue)
with alternate turns.

The result should look like this:

![Stage 5 in AR.](images/ar-stage-5.gif)

### Stage 5∗ — two player client-server game

**Goal:** enable client-server architecture to play with multiple devices.

Client and server communicate via a websocket.
To enable this communication on client, switch to `ARCube.Client`
in [project/ar-cube-client/ar/Main.hs](project/ar-cube-client/ar/Main.hs)
(and in VR version as well):

```diff
 module Main where
 
-import           ARCube.Standalone (Mode (..), run)
+import           ARCube.Client (Mode (..), run)
 
 main :: IO ()
 main = run AR
```

To enable true multiplayer on server use `ARCube.Server_Stage_5`
in [project/ar-cube-server/src/ARCube/Server/Main.hs](project/ar-cube-server/src/ARCube/Server/Main.hs):

```haskell
-import           ARCube.Server               (api, mkDefaultConfig,
+import           ARCube.Server_Stage_5       (api, mkDefaultConfig,
                                               periodicUpdates, server)
```
