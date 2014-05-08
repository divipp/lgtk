
# LGtk: Lens GUI Toolkit


## What is LGtk?

LGtk is a GUI Toolkit.

Main goals of LGtk:

* Provide a Haskell EDSL for declarative description of interactive graphical applications
* Provide an API for custom widget design
* Provide a playground for high-level declarative features like
  derived state-save and undo-redo operations and
  type-driven GUI generation

Other goals:

* Be platform independent (including web browsers)
* Support several backends and be able to use native widgets for each backend

Subgoals:

* Use high-level abstractions like lenses
* Try out an FRP variant
  which is built on state-based signals instead of time-based signals


## Status

LGtk has currently two backends: Gtk and diagrams+cairo+GLFW.

The current GLFW backend implementation is not native-looking and inefficient but this is the most easy
to install and this is the recommended backend.

There is a demo application which presents the current features of LGtk.

Status per goals:

* Provide a Haskell EDSL for declarative description of interactive graphical applications
  -- the next main missing feature is widget styling to be useful
* Provide an API for custom widget design
  -- not yet worked on it
* Provide a playground for high-level declarative features
  -- derived undo-redo operations are ready, needs documentation
* Be platform independent
  -- known to work on Linux; should work on Windows and Mac too
* Support several backends and be able to use native widgets for each backend
  -- Gtk and diagrams+cairo+GLFW is supported, the diagrams+cairo+GLFW implementation is inefficient
* Use high-level abstractions like lenses
  -- lenses are used but needs documentation
* Try out an FRP variant which is built on state-based signals instead of time-based signals
  -- state-base FRP is used but needs documentation


## Demo application

The demo application presents the current features of LGtk.

TODO: pictures & video link


### Installation

The easiest is to install lgtkdemo from hackageDB:

```
cabal update
cabal install lgtk
lgtkdemo
```

To install lgtkdemo with the Gtk backend:

```
cabal update
cabal install gtk2hs-buildtools
cabal install lgtk
lgtkdemo
```


## Usage

Only some Haddock documentation is available at the moment.


## Links

TODO




