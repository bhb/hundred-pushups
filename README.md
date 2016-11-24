# hundred-pushups

A mobile app based on the plan at http://www.100pushups.com/

## Usage

Install deps:

```bash
 brew install node
 brew install watchman
```

Install React Native CLI tool:

`npm install -g react-native-cli`

Install `re-natal`:

`npm install -g re-natal`

To open emacs with access to inf-clojure and a REPL, do

`emacs .dir-locals.el` and type `y` at prompt

To get started with your new app, first cd into its directory:
`cd hundred-pushups`

Open iOS app in xcode and run it:

`react-native run-ios`

To use figwheel type:

```bash
re-natal deps
re-natal use-figwheel
lein figwheel ios
```

Reload the app in simulator

At the REPL prompt type this:
`(in-ns 'hundred-pushups.ios.core)`

Changes you make via the REPL or by changing your .cljs files should appear live.

Try this command as an example:
` (dispatch [:set-greeting "Hi everyone"])`

#### Starting Figwheel REPL from nREPL

Starting Figwheel REPL from nREPL

To start Figwheel within nREPL session:

`$ lein repl`

Then in the nREPL prompt type:

`user=> (start-figwheel "ios")`

Or, for Android build type:

`user=> (start-figwheel "android")`

Or, for both type:

`user=> (start-figwheel "ios" "android")`

### Tests

`lein with-profile +tools test-refresh :changes-only`

## License

Copyright © 2016 Ben Brinckerhoff and Dan Mayer

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
