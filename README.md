# hundred-pushups

A mobile app unoffially based on the plan at [100 Pushups](http://www.100pushups.com). This app is *not* endorsed by [100 Pushups](http://www.100pushups.com)!

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


### Running in iOS
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


### Running in Android

Open Android project in Android Studio

* Upgrade Android Studio
* Android Studio, open project `./android`
* open emulator or connect device
* Run the app
  * `react-native run-android` `#this is much faster` 
  * run app "click play button" (select simulator or device)
  * either way app will fail until figwheel is running, but you need the initial build to start figwheel
* run figwheel
* re-run the app as above this time it should connect to figwheels prompt

To use figwheel type:

```bash
re-natal use-android-device avd
or
re-natal use-android-device real
re-natal use-figwheel
lein figwheel android
```

If you get an error make sure you have a simulator or real device already running.

Reload the app in simulator

At the REPL prompt type this:
`(in-ns 'hundred-pushups.android.core)`

### Interacting with Running App

Changes you make via the REPL or by changing your .cljs files should appear live.

Try this command as an example:
`(dispatch [:set-greeting "Hi everyone"])`


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

### Logs

To see log output, run one of the following


```bash
$ react-native log-ios
$ react-native log-android
```

### Debug menu

iOS: `cmd + ctrl + Z`
android: `cmd + m`

### Todo

* move time selection to be number + am/pm picker
* add snooze

## License

####

Copyright © 2017 Ben Brinckerhoff and Dan Mayer

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
