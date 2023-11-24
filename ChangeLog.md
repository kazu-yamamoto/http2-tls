# Revision history for http2-tls

## 0.2.0

* Adding `runIO`.
* Breaking change: `Client.runH2C` takes `Settings`.
* Adding `settingsNumberOfWorkers`, `settingsConcurrentStreams`, `settingsConnectionWindowSize` and `settingsStreamWindowSize` to `Server.Settings`.
* Adding `settingsCacheLimit`, `settingsConcurrentStreams`, `settingsConnectionWindowSize` and `settingsStreamWindowSize`` to `Client.Settings`.

## 0.1.0

* Breaking chaange: `run` takes `Settings` as an argument.
  [#2](https://github.com/kazu-yamamoto/http2-tls/pull/2/)

## 0.0.1

* Supporting `tls` v1.8.0.

## 0.0.0

* First version.
