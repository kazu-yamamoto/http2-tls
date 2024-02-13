# Revision history for http2-tls

## 0.2.5

* Adding SessionManager to Settings.

## 0.2.4

* Allowing tls v2.0.

## 0.2.3

* Export `settingsServerNameOverride`
  [#9](https://github.com/kazu-yamamoto/http2-tls/pull/9)

## 0.2.2

* Reuse H2CLient.Authority
  [#8](https://github.com/kazu-yamamoto/http2-tls/pull/8)
* Using http2 v5.1

## 0.2.1

* Adding runIOH2C.
* Adding runWithConfig, runH2CWithConfig and runTLSWithConfig.
  [#6](https://github.com/kazu-yamamoto/http2-tls/pull/6)

## 0.2.0

* Adding `runIO`.
* Breaking change: `Client.runH2C` takes `Settings`.
* Adding `settingsNumberOfWorkers`, `settingsConcurrentStreams`, `settingsConnectionWindowSize` and `settingsStreamWindowSize` to `Server.Settings`.
* Adding `settingsCacheLimit`, `settingsConcurrentStreams`, `settingsConnectionWindowSize` and `settingsStreamWindowSize`` to `Client.Settings`.

## 0.1.0

* Breaking chaange: `run` takes `Settings` as an argument.
  [#2](https://github.com/kazu-yamamoto/http2-tls/pull/2)

## 0.0.1

* Supporting `tls` v1.8.0.

## 0.0.0

* First version.
