# Revision history for http2-tls

## 0.3.0

* Usuing network-run v0.3. This means that IPv6Only is set to sockets.

## 0.2.11

* Allowing time-manger v0.1

## 0.2.10

* Allowing http2 v5.2

## 0.2.9

* Using runTCPClient
  [#13](https://github.com/kazu-yamamoto/http2-tls/pull/13)

## 0.2.8

* Using the latest network-control.
* Implementing util/h2-client and util/h2-server.

## 0.2.7

* Compatibility with tls < 2.0
  [#11](https://github.com/kazu-yamamoto/http2-tls/pull/11)
* Defining `settingsEarlyDataSize`.

## 0.2.6

* Defining `settingsWantSessionResume` and `settingsUseEarlyData`.
* Introduce settingsOpenServerSocket.
  [#10](https://github.com/kazu-yamamoto/http2-tls/pull/10)

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
