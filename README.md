# Argonaut Aeson Generic Json Codecs

[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
[![Documentation](https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic/badge)](https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic)
[![Build Status](https://travis-ci.org/coot/purescript-argonaut-aeson-generic.svg?branch=master)](https://travis-ci.org/coot/purescript-argonaut-aeson-generic)

Generic codec for aeson generic encoding (only supporting
[defaultOptions](http://hackage.haskell.org/package/aeson-1.2.3.0/docs/Data-Aeson-TH.html#v:defaultOptions)
with `allNullaryToStringTag` set to `true`).

The package provides `genericEncodeAeson` and `genericDecodeAeson` function for
data types that have a `Generic.Rep` instance.

It is using `Data.Generic.Rep` hence it will work with `purescirpt-0.12`,
unlike
[purescript-argonaut-generic-codec](https://github.com/eskimor/purescript-argonaut-generic-codecs)
which at the moment is based on `Data.Generic` for which generic deriving has
been removed from the purescript compiler in this
[commit](https://github.com/purescript/purescript/commit/fe6a0981f83134c5fc2b5669c672dd8285b43c8b).

It is based on [purescript-argonaut-generic](https://github.com/purescript-contrib/purescript-argonaut-generic).
