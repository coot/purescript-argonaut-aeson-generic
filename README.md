# Argonaut Aeson Generic Json Codecs

[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
[![Documentation](https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic/badge)](https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic)
[![Build Status](https://travis-ci.org/coot/purescript-argonaut-aeson-generic.svg?branch=master)](https://travis-ci.org/coot/purescript-argonaut-aeson-generic)

Generic codec for aeson generic encoding. The promise is to support
interoperation with [the generic encoding of Haskell's Aeson]. The default
options mirror Aeson's _(so you can use `defaultOptions` on both sides)_, and
additionally all combinations of flags `allNullaryToStringTag` and
`tagSingleConstructors` are supported.

The package provides `genericEncodeAeson` and `genericDecodeAeson` function for
data types that have a `Generic.Rep` instance.

It is updated to work with `purescript-0.15`.

[the generic encoding of Haskell's Aeson]: https://hackage.haskell.org/package/aeson-1.5.4.1/docs/Data-Aeson.html#v:genericToJSON
