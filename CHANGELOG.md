# Revision history for mmzk-typeid


## 0.6.3.0 -- Unreleased

* Update implementation so that the prefix for `KindID` now conforms with specification v0.3.0.

* More useful compile-time errors for invalid `KindID` prefixes.

* More tests.

* Fix documentation typos.


## 0.6.2.0 -- 2024-05-28

* Fix the bug where the first 32768 `TypeID`s may not of the same timestamp.

* Test on GHC 9.8.2.


## 0.6.0.1 -- 2024-04-26

* Fix typo in the maintainer's email address.
  * Astounded at the fact that I mismatched the local part and domain name and didn't realise it for a year.

* More test cases on parsing.

* Fix other typos and inconsistencies in the documentation.


## 0.6.0.0 -- 2024-04-19

* Update implementation to conform with specification v0.3.0.
  * Allow `TypeID` prefix to contain underscores.
  * Update parsing logic as well as `Binary` and `Storable` instances to reflect the changes.
  * Add tests.

* Have a breaking change in `TypeIDError` to better reflect the error cases for the update in the specification.


## 0.5.0.2 -- 2024-3-10

* Add `Typeable` and `Data` instances for `TypeID` and `KindID`.
  * Kindly contributed by @shinzui.

* Fix all warnings.


## 0.5.0.1 -- 2023-9-18

* Fix bad links in the documentation.


## 0.5.0.0 -- 2023-08-31

* Support `TypeID` and `KindID` with `UUID` suffixes of version 5.
  * They are exported in `Data.TypeID.V5` and `Data.KindID.V5`.

* Tests for V5 `TypeID` and `KindID`.

* Change signature for `genID_` to support `UUID`v5.

* Decide against moving the `decorate` method.


## 0.4.0.1 -- 2023-08-19

* Support `TypeID` and `KindID` with `UUID` suffixes of version 1.
  * They are exported in `Data.TypeID.V1` and `Data.KindID.V1`.

* Tests for V1 `TypeID` and `KindID`.

* Fix documentation typos.

* The `decorate` method will be moved from `IDGen` to `IDType` in the next major release.

* The type signature for `genID_` is likely to change in the next major release to support `UUID`v5. Hopefully it will not affect any existing concrete functions.


## 0.4.0.0 -- 2023-08-08

* Support `TypeID` and `KindID` with `UUID` suffixes of version 4.
  * They are exported in `Data.TypeID.V4` and `Data.KindID.V4`.
  * By default, `TypeID` and `KindID` has a `UUID` suffix of version 7.
  * The default `TypeID` and `KindID` is also exported via `Data.TypeID.V7` and
    `Data.KindID.V7`.
  * The constructor shapes have been changed, but it should not cause any
    problems since they are not exported.

* Remove deprecated `nil` functions.

* Provide some default implementations for methods of `IDConv`.

* Fix typoes in the Haddock.

* Tests for V4 `TypeID` and `KindID`.


## 0.3.1.0 -- 2023-07-23

* Add `parseStringM`, `parseTextM`, and `parseByteStringM` to `IDConv`.
  * Instead of returning an `Either`, they throw an exception when the input is
    invalid.

* Add unsafe methods to `IDConv`.

* Implement `Storable` and `Binary` instances for `TypeID` and `KindID`.
  * These instances are experimental since the specification does not propose
    any serialisation format.


## 0.3.0.1 -- 2023-07-18

* Add a version upper-bound for 'uuid-types'.

* Fix documentation typos.


## 0.3.0.0 -- 2023-07-17

* Use 'uuid-types' package's `UUID` instead of a custom type.
  * `Data.UUID.V7` only retains the generation functions.
  * Other modules are not affected by this change.

* Add `Read` and `Hashable` instances for `TypeID` and `KindID`.

* Move `ValidPrefix` and `ToPrefix` to `Data.KindID.Class` module.
  * They are no longer exported from `Data.KindID`.

* Remove deprecated functions `unUUID`, `parseStringWithPrefix`,
  `parseTextWithPrefix`, `parseByteStringWithPrefix`, `nil`, and `decorate`.

* Re-implement `Show` instances for `TypeID` and `KindID` using pretty-print
  `toString`.

* Implement `TypeID` generation based on stateless `UUID`v7.
  * It is faster but does not guarantee monotonicity if multiple processes are
    generating `TypeID`s at the same time.

* Introduce unsafe `TypeID` and `KindID` functions for parsing and generating.
  They do not check the validity of the input and only behave well when the
  input is guaranteed to be valid.

* Add validity check on `TypeID` and `KindID` generation.
  * `checkID` checks the prefix and the `UUID`'s version and variant.
  * `checkIDWithEnv` also checks that the `UUID` is generated in the past.

* Deprecate `nilTypeID` and `nilKindID` since they are not useful.

* Remove dependency on 'transformers'.

* Fix typos in the documentation.

* More tests.


## 0.2.0.0 -- 2023-07-14

* Implement `KindID` to take arbitrary prefix type.
  * It can be a `Symbol` as before, but it can also be any type that implements
    `ToPrefix` which dictates how to translate the prefix type to a `Symbol`.

* Fix orphan instances for `TypeID` and `KindID`.

* Add `FromJSONKey` and `ToJSONKey` instances for `TypeID` and `KindID`.

* Introduce `IDType` class to unify the `getPrefix`, `getUUID`, and `getTime`
  functions of `TypeID` and `KindID`.

* Introduce `IDConv` class to unify the various conversion functions between
  `TypeID`/`KindID` and `String`/`Text`/`ByteString`.
  * The original concrete functions remain, and the class is provided as an
    alternative.

* Make the generation functions work with any `MonadIO` than just `IO`.

* Introduct `IDGen` class to unify the generation functions for `TypeID` and
  `KindID`.
  * The original concrete functions remain, and the class is provided as an
    alternative.

* Deprecate `unUUID`, `parseStringWithPrefix`, `parseTextWithPrefix`,
  `parseByteStringWithPrefix`, `nil`, and `decorate`. They are either replaced
  by functions of other names or are no longer necessary.
  * They will be removed in the next major version.

* The `UUID` type is expected to be removed in the next major version in favour
  of the type from the 'uuid-types' package.

* More tests.


## 0.1.0.0 -- 2023-07-11

* First version. Released on an unsuspecting world.

* Implement `TypeID` as specified at https://github.com/jetpack-io/typeid.

* Add unit tests.

* Add type-level `TypeID` prefixes.

* Add `FromJSON` and `ToJSON` instances for `TypeID` and `KindID`.
