# Revision history for mmzk-typeid


## UNLRELEASED CHANGES

* Provide some default implementations for methods of `IDConv`.


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
