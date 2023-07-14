# Revision history for mmzk-typeid


## 0.2.0.0 -- 2023-MM-DD

* Remove deprecated functions `unUUID`, `parseStringWithPrefix`,
  `parseTextWithPrefix`, `parseByteStringWithPrefix`, `nil`, and `decorate`.


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
