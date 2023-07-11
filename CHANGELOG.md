# Revision history for mmzk-typeid


## 0.2.0.0 -- 2023-XX-XX

* Implement `KindID` to take arbitrary prefix type.
  * It can be a `Symbol` as before, or it can be any type that implements
    `ToPrefix` which dictates how to translate the prefix type to a `Symbol`.

* Fix orphan instances for `TypeID` and `KindID`.


## 0.1.0.0 -- 2023-07-11

* First version. Released on an unsuspecting world.

* Implement TypeID as specified at https://github.com/jetpack-io/typeid.

* Add unit tests.

* Add type-level TypeID prefixes.

* Add `FromJSON` and `ToJSON` instances for `TypeID` and `KindID`.
