# Tdf

![License](https://img.shields.io/github/license/jacobtread/tdf?style=for-the-badge)
![Cargo Version](https://img.shields.io/crates/v/tdf?style=for-the-badge)
![Cargo Downloads](https://img.shields.io/crates/d/tdf?style=for-the-badge)

Library for serializing and deserializing the tdf network format used by BlazeSDK which is present in games such as 
Mass Effect 3, Mass Effect Andromeda, Battlefield 3, etc (Lots of EA games). 

This is a re-write of my previous library [blazepk](https://github.com/jacobtread/BlazePK-rs). This version provides much more accurate 
naming and support for the different structures along with a derive macro implementation. 

## Cargo

Using `tdf` with cargo

```yaml
[dependencies]
tdf = "0.1"
```
or 

```shell
cargo add tdf
```

## Derive Macros

Tdf comes with the feature flag `derive` (Enabled by default) which allows deriving deserialize and serialize implementations automatically. There are 3 derive macros
that you can use they are: TdfDeserialize, TdfSerialize, and TdfTyped

* **TdfDeserialize** - Allows the structure to be deserialized from tdf bytes
* **TdfSerialize** - Allows the structure to be serialized into tdf bytes
* **TdfTyped** Derives the TdfType for the structure so that it can be used as a tag value
  * This is only supported for Groups, Repr Enums, and Tagged Enums


### Deriving Structures

When creating structures for example to use as the contents of a Packet you should use this section, if you would like to use a structure as a group (Structures must be groups if you would like to tag them as values) see [Deriving Groups](#deriving-groups) for that.

Below is an example with a simple structure:

```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize)]
pub struct ExampleStructure {
    #[tdf(tag = "TEST")]
    pub my_value: u32
}

```

#### Lifetimes

You can provide a lifetime to a TdfDeserialize struct (The lifetime can be whatever you like and doesn't have to be 'de)

Lifetimes can be used for values like strings and Blobs to prevent needing to copy 
the underlying data, instead directly borrowing from the deserialize buffer:

```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize)]
pub struct ExampleStructure<'de> {
    #[tdf(tag = "TEST")]
    pub my_value: &'de str
}

```

> **Warning**
> TdfDeserialize structures can **NOT** have more than one lifetime more than one 
> lifetime will cause a compile time errror. However if you structure only implements
> TdfSerialize you can have as many lifetimes as you like

#### Generics

You can also provide generic parameters to your structures as long as the `where` clause is specified directly on the structure:

```rust
use tdf::prelude::*;

#[derive(TdfSerialize)]
pub struct ExampleStructure<T> 
where T: TdfSerialize + TdfTyped
{
    #[tdf(tag = "TEST")]
    pub my_value: T
}

```

> **Note**
> Any generic types that you want to use as a tag must also implement the `TdfTyped` trait

When using generic types with TdfDeserialize you must specify the lifetime like in the lifetimes example, if your structure doesn't make use of the deserialize lifetime you will get an error for it being unused, you can solve this by using `PhantomData` and marking it as skip (See [Skipping fields](#skipping-fields)):

```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize)]
pub struct ExampleStructure<'de, T> 
where T: TdfSerialize + TdfDeserialize<'de> + TdfTyped
{
    #[tdf(tag = "TEST")]
    pub my_value: T,
    #[tdf(skip)]
    pub _marker: PhantomData<&'de T>,
}

```

#### Skipping Fields

You can tell the macro to skip any fields that you don't want to include in serialization using `#[tdf(skip)]`

```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize)]
pub struct ExampleStructure {
    #[tdf(tag = "TEST")]
    pub my_value: u32,
    #[tdf(default)]
    pub skip_me: u32
}

```

Fields can also be skipped on tagged enum variants with named fields 

> **Note**
> If you are deriving `TdfDeserialize` any skipped types must implement `Default` because the 
> deserializer will use the `Default::default` implementation for that type in order to fill 
> in the struct

### Deriving Groups

When serializing structures in order to represent them as a group value that can 
be tagged they must have special leading and trailing data which is created if you 
specify `#[tdf(group)]` on your structure:

```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
#[tdf(group)]
pub struct ExampleStructure {
    #[tdf(tag = "TEST")]
    pub my_value: u32
}

```

Once a structure is marked as a group you can derive the `TdfTyped` and use it as a group in tags

For structures that require a (2) prefix you can use the `#[tdf(prefix_two)]` attribute on the structure:

```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
#[tdf(group, prefix_two)]
pub struct ExampleStructure {
    #[tdf(tag = "TEST")]
    pub my_value: u32
}

```


> **Warning**
> When a structure is marked as a group structure you cannot use it as a Packet body as it contains the extra bytes that represent it as a struct which will likely cause errors when another tool/client attempts to parse it


### Repr Enums

If you want to define an enum of possible values that maps to one of the existing primitive types (u8, i8, u16, i16, u32, i32, u64, i64, usize, isize) 
you can define a repr enum:

> **Note**
> Repr enums are serialized as variable-length integers aka TdfType::VarInt

```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
#[repr(u8)]
pub enum ExampleReprEnum {
    Test = 0x0,
    Test2 = 0x1,
    Test3 = 0x3
}


```

Don't forget to derive `TdfTyped` to use this value within tags

> **Note**
> When defining a repr enum you **MUST** use `#[repr(%TYPE%)]` otherwise the type will not be known

> **Warning**
> Unlike standard repr enums you **CANNOT** omit the discriminant for each value, as at this stage the
> macro cannot infer the next values.

#### Default Variant

When using the above enum if a value you will run into a `DecodeError` at runtime if a discriminant not present in the enum is deserialized. 
To prevent this behavior you can specify a default variant:


```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
#[repr(u8)]
pub enum ExampleReprEnum {
    Test = 0x0,
    Test2 = 0x1,
    Test3 = 0x3,
    #[tdf(default)]
    MyDefault = 0x4,
}
```


### Tagged Enums

You can represent BlazeSDK Tagged Unions as Rust enums with fields:


```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
pub enum ExampleTaggedEnum {
    #[tdf(key = 0x0, tag = "VALU")]
    Test {
        #[tdf(tag = "TEST")]
        test: String,
    },
    #[tdf(key = 0x1, tag = "VALU")]
    Test2(String)
}
```

> In the above the "key" attribute for each enum variant is the discriminant to use for that variant. 
> The "tag" attribute on each enum variant is the tag that will be used when tagging the value of the union

In the above there is a variant with named fields and one with a single unnamed field.

Variants with named fields are treated and serialized as groups (TdfType::Group) so the values within them have all 
the same conditions as [Deriving Groups](#deriving-groups) and most of the information from [Deriving Structures](#deriving-structures) also carries over.

Variants with unnamed fields (You are only allowed to specify 1 unnamed field as shown in `ExampleTaggedEnum::Test2`) the value is serialized as
the type of value provided, in this case it Test2 would have a tagged value of VALU with the type of string. 

#### Default Variant

When using the above enum just like in the repr example you will run into a `DecodeError`  at runtime if a discriminant not present in the enum is deserialized.
To prevent this behavior you can specify a default variant:

```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
pub enum ExampleTaggedEnum {
    #[tdf(key = 0x0, tag = "VALU")]
    Test {
        #[tdf(tag = "TEST")]
        test: String,
    },
    #[tdf(key = 0x1, tag = "VALU")]
    Test2(String)

    #[tdf(default)]
    DefaultValue
}
```

> For tagged enums that implement TdfSerialize, if a default value is serialized it will use the `unset` discriminant

#### Unset Variant

Tdf tagged unions have a special unset type discriminant so you will run into a `DecodeError`  at runtime if an unset discriminant is deserialized.
To prevent this behavior you can specify a unset variant:

```rust
use tdf::prelude::*;

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
pub enum ExampleTaggedEnum {
    #[tdf(key = 0x0, tag = "VALU")]
    Test {
        #[tdf(tag = "TEST")]
        test: String,
    },
    #[tdf(key = 0x1, tag = "VALU")]
    Test2(String)

    #[tdf(unset)]
    Unset
}
```

> **Note**
> You cannot use a default variant as an unset variant they must be seperate as the default variant handles skipping
> extra values that might exist which will fail 

## Implementing TdfSerialize

## Implementing TdfDeserialize