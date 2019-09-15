# ppx-ctypes-helper

## Purpose

The purpose of this preprocessor extension is to heavily simplify the usage of the ctypes library, especially when dealing with structures and enums.

## Installation

### Using esy

`esy add ppx-ctypes-helper`

### With dune

Add the following line to your `dune` file.

```
(preprocess ( pps ppx-ctypes-helper.lib ))
```

### With pesy

Add the following key to your `package.json` or `esy.json` file.

```json
"preprocess": "pps ppx-ctypes-helper.lib"
```

## Usage

### %%struct

The `%%struct` extension sets up a full structure comprised of a ctype definition with fields, utility operators, types and a view that will automatically serialize the structure into/from an ocaml record.

The available typenames are the same as those supported by ctypes. See [here](https://github.com/ocamllabs/ocaml-ctypes/blob/master/src/ctypes/ctypes_static.mli#L118) for the list.

#### Syntax

`(typename)("*" - 0 or more)("?" - 0 or 1 occurence)([fieldName] - 0 or 1 occurence with an optional field name between the brackets)`

Spaces are allowed between parts.

##### Conventions

- To be considered as a **structure**, a typename must start with an uppercase letter.
- To be considered as an **enum**, a typename must contain only uppercase letters and underscores.

#### Example

##### Reasonml syntax

```reasonml
%struct
(
    "StructureName",
    {
        integer: "int",
        text: "string",
        optional_string: "string_opt",
        // By default, uses the "size" field to determine the length  of the array.
        string_array: "string[]",
        size: "int32_t",
        other_structure: "OtherStructure",
        other_structure_pointer: "OtherStructure *",
        other_structure_pointer_optional: "OtherStructure *?",
        // Uses the "count" field to determine the length.
        other_structure_array: "OtherStructure[count]",
        count: "int",
        other_structure_pointer_optional_array: "OtherStructure *?[]",
        enumeration: "ENUM_NAME"
    }
)
```

##### Ocaml syntax

```ocaml
[%%struct ("StructureName", {
    integer = "int";
    text = "string";
    optional_string = "string_opt";
    string_array = "string[]";
    size = "int32_t";
    other_structure = "OtherStructure";
    other_structure_pointer = "OtherStructure *?";
    other_structure_pointer_optional = "OtherStructure *?";
    other_structure_array = "OtherStructure[count]";
    count = "int";
    other_structure_pointer_optional_array = "OtherStructure *?[]";
    enumeration = "ENUM_NAME";
})]
```

##### Generated code

```reasonml
Module StructureName {
  /* Abstract type to specialize the structure. */
  type t;

  /* Ctypes structure definition. */
  let structure: Ctypes.typ(Ctypes.structure(t));

  /* Operators to access and set structure fields. */

  /* To set some fields: structure >. field >= value >. field2 >= value2 */
  /* To get a field value: structure >? field */

  let ( >. ): ('a, 'b) => ('a, 'b);
  let ( >= ):
    ((Ctypes.structured('a, 'b), Ctypes.field('c, Ctypes.structured('a, 'b))),
    'c) => Ctypes.structured('a, 'b);
  let ( >? ):
    (Ctypes.structured('a, 'b),
    Ctypes.field('c, Ctypes.structured('a, 'b))) => 'c;

  /* Field definitions. */

  let integer: Ctypes.field(int, Ctypes.structure(t));
  let text: Ctypes.field(string, Ctypes.structure(t));
  let optional_string: Ctypes.field(option(string), Ctypes.structure(t));
  let string_array:
    Ctypes.field(option(Ctypes_static.ptr(string)), Ctypes.structure(t));
  let size: Ctypes.field(int32, Ctypes.structure(t));
  let other_structure:
    Ctypes.field(OtherStructure.t_view, Ctypes.structure(t));
  let other_structure_pointer:
    Ctypes.field(Ctypes_static.ptr(OtherStructure.t_view),
                  Ctypes.structure(t));
  let other_structure_pointer_optional:
    Ctypes.field(option(Ctypes_static.ptr(OtherStructure.t_view)),
                  Ctypes.structure(t));
  let other_structure_array:
    Ctypes.field(option(Ctypes_static.ptr(OtherStructure.t_view)),
                  Ctypes.structure(t));
  let count: Ctypes.field(int, Ctypes.structure(t));
  let other_structure_pointer_optional_array:
    Ctypes.field(option(Ctypes_static.ptr(option(Ctypes_static.ptr(OtherStructure.t_view)))),
                  Ctypes.structure(t));
  let enumeration: Ctypes.field(ENUM_NAME.t, Ctypes.structure(t));

  /* Type definition for the serialized record. */

  type t_view = {
    enumeration: ENUM_NAME.t,
    other_structure_pointer_optional_array:
      list(option(OtherStructure.t_view)),
    count: int,
    other_structure_array: list(OtherStructure.t_view),
    other_structure_pointer_optional: option(OtherStructure.t_view),
    other_structure_pointer: OtherStructure.t_view,
    other_structure: OtherStructure.t_view,
    size: int32,
    string_array: list(string),
    optional_string: option(string),
    text: string,
    integer: int,
  };

  /* A pair of functions + a view that are used to automatically convert from/to the record with the ctypes structure. */

  let read: Ctypes.structured(t, [ `Struct ]) => t_view;
  let write: t_view => Ctypes.structured(t, [ `Struct ]);
  let view: Ctypes.typ(t_view);
}
```

### %%enum

#### Example

##### Ocaml syntax

```ocaml
[%%enum type int_enum = Ten [@as 10] | One | Two | Twenty [@as 20]]
[%%enum type string_enum = Hello [@as "Hello"] | World]
```

##### Reasonml syntax

```reasonml
%enum
type int_enum =
    | Ten [@as 10]
    | One
    | Two
    | Twenty [@as 20]];

%enum
type string_enum =
    | Hello [@as "Hello"]
    | World;
```

##### Generated code

```ocaml
module INT_ENUM =
  struct
    type t =
      | Ten [@as 10]
      | One
      | Two
      | Twenty [@as 20]
    let view =
      Ctypes.view
        ~read:((function | 10 -> Ten | 1 -> One | 2 -> Two | 20 -> Twenty)
        [@warning "-8"])
        ~write:(function | Ten -> 10 | One -> 1 | Two -> 2 | Twenty -> 20)
        Ctypes.int
  end
module STRING_ENUM =
  struct
    type t =
      | Hello [@as "Hello"]
      | World
    let view =
      Ctypes.view ~read:((function | "Hello" -> Hello | "World" -> World)
        [@warning "-8"])
        ~write:(function | Hello -> "Hello" | World -> "World") Ctypes.string
  end
```