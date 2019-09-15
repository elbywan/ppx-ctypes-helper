open Ppxlib;
open Helpers;

module StructureAst = {
  type typeRepresentation =
    | Native(string, int, option(string), bool)
    | Mapped(string, string, int, option(string), bool)
    | Structure(string, int, option(string), bool)
    | Enum(string, int, option(string), bool)
    | Custom(Ppxlib.expression, string);

  /* String representation to type representation */
  let coerceTypeRepresentation = stringRepr => {
    let s = stringRepr |> Str.(global_replace(regexp(" "), ""));

    let matches = Str.string_match(typePattern, s, 0);

    if (!matches) {
      Location.raise_errorf("[ppx-ctypes-helper] Cannot coerce type: %s", s);
    };

    let stringRepr = Str.matched_group(1, s);
    let arrayRepr =
      switch (Str.matched_group(4, s)) {
      | "[]" => Some("size")
      | s => Some(String.sub(s, 1, (s |> String.length) - 2))
      | exception Not_found => None
      };

    let hasArrayIndirection =
      Base.Option.(arrayRepr >>| (_ => true) |> value(~default=false));
    let indirections =
      switch (Str.matched_group(2, s)) {
      | s => (s |> String.length) + (hasArrayIndirection ? 1 : 0)
      | exception Not_found => 0 + (hasArrayIndirection ? 1 : 0)
      };

    let optional =
      switch (Str.matched_group(3, s)) {
      | s => true
      | exception Not_found => false
      };

    switch (stringRepr) {
    | "char" => Native("char", indirections, arrayRepr, optional)
    | "float" => Native("float", indirections, arrayRepr, optional)
    | "int" => Native("int", indirections, arrayRepr, optional)
    | "bool" => Native("bool", indirections, arrayRepr, optional)
    | "string" => Native("string", indirections, arrayRepr, optional)
    | "nativeint" => Native("nativeint", indirections, arrayRepr, optional)
    | "void" => Mapped("void", "unit", indirections, arrayRepr, optional)
    | "double" => Mapped("double", "float", indirections, arrayRepr, optional)
    | "ldouble" =>
      Mapped("ldouble", "LDouble.t", indirections, arrayRepr, optional)
    | "schar" => Mapped("schar", "int", indirections, arrayRepr, optional)
    | "sint" => Mapped("sint", "int", indirections, arrayRepr, optional)
    | "short" => Mapped("short", "int", indirections, arrayRepr, optional)
    | "int8_t" => Mapped("int8_t", "int", indirections, arrayRepr, optional)
    | "int16_t" => Mapped("int16_t", "int", indirections, arrayRepr, optional)
    | "camlint" => Mapped("camlint", "int", indirections, arrayRepr, optional)
    | "int32_t" =>
      Mapped("int32_t", "int32", indirections, arrayRepr, optional)
    | "int64_t" =>
      Mapped("int64_t", "int64", indirections, arrayRepr, optional)
    | "complex32" =>
      Mapped("complex32", "Complex.t", indirections, arrayRepr, optional)
    | "complex64" =>
      Mapped("complex64", "Complex.t", indirections, arrayRepr, optional)
    | "complexld" =>
      Mapped("complexld", "ComplexL.t", indirections, arrayRepr, optional)
    | "long" =>
      Mapped("long", "Signed.long", indirections, arrayRepr, optional)
    | "llong" =>
      Mapped("llong", "Signed.llong", indirections, arrayRepr, optional)
    | "uchar" =>
      Mapped("uchar", "Unsigned.uchar", indirections, arrayRepr, optional)
    | "uint8_t" =>
      Mapped("uint8_t", "Unsigned.uint8", indirections, arrayRepr, optional)
    | "uint16_t" =>
      Mapped("uint16_t", "Unsigned.uint16", indirections, arrayRepr, optional)
    | "uint32_t" =>
      Mapped("uint32_t", "Unsigned.uint32", indirections, arrayRepr, optional)
    | "uint64_t" =>
      Mapped("uint64_t", "Unsigned.uint64", indirections, arrayRepr, optional)
    | "ushort" =>
      Mapped("ushort", "Unsigned.ushort", indirections, arrayRepr, optional)
    | "uint" =>
      Mapped("uint", "Unsigned.uint", indirections, arrayRepr, optional)
    | "ulong" =>
      Mapped("ulong", "Unsigned.ulong", indirections, arrayRepr, optional)
    | "ullong" =>
      Mapped("ullong", "Unsigned.ullong", indirections, arrayRepr, optional)
    | "string_opt" =>
      Mapped("string_opt", "option.string", indirections, arrayRepr, optional)
    | ident =>
      if (Str.string_match(enumPattern, ident, 0)) {
        Enum(ident, indirections, arrayRepr, optional);
      } else if (Str.string_match(structPattern, ident, 0)) {
        Structure(ident, indirections, arrayRepr, optional);
      } else {
        Location.raise_errorf(
          "[ppx-ctypes-helper] Cannot coerce type: %s",
          s,
        );
      }
    };
  };

  /* Type representation to field expression */
  let coerceTypeExpression = (~indirectionCoeff=0, loc, typeRepr) => {
    open Ast_builder.Default;
    let (matchedType, indirections, optional, isArray) =
      switch (typeRepr) {
      | Native(name, indirections, arrayField, optional) => (
          evar(loc, "Ctypes." ++ name),
          indirections,
          optional,
          arrayField |> Base.Option.is_some,
        )
      | Mapped(original, _, indirections, arrayField, optional) => (
          evar(loc, "Ctypes." ++ original),
          indirections,
          optional,
          arrayField |> Base.Option.is_some,
        )
      | Enum(name, indirections, arrayField, optional) => (
          evar(loc, name ++ ".view"),
          indirections,
          optional,
          arrayField |> Base.Option.is_some,
        )
      | Structure(name, indirections, arrayField, optional) => (
          evar(loc, name ++ ".view"),
          indirections,
          optional,
          arrayField |> Base.Option.is_some,
        )
      | Custom(_) =>
        Location.raise_errorf("[ppx-ctypes-helper] Coercion error.")
      };
    let adjustedIndirections = indirections + indirectionCoeff;

    let rec applyIndirections = (~i=0, ~acc=matchedType, unit) =>
      if (i < adjustedIndirections) {
        applyIndirections(
          ~i=i + 1,
          ~acc=
            i == indirections
            - (isArray ? 2 : 1)
            && optional
            || i == indirections
            - 1
            && isArray
              ? [%expr Ctypes.ptr_opt @@ [%e acc]]
              : [%expr Ctypes.ptr @@ [%e acc]],
          (),
        );
      } else {
        acc;
      };
    applyIndirections();
  };

  /* Type representation to field type */
  let coerceFieldType = (loc, typeRepr, fieldName) => {
    open Ast_builder.Default;
    open Helpers;

    let rec applyIndirections = (~count=0, ~optional=false, typ) =>
      if (count <= 0) {
        typ;
      } else if (optional) {
        optionOf(loc, ptrOf(loc, typ))
        |> applyIndirections(~count=count - 1);
      } else {
        ptrOf(loc, typ) |> applyIndirections(~count=count - 1);
      };

    switch (typeRepr) {
    | Mapped(_, name, indirections, arrayField, optional)
    | Native(name, indirections, arrayField, optional) =>
      let (identifierTxt, desc) =
        !String.contains(name, '.')
          ? (Lident(name), [])
          : (
            switch (name |> String.split_on_char('.')) {
            | ["option", field, ..._] => (
                Lident("option"),
                [ptyp_constr(~loc, {loc, txt: Lident(field)}, [])],
              )
            | [m, field, ..._] => (Ldot(Lident(m), field), [])
            | _ =>
              Location.raise_errorf(
                "[ppx-ctypes-helper] Cannot interpret field type: %s",
                name,
              )
            }
          );
      let rawType = ptyp_constr(~loc, {loc, txt: identifierTxt}, desc);

      if (arrayField == None) {
        rawType |> applyIndirections(~count=indirections, ~optional);
      } else {
        arrayOf(
          loc,
          rawType |> applyIndirections(~count=indirections - 1, ~optional),
        );
      };
    | Enum(name, indirections, arrayField, optional) =>
      let rawType =
        ptyp_constr(~loc, {loc, txt: Ldot(Lident(name), "t")}, []);
      if (arrayField == None) {
        rawType |> applyIndirections(~count=indirections, ~optional);
      } else {
        arrayOf(
          loc,
          rawType |> applyIndirections(~count=indirections - 1, ~optional),
        );
      };
    | Structure(name, indirections, arrayField, optional) =>
      let structureViewType =
        ptyp_constr(~loc, {loc, txt: Ldot(Lident(name), "t_view")}, []);

      if (arrayField == None) {
        if (indirections == 1) {
          optional ? optionOf(loc, structureViewType) : structureViewType;
        } else {
          structureViewType
          |> applyIndirections(~count=indirections, ~optional);
        };
      } else if (indirections == 2) {
        arrayOf(
          loc,
          optional ? optionOf(loc, structureViewType) : structureViewType,
        );
      } else {
        arrayOf(
          loc,
          structureViewType
          |> applyIndirections(~count=indirections - 1, ~optional),
        );
      };
    | Custom(_, fieldName) =>
      ptyp_var(~loc, fieldName |> String.lowercase_ascii)
    };
  };

  let makeViewTypeDeclaration =
      (loc, fields: list((string, typeRepresentation))) => {
    open Ast_builder.Default;
    let makePTypeParam = name => (
      {
        ptyp_desc: Ptyp_var(name),
        ptyp_loc: loc,
        ptyp_loc_stack: [],
        ptyp_attributes: [],
      },
      Invariant,
    );

    let ptypeParams =
      fields
      |> List.fold_left(
           (accu, (fieldName, repr)) => {
             switch (repr) {
             | Custom(_) => [makePTypeParam(fieldName), ...accu]
             | _ => accu
             }
           },
           [],
         );
    ();

    let recordLabelDeclarations =
      fields
      |> List.fold_left(
           (accu, (fieldName, typeRepr)) => {
             [
               {
                 pld_name: {
                   txt: fieldName,
                   loc,
                 },
                 pld_mutable: Immutable,
                 pld_type: coerceFieldType(loc, typeRepr, fieldName),
                 pld_loc: loc,
                 pld_attributes: [],
               },
               ...accu,
             ]
           },
           [],
         );

    pstr_type(
      ~loc,
      Recursive,
      [
        {
          ptype_name: {
            txt: "t_view",
            loc,
          },
          ptype_params: ptypeParams,
          ptype_cstrs: [],
          ptype_private: Public,
          ptype_manifest: None,
          ptype_attributes: [],
          ptype_loc: loc,
          ptype_kind: Ptype_record(recordLabelDeclarations),
        },
      ],
    );
  };

  let makeReadFunction = (loc, fields: list((string, typeRepresentation))) => {
    open Ast_builder.Default;

    let makeArraySizeExpr = (arrayField, field) => {
      let (_, arrayFieldTypeRepresentation) =
        fields |> List.find(((name, repr)) => name == arrayField);

      switch (arrayFieldTypeRepresentation) {
      | Mapped(_, "int32", _, None, _) =>
        %expr
        Int32.to_int @@ (cstruct >? [%e evar(loc, arrayField)])
      | Mapped(_, "int64", _, None, _) =>
        %expr
        Int64.to_int @@ (cstruct >? [%e evar(loc, arrayField)])
      | _ =>
        %expr
        cstruct >? [%e evar(loc, arrayField)]
      };
    };

    let makeFieldExpression = (fieldName, typeRepr) => {
      switch (typeRepr) {
      | Structure(name, 1, None, true) =>
        /* Optional structure pointer - dereferenced as an option*/
        switch%expr (cstruct >? [%e evar(loc, fieldName)]) {
        | None => None
        | Some(ptr) => Some(Ctypes.(!@)(ptr))
        }
      | Structure(name, 1, None, false) =>
        /* Structure pointer - forcefully dereferenced */
        %expr
        cstruct >? [%e evar(loc, fieldName)] |> Ctypes.(!@)
      | Structure(name, 2, Some(arrayField), true) =>
        /* Optional array of structure pointers - dereference array contents */
        switch%expr (cstruct >? [%e evar(loc, fieldName)]) {
        | None => []
        | Some(listPtr) =>
          Ctypes.CArray.(
            to_list(
              from_ptr(listPtr, [%e makeArraySizeExpr(arrayField, fields)]),
            )
            |> List.map(
                 fun
                 | None => None
                 | Some(ptr) => Some(Ctypes.(!@)(ptr)),
               )
          )
        }
      | Structure(name, 2, Some(arrayField), false) =>
        /* Array of structure pointers - forcefully dereference array contents */
        switch%expr (cstruct >? [%e evar(loc, fieldName)]) {
        | None => []
        | Some(listPtr) =>
          Ctypes.CArray.(
            to_list(
              from_ptr(listPtr, [%e makeArraySizeExpr(arrayField, fields)]),
            )
            |> List.map(Ctypes.(!@))
          )
        }
      | Native(_, _, arrayRepr, _)
      | Mapped(_, _, _, arrayRepr, _)
      | Enum(_, _, arrayRepr, _)
      | Structure(_, _, arrayRepr, _) =>
        switch (arrayRepr) {
        | None =>
          %expr
          cstruct >? [%e evar(loc, fieldName)]
        | Some(arrayField) =>
          switch%expr (cstruct >? [%e evar(loc, fieldName)]) {
          | None => []
          | Some(listPtr) =>
            Ctypes.CArray.(
              to_list(
                from_ptr(
                  listPtr,
                  [%e makeArraySizeExpr(arrayField, fields)],
                ),
              )
            )
          }
        }
      | Custom(_) =>
        %expr
        cstruct >? [%e evar(loc, fieldName)]
      };
    };

    let recordFieldsExpressions =
      fields
      |> List.fold_left(
           (accu, (fieldName, typeRepr)) => {
             [
               (
                 {loc, txt: Lident(fieldName)},
                 makeFieldExpression(fieldName, typeRepr),
               ),
               ...accu,
             ]
           },
           [],
         );

    [%stri
      let read = cstruct => {
        %e
        pexp_record(~loc, recordFieldsExpressions, None);
      }
    ];
  };

  let makeWriteFunction = (loc, fields: list((string, typeRepresentation))) => {
    open Ast_builder.Default;

    let getRecordField = field => {
      pexp_desc:
        Pexp_field(
          [%expr [%e evar(loc, "record")]],
          {loc, txt: Lident(field)},
        ),
      pexp_loc: loc,
      pexp_loc_stack: [],
      pexp_attributes: [],
    };

    let makeFieldExpression = (structure, fieldName, typeRepr) => {
      switch (typeRepr) {
      | Structure(name, 1, None, true) =>
        /* Optional structure pointer */
        %expr
        [%e structure]
        >. [%e evar(loc, fieldName)]
        >= (
             switch ([%e getRecordField(fieldName)]) {
             | None => None
             | Some(record) =>
               Some(
                 Ctypes.allocate([%e evar(loc, name ++ ".view")], record),
               )
             }
           )
      | Structure(name, 1, None, false) =>
        /* Structure pointer */
        %expr
        [%e structure]
        >. [%e evar(loc, fieldName)]
        >= Ctypes.allocate(
             [%e evar(loc, name ++ ".view")],
             [%e getRecordField(fieldName)],
           )

      | Structure(name, 2, Some(arrayField), true) =>
        /* Array of optional structure pointers */
        %expr
        [%e structure]
        >. [%e evar(loc, fieldName)]
        >= Some(
             Ctypes.CArray.(
               of_list(
                 [%e
                   coerceTypeExpression(~indirectionCoeff=-1, loc, typeRepr)
                 ],
                 [%e getRecordField(fieldName)]
                 |> List.map(
                      fun
                      | None => None
                      | Some(value) =>
                        Some(
                          Ctypes.allocate(
                            [%e
                              coerceTypeExpression(
                                ~indirectionCoeff=-2,
                                loc,
                                typeRepr,
                              )
                            ],
                            value,
                          ),
                        ),
                    ),
               )
               |> Ctypes.CArray.start
             ),
           )
      | Structure(name, 2, Some(arrayField), false) =>
        /* Array of structure pointers */
        %expr
        [%e structure]
        >. [%e evar(loc, fieldName)]
        >= Ctypes.CArray.(
             Some(
               of_list(
                 [%e
                   coerceTypeExpression(~indirectionCoeff=-1, loc, typeRepr)
                 ],
                 [%e getRecordField(fieldName)]
                 |> List.map(value =>
                      Ctypes.allocate(
                        [%e
                          coerceTypeExpression(
                            ~indirectionCoeff=-2,
                            loc,
                            typeRepr,
                          )
                        ],
                        value,
                      )
                    ),
               )
               |> start,
             )
           )
      | Structure(_, indirections, arrayRepr, optional)
      | Native(_, indirections, arrayRepr, optional)
      | Mapped(_, _, indirections, arrayRepr, optional)
      | Enum(_, indirections, arrayRepr, optional) =>
        switch (arrayRepr) {
        | None =>
          %expr
          [%e structure]
          >. [%e evar(loc, fieldName)] >= [%e getRecordField(fieldName)]
        | Some(_) =>
          %expr
          [%e structure]
          >. [%e evar(loc, fieldName)]
          >= Some(
               Ctypes.CArray.(
                 of_list(
                   [%e
                     coerceTypeExpression(~indirectionCoeff=-1, loc, typeRepr)
                   ],
                   [%e getRecordField(fieldName)],
                 )
                 |> Ctypes.CArray.start
               ),
             )
        }
      | Custom(_) =>
        %expr
        [%e structure]
        >. [%e evar(loc, fieldName)] >= [%e getRecordField(fieldName)]
      };
    };

    let rec buildStructureExpression = (structure, fields) =>
      switch (fields) {
      | [] => structure
      | [(fieldName, typeRepr)] =>
        makeFieldExpression(structure, fieldName, typeRepr)
      | [(fieldName, typeRepr), ...tail] =>
        let structure = makeFieldExpression(structure, fieldName, typeRepr);
        buildStructureExpression(structure, tail);
      };

    [%stri
      let write = record => {
        %e
        buildStructureExpression([%expr Ctypes.make(structure)], fields);
      }
    ];
  };
};

module EnumAst = {
  open Ast_builder.Default;

  [@warning "-8"]
  let defaultTypes =
      (loc, index, name, {pexp_desc: Pexp_ident({txt: Ldot(_, t)})}) =>
    switch (t) {
    | "int" => Pconst_integer(string_of_int(index), None)
    | "char" => Pconst_char(char_of_int(index))
    | "string" => Pconst_string(name, None)
    | "float" => Pconst_float(string_of_int(index), None)
    | _ =>
      Location.raise_errorf(
        "[ppx-ctypes-helper] Cannot infer field type: %s",
        name,
      )
    };

  let inferViewType = (loc, constructors) => {
    constructors
    |> List.fold_left(
         (accu, {pcd_name: {txt}, pcd_attributes}) => {
           pcd_attributes
           |> Base.List.find(~f=attr => attr.attr_name.txt == "as")
           |> Base.Option.map(
                ~f=
                  fun
                  | {
                      attr_payload:
                        PStr([
                          {
                            pstr_desc:
                              Pstr_eval(
                                {pexp_desc: Pexp_constant(value)},
                                _,
                              ),
                          },
                        ]),
                    } =>
                    switch (value) {
                    | Pconst_integer(_) => evar(~loc, "Ctypes.int")
                    | Pconst_char(_) => evar(~loc, "Ctypes.char")
                    | Pconst_string(_) => evar(~loc, "Ctypes.string")
                    | Pconst_float(_) => evar(~loc, "Ctypes.float")
                    }
                  | _ => accu,
              )
           |> Base.Option.value(~default=accu)
         },
         evar(~loc, "Ctypes.int"),
       );
  };

  let extractEnumData = (loc, constructors, inferredType) => {
    constructors
    |> List.mapi((index, {pcd_name: {txt}, pcd_attributes}) => {
         let value =
           pcd_attributes
           |> Base.List.find(~f=attr => attr.attr_name.txt == "as")
           |> Base.Option.bind(
                ~f=
                  fun
                  | {
                      attr_payload:
                        PStr([
                          {
                            pstr_desc:
                              Pstr_eval(
                                {pexp_desc: Pexp_constant(value)},
                                _,
                              ),
                          },
                        ]),
                    } =>
                    Some(value)
                  | _ => None,
              )
           |> Base.Option.value(
                ~default=defaultTypes(loc, index, txt, inferredType),
              );
         (txt, value);
       });
  };

  let readFunction = (loc, enumData) => {
    pexp_loc_stack: [],
    pexp_attributes: [
      {
        attr_name: {
          loc,
          txt: "warning",
        },
        attr_loc: loc,
        attr_payload: PStr([[%stri "-8"]]),
      },
    ],
    pexp_loc: loc,
    pexp_desc:
      Pexp_function(
        enumData
        |> List.map(((name, constant)) =>
             case(
               ~lhs={
                 ppat_desc: Ppat_constant(constant),
                 ppat_loc: loc,
                 ppat_loc_stack: [],
                 ppat_attributes: [],
               },
               ~rhs=pexp_construct(~loc, {loc, txt: Lident(name)}, None),
               ~guard=None,
             )
           ),
      ),
  };

  let writeFunction = (loc, enumData) =>
    pexp_function(
      ~loc,
      enumData
      |> List.map(((name, constant)) =>
           case(
             ~lhs={
               ppat_desc: Ppat_construct({loc, txt: Lident(name)}, None),
               ppat_loc: loc,
               ppat_loc_stack: [],
               ppat_attributes: [],
             },
             ~rhs=pexp_constant(~loc, constant),
             ~guard=None,
           )
         ),
    );
};