open Ppxlib;
open Ast_builder.Default;

let expand_function = (~loc: Location.t, ~path: string, block: structure) => {
  AstGen.EnumAst.(
    switch (block) {
    | [
        {
          pstr_desc:
            Pstr_type(
              flag,
              [
                {
                  ptype_name: {txt as enumName},
                  ptype_kind: Ptype_variant(constructors),
                } as innerType,
              ],
            ),
        } as enumType,
      ] =>
      let inferredTypeExpr = inferViewType(loc, constructors);
      let enumData = extractEnumData(loc, constructors, inferredTypeExpr);
      let enumDeclaration = {
        ...enumType,
        pstr_desc:
          Pstr_type(
            flag,
            [
              {
                ...innerType,
                ptype_name: {
                  txt: "t",
                  loc,
                },
                ptype_kind: Ptype_variant(constructors),
              },
            ],
          ),
      };

      let moduleContents = [
        enumDeclaration,
        [%stri
          let view =
            Ctypes.view(
              ~read=[%e readFunction(loc, enumData)],
              ~write=[%e writeFunction(loc, enumData)],
              [%e inferredTypeExpr],
            )
        ],
      ];

      let moduleBinding =
        Ast_builder.Default.module_binding(
          ~loc,
          ~name={txt: String.uppercase_ascii(enumName), loc},
          ~expr={
            Ast_builder.Default.pmod_structure(~loc, moduleContents);
          },
        );

      Ast_builder.Default.pstr_module(~loc, moduleBinding);
    | _ =>
      Location.raise_errorf("[ppx-ctypes-helper] Type declaration expected.")
    }
  );
};

let extension =
  Extension.declare(
    "enum",
    Extension.Context.Structure_item,
    Ast_pattern.(pstr @@ __),
    expand_function,
  );