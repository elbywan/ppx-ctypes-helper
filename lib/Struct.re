open Ppxlib;
open Ctypes;

let expand_function = (~loc: Location.t, ~path: string, expr: expression) => {
  Ast_builder.Default.(
    AstGen.StructureAst.(
      switch (expr.pexp_desc) {
      | Pexp_tuple([
          {pexp_desc: Pexp_constant(Pconst_string(structName, _))},
          {pexp_desc: Pexp_record(entries, _)},
        ]) =>
        let structNameNode = estring(~loc, structName);

        [@warning "-8"]
        let fieldsTypeRepresentations =
          entries
          |> List.filter((({txt: longident}, _)) => {
               switch (longident) {
               | Lident(_) => true
               | _ => false
               }
             })
          |> List.map((({txt: Lident(fieldName)}, expr)) => {
               let typeRepr =
                 switch (expr.pexp_desc) {
                 | Pexp_constant(Pconst_string(t, _)) =>
                   coerceTypeRepresentation(t)
                 | _ => Custom(expr, fieldName)
                 };
               (fieldName, typeRepr);
             });

        let fieldsDefinitions =
          List.fold_left(
            (acc, (fieldName, typeRepresentation)) => {
              let fieldNamePat =
                Ast_builder.Default.ppat_var(~loc, {txt: fieldName, loc});
              let fieldNameExpr =
                Ast_builder.Default.estring(~loc, fieldName);
              let fieldDefinitionExpr =
                switch (typeRepresentation) {
                | Custom(expr, _) =>
                  %expr
                  [%e expr]
                | _ => coerceTypeExpression(loc, typeRepresentation)
                };
              let fieldNode = [%stri
                let [%p fieldNamePat] =
                  Ctypes.field(
                    structure,
                    [%e fieldNameExpr],
                    [%e fieldDefinitionExpr],
                  )
              ];
              [fieldNode, ...acc];
            },
            [],
            fieldsTypeRepresentations,
          );

        let viewRelatedContent = [
          makeViewTypeDeclaration(loc, fieldsTypeRepresentations),
          makeReadFunction(loc, fieldsTypeRepresentations),
          makeWriteFunction(loc, fieldsTypeRepresentations),
          [%stri let view = Ctypes.view(~read, ~write, structure)],
        ];

        let moduleContents =
          [
            [%stri type t],
            [%stri
              let structure: Ctypes.typ(Ctypes.structure(t)) =
                Ctypes.structure([%e structNameNode])
            ],
            [%stri let (>.) = (s, fieldName) => (s, fieldName)],
            [%stri
              let (>=) = ((s, fieldName), value) => {
                Ctypes.setf(s, fieldName, value);
                s;
              }
            ],
            [%stri
              let (>?) = (s, fieldName) => {
                Ctypes.getf(s, fieldName);
              }
            ],
            ...fieldsDefinitions |> List.rev,
          ]
          @ [%str Ctypes.seal(structure)]
          @ viewRelatedContent;

        let moduleBinding =
          Ast_builder.Default.module_binding(
            ~loc,
            ~name={txt: String.capitalize_ascii(structName), loc},
            ~expr={
              Ast_builder.Default.pmod_structure(~loc, moduleContents);
            },
          );

        Ast_builder.Default.pstr_module(~loc, moduleBinding);
      | _ =>
        Location.raise_errorf(
          "[ppx-ctypes-helper] Tuple expected having signature: (name: string, fields: record).",
        )
      }
    )
  );
};

let extension =
  Extension.declare(
    "struct",
    Extension.Context.Structure_item,
    Ast_pattern.(single_expr_payload @@ __),
    expand_function,
  );