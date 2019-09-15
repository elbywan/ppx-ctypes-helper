open Ppxlib;

let typePattern =
  Str.regexp(
    "^\\([A-Za-z0-9_]+\\)\\(\\*+\\)?\\(\\?\\)?\\(\\[[A-Za-z0-9_]*\\]\\)?$",
  );
let enumPattern = Str.regexp("^[A-Z0-9_]+$");
let structPattern = Str.regexp("^[A-Z].*");

let arrayOf = (loc, t) => {
  Ast_builder.Default.ptyp_constr(~loc, {loc, txt: Lident("list")}, [t]);
};

let optionOf = (loc, t) => {
  Ast_builder.Default.ptyp_constr(~loc, {loc, txt: Lident("option")}, [t]);
};

let ptrOf = (loc, t) => {
  Ast_builder.Default.ptyp_constr(
    ~loc,
    {loc, txt: Ldot(Lident("Ctypes_static"), "ptr")},
    [t],
  );
};