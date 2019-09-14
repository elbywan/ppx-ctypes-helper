open Ppxlib;

Driver.register_transformation(
  ~rules=[Context_free.Rule.extension(Struct.extension)],
  "struct",
);

Driver.register_transformation(
  ~rules=[Context_free.Rule.extension(Enum.extension)],
  "enum",
);