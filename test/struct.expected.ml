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
module Test =
  struct
    type t
    let structure = (Ctypes.structure "Test" : t Ctypes.structure Ctypes.typ)
    let (>.) s fieldName = (s, fieldName)
    let (>=) (s, fieldName) value = Ctypes.setf s fieldName value; s
    let (>?) s fieldName = Ctypes.getf s fieldName
    let field = Ctypes.field structure "field" (Ctypes.ptr @@ Ctypes.void)
    ;;Ctypes.seal structure
    type t_view = {
      field: unit Ctypes_static.ptr }
    let read cstruct = { field = (cstruct >? field) }
    let write record = ((Ctypes.make structure) >. field) >= record.field
    let view = Ctypes.view ~read ~write structure
  end
module Name =
  struct
    type t
    let structure = (Ctypes.structure "Name" : t Ctypes.structure Ctypes.typ)
    let (>.) s fieldName = (s, fieldName)
    let (>=) (s, fieldName) value = Ctypes.setf s fieldName value; s
    let (>?) s fieldName = Ctypes.getf s fieldName
    let integer = Ctypes.field structure "integer" Ctypes.int
    let text =
      Ctypes.field structure "text"
        (Ctypes.ptr @@ (Ctypes.ptr @@ Ctypes.string))
    let stringArray =
      Ctypes.field structure "stringArray" (Ctypes.ptr_opt @@ Ctypes.string)
    let size = Ctypes.field structure "size" Ctypes.int32_t
    let stringArrayWithSize =
      Ctypes.field structure "stringArrayWithSize"
        (Ctypes.ptr_opt @@ (Ctypes.ptr @@ Ctypes.char))
    let sizeField = Ctypes.field structure "sizeField" Ctypes.int
    let character = Ctypes.field structure "character" Ctypes.uchar
    let other_structure =
      Ctypes.field structure "other_structure" (Ctypes.ptr @@ Test.view)
    let other_structure_opt =
      Ctypes.field structure "other_structure_opt"
        (Ctypes.ptr_opt @@ Test.view)
    let other_structure_list =
      Ctypes.field structure "other_structure_list"
        (Ctypes.ptr_opt @@ Test.view)
    let other_structure_ptr_list =
      Ctypes.field structure "other_structure_ptr_list"
        (Ctypes.ptr_opt @@ (Ctypes.ptr @@ Test.view))
    let other_structure_ptr_list_opt =
      Ctypes.field structure "other_structure_ptr_list_opt"
        (Ctypes.ptr_opt @@ (Ctypes.ptr_opt @@ Test.view))
    let int_enum = Ctypes.field structure "int_enum" INT_ENUM.view
    let string_enum = Ctypes.field structure "string_enum" STRING_ENUM.view
    ;;Ctypes.seal structure
    type 'character t_view =
      {
      string_enum: STRING_ENUM.t ;
      int_enum: INT_ENUM.t ;
      other_structure_ptr_list_opt: Test.t_view option list ;
      other_structure_ptr_list: Test.t_view list ;
      other_structure_list: Test.t_view list ;
      other_structure_opt: Test.t_view option ;
      other_structure: Test.t_view ;
      character: 'character ;
      sizeField: int ;
      stringArrayWithSize: char Ctypes_static.ptr list ;
      size: int32 ;
      stringArray: string list ;
      text: string Ctypes_static.ptr Ctypes_static.ptr ;
      integer: int }
    let read cstruct =
      {
        string_enum = (cstruct >? string_enum);
        int_enum = (cstruct >? int_enum);
        other_structure_ptr_list_opt =
          (match cstruct >? other_structure_ptr_list_opt with
           | None -> []
           | ((Some (listPtr))[@explicit_arity ]) ->
               let open Ctypes.CArray in
                 (to_list
                    (from_ptr listPtr (Int32.to_int @@ (cstruct >? size))))
                   |>
                   (List.map
                      (function
                       | None -> None
                       | ((Some (ptr))[@explicit_arity ]) ->
                           ((Some ((Ctypes.(!@) ptr)))[@explicit_arity ]))));
        other_structure_ptr_list =
          (match cstruct >? other_structure_ptr_list with
           | None -> []
           | ((Some (listPtr))[@explicit_arity ]) ->
               let open Ctypes.CArray in
                 (to_list
                    (from_ptr listPtr (Int32.to_int @@ (cstruct >? size))))
                   |> (List.map Ctypes.(!@)));
        other_structure_list =
          (match cstruct >? other_structure_list with
           | None -> []
           | ((Some (listPtr))[@explicit_arity ]) ->
               let open Ctypes.CArray in
                 to_list
                   (from_ptr listPtr (Int32.to_int @@ (cstruct >? size))));
        other_structure_opt =
          (match cstruct >? other_structure_opt with
           | None -> None
           | ((Some (ptr))[@explicit_arity ]) -> ((Some ((Ctypes.(!@) ptr)))
               [@explicit_arity ]));
        other_structure = ((cstruct >? other_structure) |> Ctypes.(!@));
        character = (cstruct >? character);
        sizeField = (cstruct >? sizeField);
        stringArrayWithSize =
          (match cstruct >? stringArrayWithSize with
           | None -> []
           | ((Some (listPtr))[@explicit_arity ]) ->
               let open Ctypes.CArray in
                 to_list (from_ptr listPtr (cstruct >? sizeField)));
        size = (cstruct >? size);
        stringArray =
          (match cstruct >? stringArray with
           | None -> []
           | ((Some (listPtr))[@explicit_arity ]) ->
               let open Ctypes.CArray in
                 to_list
                   (from_ptr listPtr (Int32.to_int @@ (cstruct >? size))));
        text = (cstruct >? text);
        integer = (cstruct >? integer)
      }
    let write record =
      ((((((((((((((((((((((((((((Ctypes.make structure) >. integer) >=
                                  record.integer)
                                 >. text)
                                >= record.text)
                               >. stringArray)
                              >=
                              ((Some
                                  ((let open Ctypes.CArray in
                                      (of_list Ctypes.string
                                         record.stringArray)
                                        |> Ctypes.CArray.start)))
                              [@explicit_arity ]))
                             >. size)
                            >= record.size)
                           >. stringArrayWithSize)
                          >=
                          ((Some
                              ((let open Ctypes.CArray in
                                  (of_list (Ctypes.ptr @@ Ctypes.char)
                                     record.stringArrayWithSize)
                                    |> Ctypes.CArray.start)))
                          [@explicit_arity ]))
                         >. sizeField)
                        >= record.sizeField)
                       >. character)
                      >= record.character)
                     >. other_structure)
                    >= (Ctypes.allocate Test.view record.other_structure))
                   >. other_structure_opt)
                  >=
                  (match record.other_structure_opt with
                   | None -> None
                   | ((Some (record))[@explicit_arity ]) ->
                       ((Some ((Ctypes.allocate Test.view record)))
                       [@explicit_arity ])))
                 >. other_structure_list)
                >=
                ((Some
                    ((let open Ctypes.CArray in
                        (of_list Test.view record.other_structure_list) |>
                          Ctypes.CArray.start)))
                [@explicit_arity ]))
               >. other_structure_ptr_list)
              >=
              (let open Ctypes.CArray in
                 ((Some
                     (((of_list (Ctypes.ptr @@ Test.view)
                          (record.other_structure_ptr_list |>
                             (List.map
                                (fun value -> Ctypes.allocate Test.view value))))
                         |> start)))
                 [@explicit_arity ])))
             >. other_structure_ptr_list_opt)
            >=
            ((Some
                ((let open Ctypes.CArray in
                    (of_list (Ctypes.ptr_opt @@ Test.view)
                       (record.other_structure_ptr_list_opt |>
                          (List.map
                             (function
                              | None -> None
                              | ((Some (value))[@explicit_arity ]) ->
                                  ((Some ((Ctypes.allocate Test.view value)))
                                  [@explicit_arity ])))))
                      |> Ctypes.CArray.start)))
            [@explicit_arity ]))
           >. int_enum)
          >= record.int_enum)
         >. string_enum)
        >= record.string_enum
    let view = Ctypes.view ~read ~write structure
  end
