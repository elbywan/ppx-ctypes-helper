[%%enum type int_enum = Ten [@as 10] | One | Two | Twenty [@as 20]]
[%%enum type string_enum = Hello [@as "Hello"] | World]

[%%struct ("Test", {
    field = "void*"
})]

[%%struct ("Name", {
    integer = "int";
    text = "string **";
    stringArray = "string[]";
    size = "int32_t";
    stringArrayWithSize = "char*[sizeField]";
    sizeField = "int";
    character = Ctypes.uchar;
    other_structure = "Test *";
    other_structure_list = "Test[]";
    other_structure_ptr_list = "Test*[]";
    int_enum = "INT_ENUM";
    string_enum = "STRING_ENUM";
})]
