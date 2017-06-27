
let () =
  Oprint.out_type := Printer.type';
  Oprint.out_value := Printer.value;
  Oprint.out_class_type := Printer.class_type;
  Oprint.out_module_type := Printer.module_type;
  Oprint.out_signature := Printer.signature;
  Oprint.out_sig_item := Printer.sig_item;
  Oprint.out_type_extension := Printer.type_extension
