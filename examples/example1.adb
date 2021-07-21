with Ada.Numerics;

with C_Standard_IO;
use C_Standard_IO;

with Native_Language_System;

procedure Example1 is

   package CIO renames C_Standard_IO;
   package NLS renames Native_Language_System;

   Pi : aliased constant Long_Float := 3.1457912;

   Catalog_Name    : constant String := "hello.cat";
   Catalog         : NLS.Catalog_T;
   Previous_Locale : NLS.Locale_T;

   procedure Put (Message : in Positive;
                  Text    : in Text_T)
   is
   begin
      CIO.Put (CIO.Standard_Output,
               NLS.Message (Catalog, 1, Message, Text));
   end Put;

begin  --  Example1
   NLS.Set_Locale (NLS.LC_ALL, "", Previous_Locale);
   begin
      NLS.Open_Catalog (Catalog, Catalog_Name);
   exception
      when NLS.POSIX_Error =>
         CIO.Put_Line (CIO.Standard_Error,
         "The " & Catalog_Name & " message catalog could not be opened." &
                " Exiting.");
         return;
   end;

   Put (1, Text & "Hello, world! π = " & 3.145791 & New_Line);
   Put (2, Text & "Γεια σας, 8 πλανήτες! π = " & 3.145791 & New_Line);
   Put (2, Text & "Address of π: " & Pi'Address & New_Line);
   Put (2, Text & Raw ("Здравствуй, мир!") & New_Line);
   Put (2, Text & Thousands_Grouping & Float_LL (-2145729980.0) & New_Line);
   Put (2, Text & Positive_Sign & Thousands_Grouping &
          Integer_L (102317123) & New_Line);
   Put (2, Text & "Ada.Numerics.e: " &
           Precision (60) & Float_LL (Ada.Numerics.e) & New_Line);

   NLS.Swap_Locale (NLS.LC_ALL, Previous_Locale);
   NLS.Close_Catalog (Catalog);
end Example1;
