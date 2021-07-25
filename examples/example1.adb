with Ada.Numerics;

with C_Standard_IO;
use C_Standard_IO;

with Native_Language_System;

procedure Example1 is

   package CIO renames C_Standard_IO;
   package NLS renames Native_Language_System;

   type Messages is
     (Hello, String_Length, Untranslated);

   Pi  : aliased constant Long_Float := 3.141593;
   Mir : constant String := "мир";

   Catalog_Name    : constant String := "hello.cat";
   Catalog         : NLS.Catalog_T;
   Previous_Locale : NLS.Locale_T;

   procedure Put (Message : in Messages;
                  Text    : in Text_T)
   is
   begin
      CIO.Put (CIO.Standard_Output,
               NLS.Message (Catalog, 1, Messages'Pos (Message) + 1, Text));
   end Put;

begin  --  Example1
   NLS.Set_Locale (NLS.LC_ALL, "", Previous_Locale);
   begin
      NLS.Open_Catalog (Catalog, Catalog_Name);
   exception
      when NLS.POSIX_Error =>
         CIO.Put_Line (CIO.Standard_Error,
         "The " & Catalog_Name & " message catalog could not be opened;" &
                " is NLSPATH set? Exiting.");
         return;
   end;

   Put (Hello, Text & "Hello, world! π = " & Ada.Numerics.Pi & New_Line);

   --  Out-of-order arguments example adapted from the Gettext manual
   Put (String_Length, Text &
          "The string " & Raw (Mir) & " has " & Mir'Length & " bytes." &
          New_Line);

   Put (Untranslated, Text &
        "Address of π: " & Pi'Address & New_Line);

   Put (Untranslated, Text &
        Raw ("Здравствуй, мир!") & New_Line);

   Put (Untranslated, Text & Thousands_Grouping &
          (-2145729980.0) & New_Line);

   Put (Untranslated, Text & Positive_Sign & Thousands_Grouping &
          Integer_L (102317123) & New_Line);

   Put (Untranslated, Text &
          "Ada.Numerics.e: " & Precision (60) & Ada.Numerics.e & New_Line);

   NLS.Swap_Locale (NLS.LC_ALL, Previous_Locale);
   NLS.Close_Catalog (Catalog);
end Example1;
