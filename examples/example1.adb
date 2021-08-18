------------------------------------------------------------------------------
--  Copyright (c) 2021, Lev Kujawski.
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software")
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.
--
--  SPDX-License-Identifier: MIT-0
--
--  File:          example1.adb (Ada Subprogram Body)
--  Language:      Ada (1995) [1]
--  Author:        Lev Kujawski
--  Description:   Executable example of Felix library usage
--
--  References:
--  [1] Information technology - Programming languages - Ada,
--      ISO/IEC 8652:1995(E), 15 Feb. 1995.
------------------------------------------------------------------------------
--
--  To run this program within the examples directory on a UNIX system,
--    execute: NLSPATH=./%N ./example1

with Ada.Numerics;
with C_Standard_IO;
with Native_Language_System;

with Standard_Text;
use Standard_Text;

procedure Example1 is

   package CIO renames C_Standard_IO;
   package NLS renames Native_Language_System;

   type Messages_T is
     (Hello, String_Length, Untranslated);

   Pi  : aliased constant Long_Float := 3.141593;
   Mir : constant String := "мир";

   Catalog_Name    : constant String := "hello.cat";
   Catalog         : NLS.Catalog_T;
   Previous_Locale : NLS.Locale_T;

   procedure Put (The_Message : in Messages_T;
                  The_Text    : in Text_T)
   is
   begin
      CIO.Put (Item => Message (Catalog, 1, Messages_T'Pos (The_Message) + 1,
                                The_Text));
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

   --  Out-of-order arguments example adapted from the Gettext manual.
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
