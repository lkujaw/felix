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
--  File:          stantext.ads (Ada Package Specification)
--  Language:      Ada (1995) [1]
--  Author:        Lev Kujawski
--  Description:   Type-safe printf() emulation for string localization
--
--  References:
--  [1] Information technology - Programming languages - Ada,
--      ISO/IEC 8652:1995(E), 15 Feb. 1995.
------------------------------------------------------------------------------
--
--  Standard_Text utilizes the C library's printf family of functions to
--  guarantee that output will match that of C programs using the standard
--  library.

with Native_Language_System;
with System;

package Standard_Text is

   Usage_Error : exception;
   Error       : exception;

   type Text_T (<>) is limited private;
   type Element_T (<>) is limited private;
   type Modifier_T (<>) is limited private;

   --  The empty text
   function Text return Text_T;

   --  Modifiers
   function Precision (Number_of_Digits : in Natural) return Modifier_T;

   function Positive_Sign return Modifier_T;
   function No_Positive_Sign return Modifier_T;

   function Thousands_Grouping return Modifier_T;
   function No_Thousands_Grouping return Modifier_T;

   function "&" (Left  : in Text_T;
                 Right : in Modifier_T) return Text_T;

   function "&" (Left  : in Text_T;
                 Right : in String) return Text_T;

   function "&" (Left  : in Text_T;
                 Right : in Character) return Text_T;

   --  The parameter type Integer emulates the C default of promoting
   --  to int.
   function "&" (Left  : in Text_T;
                 Right : in Integer) return Text_T;

   --  The parameter type of Right was chosen to emulate the GNAT
   --  runtime, which implements Float_IO by converting all generic
   --  Num types to the type Long_Long_Float.
   function "&" (Left  : in Text_T;
                 Right : in Long_Long_Float) return Text_T;

   --  Print a pointer (%p)
   function "&" (Left  : in Text_T;
                 Right : in System.Address) return Text_T;

   --  Non-default types

   function "&" (Left  : in Text_T;
                 Right : in Element_T) return Text_T;

   --  Print a string without translation (%s)
   function Raw (From_String : in String) return Element_T;

   function Float_L (Value : in Long_Float) return Element_T;
   function Integer_L (Value : in Long_Integer) return Element_T;

   --  Equivalent of '\n'
   function New_Line return String;

   --  Format_Of is useful for creating translatable messages and debugging
   --  purposes.
   function Format_Of (The_Text : in Text_T) return String;
   function String_Of (The_Text : in Text_T) return String;

   function Message (From_Catalog    : in Native_Language_System.Catalog_T;
                     Set_Number      : in Positive;
                     Message_Number  : in Positive;
                     Default_Message : in Text_T) return String;

private  --  Standard_Text ---------------------------------------------------

   type Octet_T is range 0 .. 255;
   for Octet_T'Size use 8;

   type Index_Array_T is array (Positive range <>) of Positive;
   type Octet_Array_T is array (Positive range <>) of Octet_T;

   type Flag_T is (Positive_Sign,
                   Precision,
                   Thousands_Grouping);

   type Flags_T is array (Flag_T) of Boolean;

   type Argument_T is (Address,
                       Float_Long,
                       Float_Long_Long,
                       Integer_Long,
                       Integer_Regular,
                       Raw_String);

   type Type_Array_T is array (Positive range <>) of Argument_T;

   type Attributes_T is
      record
         Flag      : Flags_T;
         Precision : Natural;
      end record;

   type Text_T (Index_Last    : Natural;
                Format_Last   : Natural;
                Argument_Last : Natural) is
      record
         Attribute : Attributes_T;
         Indices   : Index_Array_T (1 .. Index_Last);
         Types     : Type_Array_T (1 .. Index_Last);
         Format    : String (1 .. Format_Last);
         --  A binary, rather than string, encoding of the arguments
         --  is utilized because different locales may have different
         --  formatting conventions for, e.g., numerals.
         Arguments : Octet_Array_T (1 .. Argument_Last);
      end record;

   type Element_T (Argument_Last : Natural) is
      record
         Kind     : Argument_T;
         Argument : Octet_Array_T (1 .. Argument_Last);
      end record;

   type Setting_T is (On, Off, Unchanged);
   type Settings_T is array (Flag_T) of Setting_T;

   type Modifier_T is
      record
         Setting   : Settings_T;
         Precision : Natural;
      end record;

end Standard_Text;
