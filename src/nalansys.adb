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
--  File:          nalansys.adb (Ada Package Body)
--  Language:      Ada (1987) [1]
--  Author:        Lev Kujawski
--  Description:   X/Open Native Language System [2] interface for Ada
--
--  References:
--  [1] Programming languages - Ada, ISO/IEC 8652:1987, 15 Jun. 1987.
--  [2] X/Open, Internationalisation Guide Version 2, G304, Jul. 1993.
------------------------------------------------------------------------------

package body Native_Language_System is

   type Octet_T is range 0 .. 255;
   for Octet_T'Size use 8;

   lc_all_c      : Integer;
   pragma Interface (C, lc_all_c);
   lc_collate_c  : Integer;
   pragma Interface (C, lc_collate_c);
   lc_ctype_c    : Integer;
   pragma Interface (C, lc_ctype_c);
   lc_messages_c : Integer;
   pragma Interface (C, lc_messages_c);
   lc_monetary_c : Integer;
   pragma Interface (C, lc_monetary_c);
   lc_numeric_c  : Integer;
   pragma Interface (C, lc_numeric_c);
   lc_time_c     : Integer;
   pragma Interface (C, lc_time_c);

   nl_cat_locale_c : Integer;
   pragma Interface (C, nl_cat_locale_c);

   Null_C_String_C : constant String := (1 => ASCII.NUL);

   function "=" (Left  : in System.Address;
                 Right : in System.Address) return Boolean
     renames System."=";

   function setlocale (category : in Integer;
                       locale   : in System.Address) return System.Address;
   pragma Interface (C, setlocale);

   function Category_Value
     (Category : in Category_T) return Integer
   is
      Result : Integer;
   begin
      case Category is
         when LC_ALL =>
            Result := lc_all_c;
         when LC_COLLATE =>
            Result := lc_collate_c;
         when LC_CTYPE =>
            Result := lc_ctype_c;
         when LC_MESSAGES =>
            Result := lc_messages_c;
         when LC_MONETARY =>
            Result := lc_monetary_c;
         when LC_NUMERIC =>
            Result := lc_numeric_c;
         when LC_TIME =>
            Result := lc_time_c;
      end case;

      return Result;
   end Category_Value;

   function C_String_Length
     (With_First_Element_Address : in System.Address) return Natural
   is
      type Memory_T is array (Natural) of Octet_T;

      Memory : Memory_T;
      for Memory'Address use With_First_Element_Address;

      Index  : Natural := 0;
   begin
      while Memory (Index) /= 0 loop
         if Index = Natural'Last then
            raise Error;
         end if;

         Index := Index + 1;
      end loop;

      return Index;
   end C_String_Length;

   procedure Verify_Catalog
     (Catalog : in Catalog_T)
   is
   begin
      if Catalog.Address = System.Null_Address then
         raise Usage_Error;
      end if;
   end Verify_Catalog;

   procedure Verify_Locale
     (Locale : in Locale_T)
   is
   begin
      if Locale.Address = System.Null_Address then
         raise Usage_Error;
      end if;
   end Verify_Locale;

   function Locale_String (Locale : in Locale_T) return String
   is
   begin
      Verify_Locale (Locale);

      declare
         Locale_String_Length : constant Natural :=
           C_String_Length (Locale.Address);
         Bounded : String (1 .. Locale_String_Length);
         for Bounded'Address use Locale.Address;
      begin
         return Bounded;
      end;
   end Locale_String;

   procedure Set_Locale
     (Category   : in     Category_T;
      New_Locale : in     String;
      Old_Locale : in out Locale_T)
   is
      C_Locale : constant String := New_Locale & ASCII.NUL;
      Result   : System.Address  := System.Null_Address;
   begin
      if Old_Locale.Address /= System.Null_Address then
         raise Usage_Error;
      end if;

      Result := setlocale (category => Category_Value (Category),
                           locale   => C_Locale (C_Locale'First)'Address);
      if Result = System.Null_Address then
         raise Error;
      end if;

      Old_Locale.Address := Result;
   end Set_Locale;

   procedure Swap_Locale (Category : in     Category_T;
                          Locale   : in out Locale_T)
   is
      Result : System.Address := System.Null_Address;
   begin
      Verify_Locale (Locale);

      Result := setlocale (category => Category_Value (Category),
                           locale   => Locale.Address);
      if Result = System.Null_Address then
         raise Error;
      end if;

      Locale.Address := Result;
   end Swap_Locale;

   procedure Open_Catalog
     (Catalog   : in out Catalog_T;
      With_Name : in     String)
   is
      function catopen (name  : in System.Address;
                        oflag : in Integer) return System.Address;
      pragma Interface (C, catopen);

      C_Name : constant String := With_Name & ASCII.NUL;
      Result : System.Address;
   begin  --  Open_Catalog
      if Catalog.Address /= System.Null_Address then
         raise Usage_Error;
      end if;

      Result := catopen (name  => C_Name (C_Name'First)'Address,
                         oflag => nl_cat_locale_c);
      if Result = System'To_Address (-1) then
         raise POSIX_Error;
      end if;

      Catalog.Address := Result;
   end Open_Catalog;

   procedure Close_Catalog (Catalog : in out Catalog_T)
   is
      function catclose (catd : in System.Address) return Integer;
      pragma Interface (C, catclose);

      Result : Integer;
   begin  --  Close_Catalog
      if Catalog.Address /= System.Null_Address then
         Result := catclose (catd => Catalog.Address);
         if Result /= 0 then
            raise POSIX_Error;
         end if;

         Catalog.Address := System.Null_Address;
      end if;
   end Close_Catalog;

   function Message
     (From_Catalog   : in Catalog_T;
      Set_Number     : in Positive;
      Message_Number : in Positive) return String
   is
      function catgets (catd   : in System.Address;
                        set_id : in Integer;
                        msg_id : in Integer;
                        s      : in System.Address) return System.Address;
      pragma Interface (C, catgets);

      Format_Address  : System.Address := System.Null_Address;
      Format_Length   : Natural        := 0;
   begin  --  Message
      Verify_Catalog (From_Catalog);

      Format_Address := catgets (catd   => From_Catalog.Address,
                                 set_id => Set_Number,
                                 msg_id => Message_Number,
                                 s      => Null_C_String_C'Address);
      if Format_Address = System.Null_Address then
         raise POSIX_Error;
      end if;

      if Format_Address = Null_C_String_C'Address then
         return "";
      else
         Format_Length := C_String_Length (Format_Address);

         declare
            Localized_Format : String (1 .. Format_Length);
            for Localized_Format'Address use Format_Address;
         begin
            return Localized_Format;
         end;
      end if;
   end Message;

end Native_Language_System;
