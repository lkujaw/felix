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
--  File:          cstandio.adb (Ada Package Body)
--  Language:      Ada (1995) [1]
--  Author:        Lev Kujawski
--  Description:   C Standard Input/Output (stdio.h) interface for Ada
--
--  References:
--  [1] Information technology - Programming languages - Ada,
--      ISO/IEC 8652:1995(E), 15 Feb. 1995.
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

package body C_Standard_IO is

   function "=" (Left  : in System.Address;
                 Right : in System.Address) return Boolean
     renames System."=";

   Address_Octet_Size : constant := Standard'Address_Size / 8;
   type Address_Octets_T is
     new Octet_Array_T (1 .. Address_Octet_Size);
   for Address_Octets_T'Size use Standard'Address_Size;

   function Address_To_Octets is new
     Ada.Unchecked_Conversion (Source => System.Address,
                               Target => Address_Octets_T);

   function Octets_To_Address is new
     Ada.Unchecked_Conversion (Source => Address_Octets_T,
                               Target => System.Address);

   Long_Float_Octet_Size : constant := Long_Float'Size / 8;
   type Long_Float_Octets_T is
     new Octet_Array_T (1 .. Long_Float_Octet_Size);
   for Long_Float_Octets_T'Size use Long_Float'Size;

   function Long_Float_To_Octets is new
     Ada.Unchecked_Conversion (Source => Long_Float,
                               Target => Long_Float_Octets_T);

   function Octets_To_Long_Float is new
     Ada.Unchecked_Conversion (Source => Long_Float_Octets_T,
                               Target => Long_Float);

   Long_Long_Float_Octet_Size : constant := Long_Long_Float'Size / 8;
   type Long_Long_Float_Octets_T is
     new Octet_Array_T (1 .. Long_Long_Float_Octet_Size);
   for Long_Long_Float_Octets_T'Size use Long_Long_Float'Size;

   function Long_Long_Float_To_Octets is new
     Ada.Unchecked_Conversion (Source => Long_Long_Float,
                               Target => Long_Long_Float_Octets_T);

   function Octets_To_Long_Long_Float is new
     Ada.Unchecked_Conversion (Source => Long_Long_Float_Octets_T,
                               Target => Long_Long_Float);

   Integer_Octet_Size : constant := Integer'Size / 8;
   type Integer_Octets_T is
     new Octet_Array_T (1 .. Integer_Octet_Size);
   for Integer_Octets_T'Size use Integer'Size;

   function Integer_To_Octets is new
     Ada.Unchecked_Conversion (Source => Integer,
                               Target => Integer_Octets_T);

   function Octets_To_Integer is new
     Ada.Unchecked_Conversion (Source => Integer_Octets_T,
                               Target => Integer);

   Long_Integer_Octet_Size : constant := Long_Integer'Size / 8;
   type Long_Integer_Octets_T is
     new Octet_Array_T (1 .. Long_Integer_Octet_Size);
   for Long_Integer_Octets_T'Size use Long_Integer'Size;

   function Long_Integer_To_Octets is new
     Ada.Unchecked_Conversion (Source => Long_Integer,
                               Target => Long_Integer_Octets_T);

   function Octets_To_Long_Integer is new
     Ada.Unchecked_Conversion (Source => Long_Integer_Octets_T,
                               Target => Long_Integer);

   Read_Mode_C       : constant String := "r"  & ASCII.NUL;
   Write_Mode_C      : constant String := "w"  & ASCII.NUL;
   Read_Write_Mode_C : constant String := "r+" & ASCII.NUL;

   Flags_Unchanged : constant Settings_T := (others => Unchanged);

   procedure Append
     (The_String : in     String;
      To_String  : in out String;
      At_Index   : in out Positive)
   is
   begin
      To_String (At_Index .. At_Index + (The_String'Length - 1)) :=
        The_String;
      At_Index := At_Index + The_String'Length;
   end Append;

   function Trim
     (Source : in String) return String
   is
   begin
      for I in Source'Range loop
         if Source (I) /= ' ' then
            declare
               Normalized : constant String (1 .. Source'Last - (I - 1)) :=
                 Source (I .. Source'Last);
            begin
               return Normalized;
            end;
         end if;
      end loop;

      return "";
   end Trim;

   function Integer_Image
     (Value : in Integer) return String
   is
   begin
      return Trim (Integer'Image (Value));
   end Integer_Image;

   function Is_Digit
     (This : in Character) return Boolean
   is
   begin
      return This in '0' .. '9';
   end Is_Digit;

   procedure Read_Integer
     (Source : in     String;
      From   : in     Positive;
      Value  :    out Integer;
      Last   :    out Natural)
   is
      Result : Integer  := 0;
      Index  : Natural  := From;
   begin
      loop
         if not Is_Digit (Source (Index)) then
            Index := Index - 1;
            exit;
         end if;

         Result := Result * 10 +
           Character'Pos (Source (Index)) - Character'Pos ('0');

         exit when Index = Source'Last;
         Index := Index + 1;
      end loop;

      Value := Result;
      Last  := Index;
   end Read_Integer;

   function File_Mode
     (File : in File_T) return File_Mode_T
   is
   begin
      if File.Address = System.Null_Address then
         raise Usage_Error;
      end if;

      return File.Mode;
   end File_Mode;

   function Is_Readable
     (File : in File_T) return Boolean
   is
   begin
      return File.Address /= System.Null_Address
        and then (File.Mode = Read or File.Mode = Read_Write);
   end Is_Readable;

   function Is_Writable
     (File : in File_T) return Boolean
   is
   begin
      return File.Address /= System.Null_Address
        and then (File.Mode = Write or File.Mode = Read_Write);
   end Is_Writable;

   function Standard_Input return File_T
   is
      function get_stdin return System.Address;
      pragma Import (C, get_stdin);

      Result : System.Address := System.Null_Address;
   begin  --  Standard_Input
      Result := get_stdin;
      if Result = System.Null_Address then
         raise Error;
      end if;

      return File_T'(Mode    => Read,
                     Address => Result);
   end Standard_Input;

   function Standard_Output return File_T
   is
      function get_stdout return System.Address;
      pragma Import (C, get_stdout);

      Result : System.Address := System.Null_Address;
   begin  --  Standard_Output
      Result := get_stdout;
      if Result = System.Null_Address then
         raise Error;
      end if;

      return File_T'(Mode    => Write,
                     Address => Result);
   end Standard_Output;

   function Standard_Error return File_T
   is
      function get_stderr return System.Address;
      pragma Import (C, get_stderr);

      Result : System.Address := System.Null_Address;
   begin  --  Standard_Error
      Result := get_stderr;
      if Result = System.Null_Address then
         raise Error;
      end if;

      return File_T'(Mode    => Write,
                     Address => Result);
   end Standard_Error;

   function Null_Output return File_T is separate;

   function C_File_Mode_String
     (File_Mode : in File_Mode_T) return System.Address
   is
   begin
      case File_Mode is
         when Read =>
            return Read_Mode_C (Read_Mode_C'First)'Address;
         when Write =>
            return Write_Mode_C (Write_Mode_C'First)'Address;
         when Read_Write =>
            return Read_Write_Mode_C (Read_Write_Mode_C'First)'Address;
      end case;
   end C_File_Mode_String;

   procedure Open_File
     (File      : in out File_T;
      File_Name : in     String;
      File_Mode : in     File_Mode_T)
   is
      function fopen (filename : in System.Address;
                      mode     : in System.Address) return System.Address;
      pragma Import (C, fopen);

   begin  --  Open_File
      if File.Address /= System.Null_Address
        or else File_Name'Length < 1
      then
         raise Usage_Error;
      end if;

      declare
         C_File_Name : aliased constant String (1 .. File_Name'Length + 1) :=
           File_Name & ASCII.NUL;

         Result : System.Address := System.Null_Address;
      begin
         Result := fopen
           (filename => C_File_Name (C_File_Name'First)'Address,
            mode     => C_File_Mode_String (File_Mode));
         if Result = System.Null_Address then
            raise POSIX_Error;
         end if;

         File := File_T'(Address => Result, Mode => File_Mode);
      end;
   end Open_File;

   procedure Close_File
     (File : in out File_T)
   is
      function fclose (stream : in System.Address) return Integer;
      pragma Import (C, fclose);

      Result : Integer;
   begin  --  Close_File
      if File.Address /= System.Null_Address then
         Result := fclose (stream => File.Address);
         if Result /= 0 then
            raise POSIX_Error;
         end if;

         File.Address := System.Null_Address;
      end if;
   end Close_File;

   procedure Put
     (File : in File_T;
      Item : in Character)
   is
      function fputc (c      : in Integer;
                      stream : in System.Address) return Integer;
      pragma Import (C, fputc);

      Result : Integer;
   begin  --  Put
      if not Is_Writable (File) then
         raise Usage_Error;
      end if;

      Result := fputc (c      => Character'Pos (Item),
                       stream => File.Address);
      if Result /= Character'Pos (Item) then
         raise POSIX_Error;
      end if;
   end Put;

   procedure Put
     (File : in File_T;
      Item : in String)
   is
      function fwrite (ptr    : in System.Address;
                       size   : in Natural;
                       nitems : in Natural;
                       stream : in System.Address) return Natural;
      pragma Import (C, fwrite);

      Result : Natural;
   begin  --  Put
      if not Is_Writable (File) then
         raise Usage_Error;
      end if;

      if Item'Length > 0 then
         Result := fwrite (ptr    => Item (Item'First)'Address,
                           size   => 1,
                           nitems => Item'Length,
                           stream => File.Address);
         if Result /= Item'Length then
            raise POSIX_Error;
         end if;
      end if;
   end Put;

   procedure Put_Line
     (File : in File_T;
      Item : in String)
   is
   begin
      Put (File, Item);
      Put (File, ASCII.LF);
   end Put_Line;

   function Text return Text_T
   is
   begin
      return Text_T'(Index_Last    => 0,
                     Format_Last   => 0,
                     Argument_Last => 0,
                     Attribute     =>
                       Attributes_T'(Flag      => (others => False),
                                     Precision => 0),
                     Types         => (others => Argument_T'First),
                     Indices       => (others => Positive'First),
                     Format        => (others => Character'First),
                     Arguments     => (others => Octet_T'First));
   end Text;

   function Precision
     (Number_of_Digits : in Natural) return Modifier_T
   is
      Flags : Settings_T := Flags_Unchanged;
   begin
      Flags (Precision) := On;
      return Modifier_T'(Setting   => Flags,
                         Precision => Number_of_Digits);
   end Precision;

   function Positive_Sign return Modifier_T
   is
      Flags : Settings_T := Flags_Unchanged;
   begin
      Flags (Positive_Sign) := On;
      return Modifier_T'(Setting   => Flags,
                         Precision => 0);
   end Positive_Sign;

   function No_Positive_Sign return Modifier_T
   is
      Flags : Settings_T := Flags_Unchanged;
   begin
      Flags (Positive_Sign) := Off;
      return Modifier_T'(Setting   => Flags,
                         Precision => 0);
   end No_Positive_Sign;

   function Thousands_Grouping return Modifier_T
   is
      Flags : Settings_T := Flags_Unchanged;
   begin
      Flags (Thousands_Grouping) := On;
      return Modifier_T'(Setting   => Flags,
                         Precision => 0);
   end Thousands_Grouping;

   function No_Thousands_Grouping return Modifier_T
   is
      Flags : Settings_T := Flags_Unchanged;
   begin
      Flags (Thousands_Grouping) := Off;
      return Modifier_T'(Setting   => Flags,
                         Precision => 0);
   end No_Thousands_Grouping;

   function "&" (Left  : in Text_T;
                 Right : in Modifier_T) return Text_T
   is
      Combined : Text_T := Left;
   begin
      for J in Right.Setting'Range loop
         case Right.Setting (J) is
            when On =>
               Combined.Attribute.Flag (J) := True;
               if J = Precision then
                  Combined.Attribute.Precision := Right.Precision;
               end if;
            when Off =>
               Combined.Attribute.Flag (J) := False;
               if J = Precision then
                  Combined.Attribute.Precision := 0;
               end if;
            when Unchanged =>
               null;
         end case;
      end loop;

      return Combined;
   end "&";

   function "&" (Left  : in Text_T;
                 Right : in String) return Text_T
   is
      Escape : Natural := 0;
   begin
      for I in Positive range Right'Range loop
         if Right (I) = '%' then
            Escape := Escape + 1;
         end if;
      end loop;

      declare
         subtype Escape_Index_T is Positive range 1 .. Right'Length + Escape;
         Escaped : String (Escape_Index_T);
         Index   : Natural := 0;
      begin
         for I in Positive range Right'Range loop
            Index := Index + 1;

            if Right (I) = '%' then
               Escaped (Index) := Right (I);
               Index           := Index + 1;
               Escaped (Index) := Right (I);
            else
               Escaped (Index) := Right (I);
            end if;
         end loop;

         return Text_T'
           (Index_Last    => Left.Index_Last,
            Format_Last   => Left.Format_Last + Right'Length,
            Argument_Last => Left.Argument_Last,
            Attribute     => Left.Attribute,
            Indices       => Left.Indices,
            Types         => Left.Types,
            Format        => Left.Format & Escaped,
            Arguments     => Left.Arguments);
      end;
   end "&";

   function "&" (Left  : in Text_T;
                 Right : in Character) return Text_T
   is
   begin
      if Right = '%' then
         return Text_T'
           (Index_Last    => Left.Index_Last,
            Format_Last   => Left.Format_Last + 2,
            Argument_Last => Left.Argument_Last,
            Attribute     => Left.Attribute,
            Indices       => Left.Indices,
            Types         => Left.Types,
            Format        => Left.Format & Right & Right,
            Arguments     => Left.Arguments);
      else
         return Text_T'
           (Index_Last    => Left.Index_Last,
            Format_Last   => Left.Format_Last + 1,
            Argument_Last => Left.Argument_Last,
            Attribute     => Left.Attribute,
            Indices       => Left.Indices,
            Types         => Left.Types,
            Format        => Left.Format & Right,
            Arguments     => Left.Arguments);
      end if;
   end "&";

   function "&" (Left  : in Text_T;
                 Right : in Integer) return Text_T
   is
      Octets : constant Integer_Octets_T := Integer_To_Octets (Right);
   begin
      return Left & Element_T'(Argument_Last => Octets'Last,
                               Kind          => Integer_Regular,
                               Argument      => Octet_Array_T (Octets));
   end "&";

   function "&" (Left  : in Text_T;
                 Right : in Long_Long_Float) return Text_T
   is
      Octets : constant Long_Long_Float_Octets_T :=
        Long_Long_Float_To_Octets (Right);
   begin
      return Left & Element_T'(Argument_Last => Octets'Last,
                               Kind          => Float_Long_Long,
                               Argument      => Octet_Array_T (Octets));
   end "&";

   function "&" (Left  : in Text_T;
                 Right : in System.Address) return Text_T
   is
      Octets : constant Address_Octets_T := Address_To_Octets (Right);
   begin
      return Left & Element_T'(Argument_Last => Octets'Last,
                               Kind          => Address,
                               Argument      => Octet_Array_T (Octets));
   end "&";

   function Conversion_To_Format
     (Conversion : in Argument_T;
      Argument   : in Positive;
      Attribute  : in Attributes_T) return String
   is
      function Is_Numeric return Boolean
      is
         Result : Boolean;
      begin
         case Conversion is
            when Address =>
               Result := False;
            when Float_Long =>
               Result := True;
            when Float_Long_Long =>
               Result := True;
            when Integer_Long =>
               Result := True;
            when Integer_Regular =>
               Result := True;
            when Raw_String =>
               Result := False;
         end case;

         return Result;
      end Is_Numeric;

      subtype Format_Index_T is Positive range 1 .. 256;
      Format_String : String (Format_Index_T);
      Format_Index  : Format_Index_T := Format_Index_T'First;
   begin  --  Conversion_To_Format
      Append ('%' & Integer_Image (Argument) & '$',
              Format_String, Format_Index);
      if Is_Numeric then
         if Attribute.Flag (Precision) then
            Append ("." & Integer_Image (Attribute.Precision),
                    Format_String, Format_Index);
         end if;
         if Attribute.Flag (Thousands_Grouping) then
            Append ("'", Format_String, Format_Index);
         end if;
         if Attribute.Flag (Positive_Sign) then
            Append ("+", Format_String, Format_Index);
         end if;
      end if;

      case Conversion is
         when Address =>
            Append ("p", Format_String, Format_Index);
         when Float_Long =>
            Append ("f", Format_String, Format_Index);
         when Float_Long_Long =>
            Append ("Lf", Format_String, Format_Index);
         when Integer_Long =>
            Append ("ld", Format_String, Format_Index);
         when Integer_Regular =>
            Append ("d", Format_String, Format_Index);
         when Raw_String =>
            Append ("s", Format_String, Format_Index);
      end case;

      return Format_String (Format_String'First .. Format_Index - 1);
   end Conversion_To_Format;

   function "&" (Left  : in Text_T;
                 Right : in Element_T) return Text_T
   is
      Format_String : constant String := Conversion_To_Format
        (Right.Kind, Left.Index_Last + 1, Left.Attribute);
   begin
      return Text_T'
        (Index_Last    => Left.Index_Last + 1,
         Format_Last   => Left.Format_Last + Format_String'Length,
         Argument_Last => Left.Argument_Last + Right.Argument'Length,
         Attribute     => Left.Attribute,
         Indices       => Left.Indices & (1 => Left.Argument_Last + 1),
         Types         => Left.Types & (1 => Right.Kind),
         Format        => Left.Format & Format_String,
         Arguments     => Left.Arguments & Right.Argument);
   end "&";

   function Float_L (Value : in Long_Float) return Element_T
   is
      Octets : constant Long_Float_Octets_T := Long_Float_To_Octets (Value);
   begin
      return Element_T'(Argument_Last => Octets'Last,
                        Kind          => Float_Long,
                        Argument      => Octet_Array_T (Octets));
   end Float_L;

   function Integer_L (Value : in Long_Integer) return Element_T
   is
      Octets : constant Long_Integer_Octets_T :=
        Long_Integer_To_Octets (Value);
   begin
      return Element_T'(Argument_Last => Octets'Last,
                        Kind          => Integer_Long,
                        Argument      => Octet_Array_T (Octets));
   end Integer_L;

   function Raw (From_String : in String) return Element_T
   is
      subtype Octets_Index_T is Positive range 1 .. From_String'Length;
      Octets : Octet_Array_T (Octets_Index_T);
      for Octets'Address use From_String (From_String'First)'Address;
   begin
      return Element_T'(Argument_Last => Octets'Length,
                        Kind          => Raw_String,
                        Argument      => Octets);
   end Raw;

   function New_Line return String
   is
   begin
      return (1 => ASCII.LF);
   end New_Line;

   function Format
     (Of_Text : in Text_T) return String
   is
   begin
      return Of_Text.Format;
   end Format;

   function String_Of
     (The_Text : in Text_T) return String
   is
   begin
      return String_Of (The_Text    => The_Text,
                        With_Format => The_Text.Format);
   end String_Of;

   function String_Of
     (The_Text    : in Text_T;
      With_Format : in String) return String
   is
      function fprintf_d (stream : in System.Address;
                          fmt    : in System.Address;
                          arg    : in Long_Float) return Integer;
      pragma Import (C, fprintf_d);

      function sprintf_d (s   : in System.Address;
                          fmt : in System.Address;
                          arg : in Long_Float) return Integer;
      pragma Import (C, sprintf_d);

      function fprintf_i (stream : in System.Address;
                          fmt    : in System.Address;
                          arg    : in Integer) return Integer;
      pragma Import (C, fprintf_i);

      function sprintf_i (s   : in System.Address;
                          fmt : in System.Address;
                          arg : in Integer) return Integer;
      pragma Import (C, sprintf_i);

      function fprintf_ld (stream : in System.Address;
                           fmt    : in System.Address;
                           arg    : in Long_Long_Float) return Integer;
      pragma Import (C, fprintf_ld);

      function sprintf_ld (s   : in System.Address;
                           fmt : in System.Address;
                           arg : in Long_Long_Float) return Integer;
      pragma Import (C, sprintf_ld);

      function fprintf_li (stream : in System.Address;
                           fmt    : in System.Address;
                           arg    : in Long_Integer) return Integer;
      pragma Import (C, fprintf_li);

      function sprintf_li (s   : in System.Address;
                           fmt : in System.Address;
                           arg : in Long_Integer) return Integer;
      pragma Import (C, sprintf_li);

      function fprintf_p (stream : in System.Address;
                          fmt    : in System.Address;
                          arg    : in System.Address) return Integer;
      pragma Import (C, fprintf_p);

      function sprintf_p (s   : in System.Address;
                          fmt : in System.Address;
                          arg : in System.Address) return Integer;
      pragma Import (C, sprintf_p);

      Null_File      : File_T;
      Conv_First     : Positive;
      Argument_Type  : Argument_T;
      Argument       : Positive    := The_Text.Indices'First;
      Positional     : Boolean     := False;
      Format_Index   : Positive    := With_Format'First;
      Output         : Ada.Strings.Unbounded.Unbounded_String;

      procedure C_Print
      is
         C_Format_String : String :=
           With_Format (Conv_First .. Format_Index - 1) & ASCII.NUL;
         Result : Integer := 0;
      begin
         --  We need to remove any remnant of the argument number.
         C_Format_String (C_Format_String'First) := '%';

         case Argument_Type is
            when Address =>
               Result := fprintf_p
                 (Null_File.Address,
                  C_Format_String (C_Format_String'First)'Address,
                  Octets_To_Address (Address_Octets_T
          (The_Text.Arguments (The_Text.Indices (Argument) ..
           The_Text.Indices (Argument) + Address_Octet_Size - 1))));
            when Float_Long =>
               Result := fprintf_d
                 (Null_File.Address,
                  C_Format_String (C_Format_String'First)'Address,
                  Octets_To_Long_Float (Long_Float_Octets_T
          (The_Text.Arguments (The_Text.Indices (Argument) ..
           The_Text.Indices (Argument) + Long_Float_Octet_Size - 1))));
            when Float_Long_Long =>
               Result := fprintf_ld
                 (Null_File.Address,
                  C_Format_String (C_Format_String'First)'Address,
                  Octets_To_Long_Long_Float (Long_Long_Float_Octets_T
          (The_Text.Arguments (The_Text.Indices (Argument) ..
           The_Text.Indices (Argument) + Long_Long_Float_Octet_Size - 1))));
            when Integer_Long =>
               Result := fprintf_li
                 (Null_File.Address,
                  C_Format_String (C_Format_String'First)'Address,
                  Octets_To_Long_Integer (Long_Integer_Octets_T
          (The_Text.Arguments (The_Text.Indices (Argument) ..
           The_Text.Indices (Argument) + Long_Integer_Octet_Size - 1))));
            when Integer_Regular =>
               Result := fprintf_i
                 (Null_File.Address,
                  C_Format_String (C_Format_String'First)'Address,
                  Octets_To_Integer (Integer_Octets_T
          (The_Text.Arguments (The_Text.Indices (Argument) ..
           The_Text.Indices (Argument) + Integer_Octet_Size - 1))));
            when Raw_String =>
               raise Error;
         end case;

         if Result < 0 then
            raise POSIX_Error;
         end if;

         declare
            --  +1 for nul terminator
            subtype Printf_Buffer_Index_T is Positive range 1 .. Result + 1;
            Printf_Buffer : String (Printf_Buffer_Index_T);
         begin
            --  Place a sentinel
            Printf_Buffer (Printf_Buffer'Last) := ' ';

            case Argument_Type is
               when Address =>
                  Result := sprintf_p
                    (Printf_Buffer (Printf_Buffer'First)'Address,
                     C_Format_String (C_Format_String'First)'Address,
                     Octets_To_Address (Address_Octets_T
   (The_Text.Arguments (The_Text.Indices (Argument) ..
    The_Text.Indices (Argument) + Address_Octet_Size - 1))));
               when Float_Long =>
                  Result := sprintf_d
                    (Printf_Buffer (Printf_Buffer'First)'Address,
                     C_Format_String (C_Format_String'First)'Address,
                     Octets_To_Long_Float (Long_Float_Octets_T
   (The_Text.Arguments (The_Text.Indices (Argument) ..
    The_Text.Indices (Argument) + Long_Float_Octet_Size - 1))));
               when Float_Long_Long =>
                  Result := sprintf_ld
                    (Printf_Buffer (Printf_Buffer'First)'Address,
                     C_Format_String (C_Format_String'First)'Address,
                     Octets_To_Long_Long_Float (Long_Long_Float_Octets_T
   (The_Text.Arguments (The_Text.Indices (Argument) ..
    The_Text.Indices (Argument) + Long_Long_Float_Octet_Size - 1))));
               when Integer_Long =>
                  Result := sprintf_li
                    (Printf_Buffer (Printf_Buffer'First)'Address,
                     C_Format_String (C_Format_String'First)'Address,
                     Octets_To_Long_Integer (Long_Integer_Octets_T
   (The_Text.Arguments (The_Text.Indices (Argument) ..
    The_Text.Indices (Argument) + Long_Integer_Octet_Size - 1))));
               when Integer_Regular =>
                  Result := sprintf_i
                    (Printf_Buffer (Printf_Buffer'First)'Address,
                     C_Format_String (C_Format_String'First)'Address,
                     Octets_To_Integer (Integer_Octets_T
   (The_Text.Arguments (The_Text.Indices (Argument) ..
    The_Text.Indices (Argument) + Integer_Octet_Size - 1))));
               when Raw_String =>
                  raise Error;
            end case;

            if Result < 0 then
               raise POSIX_Error;
            end if;

            if Printf_Buffer (Printf_Buffer'Last) /= ASCII.NUL then
               raise Error;
            end if;

            Ada.Strings.Unbounded.Append
              (Output, Printf_Buffer (1 .. Result));
         end;
      end C_Print;

      function Argument_Length return Natural
      is
         Result : Natural;
      begin
         if Argument = The_Text.Index_Last then
            Result := The_Text.Argument_Last -
              (The_Text.Indices (Argument) - 1);
         else
            Result := The_Text.Indices (Argument + 1) -
              The_Text.Indices (Argument);
         end if;

         return Result;
      end Argument_Length;

      procedure Print
      is
      begin
         if Argument_Type = Raw_String then
            --  Handle strings directly
            declare
               String_View : String (1 .. Argument_Length);
               for String_View'Address use The_Text.Arguments
                 (The_Text.Indices (Argument))'Address;
            begin
               Ada.Strings.Unbounded.Append (Output, String_View);
            end;
         else
            C_Print;
         end if;
      end Print;

      --  Page 213 of C435 (the Unix 95 standard)
      procedure Parse_Conversion_Specification
      is
         Argument_Number : Integer;
         Position        : Natural;

         function Is_Flag (This : in Character) return Boolean
         is
            Result : Boolean;
         begin
            case This is
               when ''' =>
                  Result := True;
               when '+' =>
                  Result := True;
               when others =>
                  Result := False;
            end case;

            return Result;
         end Is_Flag;

      begin  --  Parse_Conversion_Specification
         Read_Integer (With_Format, Format_Index, Argument_Number, Position);

         if Position >= Format_Index
           and then Argument_Number > 0
           and then With_Format (Format_Index + 1) = '$'
         then
            Conv_First   := Format_Index + 1;
            Positional   := True;
            Argument     := Argument_Number;
            Format_Index := Format_Index + 2;
         end if;

         --  Skip precision
         if With_Format (Format_Index) = '.' then
            Format_Index := Format_Index + 1;

            while Is_Digit (With_Format (Format_Index)) loop
               Format_Index := Format_Index + 1;
            end loop;
         end if;

         --  Skip flags
         while Is_Flag (With_Format (Format_Index)) loop
            Format_Index := Format_Index + 1;
         end loop;

         case With_Format (Format_Index) is
            when 'L' =>
               Argument_Type := Float_Long_Long;
               Format_Index  := Format_Index + 1;

               if With_Format (Format_Index) /= 'f' then
                  raise Usage_Error;
               end if;
            when 'c' =>
               Argument_Type := Integer_Regular;
            when 'd' | 'i' =>
               Argument_Type := Integer_Regular;
            when 'l' =>
               Argument_Type := Integer_Long;
               Format_Index  := Format_Index + 1;

               if With_Format (Format_Index) /= 'd' then
                  raise Usage_Error;
               end if;
            when 'o' =>
               raise Usage_Error;
            when 'u' =>
               raise Usage_Error;
            when 'x' | 'X' =>
               raise Usage_Error;
            when 'f' =>
               Argument_Type := Float_Long;
            when 'e' | 'E' =>
               Argument_Type := Float_Long;
            when 'g' | 'G' =>
               Argument_Type := Float_Long;
            when 's' =>
               Argument_Type := Raw_String;
            when 'p' =>
               Argument_Type := Address;
            when others =>
               raise Usage_Error;
         end case;

         Format_Index := Format_Index + 1;
      end Parse_Conversion_Specification;

      procedure Verify_Type
      is
      begin
         if Argument_Type /= The_Text.Types (Argument) then
            raise Usage_Error;
         end if;
      end Verify_Type;

   begin  --  String_Of
      Null_File := Null_Output;

      while Format_Index <= With_Format'Last loop
         if With_Format (Format_Index) = '%' then
            Conv_First   := Format_Index;
            Format_Index := Format_Index + 1;
            if With_Format (Format_Index) = '%' then
               --  Escaped '%'.
               Ada.Strings.Unbounded.Append
                 (Output, With_Format (Format_Index));
               Format_Index := Format_Index + 1;
            else
               Parse_Conversion_Specification;
               Verify_Type;
               Print;

               if not Positional then
                  Argument := Argument + 1;
               end if;
            end if;
         else
            Ada.Strings.Unbounded.Append
              (Output, With_Format (Format_Index));
            Format_Index := Format_Index + 1;
         end if;
      end loop;

      Close_File (Null_File);
      return Ada.Strings.Unbounded.To_String (Output);
   exception
      when others =>
         Close_File (Null_File);
         raise;
   end String_Of;

end C_Standard_IO;
