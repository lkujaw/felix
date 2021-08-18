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
--  Language:      Ada (1987) [1]
--  Author:        Lev Kujawski
--  Description:   C Standard Input/Output (stdio.h) interface for Ada
--
--  References:
--  [1] Programming languages - Ada, ISO/IEC 8652:1987, 15 Jun. 1987.
------------------------------------------------------------------------------

package body C_Standard_IO is

   function "=" (Left  : in System.Address;
                 Right : in System.Address) return Boolean
     renames System."=";

   Read_Mode_C              : constant String := "r"   & ASCII.NUL;
   Write_Mode_C             : constant String := "w"   & ASCII.NUL;
   Read_Write_Mode_C        : constant String := "r+"  & ASCII.NUL;
   Address_Format_C         : constant String := "%p"  & ASCII.NUL;
   Integer_Format_C         : constant String := "%d"  & ASCII.NUL;
   Long_Integer_Format_C    : constant String := "%ld" & ASCII.NUL;
   Long_Float_Format_C      : constant String := "%f"  & ASCII.NUL;
   Long_Long_Float_Format_C : constant String := "%Lf" & ASCII.NUL;

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
      pragma Interface (C, get_stdin);

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
      pragma Interface (C, get_stdout);

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
      pragma Interface (C, get_stderr);

      Result : System.Address := System.Null_Address;
   begin  --  Standard_Error
      Result := get_stderr;
      if Result = System.Null_Address then
         raise Error;
      end if;

      return File_T'(Mode    => Write,
                     Address => Result);
   end Standard_Error;

   function C_File_Mode_String
     (Of_Mode : in File_Mode_T) return System.Address
   is
   begin
      case Of_Mode is
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
      With_Name : in     String;
      With_Mode : in     File_Mode_T)
   is
      function fopen (filename : in System.Address;
                      mode     : in System.Address) return System.Address;
      pragma Interface (C, fopen);

   begin  --  Open_File
      if File.Address /= System.Null_Address
        or else With_Name'Length < 1
      then
         raise Usage_Error;
      end if;

      declare
         C_File_Name : constant String (1 .. With_Name'Length + 1) :=
           With_Name & ASCII.NUL;

         Result : System.Address := System.Null_Address;
      begin
         Result := fopen
           (filename => C_File_Name (C_File_Name'First)'Address,
            mode     => C_File_Mode_String (Of_Mode => With_Mode));
         if Result = System.Null_Address then
            raise POSIX_Error;
         end if;

         File := File_T'(Address => Result,
                         Mode    => With_Mode);
      end;
   end Open_File;

   procedure Open_Null (File : in out File_T) is separate;

   procedure Close_File
     (File : in out File_T)
   is
      function fclose (stream : in System.Address) return Integer;
      pragma Interface (C, fclose);

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
      pragma Interface (C, fputc);

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
     (Item : in Character)
   is
   begin
      Put (File => Standard_Output, Item => Item);
   end Put;

   procedure Put
     (File : in File_T;
      Item : in String)
   is
      function fwrite (ptr    : in System.Address;
                       size   : in Natural;
                       nitems : in Natural;
                       stream : in System.Address) return Natural;
      pragma Interface (C, fwrite);

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

   procedure Put
     (Item : in String)
   is
   begin
      Put (File => Standard_Output, Item => Item);
   end Put;

   procedure Put_Line
     (File : in File_T;
      Item : in String)
   is
   begin
      Put (File, Item);
      Put (File, ASCII.LF);
   end Put_Line;

   procedure Put_Line
     (Item : in String)
   is
   begin
      Put_Line (File => Standard_Output, Item => Item);
   end Put_Line;

   --  TODO: Validate format strings within _Format functions.

   --  ADDRESS INPUT/OUTPUT --------------------------------------------------

   procedure Put_Address
     (File    : in     File_T := Standard_Output;
      Value   : in     System.Address;
      Format  : in     String;
      Written :    out Natural)
   is
      function fprintf_p (stream : in System.Address;
                          fmt    : in System.Address;
                          arg    : in System.Address) return Integer;
      pragma Interface (C, fprintf_p);

      Result : Integer;
   begin  --  Put_Address
      if not Is_Writable (File) then
         raise Usage_Error;
      end if;

      Result := fprintf_p (stream => File.Address,
                           fmt    => Format (Format'First)'Address,
                           arg    => Value);

      if Result < 0 then
         raise POSIX_Error;
      end if;

      Written := Natural'(Result);
   end Put_Address;

   function Address_Format (Format : in String) return String
   is
   begin
      if Format = "" then
         return Address_Format_C;
      else
         return Format;
      end if;
   end Address_Format;

   function Address_Image
     (Value  : in System.Address;
      Format : in String := "") return String
   is
      function sprintf_p (s   : in System.Address;
                          fmt : in System.Address;
                          arg : in System.Address) return Integer;
      pragma Interface (C, sprintf_p);

      Actual_Format : constant String := Address_Format (Format);
      Null_File     : File_T;
      Buffer_Length : Natural;
      Result        : Integer;
   begin  --  Address_Image
      Open_Null (Null_File);
      begin
         Put_Address (File    => Null_File,
                      Value   => Value,
                      Format  => Actual_Format,
                      Written => Buffer_Length);
      exception
         when others =>
            Close_File (Null_File);
            raise;
      end;
      Close_File (Null_File);

      declare
         --  Add one to Buffer_Length for ASCII.NUL terminator.
         subtype Print_Buffer_Index_T is Positive
           range 1 .. Buffer_Length + 1;
         Print_Buffer : String (Print_Buffer_Index_T);
      begin
         --  Place a sentinel
         Print_Buffer (Print_Buffer'Last) := ' ';

         Result := sprintf_p (Print_Buffer (Print_Buffer'First)'Address,
                              Actual_Format (Actual_Format'First)'Address,
                              Value);

         if Result < 0 then
            raise POSIX_Error;
         end if;

         if Result /= Buffer_Length
           or else Print_Buffer (Print_Buffer'Last) /= ASCII.NUL
         then
            raise Error;
         end if;

         return Print_Buffer (1 .. Buffer_Length);
      end;
   end Address_Image;

   --  INTEGER INPUT/OUTPUT --------------------------------------------------

   procedure Put_Integer
     (File    : in     File_T := Standard_Output;
      Value   : in     Integer;
      Format  : in     String;
      Written :    out Natural)
   is
      function fprintf_i (stream : in System.Address;
                          fmt    : in System.Address;
                          arg    : in Integer) return Integer;
      pragma Interface (C, fprintf_i);

      Result : Integer;
   begin  --  Put_Integer
      if not Is_Writable (File) then
         raise Usage_Error;
      end if;

      Result := fprintf_i (stream => File.Address,
                           fmt    => Format (Format'First)'Address,
                           arg    => Value);

      if Result < 0 then
         raise POSIX_Error;
      end if;

      Written := Natural'(Result);
   end Put_Integer;

   function Integer_Format (Format : in String) return String
   is
   begin
      if Format = "" then
         return Integer_Format_C;
      else
         return Format;
      end if;
   end Integer_Format;

   function Integer_Image
     (Value  : in Integer;
      Format : in String := "") return String
   is
      function sprintf_i (s   : in System.Address;
                          fmt : in System.Address;
                          arg : in Integer) return Integer;
      pragma Interface (C, sprintf_i);

      Actual_Format : constant String := Integer_Format (Format);
      Null_File     : File_T;
      Buffer_Length : Natural;
      Result        : Integer;
   begin  --  Integer_Image
      Open_Null (Null_File);
      begin
         Put_Integer (File    => Null_File,
                      Value   => Value,
                      Format  => Actual_Format,
                      Written => Buffer_Length);
      exception
         when others =>
            Close_File (Null_File);
            raise;
      end;
      Close_File (Null_File);

      declare
         --  Add one to Buffer_Length for ASCII.NUL terminator.
         subtype Print_Buffer_Index_T is Positive
           range 1 .. Buffer_Length + 1;
         Print_Buffer : String (Print_Buffer_Index_T);
      begin
         --  Place a sentinel
         Print_Buffer (Print_Buffer'Last) := ' ';

         Result := sprintf_i (Print_Buffer (Print_Buffer'First)'Address,
                              Actual_Format (Actual_Format'First)'Address,
                              Value);

         if Result < 0 then
            raise POSIX_Error;
         end if;

         if Result /= Buffer_Length
           or else Print_Buffer (Print_Buffer'Last) /= ASCII.NUL
         then
            raise Error;
         end if;

         return Print_Buffer (1 .. Buffer_Length);
      end;
   end Integer_Image;

   --  LONG INTEGER INPUT/OUTPUT ---------------------------------------------

   procedure Put_Long_Integer
     (File    : in     File_T := Standard_Output;
      Value   : in     Long_Integer;
      Format  : in     String;
      Written :    out Natural)
   is
      function fprintf_li (stream : in System.Address;
                           fmt    : in System.Address;
                           arg    : in Long_Integer) return Integer;
      pragma Interface (C, fprintf_li);

      Result : Integer;
   begin  --  Put_Long_Integer
      if not Is_Writable (File) then
         raise Usage_Error;
      end if;

      Result := fprintf_li (stream => File.Address,
                            fmt    => Format (Format'First)'Address,
                            arg    => Value);

      if Result < 0 then
         raise POSIX_Error;
      end if;

      Written := Natural'(Result);
   end Put_Long_Integer;

   function Long_Integer_Format (Format : in String) return String
   is
   begin
      if Format = "" then
         return Long_Integer_Format_C;
      else
         return Format;
      end if;
   end Long_Integer_Format;

   function Long_Integer_Image
     (Value  : in Long_Integer;
      Format : in String := "") return String
   is
      function sprintf_li (s   : in System.Address;
                           fmt : in System.Address;
                           arg : in Long_Integer) return Integer;
      pragma Interface (C, sprintf_li);

      Actual_Format : constant String := Long_Integer_Format (Format);
      Null_File     : File_T;
      Buffer_Length : Natural;
      Result        : Integer;
   begin  --  Long_Integer_Image
      Open_Null (Null_File);
      begin
         Put_Long_Integer (File    => Null_File,
                           Value   => Value,
                           Format  => Actual_Format,
                           Written => Buffer_Length);
      exception
         when others =>
            Close_File (Null_File);
            raise;
      end;
      Close_File (Null_File);

      declare
         --  Add one to Buffer_Length for ASCII.NUL terminator.
         subtype Print_Buffer_Index_T is Positive
           range 1 .. Buffer_Length + 1;
         Print_Buffer : String (Print_Buffer_Index_T);
      begin
         --  Place a sentinel
         Print_Buffer (Print_Buffer'Last) := ' ';

         Result := sprintf_li (Print_Buffer (Print_Buffer'First)'Address,
                               Actual_Format (Actual_Format'First)'Address,
                               Value);

         if Result < 0 then
            raise POSIX_Error;
         end if;

         if Result /= Buffer_Length
           or else Print_Buffer (Print_Buffer'Last) /= ASCII.NUL
         then
            raise Error;
         end if;

         return Print_Buffer (1 .. Buffer_Length);
      end;
   end Long_Integer_Image;

   --  LONG FLOAT INPUT/OUTPUT -----------------------------------------------

   procedure Put_Long_Float
     (File    : in     File_T := Standard_Output;
      Value   : in     Long_Float;
      Format  : in     String;
      Written :    out Natural)
   is
      function fprintf_d (stream : in System.Address;
                          fmt    : in System.Address;
                          arg    : in Long_Float) return Integer;
      pragma Interface (C, fprintf_d);

      Result : Integer;
   begin  --  Put_Long_Float
      if not Is_Writable (File) then
         raise Usage_Error;
      end if;

      Result := fprintf_d (stream => File.Address,
                           fmt    => Format (Format'First)'Address,
                           arg    => Value);

      if Result < 0 then
         raise POSIX_Error;
      end if;

      Written := Natural'(Result);
   end Put_Long_Float;

   function Long_Float_Format (Format : in String) return String
   is
   begin
      if Format = "" then
         return Long_Float_Format_C;
      else
         return Format;
      end if;
   end Long_Float_Format;

   function Long_Float_Image
     (Value  : in Long_Float;
      Format : in String := "") return String
   is
      function sprintf_d (s   : in System.Address;
                          fmt : in System.Address;
                          arg : in Long_Float) return Integer;
      pragma Interface (C, sprintf_d);

      Actual_Format : constant String := Long_Float_Format (Format);
      Null_File     : File_T;
      Buffer_Length : Natural;
      Result        : Integer;
   begin  --  Long_Float_Image
      Open_Null (Null_File);
      begin
         Put_Long_Float (File    => Null_File,
                         Value   => Value,
                         Format  => Actual_Format,
                         Written => Buffer_Length);
      exception
         when others =>
            Close_File (Null_File);
            raise;
      end;
      Close_File (Null_File);

      declare
         --  Add one to Buffer_Length for ASCII.NUL terminator.
         subtype Print_Buffer_Index_T is Positive
           range 1 .. Buffer_Length + 1;
         Print_Buffer : String (Print_Buffer_Index_T);
      begin
         --  Place a sentinel
         Print_Buffer (Print_Buffer'Last) := ' ';

         Result := sprintf_d (Print_Buffer (Print_Buffer'First)'Address,
                              Actual_Format (Actual_Format'First)'Address,
                              Value);

         if Result < 0 then
            raise POSIX_Error;
         end if;

         if Result /= Buffer_Length
           or else Print_Buffer (Print_Buffer'Last) /= ASCII.NUL
         then
            raise Error;
         end if;

         return Print_Buffer (1 .. Buffer_Length);
      end;
   end Long_Float_Image;

   --  LONG LONG FLOAT INPUT/OUTPUT ------------------------------------------

   procedure Put_Long_Long_Float
     (File    : in     File_T := Standard_Output;
      Value   : in     Long_Long_Float;
      Format  : in     String;
      Written :    out Natural)
   is
      function fprintf_ld (stream : in System.Address;
                           fmt    : in System.Address;
                           arg    : in Long_Long_Float) return Integer;
      pragma Interface (C, fprintf_ld);

      Result : Integer;
   begin  --  Put_Long_Long_Float
      if not Is_Writable (File) then
         raise Usage_Error;
      end if;

      Result := fprintf_ld (stream => File.Address,
                            fmt    => Format (Format'First)'Address,
                            arg    => Value);

      if Result < 0 then
         raise POSIX_Error;
      end if;

      Written := Natural'(Result);
   end Put_Long_Long_Float;

   function Long_Long_Float_Format (Format : in String) return String
   is
   begin
      if Format = "" then
         return Long_Long_Float_Format_C;
      else
         return Format;
      end if;
   end Long_Long_Float_Format;

   function Long_Long_Float_Image
     (Value  : in Long_Long_Float;
      Format : in String := "") return String
   is
      function sprintf_ld (s   : in System.Address;
                           fmt : in System.Address;
                           arg : in Long_Long_Float) return Integer;
      pragma Interface (C, sprintf_ld);

      Actual_Format : constant String := Long_Long_Float_Format (Format);
      Null_File     : File_T;
      Buffer_Length : Natural;
      Result        : Integer;
   begin  --  Long_Long_Float_Image
      Open_Null (Null_File);
      begin
         Put_Long_Long_Float (File    => Null_File,
                              Value   => Value,
                              Format  => Actual_Format,
                              Written => Buffer_Length);
      exception
         when others =>
            Close_File (Null_File);
            raise;
      end;
      Close_File (Null_File);

      declare
         --  Add one to Buffer_Length for ASCII.NUL terminator.
         subtype Print_Buffer_Index_T is Positive
           range 1 .. Buffer_Length + 1;
         Print_Buffer : String (Print_Buffer_Index_T);
      begin
         --  Place a sentinel at the end.
         Print_Buffer (Print_Buffer'Last) := ' ';

         Result := sprintf_ld (Print_Buffer (Print_Buffer'First)'Address,
                               Actual_Format (Actual_Format'First)'Address,
                               Value);

         if Result < 0 then
            raise POSIX_Error;
         end if;

         if Result /= Buffer_Length
           or else Print_Buffer (Print_Buffer'Last) /= ASCII.NUL
         then
            raise Error;
         end if;

         return Print_Buffer (1 .. Buffer_Length);
      end;
   end Long_Long_Float_Image;

end C_Standard_IO;
