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
--  File:          cstandio.ads (Ada Package Specification)
--  Language:      Ada (1987) [1]
--  Author:        Lev Kujawski
--  Description:   C Standard Input/Output (stdio.h) interface for Ada
--
--  References:
--  [1] Programming languages - Ada, ISO/IEC 8652:1987, 15 Jun. 1987.
------------------------------------------------------------------------------

with System;

package C_Standard_IO is
   pragma Preelaborate;

   POSIX_Error : exception;
   Usage_Error : exception;
   Error       : exception;

   type File_T is limited private;

   type File_Mode_T is (Read, Write, Read_Write);
   function File_Mode   (File : in File_T) return File_Mode_T;
   function Is_Readable (File : in File_T) return Boolean;
   function Is_Writable (File : in File_T) return Boolean;

   function Standard_Input  return File_T;
   function Standard_Output return File_T;
   function Standard_Error  return File_T;

   procedure Open_File (File      : in out File_T;
                        With_Name : in     String;
                        With_Mode : in     File_Mode_T);

   procedure Open_Null (File : in out File_T);

   procedure Close_File (File : in out File_T);

   procedure Put (File : in File_T;
                  Item : in Character);
   procedure Put (Item : in Character);

   procedure Put (File : in File_T;
                  Item : in String);
   procedure Put (Item : in String);

   procedure Put_Line (File : in File_T;
                       Item : in String);
   procedure Put_Line (Item : in String);

   --  IMAGE FUNCTIONS -------------------------------------------------------

   function Address_Image (Value  : in System.Address;
                           Format : in String := "") return String;

   function Integer_Image (Value  : in Integer;
                           Format : in String := "") return String;

   function Long_Integer_Image (Value  : in Long_Integer;
                                Format : in String := "") return String;

   function Long_Float_Image (Value  : in Long_Float;
                              Format : in String := "") return String;

   function Long_Long_Float_Image (Value  : in Long_Long_Float;
                                   Format : in String := "") return String;

private  --  C_Standard_IO ---------------------------------------------------

   type File_T is
      record
         Address : System.Address := System.Null_Address;
         Mode    : File_Mode_T;
      end record;
   pragma Pack (File_T);

end C_Standard_IO;
