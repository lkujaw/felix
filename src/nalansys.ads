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
--  File:          nalansys.ads (Ada Package Specification)
--  Language:      Ada (1987) [1]
--  Author:        Lev Kujawski
--  Description:   X/Open Native Language System [2] interface for Ada
--
--  References:
--  [1] Programming languages - Ada, ISO/IEC 8652:1987, 15 Jun. 1987.
--  [2] X/Open, Internationalisation Guide Version 2, G304, Jul. 1993.
------------------------------------------------------------------------------

with System;

package Native_Language_System is
   pragma Preelaborate;

   Error       : exception;
   POSIX_Error : exception;
   Usage_Error : exception;

   type Category_T is (LC_ALL,
                       LC_COLLATE,
                       LC_CTYPE,
                       LC_MESSAGES,
                       LC_MONETARY,
                       LC_NUMERIC,
                       LC_TIME);

   type Locale_T is limited private;

   function Locale_String (Locale : in Locale_T) return String;

   --  Equivalent of setlocale()
   procedure Set_Locale (Category   : in     Category_T;
                         New_Locale : in     String;
                         Old_Locale : in out Locale_T);

   procedure Swap_Locale (Category : in     Category_T;
                          Locale   : in out Locale_T);

   type Catalog_T is limited private;

   --  Equivalent of catopen()
   procedure Open_Catalog (Catalog   : in out Catalog_T;
                           With_Name : in     String);

   --  Equivalent of catclose()
   procedure Close_Catalog (Catalog : in out Catalog_T);

   --  Equivalent of catgets()
   function Message
     (From_Catalog   : in Catalog_T;
      Set_Number     : in Positive;
      Message_Number : in Positive) return String;

private

   type Catalog_T is
      record
         Address : System.Address := System.Null_Address;
      end record;
   pragma Pack (Catalog_T);

   type Locale_T is
      record
         Address : System.Address := System.Null_Address;
      end record;
   pragma Pack (Locale_T);

end Native_Language_System;
