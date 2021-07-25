separate (C_Standard_IO) function Null_Output return File_T
is
   Null_File : File_T;
begin
   --  Access NUL via the Win32 device namespace
   Open_File (Null_File, "\\.\NUL", Write);
   return Null_File;
end Null_Output;
