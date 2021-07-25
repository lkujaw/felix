separate (C_Standard_IO) function Null_Output return File_T
is
   Null_File : File_T;
begin
   Open_File (Null_File, "/dev/null", Write);
   return Null_File;
end Null_Output;
