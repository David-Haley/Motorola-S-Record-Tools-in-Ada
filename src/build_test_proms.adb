-- Program to build basic prom data for test purposes with known content.
-- Author    : David Haley
-- Created   : 23/01/2023
-- Last_Edit : 23/01/2023

with Ada.Text_IO; use Ada.Text_IO;
with Motorola_Proms; use Motorola_Proms;
with Interfaces; use Interfaces;

procedure Build_Test_Proms is

   Hex_File : File_Type;
   Prom : Proms;

begin -- Build_Test_Proms
   for D in Byte loop
      Write_Data (Prom, Prom_Addresses (D), D);
   end loop;
   Write_Header (Prom, "Short Address Test");
   Set_Entry_Address (Prom, Prom_Addresses (Byte'Last / 2));
   Create (Hex_File, Out_File, "Short_Address_Test.hex");
   Put (Hex_File, Prom);
   Close (Hex_File);
   Clear (Prom);
   for D in Byte loop
      Write_Data (Prom, Prom_Addresses (D) + 16#10000#, D);
   end loop;
   Write_Header (Prom, "Long Address Test");
   Set_Entry_Address (Prom, (Get_Device_Lower_Bound (Prom) +
                        Get_Device_Upper_Bound (Prom)) / 2);
   Create (Hex_File, Out_File, "Long_Address_Test.hex");
   Put (Hex_File, Prom);
   Close (Hex_File);
end Build_Test_Proms;
