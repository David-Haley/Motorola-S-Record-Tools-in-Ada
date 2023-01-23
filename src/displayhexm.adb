-- Tool for displaying the contents of Motorola S record files
-- Author    : David Haley
-- Created   : 21/01/2023
-- Last Edit : 24/01/2023
-- 20230124 : Correction to allow multiple errors in the one hex file to be
-- Reported.

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces; use Interfaces;
with Motorola_Proms; use Motorola_Proms;
with Hex_Output;

procedure DisplayHexM is

   package Address_IO is new Hex_Output (Prom_Addresses);
   use Address_IO;

   package Byte_IO is new Hex_Output (Byte);
   use Byte_IO;

   package Checksum_IO is new Hex_Output (Check_Sums);
   use Checksum_IO;

   Bytes_per_Line : constant Prom_Addresses := 16#10#;

   Hex_File : aliased File_Type;
   Prom : Proms;
   Line_Low, Line_High : Prom_Addresses;
   Finished, Resuming : Boolean := False;

begin -- DisplayHexM
   Put_Line ("DisplayHexM version 20230124");
   if Argument_Count = 0 then
      Put_Line ("Usage DisplayHexM Hex_File_Name");
   else
      Open (Hex_File, In_File, Argument (1));
      Put_Line ("Hex file full path:");
      Put_Line (Name (Hex_File));
      while not Finished loop
         begin -- reading exception block
            if Resuming then
               Resume_Create (Prom);
            else
               Create_Prom (Prom, Hex_File'Unchecked_Access);
            end if; -- Resuming
            Finished := True;
         exception
            when Ee : Unexpected_EOF =>
               Put_Line (Exception_Message (Ee));
               Finished := True;
            when Eo : others =>
               Put_Line (Exception_Message (Eo));
               Resuming := True;
         end; -- reading exception block
      end loop; -- not Finished
      Close (Hex_File);
      if Length (Get_Header (Prom)) /= 0 then
         Put_Line ("Header information:");
         Put_Line (To_String (Get_Header (Prom)));
      end if; -- Length (Get_Header (Prom)) /= 0
      Put ("Address range: ");
      Put (Get_Device_Lower_Bound (Prom));
      Put (" .. ");
      Put (Get_Device_Upper_Bound (Prom));
      New_Line;
      Put ("Checksum for the above limits: ");
      Put (Limit_Check_Sum (Prom));
      New_Line;
      Put ("Entry address: ");
      Put (Get_Entry_Address (Prom));
      New_Line;
      Line_low := Get_Device_Lower_Bound (Prom);
      if Line_Low mod Bytes_per_Line = 0 then
         Line_High := Line_Low + Bytes_per_Line - 1;
      else
         Line_High := (Line_Low / Bytes_per_Line) * Bytes_per_Line - 1;
      end if; -- Line_Low mod Bytes_per_Line = 0
      loop -- write one row
         Put (Line_Low);
         Put (": ");
         for A in Prom_Addresses range Line_Low .. Line_High loop
            Put (' ');
            Put (Read_Data (Prom, A), 2);
         end loop; -- Prom_Addresses range Line_Low .. Line_High
         Set_Col (59);
         for A in Prom_Addresses range Line_Low .. Line_High loop
            if not Is_Control (Character'Val (Read_Data (Prom, A))) then
               Put (Character'Val (Read_Data (Prom, A)));
            else
               Put ('.');
            end if; -- not Is_Control (Character'Val (Read_Data (Prom, A)))
         end loop; -- Prom_Addresses range Line_Low .. Line_High
         New_Line;
         exit when Line_Low + Bytes_per_Line > Get_Device_Upper_Bound (Prom);
         Line_Low := Line_Low + Bytes_per_Line;
         if Line_Low + Bytes_per_Line - 1 <= Get_Device_Upper_Bound (Prom) then
            Line_High := Line_Low + Bytes_per_Line - 1;
         else
            Line_High := Get_Device_Upper_Bound (Prom);
         end if; -- Line_Low + Bytes_per_Line - 1 <= ...
      end loop;  -- write one row
   end if; -- Argument_Count = 0
end DisplayHexM;
