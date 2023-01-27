-- Rewrite of C++ version using Motorola_Proms
-- Author    : David Haley
-- Created   : 20/01/2023
-- Last Edit : 27/01/2023
-- 20230127 : Display Motorola_Proms library version and display full width of
-- Checksum.
-- 20230124 : Report multiple errors in files to be compared.

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces; use Interfaces;
with Motorola_Proms; use Motorola_Proms;
with Hex_Output;

with Motorola_Proms; use Motorola_Proms;

procedure Comphexm is

   package Address_IO is new Hex_Output (Prom_Addresses);
   use Address_IO;

   package Byte_IO is new Hex_Output (Byte);
   use Byte_IO;

   package Checksum_IO is new Hex_Output (Check_Sums);
   use Checksum_IO;

   procedure Read_Prom (Hex_File_Access : in Hex_File_Access_Type;
                        Prom : in out Proms;
                        Errors_Detected : in out Boolean) is

      Finished, Resuming : Boolean := False;

   begin -- Read_Prom
      while not Finished loop
         begin -- reading exception block
            if Resuming then
               Resume_Create (Prom);
            else
               Create_Prom (Prom, Hex_File_Access);
            end if; -- Resuming
            Finished := True;
         exception
            when Ee : Unexpected_EOF =>
               Put_Line (Exception_Message (Ee));
               Finished := True;
               Errors_Detected := True;
            when Eo : others =>
               Put_Line (Exception_Message (Eo));
               Resuming := True;
               Errors_Detected := True;
         end; -- reading exception block
      end loop; -- not Finished
   end Read_Prom;

   Hex_File : aliased File_Type;
   Prom_1, Prom_2 : Proms;
   Low, High : Prom_Addresses;
   Errors_Detected : Boolean := False;
   Answer : Character := 'Y';

begin -- Comphexm
   Put_Line ("CompHexM version 20230127");
   Put_Line ("Motorola_Proms library version " & Motorola_Proms'Body_Version);
   if Argument_Count /= 2 then
      Put_Line ("Usage CompHexM Hex_File_Name_1 Hex_File_Name_2");
   else
      New_Line;
      -- Reading Prom_1 and reporting details
      Open (Hex_File, In_File, Argument (1));
      Put_Line ("Hex file one full path:");
      Put_Line (Name (Hex_File));
      Read_Prom (Hex_File'Unchecked_Access, Prom_1, Errors_Detected);
      Close (Hex_File);
      if Length (Get_Header (Prom_1)) /= 0 then
         Put_Line ("Header information:");
         Put_Line (To_String (Get_Header (Prom_1)));
      end if; -- Length (Get_Header (Prom)) /= 0
      Put ("Address range: ");
      Put (Get_Device_Lower_Bound (Prom_1));
      Put (" .. ");
      Put (Get_Device_Upper_Bound (Prom_1));
      New_Line;
      Put ("Checksum for the above limits: ");
      Put (Limit_Check_Sum (Prom_1));
      New_Line;
      Put ("Entry address: ");
      Put (Get_Entry_Address (Prom_1));
      New_Line;
      New_Line;
      -- Reading Prom_2 and reporting details
      Open (Hex_File, In_File, Argument (2));
      Put_Line ("Hex file two full path:");
      Put_Line (Name (Hex_File));
      Read_Prom (Hex_File'Unchecked_Access, Prom_2, Errors_Detected);
      Close (Hex_File);
      if Length (Get_Header (Prom_2)) /= 0 then
         Put_Line ("Header information:");
         Put_Line (To_String (Get_Header (Prom_2)));
      end if; -- Length (Get_Header (Prom)) /= 0
      Put ("Address range: ");
      Put (Get_Device_Lower_Bound (Prom_2));
      Put (" .. ");
      Put (Get_Device_Upper_Bound (Prom_2));
      New_Line;
      Put ("Checksum for the above limits: ");
      Put (Limit_Check_Sum (Prom_2));
      New_Line;
      Put ("Entry address: ");
      Put (Get_Entry_Address (Prom_2));
      New_Line;
      Low := Prom_Addresses'Min (Get_Device_Lower_Bound (Prom_1),
                                 Get_Device_Lower_Bound (Prom_2));
      High := Prom_Addresses'Max (Get_Device_Upper_Bound (Prom_1),
                                  Get_Device_Upper_Bound (Prom_2));
      New_Line;
      Put ("Comparison address range: ");
      Put (Low);
      Put (" .. ");
      Put (High);
      New_Line;
      if Errors_Detected then
         Put ("Errors detected in hex files, continue comparison [ y | n ]: ");
         Get (Answer);
      end if; -- Errors_Detected
      if (Answer = 'y' or Answer = 'Y') and Compare (Prom_1, Prom_2) then
         Put_Line ("Files are functionally the same");
         Put
           ("Only valid for 27xx, 27xxx and similar devices that default to ");
         Put (Empty);
         Put_Line (" for unprogrammed addresses.");
      elsif Answer = 'y' or Answer = 'Y' then
         Put_Line ("Files are different, difference report follows:");
         for A in Prom_Addresses range Low .. High loop
            if Read_Data (Prom_1, A) /= Read_Data (Prom_2, A) then
               Put (A, 6);
               Put (": ");
               Put (Read_Data (Prom_1, A), 2);
               Put (' ');
               if not Is_Control (Character'Val (Read_Data (Prom_1, A))) then
                  Put (Character'Val (Read_Data (Prom_1, A)));
               else
                  Put ('.');
               end if; -- not Is_Control (Character'Val (Read_Data (Prom_1, A)))
               Put (" /= ");
               Put (Read_Data (Prom_2, A), 2);
               Put (' ');
               if not Is_Control (Character'Val (Read_Data (Prom_1, A))) then
                  Put (Character'Val (Read_Data (Prom_2, A)));
               else
                  Put ('.');
               end if; -- not Is_Control (Character'Val (Read_Data (Prom_2, A)))
               New_Line;
            end if; -- Read_Data (Prom_1, A) /= Read_Data (Prom_2, A)
         end loop; -- A in Prom_Addresses range Low .. High
         Put_Line ("** End of Comparison **");
      end if; -- (Answer = 'y' or Answer = 'Y') and Compare (Prom_1, Prom_2)
   end if; -- Argument_Count /2
end Comphexm;
