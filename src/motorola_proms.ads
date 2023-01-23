-- This file specifies the Motorola_Prom Package.
-- Author    : David Haley
-- Created   : 25/08/1997
-- Last Edit : 21/01/2023
-- 20/06/1998 CheckSum function added.
-- 14/03/1999 ReadCheckSum added and CheckSum replaced by WriteCheckSum.
-- 17/11/2000 Supported Address range increased to 4Mb.
-- 21/03/2002 Constructor without parametres added
-- 02/08/2002 Conversion to Ada
-- 05/08/2002 Limit_Check_Sum added
-- 10/10/2002 Check_Sums changed to 16 bits
-- 01/11/2002 Write_String added
-- 06/03/2003 Unexpected_EOL exception added.
-- 12/03/2003 File access variable added.
-- 18/03/2003 Prom_Addresses changed to subtype of Interfaces.Unsigned_32. File
--            access declaration altered to derived type of Ada.Text_IO.
-- 21/01/2023 Improved exception handling.  Update to S0 record reading and
--            provision for S0 write; Provision for Setting and reading entry
--            address and extension to the full extended exorciser range.
-- 22/01/2023 Check_Sums now Unsigned_32.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

package Motorola_Proms is

   type Proms is limited private;

   subtype Prom_Addresses is Unsigned_32 range 0 .. 16#FFFFFF#;

   Minimum_Address : constant Prom_Addresses := Prom_Addresses'First;
   Maximum_Address : constant Prom_Addresses := Prom_Addresses'Last;

   type Byte is new Interfaces.Unsigned_8;

   Empty : constant Byte := 16#FF#;
   -- Applicable to 27xx, 27xxx and similar devices which default to FF before
   -- programming.

   type Check_Sums is new Unsigned_32;

   type Hex_File_Access_Type is new File_Access; 

   Bad_Limits, Bad_Start_Of_Record, Bad_Record_Type, Hex_Error, Bad_Checksum,
   Unexpected_Characters, Unexpected_EOL, Unexpected_EOF, Memory_Size_Exceded :
   exception;
   
   procedure Clear (Prom : out Proms);
   -- Clears all prom date only needs to be called if the same Prom is to be
   -- used with different data.

   procedure Create_Prom (Prom : out Proms; 
                          Hex_File_Access : in Hex_File_Access_Type);
   -- Create a Prom from a Motorola S record file

   procedure Resume_Create (Prom : in out Proms);
   -- resumes reading of an S record file after an exception has occured.

   procedure Write_Data (Prom : in out Proms; Address : in Prom_Addresses;
                         Data_To_Write : in Byte);
   -- Writes data to Prom

   procedure Write_String (Prom : in out Proms; Text : in String;
                           Start_At : Prom_Addresses);
   -- copies a string of arbitary length into consecutive Prom bytes with the
   -- first character being placed in the location Start_At.

   procedure Set_Device_Limits (Prom : in out Proms;
                                Lower_Bound, Upper_Bound : in Prom_Addresses);
   -- Sets bounds for check sum calculation and writing of S record File.
   
   procedure Write_Header (Prom : in out Proms;
                           Text : in String);
   -- Creates or overwrites header data (S0 record). the Text cannot exceed
   -- 252 bytes, allows for the address (0000) and checksum.
   
   procedure Set_Entry_Address (Prom : in out Proms;
                                Address : Prom_Addresses);
   -- sets the entry address, to be valid the address must lie in the range of
   -- the prom limits.

   function Read_Data (Prom : in Proms;
                       Address : in Prom_Addresses) return Byte;
   -- Reads data from Prom
   
   function Get_Header (Prom : in Proms) return Unbounded_String;
   -- returns the header string if any
   
   function Get_Entry_Address (Prom : in Proms) return Prom_Addresses;
   -- returns entru Affress from end record

   function Get_Device_Lower_Bound (Prom : in Proms) return Prom_Addresses;
   -- Returns current lower bound of Prom

   function Get_Device_Upper_Bound (Prom : in Proms) return Prom_Addresses;
   -- Returns current upper bound of Prom

   procedure Put (Output_File : in File_Type; Prom : in out Proms);
   -- Writes Prom Data to a file

   function Compare (Prom_One, Prom_Two : in Proms) return boolean;
   -- Tests for equality of the Prom data

   function Read_Check_Sum (Prom : in Proms) return Check_Sums;
   -- Call only valid if Prom Created by reading a Hex file

   function Write_Check_Sum (Prom : in Proms) return Check_Sums;
   -- returns ckeck sum an S record file between the lower and upper bounds.

   function Limit_Check_Sum (Prom : in Proms) return Check_Sums;
   -- returns Eprom ckeck sum between the lower and upper bounds.

private

   type Data_Arrays is array (Prom_Addresses) of Byte;
   pragma pack (Data_Arrays);

   type Format is (Exorcisor, Extended_Exorcisor);

   type Proms is record
      Header : Unbounded_String := Null_Unbounded_String;
      Data : Data_Arrays := (others => Empty);
      Low : Prom_Addresses := Maximum_Address;
      High : Prom_Addresses := Minimum_Address;
      Record_Format : Format := Exorcisor;
      Input_Access : Hex_File_Access_Type := null;
      File_Read_Check_Sum : Check_Sums := 0;
      End_Record_Found, Resuming : Boolean := False;
      Entry_Address : Prom_Addresses := 0;
   end record; -- Proms

end Motorola_Proms;
