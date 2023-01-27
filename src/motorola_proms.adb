-- This file implements the constructor and various function for the Proms
-- class.
-- Author    : David Haley
-- Created   : 25/08/1997
-- Last Edit : 24/01/2023
-- 23/06/1998 CheckSum function added, hex number formatting cleaned up.
-- 15/09/1998 NumberOfRecordBytes changed from unsigned char to int
--            this was necessary to prevent a search for 255 data bytes if
--            a negative byte count is calculated.
-- 12/03/1999 << operator changed to suppress the writing of records which
--            are entirely 0xFF. Bytes_Per_Record increased from 0x10 to 0x20
--            to reduce the size of the output file. Checksum calculation
--            when writing Extended Exorcisor corrected.
-- 14/03/1999 ReadCheckSum added and CheckSum replaced by WriteCheckSum.
-- 17/11/2000 File open mode corrected and other corrections for Win32
--            implementation.
-- 21/03/2002 Constructor without parametres added
-- 31/07/2002 Converted to Ada
-- 05/08/2002 Limit_Check_Sum added
-- 31/10/2002 Put and Write_Checksum corrected
-- 01/11/2002 Write_String added
-- 02/11/2002 Corrected supression of records that contain 16#0FF# in Put
-- 06/03/2003 Correction to exceptions in Resume_Create and restructured to
--            improve readability.
-- 24/03/2003 Hex_Output used to replace existing output procedures.
-- 22/10/2003 Comment records (type '0') excluded from File_Read_Check_Sum
--            calculations.
-- 21/01/2023 Improved exception handling.  Update to S0 record reading and
--            provision for S0 write; Provision for Setting and reading entry
--            address and extension to the full extended exorciser range.
-- 24/01/2023 Simplified reading of Strings.
-- 27/01/2023 Corrected various error messages.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Interfaces; use Interfaces;
with Hex_Output;

package body Motorola_Proms is

   Byte_Mask        : constant Prom_Addresses := 16#00_00FF#;
   Bytes_Per_Record : constant Prom_Addresses := 16#20#;

   subtype Record_Types is Natural with
        Static_Predicate => Record_Types in 0 | 1 | 2 | 8 | 9;

   Char_per_Byte : constant := 2;
   
   Short_Address_Bytes : constant := 2; -- Two byte address
   Long_Address_Bytes : constant := 3; -- Three Byte address
   -- Length of addreses in characters
   Short_Address_Length : constant := Short_Address_Bytes * Char_per_Byte;
   Long_Address_Length  : constant := Long_Address_Bytes * Char_per_Byte;

   subtype Byte_Buffers is String (1 .. Char_per_Byte);
   
   subtype Long_Address_Buffers is
     String (1 .. Char_per_Byte * Long_Address_Bytes);

   Checksum_Bytes : constant := 1;

   package Record_Number_IO is new Ada.Text_IO.Integer_IO (Positive_Count);

   package Byte_Out is new Hex_Output (Byte);

   package Address_Out is new Hex_Output (Prom_Addresses);

   procedure Clear (Prom : out Proms) is
   -- Clears all prom date only needs to be called if the same Prom is to be
   -- used with different data.

   begin -- Clear
      Prom.Header              := Null_Unbounded_String;
      Prom.Data                := (others => Empty);
      Prom.Low                 := Maximum_Address;
      Prom.High                := Minimum_Address;
      Prom.Record_Format       := Exorcisor;
      Prom.Input_Access        := null;
      Prom.File_Read_Check_Sum := 0;
      Prom.End_Record_Found    := False;
      Prom.Resuming            := False;
      Prom.Entry_Address       := 0;
   end Clear;

   procedure Create_Prom
     (Prom : out Proms; Hex_File_Access : in Hex_File_Access_Type)
   is

   begin -- Create_Prom
      Prom.Input_Access := Hex_File_Access;
      Resume_Create (Prom);
   end Create_Prom;

   function Address_Checksum (Address : in Prom_Addresses) return Byte is
     (Byte (Shift_Right (Address, Byte'Size * 2) and Byte_Mask) +
      Byte (Shift_Right (Address, Byte'Size) and Byte_Mask) +
      Byte (Address and Byte_Mask));

   procedure Resume_Create (Prom : in out Proms) is

      function Hex_To_Natural (Number_String : in String) return Natural is

      begin -- Hex_To_Natural
         return Natural'Value ("16#" & Number_String & '#');
      exception
         when others =>
            raise Hex_Error
              with """" & Number_String & """ not a valid hexidecimal number";
      end Hex_To_Natural;

      procedure Check_Start_Of_Record (S_Record : in Unbounded_String;
                                       Start_At : in out Positive) is

      begin -- Check_Start_Of_Record
         if Start_At > Length (S_Record) then
            raise Unexpected_EOL
              with "Expected ""S"" and found end of line";
         end if; -- Start_At > Length (S_Record)
         if Element (S_Record, Start_At) /= 'S' and
           Element (S_Record, Start_At) /= 's'
         then
            raise Bad_Start_Of_Record
              with "Expected 'S' and found '" & Element (S_Record, Start_At) &
            "'";
         end if; --  Element (S_Record, Start_at) /= 'S' and
         Start_At := Start_At + 1;
      end Check_Start_Of_Record;

      procedure Get_Record_Type (S_Record : in Unbounded_String;
                                 Start_At : in out Positive;
                                 Record_Type : out Record_Types;
                                 End_Record_Found : out Boolean) is

      begin -- Get_Record_Type is
         -- This may appear clumsy but the Static_Predicate was not causing
         -- an exception to be raised on conversion to Record_Types.
         if Natural'Value (Slice (S_Record, Start_At, Start_At)) in
           Record_Types then
            Record_Type := Record_Types'Value (Slice (S_Record, Start_At,
                                               Start_At));
         else
            raise Bad_Record_Type with
              "Expected 0, 1, 2, 8 or 9 and found '" &
              Element (S_Record, Start_AT) & "'";
         end if; -- Natural'Value (Slice (S_Record, Start_At, Start_At)) in
         End_Record_Found := Record_Type = 8 or Record_Type = 9;
         Start_At := Start_At + 1;
      exception
         when Index_Error =>
            raise Unexpected_EOL with "End of line before record type";
      end Get_Record_Type;

      procedure Get_Byte_Count (S_Record : in Unbounded_String;
                                Start_At : in out Positive;
                                Number_Of_Record_Bytes : out Byte) is

      begin -- Get_Byte_Count
         Number_Of_Record_Bytes :=
           Byte
             (Hex_To_Natural
                (Slice (S_Record, Start_At, Start_At + Char_per_Byte - 1)));
         Start_At := Start_At + Char_per_Byte;
      exception
         when Index_Error =>
            raise Unexpected_EOL
              with "End of line before byte count";
         when E : Hex_Error =>
            raise Hex_Error with "Bad byte count " & Exception_Message (E);
      end Get_Byte_Count;

      procedure Get_Address (S_Record : in Unbounded_String;
                             Start_At : in out Positive;
                             Offset : out Prom_Addresses;
                             Record_Type : in Record_Types;
                             Number_Of_Record_Bytes : in out Byte;
                             Record_Check_Sum : in out Byte;
                             Low, High, Entry_Address : in out Prom_Addresses;
                             Record_Format : in out Format) is

      begin -- Get_Address
         case Record_Type is
            when 0 | 1 | 9 => -- four hex digit address
               Offset := Prom_Addresses (Hex_To_Natural (Slice (S_Record,
                                         Start_At, Start_At +
                                           Short_Address_Length - 1)));
               Number_Of_Record_Bytes := Number_Of_Record_Bytes -
                 Short_Address_Bytes - 1;
               -- reduced by address and checksum
               Start_At := Start_At + Short_Address_Length;
            when 2 | 8 => -- six hex digit address
               Record_Format := Extended_Exorcisor;
               Offset := Prom_Addresses (Hex_To_Natural (Slice (S_Record,
                                         Start_At, Start_At +
                                           Long_Address_Length - 1)));
               Number_Of_Record_Bytes := Number_Of_Record_Bytes -
                 Long_Address_Bytes - 1;
               -- reduced by address and checksum
               Start_At := Start_At + Long_Address_Length;
         end case;  -- Record_Type
         Record_Check_Sum := Record_Check_Sum + Address_Checksum (Offset);
         if Record_Type = 1 or Record_Type = 2 then
            if Offset < Low then
               Low := Offset;
            end if; -- Offset < Low
            if Number_Of_Record_Bytes > 0 and
              Offset + Prom_Addresses (Number_Of_Record_Bytes) - 1 > High
            then
               High := Offset + Prom_Addresses (Number_Of_Record_Bytes) - 1;
            end if; -- Number_Of_Record_Bytes > 0 and
         end if; -- Record_Type = 1 or Record_Type = 2
         if Record_Type = 8 or Record_Type = 9 then
            Entry_Address := Offset;
         end if; -- Record_Type = 8 or Record_Type = 9
      exception
         when Index_Error =>
            raise Unexpected_EOL
              with "End of line before address";
         when E : Hex_Error =>
            raise Hex_Error with "Bad address " & Exception_Message (E);
      end Get_Address;

      procedure Get_Data_Bytes (S_Record : in     Unbounded_String;
                                Start_At : in out Positive;
                                Data : in out Data_Arrays;
                                Offset : in Prom_Addresses;
                                Number_Of_Record_Bytes : in Byte;
                                Record_Check_Sum : in out Byte) is

      begin -- Get_Data_Bytes
         for I in Natural range 1 .. Natural (Number_Of_Record_Bytes) loop
            Data (Offset + Prom_Addresses (I - 1)) :=
              Byte (Hex_To_Natural (Slice (S_Record, Start_At, Start_At +
                      Char_per_Byte - 1)));
            Record_Check_Sum := Record_Check_Sum +
              Byte (Hex_To_Natural (Slice (S_Record, Start_At,
                    Start_At + Char_per_Byte - 1)));
            -- Total for current record
            Start_At := Start_At + Char_per_Byte;
         end loop; -- for I in Natural range 1 .. Number_Of_Record_Bytes
      exception
         when Index_Error =>
            raise Unexpected_EOL
              with "Unexpected end of line reading data bytes";
         when E : Hex_Error =>
            raise Hex_Error with "Bad data " & Exception_Message (E);
      end Get_Data_Bytes;

      procedure Get_Header (S_Record : in Unbounded_String;
                            Start_At : in out Positive;
                            Offset : in Prom_Addresses;
                            Number_Of_Record_Bytes : in Byte;
                            Record_Check_Sum : in out Byte;
                            Header : out Unbounded_String) is

         Hex_Char : Byte;

      begin -- Get_Header
         Header := Null_Unbounded_String;
         if (Shift_Right (Offset, Byte'Size) and Byte_Mask) /= 0 then
            Header := Header &
              Character'Val (Shift_Right (Offset, Byte'Size) and Byte_Mask);
         end if; -- (Shift_Right(Offset, Byte'Size) and Byte_Mask) /= 0
         if (Offset and Byte_Mask) /= 0 then
            Header := Header & Character'Val (Offset and Byte_Mask);
         end if; -- (Offset and Byte_Mask) /= 0
      -- The address of an S0 record should be 0000 the above allows for this
      -- being used erroneously for text, e.g. Interlogic
         for C in Natural range 1 .. Natural (Number_Of_Record_Bytes) loop
            Hex_Char := Byte (Hex_To_Natural (Slice (S_Record, Start_At,
                              Start_At + Char_per_Byte - 1)));
            Header := Header & Character'Val (Hex_Char);
            Record_Check_Sum := Record_Check_Sum + Hex_Char;
            Start_At := Start_At + Char_per_Byte;
         end loop; -- C in Natural range 1 .. Natural (Number_Of_Record_Bytes)
      exception
         when Index_Error =>
            raise Unexpected_EOL
              with "Unexpected end of line reading header";
         when E : Hex_Error =>
            raise Hex_Error with "Bad header " & Exception_Message (E);
      end Get_Header;

      procedure Verify_Checksum (S_Record  : in Unbounded_String;
                                 Start_At : in out Positive;
                                 Check_Sum : in Byte) is

         Check_Sum_Buffer : Byte_Buffers;
         File_Check_Sum   : Byte;

      begin -- Verify_Checksum
         File_Check_Sum := Byte (Hex_To_Natural (Slice (S_Record, Start_At,
                                 Start_At + Char_per_Byte - 1)));
         if File_Check_Sum /= not Check_Sum then
            Byte_Out.Put (Check_Sum_Buffer, not Check_Sum);
            raise Bad_Checksum
              with "File checksum " &
              Slice (S_Record, Start_At, Start_At + Char_per_Byte - 1) &
              " does not match calculated checksum " & Check_Sum_Buffer;
         end if; -- File_Check_Sum /= not Check_Sum
         Start_At := Start_At + Char_per_Byte;
      exception
         when Index_Error =>
            raise Unexpected_EOL with "End of line before checksum";
         when E : Hex_Error =>
            raise Hex_Error with "Bad checksum " & Exception_Message (E);
      end Verify_Checksum;

      Record_Type : Record_Types;
      Number_Of_Record_Bytes : Byte;
      Offset : Prom_Addresses;
      Record_Check_Sum : Byte; -- Checksum for each record
      Input_File : File_Type renames Prom.Input_Access.all;
      S_Record : Unbounded_String;
      Start_At : Positive;
      Line_Number : Positive_Count;

   begin -- Resume_Create
      if not Prom.End_Record_Found then
         Line_Number := Line (Input_File);
         Get_Line (Input_File, S_Record);
         Start_At := 1;
         Prom.Resuming := True;
      end if; -- not Prom.End_Record_Found
      while not Prom.End_Record_Found loop
         Check_Start_Of_Record (S_Record, Start_At);
         Get_Record_Type
           (S_Record, Start_At, Record_Type, Prom.End_Record_Found);
         Get_Byte_Count (S_Record, Start_At, Number_Of_Record_Bytes);
         Record_Check_Sum := Number_Of_Record_Bytes;
         Get_Address
           (S_Record, Start_At, Offset, Record_Type, Number_Of_Record_Bytes,
            Record_Check_Sum, Prom.Low, Prom.High, Prom.Entry_Address,
            Prom.Record_Format);
         if Record_Type = 0 then
            Get_Header
              (S_Record, Start_At, Offset, Number_Of_Record_Bytes,
               Record_Check_Sum, Prom.Header);
         elsif Record_Type = 1 or Record_Type = 2 then
            Get_Data_Bytes
              (S_Record, Start_At, Prom.Data, Offset, Number_Of_Record_Bytes,
               Record_Check_Sum);
            Prom.File_Read_Check_Sum :=
              Prom.File_Read_Check_Sum + Check_Sums (Record_Check_Sum);
         end if; -- Record_Type = 0
         Verify_Checksum (S_Record, Start_At, Record_Check_Sum);
         if Start_At <= Length (S_Record) then
            raise Unexpected_Characters
              with "Unexpected characters """ &
              Slice (S_Record, Start_At, Length (S_Record)) &
              """ at end of line";
         end if; -- Start_At <= Length (S_Record)
         if not Prom.End_Record_Found then
            Line_Number := Line (Input_File);
            Get_Line (Input_File, S_Record);
            Start_At := 1;
         end if; -- not Prom.End_Record_Found
      end loop; -- not Prom.End_Record_Found
   exception
      when End_Error =>
         raise Unexpected_EOF
           with "End of file before end record";
      when E : others =>
         Raise_Exception (Exception_Identity (E),
            Exception_Message (E) & ", at line" & Line_Number'Img);
   end Resume_Create;

   procedure Write_Data (Prom : in out Proms;
                         Address : in Prom_Addresses;
                         Data_To_Write : in Byte) is

   begin -- Write_Data
      if Address < Prom.Low then
         Prom.Low := Address;
      end if;
      if Prom.High < Address then
         Prom.High := Address;
      end if;
      Prom.Data (Address) := Data_To_Write;
   end Write_Data; -- Write_Data

   procedure Write_String (Prom : in out Proms;
                           Text : in String;
                           Start_At : Prom_Addresses) is

      Address : Prom_Addresses;

   begin -- Write_String
      if Start_At < Prom.Low then
         Prom.Low := Start_At;
      end if;
      for String_Index in Integer range Text'First .. Text'Last loop
         begin
            Address := Start_At + Prom_Addresses (String_Index - Text'First);
         exception
            when Constraint_Error =>
               raise Memory_Size_Exceded;
         end;
         if Prom.High < Address then
            Prom.High := Address;
         end if;
         Prom.Data (Address) := Byte (Character'Pos (Text (String_Index)));
      end loop;
   end Write_String;

   procedure Write_Header (Prom : in out Proms;
                           Text : in String) is

   -- Creates or overwrites header data (S0 record). the Text cannot exceed
   -- 252 bytes, allows for the address (0000) and checksum.

   begin -- Write_Header
      Prom.Header := To_Unbounded_String (Text);
   end Write_Header;

   procedure Set_Entry_Address (Prom : in out Proms;
                                Address : Prom_Addresses) is

      -- sets the entry address, to be valid the address must lie in the range
      -- of the prom limits.

      Low_String, High_String : Long_Address_Buffers;

   begin -- Set_Entry_Address
      if Address < Get_Device_Lower_Bound (Prom) or
        Address > Get_Device_Upper_Bound (Prom)
      then
         Address_Out.Put (Low_String, Get_Device_Lower_Bound (Prom));
         Address_Out.Put (High_String, Get_Device_Upper_Bound (Prom));
         raise Memory_Size_Exceded
           with "Entry address must be within device limits " & Low_String &
           " .. " & High_String;
      else
         Prom.Entry_Address := Address;
      end if; -- Address < Get_Device_Lower_Bound (Prom)
   end Set_Entry_Address; -- Set_Entry_Address

   function Read_Data
     (Prom : in Proms; Address : in Prom_Addresses) return Byte is
     (Prom.Data (Address));

   function Get_Header (Prom : in Proms) return Unbounded_String is
     (Prom.Header);
   -- returns the header string if any

   function Get_Entry_Address (Prom : in Proms) return Prom_Addresses is
     (Prom.Entry_Address);
   -- returns entry Affress from end record

   procedure Set_Device_Limits (Prom : in out Proms;
                                Lower_Bound, Upper_Bound : in Prom_Addresses) is

   begin -- Set_Device_Limits
      Prom.Low  := Lower_Bound;
      Prom.High := Upper_Bound;
   end Set_Device_Limits;

   function Get_Device_Lower_Bound (Prom : in Proms) return Prom_Addresses is
     (Prom.Low);

   function Get_Device_Upper_Bound (Prom : in Proms) return Prom_Addresses is
     (Prom.High);

   procedure Put (Output_File : in File_Type;
                  Prom : in out Proms) is

      Exorcisor_Limit    : constant Prom_Addresses := 16#1_0000#;
      Address            : Prom_Addresses;
      Bytes_To_Write     : Prom_Addresses;
      Something_To_Write : Boolean;
      Record_Check_Sum   : Byte;

   begin -- Put
      if Length (Prom.Header) > 0 then
         -- Write header
         Put (Output_File, "S0");
         Byte_Out.Put
           (Output_File,
            Byte
              (Length (Prom.Header) + Short_Address_Bytes + Checksum_Bytes));
         Record_Check_Sum :=
           Byte (Length (Prom.Header) + Short_Address_Bytes + Checksum_Bytes);
         Address_Out.Put (Output_File, 0, Short_Address_Bytes * Char_per_Byte);
         for C in Positive range 1 .. Length (Prom.Header) loop
            Byte_Out.Put
              (Output_File, Byte (Character'Pos (Element (Prom.Header, C))));
            Record_Check_Sum :=
              Record_Check_Sum +
              Byte (Character'Pos (Element (Prom.Header, C)));
         end loop; -- C in Positive range 1 .. Length (Prom.Header)
         Byte_Out.Put (Output_File, not Record_Check_Sum);
         New_Line (Output_File);
      end if; -- Length (Prom.Header) > 0
      -- Write data bytes
      Address := Prom.Low;
      if Address < Exorcisor_Limit then
         Prom.Record_Format := Exorcisor;
      else
         Prom.Record_Format := Extended_Exorcisor;
      end if;
      while Address <= Prom.High loop
         Bytes_To_Write := Bytes_Per_Record;
         if (Prom.High - Address + 1) < Bytes_Per_Record then
            Bytes_To_Write := Prom.High - Address + 1;
         end if;
         Something_To_Write := False;
         for Add in Prom_Addresses range Address ..
             Address + Prom_Addresses (Bytes_To_Write) - 1
         loop
            Something_To_Write :=
              Something_To_Write or (Prom.Data (Add) /= Empty);
         end loop;
         if Something_To_Write then
            Record_Check_Sum := Byte (Bytes_To_Write);
            if Address + Prom_Addresses (Bytes_To_Write) - 1 >= Exorcisor_Limit
            then
               -- Extended Exorciser
               Put (Output_File, "S2");
               Byte_Out.Put
                 (Output_File,
                  Byte (Bytes_To_Write + Long_Address_Bytes + Checksum_Bytes));
               Prom.Record_Format := Extended_Exorcisor;
               Record_Check_Sum   :=
                 Record_Check_Sum + Long_Address_Bytes + Checksum_Bytes;
               -- + value of byte count
               Address_Out.Put
                 (Output_File, Address, Long_Address_Bytes * Char_per_Byte);
            else
               -- Exorcisor
               Put (Output_File, "S1");
               Byte_Out.Put
                 (Output_File,
                  Byte
                    (Bytes_To_Write + Short_Address_Bytes + Checksum_Bytes));
               Record_Check_Sum :=
                 Record_Check_Sum + Short_Address_Bytes + Checksum_Bytes;
               -- + value of byte count
               Address_Out.Put
                 (Output_File, Address, Short_Address_Bytes * Char_per_Byte);
            end if; --  Address + Prom_Addresses (Bytes_To_Write) - 1 >= ...
            Record_Check_Sum := Record_Check_Sum + Address_Checksum (Address);
            while Bytes_To_Write > 0 loop
               Record_Check_Sum := Record_Check_Sum + Prom.Data (Address);
               Byte_Out.Put (Output_File, Prom.Data (Address));
               Address        := Address + 1;
               Bytes_To_Write := Bytes_To_Write - 1;
            end loop; -- while Bytes_To_Write > 0
            Byte_Out.Put (Output_File, not Record_Check_Sum);
            New_Line (Output_File);
         else
            Address := Address + Prom_Addresses (Bytes_To_Write);
         end if; -- if Something_To_Write
      end loop; -- while Address <= Prom.High
      -- Write end record
      Address := Prom.Entry_Address;
      if Prom.Record_Format = Extended_Exorcisor then
         Put (Output_File, "S804");
         Record_Check_Sum := Long_Address_Bytes + Checksum_Bytes;
         Address_Out.Put
           (Output_File, Address, Long_Address_Bytes * Char_per_Byte);
      else
         Put (Output_File, "S903");
         Record_Check_Sum := Short_Address_Bytes + Checksum_Bytes;
         Address_Out.Put
           (Output_File, Address, Short_Address_Bytes * Char_per_Byte);
      end if; -- Prom.Record_Format = Extended_Exorcisor
      Record_Check_Sum :=
        Record_Check_Sum + Address_Checksum (Prom.Entry_Address);
      Byte_Out.Put (Output_File, not Record_Check_Sum);
      New_Line (Output_File);
   end Put;

   function Compare (Prom_One, Prom_Two : in Proms) return Boolean is
     (Prom_One.Data = Prom_Two.Data);

   function Read_Check_Sum (Prom : in Proms) return Check_Sums is
     (Prom.File_Read_Check_Sum);

   function Write_Check_Sum (Prom : in Proms) return Check_Sums is

      Data_Limits_Check_Sum : Check_Sums := 0;
      Address               : Prom_Addresses;
      Something_To_Write    : Boolean;
      Bytes_To_Write        : Prom_Addresses;

   begin -- Write_Check_Sum
      Address := Prom.Low;
      while Address <= Prom.High loop
         if Prom.High - Address + 1 < Prom_Addresses (Bytes_Per_Record) then
            Bytes_To_Write := Prom.High - Address + 1;
         else
            Bytes_To_Write := Bytes_Per_Record;
         end if;
         Something_To_Write := False;
         for Add in Prom_Addresses range Address ..
             Address + Prom_Addresses (Bytes_To_Write) - 1
         loop
            Something_To_Write :=
              Something_To_Write or Prom.Data (Add) /= Empty;
         end loop;
         if Something_To_Write then
            while Bytes_To_Write > 0 loop
               Data_Limits_Check_Sum :=
                 Data_Limits_Check_Sum + Check_Sums (Prom.Data (Address));
               Bytes_To_Write := Bytes_To_Write - 1;
               Address        := Address + 1;
            end loop; -- while Bytes_To_Write > 0
         else
            Address := Address + Prom_Addresses (Bytes_To_Write);
         end if; -- Something_To_Write
      end loop; -- while Address <= Prom.High
      return Data_Limits_Check_Sum;
   end Write_Check_Sum;

   function Limit_Check_Sum (Prom : in Proms) return Check_Sums is

      Data_Limits_Check_Sum : Check_Sums;

   begin -- Limit_Check_Sum
      Data_Limits_Check_Sum := 0;
      for Address in Prom_Addresses range Prom.Low .. Prom.High loop
         Data_Limits_Check_Sum :=
           Data_Limits_Check_Sum + Check_Sums (Prom.Data (Address));
      end loop; -- for Address in Prom_Addresses range Prom.Low ...
      return Data_Limits_Check_Sum;
   end Limit_Check_Sum;

end Motorola_Proms;
