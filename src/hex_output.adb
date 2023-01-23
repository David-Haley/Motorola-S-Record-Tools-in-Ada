-- A generic package to text file output from unsigned input up to 64 bits.

-- Author    : David Haley
-- Created   : 19/03/2003
-- Last Edit : 22/01/2023
-- 20230122 : Support for numbers increased from 32 to 64 bits.

with Interfaces; use Interfaces;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Hex_Output is
   
   subtype Hex_Numbers is Unsigned_64;

   procedure Put (File : in File_Type; Item : in Num;
      Width : in Field := Default_Width) is

   To : String (1 .. Width);

   begin -- Put
      Put (To, Item);
      Put (File, To);
   end Put; -- File, Item, Field_Width

   procedure Put (Item : in Num; Width : in Field := Default_Width) is

   To : String (1 .. Width);

   begin -- Put
      Put (To, Item);
      Put (To);
   end Put; -- Item, Field_Width
   		
   procedure Put (To : out String; Item : in Num) is

      subtype Nibble is Hex_Numbers range 0 .. 16#0F#;
      
      Nobble_Mask : Nibble := Nibble'Last;

      Look_Up : constant array (Nibble) of Character :=
         ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
         'A', 'B', 'C', 'D', 'E', 'F');

      Quotient : Hex_Numbers;
      Non_Zero : Boolean := True;

   begin -- Put
      Quotient := Hex_Numbers (Item);
      To := To'Length * Fill_Character;
      for I in reverse Field range 1 .. To'Length loop
         if Non_Zero then
            To (I) := Look_Up (Quotient and Nobble_Mask);
            Quotient := Shift_Right (Quotient, Bits_per_Hex_Digit);
            Non_Zero := Quotient > 0;
         end if;
      end loop;
      if Non_Zero then
         To := To'Length * '*';
      end if;
   end Put; -- String, Field_Width

end Hex_Output;
