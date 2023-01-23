-- A generic package to text file output from unsigned input up to 64 bits.

-- Author    : David Haley
-- Created   : 18/03/2003
-- Last Edit : 22/01/2023
-- 20230122 : Support for numbers increased from 32 to 64 bits.

with Ada.Text_IO; use Ada.Text_IO;

generic

   type Num is mod <>;

package Hex_Output is
   
   Bits_per_Hex_Digit : constant := 4;

   Default_Width : Field := Num'Size / Bits_per_Hex_Digit;
   -- Note Field is declared in Ada.Text_IO
   Fill_Character : Character := '0';

   procedure Put (File : in File_Type; Item : in Num;
      Width : in Field := Default_Width);

   procedure Put (Item : in Num; Width : in Field := Default_Width);
   		
   procedure Put (To : out String; Item : in Num);

end Hex_Output;
