--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with System; use System;

-- STM32F4xx CRC calculation unit

package STM32F4.CRC_calculation_unit is

private

   -- CRC_CR Control

   type CRC_CR is record
      RESET    : Resetting; -- (write only)
      Reserved : Bits_31;
   end record with Volatile, Size => Word'Size;

   for CRC_CR use record
      RESET    at 0 range  0 ..  0; -- (write only)
      Reserved at 0 range  1 .. 31;
   end record;

   -- CRC_Register

   type CRC_Register is record
      DR         : Bits_32; -- Data (read & write)
      IDR        : Bits_8;  -- Independent data (read & write)
      Reserved_1 : Bits_24;
      CR         : CRC_CR;  -- Control
   end record with Volatile, Size => 16#0C# * Byte'Size;

   for CRC_Register use record
      DR         at 16#00# range  0 .. 31; -- Data (read & write)
      IDR        at 16#04# range  0 ..  7; -- Independent data (read & write)
      Reserved_1 at 16#04# range  8 .. 31;
      CR         at 16#08# range  0 .. 31; -- Control
   end record;

   -- Set register addresse

   CRC : CRC_Register with
     Volatile, Address => System'To_Address (Base_CRC), Import;

end STM32F4.CRC_calculation_unit;
