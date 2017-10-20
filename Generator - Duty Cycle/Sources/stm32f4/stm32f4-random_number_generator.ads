--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with System; use System;

-- STM32F4xx Random number generator

package STM32F4.Random_number_generator is

private

   -- RNG_CR Control

   type RNG_CR is record
      Reserved_1 : Bits_2;
      RNGEN      : Enabler; -- Random number generator enable (read & write)
      IE         : Enabler; -- Interrupt enable (read & write)
      Reserved_2 : Bits_28;
   end record with Volatile, Size => Word'Size;

   for RNG_CR use record
      Reserved_1 at 0 range  0 ..  1;
      RNGEN      at 0 range  2 ..  2; -- Random number generator enable (read & write)
      IE         at 0 range  3 ..  3; -- Interrupt enable (read & write)
      Reserved_2 at 0 range  4 .. 31;
   end record;

   -- RNG_SR Status

   type RNG_SR is record
      DRDY       : Readiness; -- Data ready (read only)
      CECS       : Status;    -- Clock error current status (read only)
      SECS       : Status;    -- Seed error current status (read only)
      Reserved_1 : Bits_2;
      CEIS       : Occurance; -- Clock error interrupt status (read & clear on write)
      SEIS       : Occurance; -- Seed error interrupt status (read & clear on write)
      Reserved_2 : Bits_25;
   end record with Volatile, Size => Word'Size;

   for RNG_SR use record
      DRDY       at 0 range  0 ..  0; -- Data ready (read only)
      CECS       at 0 range  1 ..  1; -- Clock error current status (read only)
      SECS       at 0 range  2 ..  2; -- Seed error current status (read only)
      Reserved_1 at 0 range  3 ..  4;
      CEIS       at 0 range  5 ..  5; -- Clock error interrupt status (read & clear on write)
      SEIS       at 0 range  6 ..  6; -- Seed error interrupt status (read & clear on write)
      Reserved_2 at 0 range  7 .. 31;
   end record;

   -- RNG_Register

   type RNG_Register is record
      CR : RNG_CR;  -- Control
      SR : RNG_SR;  -- Status
      DR : Bits_32; -- Data
   end record with Volatile, Size => 16#0C# * Byte'Size;

   for RNG_Register use record
      CR at 16#00# range  0 .. 31; -- Control
      SR at 16#04# range  0 .. 31; -- Status
      DR at 16#08# range  0 .. 31; -- Data
   end record;

   -- Set register addresse

   RNG : RNG_Register with
     Volatile, Address => System'To_Address (Base_RNG), Import;

end STM32F4.Random_number_generator;
