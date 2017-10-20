--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with System; use System;

-- STM32F4xx Interrupts and Events

package STM32F4.Interrupts_and_Events is

   -- Range of external interrupts

   type External_Interrupt_No is range 0 .. 15;

   -- EXTI_Register

   subtype Lines is Bit_Range range 0 .. 22;

private

   -- EXTI_Register

   type EXTI_Register is record
      IMR        : Mask_Array    (Lines); -- Interrupt mask on line x (read & write)
      Reserved_1 : Bits_9;
      EMR        : Mask_Array    (Lines); -- Event mask on line x (read & write)
      Reserved_2 : Bits_9;
      RTSR       : Enabler_Array (Lines); -- Rising trigger event configuration bit of line x (read & write)
      Reserved_3 : Bits_9;
      FTSR       : Enabler_Array (Lines); -- Falling trigger event configuration bit of line x (read & write)
      Reserved_4 : Bits_9;
      SWIER      : Event_Array   (Lines); -- Software Interrupt on line x (read & write)
      Reserved_5 : Bits_9;
      PR         : Pending_Array (Lines); -- Pending bit (read & clear on write)
      Reserved_6 : Bits_9;
   end record with Volatile, Size => Byte'Size * 16#18#;

   for EXTI_Register use record
      IMR        at 16#00# range  0 .. 22;
      Reserved_1 at 16#00# range 23 .. 31;
      EMR        at 16#04# range  0 .. 22;
      Reserved_2 at 16#04# range 23 .. 31;
      RTSR       at 16#08# range  0 .. 22;
      Reserved_3 at 16#08# range 23 .. 31;
      FTSR       at 16#0C# range  0 .. 22;
      Reserved_4 at 16#0C# range 23 .. 31;
      SWIER      at 16#10# range  0 .. 22;
      Reserved_5 at 16#10# range 23 .. 31;
      PR         at 16#14# range  0 .. 22;
      Reserved_6 at 16#14# range 23 .. 31;
   end record;

   -- Set register address

   EXTI : EXTI_Register with
     Volatile, Address => System'To_Address (Base_EXTI), Import;

end STM32F4.Interrupts_and_Events;
