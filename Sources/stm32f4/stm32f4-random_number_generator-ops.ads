--
-- Uwe R. Zimmer, Australia 2015
--

-- STM32F4xx Random number generator Operations

package STM32F4.Random_number_generator.Ops is

   procedure Random_Enable  with Inline;
   procedure Random_Disable with Inline;

   procedure Random_Enable_Interrupt  with Inline; -- HASH_RNG_Interrupt
   procedure Random_Disable_Interrupt with Inline; -- HASH_RNG_Interrupt

   function Random_Is_Enabled return Boolean with Inline;

   -- Random_Data:
   -- If called too frequent then there might be a maximum delay of
   -- 40 RNG_CLK cylcles.
   -- The generation of a new random number is automatically started after
   -- every call.

   function Random_Data return Bits_32 with Inline;

   function Data_Ready  return Boolean with Inline;
   function Clock_Error return Boolean with Inline;
   function Seed_Error  return Boolean with Inline;

   function Clock_Error_Interrupt return Boolean with Inline;
   function Seed_Error_Interrupt  return Boolean with Inline;

   procedure Clear_Clock_Error_Interrupt with Inline;
   procedure Clear_Seed_Error_Interrupt  with Inline;

end STM32F4.Random_number_generator.Ops;
