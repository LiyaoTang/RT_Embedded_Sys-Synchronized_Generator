--
-- Uwe R. Zimmer, Australia 2015
--

-- STM32F4xx Random number generator Operations

package body STM32F4.Random_number_generator.Ops is

   procedure Random_Enable is -- RNG.CR.RNGEN := Enable;

      CR_Copy : RNG_CR := RNG.CR; -- Word access only

   begin
      CR_Copy.RNGEN := Enable;
      RNG.CR := CR_Copy;
   end Random_Enable;

   procedure Random_Disable is -- RNG.CR.RNGEN := Disable;

      CR_Copy : RNG_CR := RNG.CR; -- Word access only

   begin
      CR_Copy.RNGEN := Disable;
      RNG.CR := CR_Copy;
   end Random_Disable;

   function Random_Is_Enabled return Boolean is -- (RNG.CR.RNGEN = Enable);

      CR_Copy : constant RNG_CR := RNG.CR; -- Word access only

   begin
      return CR_Copy.RNGEN = Enable;
   end Random_Is_Enabled;

   procedure Random_Enable_Interrupt is -- RNG.CR.IE := Enable;

      CR_Copy : RNG_CR := RNG.CR; -- Word access only

   begin
      CR_Copy.IE := Enable;
      RNG.CR := CR_Copy;
   end Random_Enable_Interrupt;

   procedure Random_Disable_Interrupt is -- RNG.CR.IE := Disable;

      CR_Copy : RNG_CR := RNG.CR; -- Word access only

   begin
      CR_Copy.IE := Disable;
      RNG.CR := CR_Copy;
   end Random_Disable_Interrupt;

   function Random_Data return Bits_32 is

   begin
      while not Data_Ready loop
         null;
      end loop;
      return RNG.DR;
   end Random_Data;

   function Data_Ready  return Boolean is -- (RNG.SR.DRDY = Ready);

      SR_Copy : constant RNG_SR := RNG.SR; -- Word access only

   begin
      return SR_Copy.DRDY = Ready;
   end Data_Ready;

   function Clock_Error return Boolean is -- (RNG.SR.CECS = Faulty);

      SR_Copy : constant RNG_SR := RNG.SR; -- Word access only

   begin
      return SR_Copy.CECS = Faulty;
   end Clock_Error;

   function Seed_Error  return Boolean is -- (RNG.SR.SECS = Faulty);

      SR_Copy : constant RNG_SR := RNG.SR; -- Word access only

   begin
      return SR_Copy.SECS = Faulty;
   end Seed_Error;

   function Clock_Error_Interrupt return Boolean is -- (RNG.SR.CEIS = Occured);

      SR_Copy : constant RNG_SR := RNG.SR; -- Word access only

   begin
      return SR_Copy.CEIS = Occured;
   end Clock_Error_Interrupt;

   function Seed_Error_Interrupt  return Boolean is -- (RNG.SR.SEIS = Occured);

      SR_Copy : constant RNG_SR := RNG.SR; -- Word access only

   begin
      return SR_Copy.SEIS = Occured;
   end Seed_Error_Interrupt;

   procedure Clear_Clock_Error_Interrupt is -- RNG.SR.CEIS := Not_Occured;

      SR_Copy : RNG_SR := RNG.SR; -- Word access only

   begin
      SR_Copy.CEIS := Not_Occured;
      RNG.SR := SR_Copy;
   end Clear_Clock_Error_Interrupt;

   procedure Clear_Seed_Error_Interrupt  is -- RNG.SR.SEIS := Not_Occured;

      SR_Copy : RNG_SR := RNG.SR; -- Word access only

   begin
      SR_Copy.SEIS := Not_Occured;
      RNG.SR := SR_Copy;
   end Clear_Seed_Error_Interrupt;

end STM32F4.Random_number_generator.Ops;
