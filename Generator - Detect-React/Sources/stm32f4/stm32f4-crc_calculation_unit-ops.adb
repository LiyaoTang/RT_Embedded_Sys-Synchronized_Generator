package body STM32F4.CRC_calculation_unit.Ops is

   procedure Reset_CRC is

   begin
      CRC.CR.RESET := Reset;
   end Reset_CRC;

   procedure Add_Data (Data : Bits_32) is

   begin
      CRC.DR := Data;
   end Add_Data;

   function Read_CRC return Bits_32 is (CRC.DR);

end STM32F4.CRC_calculation_unit.Ops;
