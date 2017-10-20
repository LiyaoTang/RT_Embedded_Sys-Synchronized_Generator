--
-- Uwe R. Zimmer, Australia 2015
--

-- STM32F4xx CRC calculation unit Operations

package STM32F4.CRC_calculation_unit.Ops is

   procedure Reset_CRC;
   procedure Add_Data (Data : Bits_32);

   function Read_CRC return Bits_32;

end STM32F4.CRC_calculation_unit.Ops;
