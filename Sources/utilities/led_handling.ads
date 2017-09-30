--
-- Uwe R. Zimmer, Australia 2015
--

with STM32F4.General_purpose_IOs; use STM32F4.General_purpose_IOs;

package LED_Handling is

   procedure Initialize (Wire  : Port_Pin);
   procedure Initialize (Wires : Port_Pins);

end LED_Handling;
