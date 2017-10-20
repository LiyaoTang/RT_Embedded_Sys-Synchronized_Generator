--
-- Uwe R. Zimmer, Australia 2015
--

with STM32F4.General_purpose_IOs.Ops;     use STM32F4.General_purpose_IOs.Ops;
with STM32F4.Reset_and_clock_control.Ops; use STM32F4.Reset_and_clock_control.Ops;

package body LED_Handling is

   procedure Initialize (Wire : Port_Pin) is

      Port : GPIO_Ports renames Wire.Port;

   begin
      Enable (Port);
      Initialize_Pin (Wire  => Wire,
                      Mode  => General_purpose_output,
                      Out_T => Push_Pull,
                      Speed => Speed_High,
                      Pull  => No_Pull);
   end Initialize;

   procedure Initialize (Wires : Port_Pins) is

   begin
      for LED in Wires'Range loop
         Initialize (Wires (LED));
      end loop;
   end Initialize;

end LED_Handling;
