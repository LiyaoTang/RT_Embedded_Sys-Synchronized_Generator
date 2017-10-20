--
-- Uwe R. Zimmer, Australia 2015
--

with Discovery_Board.Config;          use Discovery_Board.Config;
with LED_Handling;                    use LED_Handling;
with STM32F4.General_purpose_IOs.Ops; use STM32F4.General_purpose_IOs.Ops;

package body Discovery_Board.LED_Interface is

   procedure On (LED : LEDs) is

   begin
      Set (LED_Wires (LED));
   end On;

   procedure Off (LED : LEDs) is

   begin
      Reset (LED_Wires (LED));
   end Off;

   procedure All_On is

   begin
      for LED of LED_Wires loop
         Set (LED);
      end loop;
   end All_On;

   procedure All_Off is

   begin
      for LED of LED_Wires loop
         Reset (LED);
      end loop;
   end All_Off;

   procedure Toggle (LED : LEDs) is

   begin
      Atomic_Switch.Toggle (LED_Wires (LED));
   end Toggle;

begin
   for LED of LED_Wires loop
      Initialize (LED);
   end loop;
   All_Off;
end Discovery_Board.LED_Interface;
