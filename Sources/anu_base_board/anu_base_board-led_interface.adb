--
-- Uwe R. Zimmer, Australia 2015
--

--
-- Uwe R. Zimmer, Australia 2015
--

with ANU_Base_Board.Config;           use ANU_Base_Board.Config;
with LED_Handling;                    use LED_Handling;
with STM32F4.General_purpose_IOs;     use STM32F4.General_purpose_IOs;
with STM32F4.General_purpose_IOs.Ops; use STM32F4.General_purpose_IOs.Ops;

package body ANU_Base_Board.LED_Interface is

   function To_Wire (LED : LEDs) return Port_Pin is

      (LED_Wires (LED.Port, LED.Side)) with Inline;

   procedure On (LED : LEDs) is

   begin
      Set (To_Wire (LED));
   end On;

   procedure Off (LED : LEDs) is

   begin
      Reset (To_Wire (LED));
   end Off;

   procedure All_On is

   begin
      for Port in Com_Ports loop
         for Side in Indicators loop
            On ((Port, Side));
         end loop;
      end loop;
   end All_On;

   procedure All_Off is

   begin
      for Port in Com_Ports loop
         for Side in Indicators loop
            Off ((Port, Side));
         end loop;
      end loop;
   end All_Off;

   procedure Toggle (LED : LEDs) is

   begin
      Atomic_Switch.Toggle (To_Wire (LED));
   end Toggle;

begin
   for Port in Com_Ports loop
      for Side in Indicators loop
         Initialize (LED_Wires (Port, Side));
      end loop;
   end loop;
   All_Off;
end ANU_Base_Board.LED_Interface;
