--
-- Uwe R. Zimmer, Australia 2015
--

with STM32F4.General_purpose_IOs; use STM32F4.General_purpose_IOs;

package Discovery_Board.Config is

   LED_Wires : constant array (LEDs) of Port_Pin :=
     (Green  => (Port => D, Pin => 12),
      Orange => (Port => D, Pin => 13),
      Red    => (Port => D, Pin => 14),
      Blue   => (Port => D, Pin => 15));

   Button_Wire : constant Port_Pin := (Port => A, Pin => 0);

end Discovery_Board.Config;
