--
-- Uwe R. Zimmer, Australia 2015
--

package ANU_Base_Board.LED_Interface is

   pragma Elaborate_Body;

   procedure On  (LED : LEDs) with Inline;
   procedure Off (LED : LEDs) with Inline;

   procedure All_Off with Inline;
   procedure All_On  with Inline;

   procedure Toggle (LED : LEDs); -- Atomic, so potentially blocking

end ANU_Base_Board.LED_Interface;
