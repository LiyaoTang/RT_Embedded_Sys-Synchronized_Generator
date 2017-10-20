--
-- Uwe R. Zimmer, Australia 2015
--

package Discovery_Board.LED_Interface is

   pragma Elaborate_Body;

   procedure On  (LED : LEDs) with Inline;
   procedure Off (LED : LEDs) with Inline;

   procedure All_Off with Inline;
   procedure All_On  with Inline;

   procedure Toggle (LED : LEDs); -- Atomic, so potentially blocking

   subtype User_LED_Numbers is Natural range 3 .. 6;

   User_LEDs : array (User_LED_Numbers) of LEDs := (3 => Orange,
                                                    4 => Green,
                                                    5 => Red,
                                                    6 => Blue);

end Discovery_Board.LED_Interface;
