--
-- Uwe R. Zimmer, Australia 2015
--

package Discovery_Board.Button is

   pragma Elaborate_Body;

   type Blue_Button_State is (Off, On);

   function Current_Blue_Button_State return Blue_Button_State;

end Discovery_Board.Button;
