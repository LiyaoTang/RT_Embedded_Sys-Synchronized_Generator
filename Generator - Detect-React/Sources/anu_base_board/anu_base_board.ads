--
-- Uwe R. Zimmer, Australia 2015
--

package ANU_Base_Board is

   type Com_Ports is range 1 .. 4;

   type Indicators is (Left, Right);

   L : Indicators renames Left;
   R : Indicators renames Right;

   type Com_Directions is (Receive, Transmit);

   Rx : Com_Directions renames Receive;
   Tx : Com_Directions renames Transmit;

   type Com_Features is (Enable, Data);

   En : Com_Features renames Enable;
   Da : Com_Features renames Data;

   type LEDs is record
      Port : Com_Ports;
      Side : Indicators;
   end record;

   type Coms is record
      Port      : Com_Ports;
      Direction : Com_Directions;
   end record;

end ANU_Base_Board;
