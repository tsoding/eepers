with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;

procedure Test is
    type Item_Kind is (Key, Bomb);

    type Item(Kind: Item_Kind) is record
        case Kind is
           when Key => null;
           when Bomb =>
               Cooldown: Integer;
        end case;
    end record;
begin
    null;
end;
