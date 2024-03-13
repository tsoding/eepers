with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;

procedure Test is
    type Item_Kind is (Key, Bomb, Checkpoint);
    type Item(Kind: Item_Kind := Key) is record
        case Kind is
            when Key | Checkpoint => null;
            when Bomb =>
                Cooldown: Integer;
        end case;
    end record;
    type Map is array (Natural range <>) of Item;
    type Map_Access is access Map;

    Items: Map_Access := null;
begin
    Items := new Map(1..10);
    Items(1) := (Kind => Bomb, Cooldown => 10);
    Put_Line(Items.all'Image);
end;
