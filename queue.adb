with Ada.Unchecked_Deallocation;

package body Queue is
    procedure Delete_Items_Array is new Ada.Unchecked_Deallocation(Items_Array, Items_Array_Access);

    procedure Grow(Q: in out Queue) is
        New_Items: constant Items_Array_Access := new Items_Array(0..Q.Items'Length*2-1);
    begin
        for Offset in Q.Items'Range loop
            New_Items(Offset) := Q.Items((Q.Start + Offset) mod Q.Items'Length);
        end loop;
        Delete_Items_Array(Q.Items);
        Q.Items := New_Items;
        Q.Start := 0;
    end;

    procedure Enqueue(Q: in out Queue; X: Item) is
    begin
        if Q.Items = null then
            Q.Items := new Items_Array(0..INIT_CAPACITY-1);
        end if;

        if Q.Count >= Q.Items'Length then
            Grow(Q);
        end if;

        Q.Items((Q.Start + Q.Count) mod Q.Items'Length) := X;
        Q.Count := Q.Count + 1;
    end;

    function Dequeue(Q: in out Queue; X: out Item) return Boolean is
    begin
        if Q.Count <= 0 then
            return False;
        end if;

        X := Q.Items(Q.Start);
        Q.Start := (Q.Start + 1) mod Q.Items'Length;
        Q.Count := Q.Count - 1;

        return True;
    end;
end Queue;
