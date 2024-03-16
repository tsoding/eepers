generic
    type Item is private;
package Queue is
    INIT_CAPACITY: constant Integer := 256;

    type Items_Array is array (Natural range <>) of Item;
    type Items_Array_Access is access Items_Array;

    type Queue is record
        Items: Items_Array_Access := null;
        Start: Integer := 0;
        Count: Integer := 0;
    end record;

    procedure Grow(Q: in out Queue);
    procedure Enqueue(Q: in out Queue; X: Item);
    function Dequeue(Q: in out Queue; X: out Item) return Boolean;
end Queue;
